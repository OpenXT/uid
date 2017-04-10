(*
 * Copyright (c) 2011 Citrix Systems, Inc.
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

open Stringext

module D = Debug.Debugger(struct let name="uid" end)

open D

let debug = ref true

let dbg fmt =
	let logger s = if !debug then Printf.printf "%s\n%!" s in
	Printf.ksprintf logger fmt

type conn_state =
{
	conn  : Async_conn.t;
	domid : int;
	mutable http_req_parser : Http.Request.state;
	mutable pending_requests: Http.Request.t list;
}

let get_loop server_conn = Async_conn.get_eventloop server_conn.conn
let get_domid conn = conn.domid

(* Request handling:

   To support asynchronous responses, a request_handler can return
   Pending.  In this case, the request is put on the pending queue,
   and any subsequent requests received on that connection are
   appended to this queue.  When the response is available, the
   finish_async_response function dequeues this first pending response
   on the connection, sends the response, and dispatches the remaining
   requests if any (naturally, until they are all dispatched or until
   any handler returns Pending).
*)

type request_status =
	| Pending
	| Done of Http.Response.t

type request_handler = conn_state -> Http.Request.t -> (* path *) string -> request_status

type dispatcher =
{
	get_file_handler : request_handler;
	meta_handler : request_handler;
	dbus_proxy_handler : request_handler;
	default_handler : request_handler;
}

let dispatcher = ((ref None) : dispatcher option ref)

let set_dispatcher disp = dispatcher := Some disp

(* Connection management *)

module Conns = Connection_table.Make(struct type conn = conn_state end)

let cleanup_and_close conn =
	Conns.remove_conn (Async_conn.get_handle conn.conn);
	Async_conn.close conn.conn

let send_resp conn resp =
	let respbuf = Buffer.create 512 in
	Http.Response.serialize respbuf resp;
	Async_conn.send conn.conn (Buffer.contents respbuf)

let handle_conn_error conn e =
	(match e with
	| Http.Request.Http_error e ->
		Printf.eprintf "Error parsing request: %s\n%!" (Http.Request.string_of_error e)
	| Http.Headers.Http_error e ->
		Printf.eprintf "Error parsing request headers: %s\n%!" (Http.Headers.string_of_error e)
	| Http.Payload.Http_error e ->
		Printf.eprintf "Error parsing request payload: %s\n%!" (Http.Payload.string_of_error e)
	| Jsonrpc.Invalid_request e ->
		Printf.eprintf "%s\n" (Jsonrpc.string_of_req_error e)
	| e ->
		Printf.eprintf "Unexpected error: %s%!" (Printexc.get_backtrace ())
	);
	cleanup_and_close conn

let enqueue_request conn r =
	conn.pending_requests <- (conn.pending_requests @ [ r ])

let get_default_headers () =
	(* Add more as needed. *)
	[ ("Server", [ "XCI_UID" ]) ]

let make_http_server_error msg = 
	let payload_content = Buffer.create 10 in
	Buffer.add_string payload_content msg;
	let payload = { 
		Http.Payload.content  = payload_content;
		Http.Payload.trailers = [];
	} in 
	Http.Response.make_response
		~payload:payload
		~headers:(get_default_headers ())
		Http.Response_header.Status_internal_server_error

let event_prefix = "/event"
and signal_reg_prefix = "/signal-register"
and signal_prefix = "/signal"

(* the url passed has the format /signal/queue_id/minimum_signal_id *)
let rec signal_handler con req path =
	let domid = get_domid con in
	try
		let qid, signal_id =
			let p = String.length signal_prefix + 1 in
			let suffix = String.sub path p (String.length path - p) in
			match String.split '/' suffix with
			| qid :: signal_id :: [] -> 
				  (try (int_of_string qid, Int64.of_string signal_id)
				  with _ -> raise (Invalid_argument suffix)
				  )
			| _ -> raise (Invalid_argument suffix)
		in
		let handle = Async_conn.get_handle con.conn in
		let response_for (domid,qid) =
			if Conns.has_conn handle then (
				match Dbus_signals.pop_json (domid,qid) signal_id with
				| None -> None
				| Some json ->
					  let buf = Buffer.create 1024 in
					  Buffer.add_string buf (Json.to_string json);
					  let payload = { Http.Payload.content = buf;
							  Http.Payload.trailers = [] } in
					  let resp = Http.Response.make_response
						  ~payload
						  ~headers:(Http.add_header "Content-Type" "application/json" (get_default_headers ()))
						  Http.Response_header.Status_ok in
					    Some resp
			) else (Dbus_signals.unset_queue_filled_cb (domid,qid); None)
		in
		  (match response_for (domid,qid) with
		    | Some r -> Done r
		    | None   ->
			Dbus_signals.set_queue_filled_cb (domid,qid) (
			  fun q -> match response_for q with
			    | Some r -> finish_async_resp con r; false
			    | None   -> true (* continue wait *) );
			Pending)
	with Invalid_argument suff ->
		Done 
			(make_http_server_error (Printf.sprintf "invalid request, failed to parse queue id & signal id: %s" suff))

and signal_register_handler con req path =
	let domid = get_domid con in
	let p = String.length signal_reg_prefix + 1 in
	(try
		let suffix = String.sub path p (String.length path - p) in
		let qid, intf_name = 
			match String.split '/' suffix with
			| id :: intf_name :: [] -> (int_of_string id, intf_name)
			| intf_name :: []       -> (0, intf_name)
			| _                     -> raise (Invalid_argument suffix)
		in
		dbg "registering signal handling for interface: %s queue %d" intf_name qid;
		Dbus_signals.listen_to_interface (domid,qid) intf_name
	with _ ->
		dbg "bad interface name, not registering!"
	);
	let resp = Http.Response.make_response 
			    ~headers:(get_default_headers ())
			    Http.Response_header.Status_ok
	in
	Done resp
		
and event_handler con req path = 
	let meth = req.Http.Request.request.Http.Request_header.meth in
	dbg "event handler triggered for %s %s ..." (Http.Request_header.string_of_meth meth) path;
  	let json = 
	  Json.Object [|
	     "last_event", Json.String "";
	     "events", Json.Array (Array.of_list []) |] in
	let buf = Buffer.create 2048 in
	Buffer.add_string buf (Json.to_string json);
	let payload = { Http.Payload.content = buf;
		        Http.Payload.trailers = [] } in
	let resp = (Http.Response.make_response 
		      ~payload
		      ~headers:(Http.add_header "Content-Type" "application/json" (get_default_headers ()))
		      Http.Response_header.Status_ok)
	in Done resp

and log_handler con req path =
	(* extract the logging level /log/level *)
	let meth = req.Http.Request.request.Http.Request_header.meth in
	let body = req.Http.Request.payload in
	let resp = match (meth,body) with
	 	   | Http.Request_header.Post, Some payload ->
			let p = String.length (Ui_config.get_log_prefix()) in
			let level  = int_of_string (String.right path (String.length path - p)) in
			let log = match level with
			  | 3 -> D.error "%s"
			  | 2 -> D.debug "%s"
			  | 1 -> D.warn "%s"
			  | _ -> D.info "%s" in
			log (Buffer.contents payload.Http.Payload.content);
			Http.Response.make_response
				~headers:(get_default_headers ())
				Http.Response_header.Status_ok
		  | _ ->
			Http.Response.make_response
				~headers:(get_default_headers ())
				Http.Response_header.Status_bad_request
	in Done resp

and get_handler_and_path r =
	let dispatcher = Opt.unbox !dispatcher in
	let url = r.Http.Request.request.Http.Request_header.url in
	match url with
	| Http.Request_header.Star ->
		dispatcher.default_handler, "*" 
	| Http.Request_header.Uri u ->
		(match (Uri.normalize u).Uri.path with
		 | None ->
			dispatcher.default_handler, ""
		 | Some p ->
			if String.startswith (Ui_config.get_meta_prefix ()) p
			then dispatcher.meta_handler, p
			else if String.startswith (Ui_config.get_dbus_prefix ()) p
			then dispatcher.dbus_proxy_handler, p
			else if String.startswith event_prefix p
			then event_handler, p
			else if String.startswith (Ui_config.get_log_prefix ()) p
			then log_handler, p
			else if String.startswith signal_reg_prefix p
			then signal_register_handler, p
			else if String.startswith signal_prefix p
			then signal_handler, p
			else if String.startswith (Ui_config.get_files_prefix ()) p
			then dispatcher.get_file_handler, p
			else dispatcher.default_handler, p
		)

and dispatch_first_pending_request conn =
	match conn.pending_requests with
	| [] ->
		true
	| r :: rest ->
		(let handler, path = get_handler_and_path r in
		 match handler conn r path with
		 | Pending ->
			true
		 | Done resp ->
			send_resp conn resp;
			conn.pending_requests <- rest;
			dispatch_first_pending_request conn
		)

and finish_async_resp conn resp =
	match conn.pending_requests with
	| [] ->
		  failwith "async resp with empty pending req queue!"
	| r :: rest ->
		  if Conns.has_conn (Async_conn.get_handle conn.conn) then (
			  send_resp conn resp;
			  conn.pending_requests <- rest;
			  ignore (dispatch_first_pending_request conn)
		   )

let process_request conn r  =
	if Ui_config.get_log_http_reqs() then (
		let buf = Buffer.create 8192 in
		Http.Request.serialize buf r;
		let reqstr = Buffer.contents buf in
		Printf.printf "HTTP REQUEST: %s\n" reqstr;
	);
	let sockid = (Unixext.int_of_file_descr (Async_conn.get_fd conn.conn)) in
	dbg "Got a %s request with uri %s on socket %d... "
		(Http.Request_header.string_of_meth r.Http.Request.request.Http.Request_header.meth)
		(Http.Request_header.string_of_url r.Http.Request.request.Http.Request_header.url)
		sockid;
	match conn.pending_requests with
	| [] ->
		enqueue_request conn r;
		dispatch_first_pending_request conn
	| _ ->
		(* There is a request with a pending response; we just
		   enqueue this request. *)
		enqueue_request conn r;
		true

let rec parse_input conn str off len =
	let parse_result =
		try Some (Http.Request.parse_substring conn.http_req_parser str off len)
		with e -> handle_conn_error conn e; None
	in
	match parse_result with
	| None -> ()
	| Some (Http.Request.Error e) ->
		Printf.eprintf "Error parsing http request: %s\n%!" e;
		cleanup_and_close conn
	| Some (Http.Request.Parse_incomplete s) ->
		conn.http_req_parser <- s
	| Some (Http.Request.Result (r, nconsumed)) ->
		conn.http_req_parser <- Http.Request.init_state ();
		if process_request conn r then
			parse_input conn str (off + nconsumed) (len - nconsumed)

(* Connection event-handling callbacks *)

let recv_callback ac str off len =
	let conn = Conns.get_conn (Async_conn.get_handle ac) in
	parse_input conn str off len

let shutdown_callback ac =
	let conn = Conns.get_conn (Async_conn.get_handle ac) in
	Printf.eprintf "Close received on ui connection %d.\n%!"
		(Unixext.int_of_file_descr (Async_conn.get_fd conn.conn));
	cleanup_and_close conn

let send_done_callback ac =
	()

let error_callback ac (c, f, m) =
	let conn = Conns.get_conn (Async_conn.get_handle ac) in
	handle_conn_error conn (Unix.Unix_error (c, f, m))

let http_conn_callbacks =
{
	Async_conn.recv_callback = recv_callback;
	Async_conn.shutdown_callback = shutdown_callback;
	Async_conn.error_callback = error_callback;
	Async_conn.send_done_callback = send_done_callback;
	(* We don't expect these callbacks. *)
	Async_conn.connect_callback = (fun _ -> assert false);
}


(* HTTP server *)

type server_conn =
{
	server_fd : Unix.file_descr;
	server_evloop : Eventloop.t;
	server_evhandle : Eventloop.handle;
}

let server_conn = ref (None : server_conn option)

let set_server el h fd =
	server_conn := Some { server_fd = fd;
			      server_evloop = el;
			      server_evhandle = h;
			    }

let with_xs f =
	let xs = Xenstore.Xs.daemon_open () in
	let v = try f xs with e -> (Xenstore.Xs.close xs; raise e) in
	Xenstore.Xs.close xs;
	v

let uuid_of_domid domid =
	with_xs (fun xs ->
			 let path = xs.Xenstore.Xs.read ("/local/domain/" ^ string_of_int domid ^ "/vm") in
			 xs.Xenstore.Xs.read (path ^ "/uuid"))
let domid_of_ipstring str =
	let h = List.hd (List.rev (String.split '.' str)) in
	int_of_string h
let domid_of_saddr = function
	| Unix.ADDR_UNIX _ -> (-1)
	| Unix.ADDR_INET (addr, port) ->
		  try (
			  let str_addr = Unix.string_of_inet_addr addr in
			  domid_of_ipstring str_addr
		  ) with _ -> (-1)
		  
let allow_connection = function
	| Unix.ADDR_UNIX _ -> true
	| Unix.ADDR_INET (addr, port) ->
		  try (
			  let str_addr = Unix.string_of_inet_addr addr in
			  let domid = domid_of_ipstring str_addr in
			  if domid = 0 then
				  true
			  else (
				  let uuid = uuid_of_domid domid in
				  match Ui_config.get_only_uuids () with
				  | None -> true
				  | Some uuids -> List.mem uuid uuids
			  )
		  ) with _ -> false
		  
let get_server_callbacks () =
	let accept_callback el h fd sa =
		match allow_connection sa with
		| false -> (try Unix.close fd with _ -> ())
		| true  ->
			  let aconn = Async_conn.attach el fd http_conn_callbacks in
			  let conn = { conn = aconn;
				       http_req_parser = Http.Request.init_state ();
				       pending_requests = [];
				       domid = domid_of_saddr sa;
				     } in
			  Conns.add_conn (Async_conn.get_handle aconn) conn in
	let error_callback el h (code, f, m) =
		Printf.eprintf "Error on listening socket: %s in %s %s!\n%!"
			(Unix.error_message code) f m;
		(* TODO: reopen a listening socket? *)
	in
	{
		Eventloop.accept_callback = accept_callback;
		Eventloop.error_callback = error_callback;

		(* We should never get the below callbacks. *)
		Eventloop.connect_callback = (fun _ _ -> assert false);
		Eventloop.recv_ready_callback = (fun _ _ -> assert false);
		Eventloop.send_ready_callback = (fun _ _ -> assert false);
	}

let default_handler conn req path =
	let meth = req.Http.Request.request.Http.Request_header.meth in
	dbg "default handler triggered for %s %s ..." (Http.Request_header.string_of_meth meth) path;
	let resp = (Http.Response.make_response ~headers:(get_default_headers ())
		      Http.Response_header.Status_not_implemented)
	in Done resp
