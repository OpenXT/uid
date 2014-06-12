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

open Pervasiveext

let debug = ref true

let dbg fmt =
	let logger s = if !debug then Printf.printf "%s\n%!" s in
	Printf.ksprintf logger fmt

(* quick and small mime.types for what we have *)
let mime_types = [
	("png",  "image/png");
	("gif",  "image/gif");
	("jpeg", "image/jpeg");
	("jpg",  "image/jpeg");
	("css",  "text/css");
	("htm",  "text/html");
	("html", "text/html");
	("js",   "application/x-javascript");
]

let is_image mime =
  match mime with
    | "image/png" -> true
    | "image/gif" -> true
    | "image/jpeg" -> true
    | _ -> false

let is_html mime = mime = "text/html"

let mkmsg ~service ~obj ~interface ~member ~args =
	let m = DBus.Message.new_method_call service obj interface member in
	ignore (DBus.Message.append m args);
	m

let get_language cb = 
  let m = mkmsg ~service:"com.citrix.xenclient.xenmgr" ~obj:"/" ~interface:"org.freedesktop.DBus.Properties" ~member:"Get" ~args:[ DBus.String "com.citrix.xenclient.xenmgr.config"; DBus.String "language" ] in
    Dbus_interface.send_request m (fun resp ->
				     match DBus.Message.get resp with
				       | [ DBus.Variant (DBus.String out_1) ] -> cb out_1
				       | _ -> cb ""
				  )

let get_mime_type filename =
	let rec matcher = function
		| [] -> None
		| (ext, mime) :: rest ->
			if Filename.check_suffix filename ("." ^ ext)
			then Some mime
			else matcher rest
	in matcher mime_types

let get_file_headers filename callback =
	let headers = Httpserver.get_default_headers () in
	match get_mime_type filename with
	| None -> callback headers
	| Some mime ->
		let add_locale_header h hcb =
		  if is_html mime then
		    get_language (fun l ->
				    if l <> "" then hcb (h@[ "Set-Cookie",("locale=" ^ l) ])
				    else hcb h)
		  else hcb h
		in
		let h0 = [ "Content-Type", mime ] @ (if is_image mime then ["Expires", "Sun, 01 Dec 2030 00:00:00 GMT"] else []) in
		add_locale_header h0 (fun h1 ->	callback (List.fold_left (fun acc (k,v) -> Http.add_header k v acc) headers h1))

let read_whole_file ic =
	let buf = Buffer.create 2048 in
	let str = String.create 1024 in
	let rec do_read () =
		(* Don't use input_line, since it does not preserve newlines. *)
		let read = input ic str 0 (String.length str) in
		match read with
                | 0 -> raise End_of_file
		| _ -> Buffer.add_substring buf str 0 read; do_read ()
	in try do_read () with End_of_file -> buf

let strip_files_prefix path =
	(* This assumes that the files_prefix and files_dir end
	   in a '/'.  We remove the leading slash from the path via the
	   files_prefix, and the prefixing with files_dir (later) puts
	   it back.
	*)
	let p = Ui_config.get_files_prefix () in
	let ulen, plen = String.length path, String.length p in
	if ulen >= plen then String.sub path plen (ulen - plen)
	else path

let file_response conn req path =

	let filesize_too_big size =
		((Int64.compare size (Int64.of_int (Ui_config.get_max_payload_size ()))) = 1) in

	let send_file_response filename =
	  let evtloop = Httpserver.get_loop conn in
	  ignore (Eventloop.start_timer_asap evtloop (fun () -> 
		get_file_headers filename (fun headers ->
			let ic = open_in filename in
			let payload = finally (fun () -> read_whole_file ic) (fun () -> close_in ic) in
			let payload = { Http.Payload.content = payload;
					Http.Payload.trailers = [] } in
			dbg "%s -> Ok" filename;
			Httpserver.finish_async_resp conn
			  (Http.Response.make_response ~payload ~headers Http.Response_header.Status_ok)
		)
	  ));
	  Httpserver.Pending in

	let meth = req.Http.Request.request.Http.Request_header.meth in
	if meth <> Http.Request_header.Get then
		Httpserver.Done ((Http.Response.make_response ~headers:(Httpserver.get_default_headers ())
		   Http.Response_header.Status_method_not_allowed))
	else
		let filename = strip_files_prefix path in
		let filename = if filename = "" then Ui_config.get_root_file () else filename in
		let filename = Ui_config.get_files_dir () ^ filename in
		dbg "Dispatching %s => %s ...%!" path filename;
		try
			let fstat = Unix.LargeFile.stat filename in
			if fstat.Unix.LargeFile.st_kind = Unix.S_DIR then begin
				(** A directory entry. Try to get an index file from that directory. *)
				dbg "%s -> Directory file" filename;
				let rec find_index_file candidates = 
					match candidates with
					| [] -> Httpserver.Done ((Http.Response.make_response ~headers:(Httpserver.get_default_headers ())
							Http.Response_header.Status_forbidden))
					| file :: others -> (
						try
						 let indexname = filename ^ "/" ^ file in (* Check the possible index file *) 
						 let fstat = Unix.LargeFile.stat indexname in 
						 let fsize = fstat.Unix.LargeFile.st_size in
						 if ((fstat.Unix.LargeFile.st_kind <> Unix.S_REG) || (filesize_too_big fsize)) then 
							find_index_file others
						 else
							send_file_response indexname
						with _ -> find_index_file others
					  ) in
				find_index_file (Ui_config.get_index_files ())
			end else if fstat.Unix.LargeFile.st_kind <> Unix.S_REG then begin
				dbg "%s -> Forbidden (not regular file)" filename;
				Httpserver.Done ((Http.Response.make_response ~headers:(Httpserver.get_default_headers ())
				   Http.Response_header.Status_forbidden))
			end else if (filesize_too_big (fstat.Unix.LargeFile.st_size)) then begin
				dbg "%s -> Entity Too Large (%Ld bytes)" filename fstat.Unix.LargeFile.st_size;
				Httpserver.Done ((Http.Response.make_response ~headers:(Httpserver.get_default_headers ())
				   Http.Response_header.Status_request_entity_too_large))
			end else 
				send_file_response filename
		with
		| Unix.Unix_error (Unix.ENOENT, f, m) ->
			dbg "%s -> Not found" filename;
		        Httpserver.Done ((Http.Response.make_response ~headers:(Httpserver.get_default_headers ())
			   Http.Response_header.Status_not_found))
		| Sys_error _ ->
			dbg "%s -> Forbidden (permissions)" filename;
			Httpserver.Done ((Http.Response.make_response ~headers:(Httpserver.get_default_headers ())
			   Http.Response_header.Status_forbidden))

let file_handler conn req path =
	file_response conn req path
