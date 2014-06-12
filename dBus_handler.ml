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

(*
 *	Author: Jean-Sebastien Legare <first.last@citrix.com>
 * 
 *	Provides code to handle incoming javascript requests from the
 *      browser and converting them into dbus message requests or
 *      signals. The responses to these messages are then converted
 *      back into json values.
 * 
 *      We deviate from the spec for the moment to ease our lives.  The
 *      params field of the rpc object is the following dictionary:
 *
 *      {
 *        "destination":"com.example.HelloWorld"
 *        "path":"/org/example/MyObject",
 *        "method": "getList"
 *        "interface":"org.example.invokable", (optional key)
 *        "signature":"dbus_sign", (optional signature)
 *        "args":[ <arg0>,<arg1>,...,<argn> ]
 *      }
 *      
 *      The signature is a dbus signature that removes the ambiguities
 *      in the json->dbus conversion. If it is not given, the json
 *      arguments will be converted according to a default conversion
 *      rule. see dBus_conv.ml for details.
 *)


(** Error code returned jsonrpc error responses *)
let json_rpc_dbus_error_code = 100

let string_of_error_name = function
        | DBus.ERR_FAILED -> "Failed"
        | DBus.ERR_NO_MEMORY -> "No memory"
        | DBus.ERR_SERVICE_UNKNOWN -> "Service unknown"
        | DBus.ERR_NAME_HAS_NO_OWNER -> "Name has no owner"
        | DBus.ERR_NO_REPLY -> "No reply"
        | DBus.ERR_IO_ERROR -> "IO error"
        | DBus.ERR_BAD_ADDRESS -> "Bad address"
        | DBus.ERR_NOT_SUPPORTED -> "Not supported"
        | DBus.ERR_LIMITS_EXCEEDED -> "Limits exceeded"
        | DBus.ERR_ACCESS_DENIED -> "Access Denied"
        | DBus.ERR_AUTH_FAILED -> "Auth failed"
        | DBus.ERR_NO_SERVER -> "No server"
        | DBus.ERR_TIMEOUT -> "Timeout"
        | DBus.ERR_NO_NETWORK -> "No network"
        | DBus.ERR_ADDRESS_IN_USE -> "Address in use"
        | DBus.ERR_DISCONNECTED -> "Disconnected"
        | DBus.ERR_INVALID_ARGS -> "Invalid args"
        | DBus.ERR_FILE_NOT_FOUND -> "File not found"
        | DBus.ERR_FILE_EXISTS -> "File exists"
        | DBus.ERR_UNKNOWN_METHOD -> "Unknown method"
        | DBus.ERR_TIMED_OUT -> "Timed out"
        | DBus.ERR_MATCH_RULE_NOT_FOUND -> "Match rule not found"
        | DBus.ERR_MATCH_RULE_INVALID -> "Match rule invalid"
        | DBus.ERR_SPAWN_EXEC_FAILED -> "Spawn exec failed"
        | DBus.ERR_SPAWN_FORK_FAILED -> "Spawn fork failed"
        | DBus.ERR_SPAWN_CHILD_EXITED -> "Spawn child exited"
        | DBus.ERR_SPAWN_CHILD_SIGNALED -> "Spawn child signaled"
        | DBus.ERR_SPAWN_FAILED -> "Spawn failed"
        | DBus.ERR_SPAWN_SETUP_FAILED -> "Setup failed"
        | DBus.ERR_SPAWN_CONFIG_INVALID -> "Config invalid"
        | DBus.ERR_SPAWN_SERVICE_INVALID -> "Service invalid"
        | DBus.ERR_SPAWN_SERVICE_NOT_FOUND -> "Service not found"
        | DBus.ERR_SPAWN_PERMISSIONS_INVALID -> "Permissions invalid"
        | DBus.ERR_SPAWN_FILE_INVALID -> "Spawn file invalid"
        | DBus.ERR_SPAWN_NO_MEMORY -> "Spawn no memory"
        | DBus.ERR_UNIX_PROCESS_ID_UNKNOWN -> "Unix process ID unknown"
        | DBus.ERR_INVALID_SIGNATURE -> "Invalid signature"
        | DBus.ERR_INVALID_FILE_CONTENT -> "Invalid file content"
        | DBus.ERR_SELINUX_SECURITY_CONTEXT_UNKNOWN -> "SELinux Security context unknown"
        | DBus.ERR_ADT_AUDIT_DATA_UNKNOWN -> "ADT Audit data unknown"
        | DBus.ERR_OBJECT_PATH_IN_USE -> "Object path in use"



let make_http_server_error msg = 
	let payload_content = Buffer.create 10 in
	Buffer.add_string payload_content msg;
	let payload = { 
		Http.Payload.content  = payload_content;
		Http.Payload.trailers = [];
	} in 
	Http.Response.make_response
		~payload:payload
		~headers:(Httpserver.get_default_headers ())
		Http.Response_header.Status_internal_server_error

exception Invalid_json_req of string
exception Invalid_json_resp of string

(** Replace dashes by underscores implicitly, and filter out invalid characters *)
let sanitize_dbus_object_path str =
	let no_dashes = Stringext.String.replace "-" "_" str in
	let valid_char = function
		| 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' | '.' | '/' -> true
		| _ -> false 
	in	
	Stringext.String.filter_chars no_dashes valid_char

let dbus_message_of_json_params (params:Json.t) =
	let try_json_conv ?(context="") f =
		try 
			f () 
		with Json_conv.Json_conv_error err ->
			raise (Invalid_json_req (context ^ (Json_conv.string_of_error err))) in	
	
	let get_string_field obj_table field_name =
		let field = Json_conv.get_object_field obj_table field_name in
		Json_conv.string_of_json ~permissive:false field in
	
	let get_optional_string_field obj_table field_name =
		match Json_conv.get_optional_object_field obj_table field_name with
		| Json.Null -> None
		| some -> Some (Json_conv.string_of_json ~permissive:false some) in
	
	let get_array_field obj_table field_name = 
		let field = Json_conv.get_object_field obj_table field_name in
		Json_conv.get_array field in

	let dict = try_json_conv ~context:"Invalid params format: " 
			(fun () -> Json_conv.get_object_table params) in
	let path_str = try_json_conv ~context:"Invalid dbus path: " 
			(fun () -> sanitize_dbus_object_path (get_string_field dict "path")) in
	let method_str = try_json_conv ~context:"Invalid dbus method: " 
			(fun () -> get_string_field dict "method") in
	let interface_str = try_json_conv ~context:"Invalid dbus interface: " 
			(fun () -> get_optional_string_field dict "interface") in
	let signature_str =
		match interface_str, method_str with
		| (Some "org.freedesktop.DBus.Properties"), "Set" -> Some "ssv"
		| _ -> try_json_conv ~context:"Invalid json signature: " 
			  (fun () ->  get_optional_string_field dict "signature") in
	let destination_str = try_json_conv ~context:"Invalid destination: "
			(fun () -> get_string_field dict "destination") in
	let json_args = Array.to_list (try_json_conv ~context:"Invalid format for json args array: "
			(fun () -> get_array_field dict "args")) in
	let dbus_args = 
		try match signature_str with 
		| Some signature -> 	
			let msg_signature = DBus_conv.dbus_siglist_of_string signature in
			if List.length msg_signature <> List.length json_args then
				raise (Invalid_json_req "Args length mismatch with signature.") 
			else
				List.map2 DBus_conv.dbus_of_json json_args msg_signature
		| None -> (* use the natural conversion *)
			List.map DBus_conv.dbus_of_json_natural json_args
		with
		| DBus_conv.Error err -> raise (Invalid_json_req (DBus_conv.string_of_error err))
	in
	let msg = DBus.Message.new_method_call destination_str path_str (match interface_str with None -> "" | Some x -> x) method_str in
	DBus.Message.append msg dbus_args;
	msg

(* Parse json containing error message as a string with format error_code:error_message.
   Return error code and message without the error code part *)
let parse_error_json = function
      |	Json.Array [| Json.String str |] as x ->
		(try
			let i        = String.index str ':' in
			let code_str = String.sub str 0 i in
			let msg_str  = String.sub str (i+1) (String.length str - i - 1) in
			let code     = int_of_string code_str in
			(code, (Json.Array [| Json.String msg_str |]))	
		with _ -> (json_rpc_dbus_error_code, x))
      | x ->
		(json_rpc_dbus_error_code, x)

let dbus_response_to_json (m:DBus.message) (reqid:Json.t option) = 
	let extract_out_params m = Json.Array (Array.of_list (List.map DBus_conv.json_of_dbus_ty (DBus.Message.get m))) in
	match (DBus.Message.get_type m) with

	 (* Only the UI sends request, not vice versa *)
	 | DBus.Message.Method_call
	 | DBus.Message.Signal
	 | DBus.Message.Invalid -> raise (Invalid_json_resp (
		Printf.sprintf "Received response of type '%s'." (DBus.Message.string_of_message_ty (DBus.Message.get_type m))))

	 | DBus.Message.Method_return -> 
		Jsonrpc.response_to_json (
  		  Jsonrpc.response_make_success 
				(Opt.unbox reqid) 
				(extract_out_params m)
		)
	 | DBus.Message.Error ->
		   Jsonrpc.response_to_json (
             		   let params = extract_out_params m in
			   let err_code, params = parse_error_json params in
			   let err_name = match DBus.Message.get_error_name m with None -> "?" | Some x -> string_of_error_name x in
			   Jsonrpc.response_make_error 
				   (Opt.unbox reqid) 
				   err_code
				   ("DBus Error: " ^ err_name)
				   (Some params)
		  )

let convert_dbus_response http_resp_cb (reqid:Json.t option) (response_msg:DBus.message) =
	let payload_content = Buffer.create 10 in
	Buffer.add_string payload_content (Json.to_string (dbus_response_to_json response_msg reqid));
	let payload = { 
		Http.Payload.content  = payload_content;
		Http.Payload.trailers = [];
	} in 
	let http_resp = Http.Response.make_response
		~payload:payload
		~headers:(Httpserver.get_default_headers ())
		Http.Response_header.Status_ok in
	http_resp_cb http_resp
	
let validate_request conn msg resp_cb =
	let domid = Httpserver.get_domid conn in
	if domid = 0 then (
	resp_cb true
	) else (
	let callmsg =
		DBus.Message.new_method_call
			"com.citrix.xenclient.rpc_proxy"
			"/"
			"com.citrix.xenclient.rpc_proxy"
			"validate_call"
	in
	let args = [ DBus.Int32 (Int32.of_int domid);
		     DBus.String (match DBus.Message.get_destination msg with Some s -> s | None -> "undefined");
		     DBus.String (match DBus.Message.get_interface msg with Some s -> s | None -> "undefined");
		     DBus.String (match DBus.Message.get_member msg with Some s -> s | None -> "undefined");
		   ] in
	DBus.Message.append callmsg args;
	Dbus_interface.send_request callmsg (
		fun resp ->
			match DBus.Message.get_type resp with
			| DBus.Message.Method_return ->
				  ( match DBus.Message.get resp with
				    | [ DBus.Bool allowed ] -> resp_cb allowed
				    | _ -> resp_cb false )
			| _ -> resp_cb false
	))

let convert_jrpc_request conn msg resp_cb =
	try
		let rpc_req = Jsonrpc.request_of_string msg in
		let reqid   = rpc_req.Jsonrpc.request_id in
		let params  = rpc_req.Jsonrpc.params in

		(** TODO Actual debugging using syslog, or debug library *)
		Printf.printf	"Converting json payload to dbus: '%s'\n%!" msg;

		let message = dbus_message_of_json_params params in
		(match DBus.Message.get_type message with
		 | DBus.Message.Signal ->
			   () (* ignore (Dbus_interface.send message) *)
		 | DBus.Message.Method_call -> 
			validate_request conn message (
				fun allow ->
					if not allow then (
						resp_cb (make_http_server_error "Method call denied.")
					) else (
						(** partial application of the arguments. The conversion will kick off when
						    a message with the right serial is received. *)
						ignore (Dbus_interface.send_request message (convert_dbus_response resp_cb reqid))
					)
			)
		 | msg_ty -> 
			resp_cb (make_http_server_error (Printf.sprintf "DBus messages of type '%s' not supported." 
									 (DBus.Message.string_of_message_ty msg_ty)))
		)
	with 
	| Jsonrpc.Invalid_request req_err -> 
		resp_cb (make_http_server_error (Jsonrpc.string_of_req_error req_err))
	| ex -> 
		let stack_trace = Printexc.to_string ex in
		resp_cb (make_http_server_error ("Uncaught exception. View server logs. \n" ^ stack_trace))

let start_request conn req resp_cb =
	let meth = req.Http.Request.request.Http.Request_header.meth in
	if meth <> Http.Request_header.Post then (
		resp_cb (Http.Response.make_response
				~headers:(Httpserver.get_default_headers ())
				Http.Response_header.Status_method_not_allowed)
	) else (
		(* POST *)
		match req.Http.Request.payload with 
			| None -> resp_cb (Http.Response.make_response
				~headers:(Httpserver.get_default_headers ())
				Http.Response_header.Status_bad_request)
			| Some payload -> begin
				try convert_jrpc_request conn (Buffer.contents payload.Http.Payload.content) resp_cb with
				| _ -> resp_cb (make_http_server_error "Uncaught exception during request.")
			end
	)	

(** Entry point for Http requests *)
let dbus_handler conn req _ = 
	let when_done http_resp = 
		Httpserver.finish_async_resp conn http_resp in
	let evtloop = Httpserver.get_loop conn in
	ignore (Eventloop.start_timer_asap evtloop (fun () -> start_request conn req when_done));
	Httpserver.Pending
