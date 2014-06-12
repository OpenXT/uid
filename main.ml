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

(**)
open Pervasiveext

let verbose = ref false

let default_ui_port = 8080
let listen_localhost = ref false

exception Missing_option_value
let unopt o = match o with Some x -> x | None -> raise Missing_option_value

let setup_http_listener el port =
	let iaddr = Unix.inet_addr_any in
	let sockaddr = Unix.ADDR_INET (iaddr, port) in
	let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
	Unix.setsockopt socket Unix.SO_REUSEADDR true;
	Unix.bind socket sockaddr;
	let h = Eventloop.register_conn el socket (Httpserver.get_server_callbacks ()) in
	Httpserver.set_server el h socket;
	Eventloop.listen el h

let setup_connections http_port el =
	setup_http_listener el http_port;
	Dbus_interface.init el

let set_dispatcher () =
	let dispatcher = { Httpserver.get_file_handler = File_handler.file_handler;
			   Httpserver.meta_handler = Httpserver.default_handler;
			   Httpserver.dbus_proxy_handler = DBus_handler.dbus_handler;
			   Httpserver.default_handler = Httpserver.default_handler }
	in Httpserver.set_dispatcher dispatcher

let main =
	Printexc.record_backtrace true;

	Logs.set_default Log.Info ["syslog:uid"];
	Logs.set_default Log.Error ["syslog:uid"];
	Logs.set_default Log.Warn ["syslog:uid"];
	Logs.set_default Log.Debug [];

	(* If a config-file is specified, process that file first.
	   Config-files are processed first, and command-line arguments
	   next, so that the latter can be used to override settings. *)

	Ui_config.read_config ();
	Ui_config.read_argline ();

	if Ui_config.get_daemonize () then
		Unixext.daemonize ();

	set_dispatcher ();
	try
		Server.start_server (setup_connections (Ui_config.get_http_port ()))
	with
	| Unix.Unix_error (e, f, m) ->
		Printf.printf "Unix error: %s: %s %s\n" (Unix.error_message e) f m;
		flush stdout
	| e ->
		Printf.printf "Uncaught exception: %s at\n" (Printexc.to_string e);
		Printf.printf "%s\n" (Printexc.get_backtrace ());
		flush stdout
