(*
 * Copyright (c) 2010 Citrix Systems, Inc.
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
 *	Provides an interface to dbus, defines methods and properties
 *      exported from the UI to DBus.
 *)

let send dbus_message = Dbus_server.send_msg dbus_message
let send_request dbus_message dbus_resp_handler = Dbus_server.send_request dbus_message dbus_resp_handler
let server_name = "com.citrix.xenclient.uid"

let grab_server_name name =
	let destination = "org.freedesktop.DBus" in
	let path = "/" in
	let interface = "org.freedesktop.DBus" in
	let meth = "RequestName" in
	let msg = DBus.Message.new_method_call destination path interface meth in
	DBus.Message.append msg [(DBus.String server_name);
				 (* no allow_replacement, no replace_existing, do_not_queue *)
				 (DBus.UInt32 0x4l);
				];
	let handler resp =
		let resp_args = DBus.Message.get resp in
		match resp_args with
		| (DBus.UInt32 rval) :: _ ->
			if rval = 1l then Printf.printf "Acquired name \"%s\"\n%!" server_name
			else begin
				Printf.printf "Unable to acquire name \"%s\": ret-code=%ld\n%!" server_name rval;
				exit 1
			end
		| _ ->
			Printf.printf "Unable to acquire name \"%s\": unexpected response%!" server_name;
			exit 1
	in
	Dbus_server.send_request msg handler

let init el =
	let conn = Dbus_server.init_connection ~use_session_bus:false el in
	Server.add_dbus_conn conn;
	grab_server_name server_name;
	Dbus_signals.init ();  
