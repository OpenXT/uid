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

type conn_state

type request_status = Pending | Done of Http.Response.t
type request_handler = conn_state -> Http.Request.t -> (* path *) string -> request_status

type dispatcher = {
	get_file_handler : request_handler;
	meta_handler : request_handler;
	dbus_proxy_handler : request_handler;
	default_handler : request_handler;
}

val get_loop : conn_state -> Eventloop.t
val get_domid : conn_state -> int

val set_dispatcher : dispatcher -> unit
val finish_async_resp : conn_state -> Http.Response.t -> unit

val set_server : Eventloop.t -> Eventloop.handle -> Unix.file_descr -> unit
val get_server_callbacks : unit -> Eventloop.conn_callbacks

(* Utilities for handlers *)
val get_default_headers : unit -> Http.header_fields
val default_handler : request_handler
