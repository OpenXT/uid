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

let dbus_conns = ref ([] : Dbus_conn.t list)

let add_dbus_conn dbc =
	dbus_conns := dbc :: !dbus_conns

let quit = ref false

let stop_server () =
	quit := true

let start_server setup_connects =
	let el = Eventloop.create () in
	setup_connects el;
	while (not !quit
	       && ((Eventloop.has_connections el) || (Eventloop.has_timers el)))
	do
		(* We need to keep this select timeout small, due to
		   the broken libdbus async API, which requires
		   dispatch to be called at each iteration of the main
		   loop. *)
		Eventloop.dispatch el 0.5;
		List.iter (fun db -> Dbus_conn.dispatch db) !dbus_conns;
	done
