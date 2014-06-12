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

(* initialise signal handling *)
val init : unit -> unit

(* listen to given dbus interface *)
val listen_to_interface : queue_id:int*int -> name:string -> unit

(* set callback to use when signals inserted into queue *)
val set_queue_filled_cb : queue_id:int*int -> ((int*int) -> bool) -> unit
val unset_queue_filled_cb : queue_id:int*int -> unit

(* pop queue contents as json object, or return None if queue is empty *)
val pop_json : queue_id:int*int -> min_signal_id:int64 -> Json.t option



