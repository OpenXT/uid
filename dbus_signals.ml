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

open Printf

module D = Debug.Debugger(struct let name="uid-signals" end)

open D

(* we count number of signals received, so we can uniquely number each signal when sending to UI *)
let num_processed = ref Int64.zero

type signal = {
	id: int64;
	interface: string;
	member: string;
	path: string;
	destination: string;
	sender: string;
	params: Json.t list;
      }

type queue = {
	interfaces: string list;
	signals: signal list;
	filled_cb: ((int*int) -> bool) option;
      }

module Queues = Map.Make (struct type t = int*int let compare = compare end)

type queues = queue Queues.t

let queues = ref Queues.empty
let queue_max_size = 1000
let empty_queue = { interfaces = []; signals = []; filled_cb = None }

let qid_str (domid,qid) = sprintf "domid:%d qid:%d" domid qid

(* add interface to match list within queue *)
let add_interface queues qid iname =
	let q  = try Queues.find qid queues with Not_found -> empty_queue in
	let q' = { q with signals = []; interfaces = iname :: q.interfaces } in
	Queues.add qid q' queues

let string_of_signal s =
	let params_strs = List.map Json.to_string s.params in
	let params = List.fold_left (fun acc x -> acc ^ ", " ^ x) "" params_strs in
	Int64.to_string s.id ^ ":" ^ s.interface ^ "." ^ s.member ^ " " ^ params

(* store a dbus signal within the queue *)
let push s queues =
	let queue s signals qid =
		(* info ">>> pushing signal %s to queue %s" (string_of_signal s) (qid_str qid); *)
	        if List.length signals < queue_max_size 
		then signals @ [s]
		else (List.tl signals) @ [s];
	in
	let process qid q =
		if List.mem s.interface q.interfaces
		then (
		  let signals' = queue s q.signals qid in
		  let q' = { q with signals = signals' } in
		    { q' with filled_cb = (match q'.filled_cb with
					     | None -> None
					     | Some cb -> if cb qid then (Some cb) else None) }
		)
                else q
	in
	Queues.mapi process queues

let unset_queue_filled_cb ~queue_id =
  try (
    let q  = Queues.find queue_id !queues in
    let q' = { q with filled_cb = None } in
      queues := Queues.add queue_id q' !queues
  )
  with Not_found -> ()

(* invoke fill callbacks *)
let iterate_cbs () =
  let process qid q =
    match q.signals with
      | [] -> ()
      | _  -> ( match q.filled_cb with
		  | None -> ()
		  | Some cb -> if cb qid then () else unset_queue_filled_cb qid )
  in
  ignore (Queues.mapi process !queues)

(* extract signal list from the queue *)
let pop qid queues =
	if not (Queues.mem qid queues)
	then []
	else let q = try Queues.find qid queues with Not_found -> empty_queue in
(*	     List.iter (fun s ->
				info "<<< popping signal %s from queue %s" (string_of_signal s) (qid_str qid)
		       ) q.signals; *)
	     q.signals

(* filter signals from queue which satisfy given predicate *)
let qfilter qid pred queues =
	if not (Queues.mem qid queues)
	then queues
	else let q = Queues.find qid queues in
	     let signals' = List.filter pred q.signals in
	     Queues.add qid { q with signals = signals' } queues

(* handle dbus message, put signals in proper queues *)
let process_msg msg =
	match DBus.Message.get_type msg with
	| DBus.Message.Signal ->
		  num_processed := Int64.add !num_processed 1L;
		  let get x = match x with Some y -> y | None -> "" in
		  let signal = {
			  id = !num_processed;
			  interface = get (DBus.Message.get_interface msg);
			  member = get (DBus.Message.get_member msg);
			  path = get (DBus.Message.get_path msg);
			  destination = get (DBus.Message.get_destination msg);
			  sender = get (DBus.Message.get_sender msg);
			  params = List.map DBus_conv.json_of_dbus_ty (DBus.Message.get msg);
			} in
		  queues := push signal !queues;
		  iterate_cbs ()
	| _ -> ()

(* setup *)
let init () = ()

(* begin listening to dbus interface signals within given queue *)
let listen_to_interface ~queue_id ~name =
	let bus = DBus.Bus.get DBus.Bus.System in
	(* We are interested in receiving all signals from system bus *)
	DBus.Bus.add_match bus ("type='signal',interface='" ^ name ^ "'") false;

	Dbus_server.remove_signal_interface name;
	Dbus_server.register_signal_interface 
		name (fun msg mem path -> process_msg msg);
	queues := add_interface !queues queue_id name

(* remove signals from queue with ID upto given ID (exclusive) *)
let drop_signals queue_id upto_signal_id =
	queues := qfilter queue_id (fun signal -> signal.id >= upto_signal_id) !queues

let set_queue_filled_cb ~queue_id cb =
  try (
    let q  = Queues.find queue_id !queues in
    let q' = { q with filled_cb = Some cb } in
      queues := Queues.add queue_id q' !queues
  )
  with Not_found -> ()

(* extract signal list from the queue as JSON data, empty the queue *)
let pop_json ~queue_id ~min_signal_id =
	(* we can clean queue up to given signal id *)
	drop_signals queue_id min_signal_id;

	let json_of_signal s =
		(* create a json representation of dbus signal *)	
		Json.Object 
			[|
			  "id"         , Json.Int s.id;
			  "interface"  , Json.String s.interface;
			  "member"     , Json.String s.member;
			  "path"       , Json.String s.path;
			  "destination", Json.String s.destination;
			  "sender"     , Json.String s.sender;
			  "params"     , Json.Array (Array.of_list s.params);
			|]
	in
	let signals = pop queue_id !queues in
	match signals with
	| [] -> None
	| q  -> Some (Json.Object [|
		      "queue_id"   , Json.Int   (Int64.of_int (snd queue_id));
		      "num_signals", Json.Int   (Int64.of_int (List.length q));
		      "signals"    , Json.Array (Array.of_list (List.map json_of_signal q))
		    |])
    

			       
  
