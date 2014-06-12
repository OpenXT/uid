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

type t = {
	config_file : string;
	http_port : int;
	files_prefix : string;
	files_dir : string;
	root_file : string; (* file used for "GET /" *)
	meta_prefix : string;
	dbus_prefix : string;
	log_prefix : string;
	daemonize : bool;
	max_payload_size : int;
	index_files  : string list;
	only_uuids : (string list) option;
	log_http_reqs : bool;
}

let default = {
	config_file = "/etc/uid.conf";
	http_port = 80;
	files_prefix = "/";
	files_dir = "/var/lib/xenmgr/html/";
	root_file = "index.html";
	meta_prefix = "/meta/";
	dbus_prefix = "/dbus/";
	log_prefix = "/log/";
	daemonize = true;
	max_payload_size = 256 * 1024;
	index_files = [ "index.html" ];
	only_uuids = None;
	log_http_reqs = false;
}

let current_config = ref default

let set_config_file file =
	current_config := {!current_config with config_file = file}

let get_http_port () = (!current_config).http_port
let get_files_prefix () = (!current_config).files_prefix
let get_files_dir () = (!current_config).files_dir
let get_meta_prefix () = (!current_config).meta_prefix
let get_dbus_prefix () = (!current_config).dbus_prefix
let get_log_prefix () = (!current_config).log_prefix
let get_daemonize () = (!current_config).daemonize
let get_root_file () = (!current_config).root_file
let get_max_payload_size () = (!current_config).max_payload_size
let get_index_files () = (!current_config).index_files
let get_only_uuids () = (!current_config).only_uuids
let get_log_http_reqs () = (!current_config).log_http_reqs

let parse_uuids s =
	let splits  = String.split ',' s in
	let splits' = List.map (String.strip String.isspace) splits in
	List.filter (fun s -> String.length s > 0) splits'

let mk_only_uuids s =
	(* HACK: if /storage/ndvm does not exist, we allow all uuids to communicate with web server, hence
         * we return null list here *)
        if not (Sys.file_exists "/storage/ndvm")
	then None
	else Some (parse_uuids s)

let read_argline () =
	let t = current_config in
	let args = [ ("--http-port", Arg.Int (fun i -> t := {!t with http_port = i}), " http port");
		     ("--no-daemonize", Arg.Unit (fun () -> t := {!t with daemonize = false}), " do not run as daemon");
		   ] in
	let usage = "uid [options]" in
	Arg.parse args (fun s -> ()) usage

let read_config () =
	let t = current_config in
	let args = [ ("http-port", Config.Int (fun i -> t := {!t with http_port = i}));
		     ("files-prefix", Config.String (fun s -> t := {!t with files_prefix = Utils.ensure_trailing_slash s}));
		     ("files-dir", Config.String (fun s -> t := {!t with files_dir = Utils.ensure_trailing_slash s}));
                     ("max-payload", Config.Int (fun i -> t := {!t with max_payload_size = i}));
		     ("root-file", Config.String (fun s -> t := {!t with root_file = s}));
		     ("meta-prefix", Config.String (fun s -> t := {!t with meta_prefix = Utils.ensure_trailing_slash s}));
		     ("dbus-prefix", Config.String (fun s -> t := {!t with dbus_prefix = Utils.ensure_trailing_slash s}));
		     ("log-prefix", Config.String (fun s -> t := {!t with log_prefix = Utils.ensure_trailing_slash s}));
		     ("daemonize", Config.Bool (fun b -> t := {!t with daemonize = b}));
		     ("index-files", Config.String (fun s -> t := {!t with index_files = !t.index_files @ [ s; ]}));
		     ("only-uuids", Config.String (fun s -> t := {!t with only_uuids = mk_only_uuids s }));
		     ("log-http-reqs", Config.Bool (fun b -> t := {!t with log_http_reqs = b}));
		   ] in
	try Config.read (!t).config_file args (fun _ _ -> ())
	with
	| Config.Error es -> List.iter (fun (k, v) -> Printf.eprintf "config error: %s = %s\n%!" k v) es
	| e -> Printf.eprintf "Ignoring config parsing error: %s ...\n%!" (Printexc.to_string e)

