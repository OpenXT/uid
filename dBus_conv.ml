(*
 *     Copyright (C) 2009 Citrix Ltd.
 *     Author Jean-Sebastien Legare <Jean-Sebastien.Legare@citrix.com>
 *
 *     Helper conversion routines from and to dbus and json.
 *)

(*
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


type error_ty =
	| Invalid_type of string
	| Invalid_value of string
	| Invalid_signature of (string (* substring *) * string (* detail *))
	| Not_implemented of string

exception Error of error_ty

let string_of_error = function
	| Invalid_type s -> s
	| Invalid_value s -> s
	| Not_implemented s -> Printf.sprintf "Not implemented: %s" s
	| Invalid_signature (sign, detail) -> Printf.sprintf "Invalid signature '%s': %s" sign detail

let raise_invalid_type from_type to_str =
	let from_str = Json.string_of_type from_type in
	raise (Error (Invalid_type (Printf.sprintf "Conversion from '%s' to '%s' failed." from_str to_str)))

let dbus_byte_of_json = function
	| Json.String s -> 
		if s = "" then 
			raise (Error (Invalid_value "Can't convert \"\" to Char."))
		else DBus.Byte s.[0]
	| j -> raise_invalid_type j "Byte";;

let dbus_bool_of_json = function 
	| Json.Bool b -> DBus.Bool b
 	| j -> raise_invalid_type j "Boolean"
	
let dbus_int32_of_json = function
 	| Json.Int i64 -> DBus.Int32 (Int64.to_int32 i64)
 	| j -> raise_invalid_type j "Int32"

let dbus_uint32_of_json = function
	| Json.Int i64 -> DBus.UInt32 (Int64.to_int32 i64)
	| j -> raise_invalid_type j "UInt32"

let dbus_int16_of_json = function
	| Json.Int i64 -> DBus.Int16 (Int64.to_int i64)
	| j -> raise_invalid_type j "Int16"

let dbus_uint16_of_json = function
	| Json.Int i64 -> DBus.UInt16 (Int64.to_int i64)
	| j -> raise_invalid_type j "UInt16"

let dbus_int64_of_json = function
	| Json.Int i64 -> DBus.Int64 i64
	| j -> raise_invalid_type j "Int64"

let dbus_uint64_of_json = function
	| Json.Int i64 -> DBus.UInt64 i64
	| j -> raise_invalid_type j "UInt64"

let dbus_string_of_json = function
	| Json.String s -> DBus.String s
	| j -> raise_invalid_type j "String"

let dbus_object_path_of_json = function
	| Json.String s -> DBus.ObjectPath s
	| j -> raise_invalid_type j "ObjectPath"

let dbus_double_of_json = function
	| Json.Float f -> DBus.Double f
	| j -> raise_invalid_type j "Double"

let dbus_variant_of_json = function
	| Json.Bool b -> DBus.Variant (DBus.Bool b)
 	| Json.Int i64 -> DBus.Variant (DBus.Int32 (Int64.to_int32 i64))
	| Json.String s -> DBus.Variant (DBus.String s)
	| Json.Float f -> DBus.Variant (DBus.Double f)
	| j -> raise_invalid_type j "Variant"

(** Converts json objects to dbus objects, expecting "j" to conform
    to the dbus signature "dbus_sig". *)
let rec dbus_of_json j dbus_sig =
	match dbus_sig with
	| DBus.SigByte -> dbus_byte_of_json j
	| DBus.SigBool -> dbus_bool_of_json j
	| DBus.SigInt16 -> dbus_int16_of_json j
	| DBus.SigUInt16 -> dbus_uint16_of_json j
	| DBus.SigInt32 -> dbus_int32_of_json j
	| DBus.SigUInt32 -> dbus_uint32_of_json j
	| DBus.SigInt64 -> dbus_int64_of_json j
	| DBus.SigDouble -> dbus_double_of_json j
	| DBus.SigString -> dbus_string_of_json j
	| DBus.SigObjectPath -> dbus_object_path_of_json j
	| DBus.SigStruct types -> dbus_struct_of_json j types
	| DBus.SigVariant -> dbus_variant_of_json j
	| _ -> raise (Error (Not_implemented "not all signature types are handled."))

and dbus_struct_of_json (j:Json.t) (sig_list:DBus.ty_sig list) =
	match j with
	| Json.Array items -> DBus.Struct (List.map2 dbus_of_json (Array.to_list items) (sig_list))
	| _ -> raise_invalid_type j "Struct"


(** This is used when the json-rpc message does not have a signature key.
    It converts the Json object to a dbus object according to common sense:
    Ints to int32s, floats to floats, dictionaries to dbus arrays of dicts, etc.

    Simple json types are handled. Of the complex types, only json dictionaries
    with string keys and string values are allowed. More complex json objects
    should be converted using the dbus signature hint (see dbus_of_json ).
*)
let rec dbus_of_json_natural = function
	| Json.String s -> DBus.String s
	| Json.Float f  -> DBus.Double f
	| Json.Int i64  -> DBus.Int32 (Int64.to_int32 i64)
	| Json.Array items -> DBus.Struct (List.map dbus_of_json_natural (Array.to_list items))
	| Json.Bool bool -> DBus.Bool bool
	| Json.Object key_values -> (
		(** handle only string values inside json dictionaries *)
		let convert_key_value_to_dbus dbus_valsig json_key_value_pair =
			let dbus_key = DBus.String (fst json_key_value_pair) in
			let dbus_val = dbus_of_json (snd json_key_value_pair) dbus_valsig in
			(dbus_key, dbus_val) in

		try 
			DBus.Array (DBus.Dicts ((DBus.SigString, DBus.SigString), 
						(List.map (convert_key_value_to_dbus DBus.SigString) (Array.to_list key_values))))
		with Error err -> raise (Error (Invalid_value (Printf.sprintf 
							"%s (For natural conversion, json dict values must be strings only)"
							(string_of_error err))))
	)		
	| j -> raise (Error (Not_implemented (Printf.sprintf "No natural conversion for json type '%s'. Use the signature hint." 
							     (Json.string_of_type j))))


let basic_type_map = [
	('y', DBus.SigByte);
	('b', DBus.SigBool);
	('s', DBus.SigString);
	('q', DBus.SigUInt16);
	('n', DBus.SigInt16);
	('i', DBus.SigInt32);
	('u', DBus.SigUInt32);
	('x', DBus.SigInt64);
	('t', DBus.SigUInt64);
	('d', DBus.SigDouble);
	('o', DBus.SigObjectPath);
	('v', DBus.SigVariant);]

let rec dbus_siglist_of_string (sigstr:string) =
	let skip s n = 
		String.sub s n ((String.length s) - n) in

	(** cuts out a struct signature at the beginning of a signature string *)
	let trim_struct_signature sign =
		assert ((String.length sign) > 0 && sign.[0] = '(');
		let rec find_endpoint s pcount endindex = 
			if pcount = 0 then (
				let struct_sig = String.sub s 1 (endindex-1) in
				let struct_after = skip s (endindex + 1) in
				(struct_sig, struct_after) (* trim outer parentheses *)
			) else if endindex >= String.length s then raise Not_found else
			match s.[endindex] with
			| '(' -> find_endpoint s (pcount + 1) (endindex + 1)
			| ')' -> find_endpoint s (pcount - 1) (endindex)
			| x   -> find_endpoint s pcount       (endindex + 1) in
		(* the end of the struct signature is reached when parentheses balance out *)
		find_endpoint sign (-1) 0 in

	(* consume the next type signature token *)
	if (String.length sigstr = 0) then []
	else
	match sigstr.[0] with 
	| 'a' -> raise (Error (Not_implemented "Array type signature parsing"))
	| '{' -> raise (Error (Not_implemented "Dictionary type signature parsing"))
	| '(' -> 
		let structsig, after = try trim_struct_signature sigstr with Not_found -> 
			raise (Error (Invalid_signature (sigstr,"Open struct"))) in
		(DBus.SigStruct (dbus_siglist_of_string structsig)) :: (dbus_siglist_of_string after)
	| base_sig -> 
		try 
			let dbus_sig = List.assoc base_sig basic_type_map in
			dbus_sig :: (dbus_siglist_of_string (skip sigstr 1))		
		with Not_found -> raise (Error (Invalid_signature (String.make 1 base_sig, "Unknown")))

let rec json_of_dbus_array_ty dbus_array_ty =
	let create_json_array convert_func item_list = Json.Array (Array.of_list (List.map convert_func item_list)) in
	match dbus_array_ty with 
	| DBus.Unknowns -> Json.Null
	| DBus.Bytes list   -> create_json_array (fun c -> Json.Int (Int64.of_int (Char.code c))) list
	| DBus.Bools list   -> create_json_array (fun b -> Json.Bool b) list
	| DBus.Int16s list 
	| DBus.UInt16s list -> create_json_array (fun i -> Json.Int (Int64.of_int i)) list
	| DBus.Int32s list 
	| DBus.UInt32s list -> create_json_array (fun i32 -> Json.Int (Int64.of_int32 i32)) list
	| DBus.Int64s list	
	| DBus.UInt64s list -> create_json_array (fun i64 -> Json.Int i64) list
	| DBus.Doubles list -> create_json_array (fun f -> Json.Float f) list
	| DBus.Strings list -> create_json_array (fun s -> Json.String s) list
	| DBus.ObjectPaths list -> create_json_array (fun o -> Json.String o) list
	| DBus.Variants list -> create_json_array (fun v -> json_of_dbus_ty v) list
	| DBus.Structs (_, strukts) -> 
		let convert_ty_list dbus_ty_list =
			create_json_array (fun dbus_ty -> json_of_dbus_ty dbus_ty) dbus_ty_list in
		create_json_array convert_ty_list strukts (* json array of json arrays *)
	| DBus.Dicts (_, key_values) ->
		let convert_key_value key_value = 
			let k,v = (json_of_dbus_ty (fst (key_value)), json_of_dbus_ty (snd (key_value))) in
			match k with
			| Json.String s -> (s, v)
			| _ -> raise (Error (Not_implemented "Can only convert dicts whose keys are strings.")) in
		Json.Object (Array.of_list (List.map convert_key_value key_values))
	| DBus.Arrays (_, arr_list) -> create_json_array json_of_dbus_array_ty arr_list
	
and json_of_dbus_ty = function
	| DBus.Byte b -> Json.Int (Int64.of_int (Char.code b))
	| DBus.Bool b -> Json.Bool b
	| DBus.Int16 i -> Json.Int (Int64.of_int i)
	| DBus.UInt16 i -> Json.Int (Int64.of_int i)
	| DBus.Int32 i32 -> Json.Int (Int64.of_int32 i32)
	| DBus.UInt32 i32 -> Json.Int (Int64.of_int32 i32)
	| DBus.Int64 i64 -> Json.Int i64
	| DBus.UInt64 i64 -> Json.Int i64
	| DBus.Double f -> Json.Float f
	| DBus.String s -> Json.String s
	| DBus.ObjectPath o -> Json.String o
	| DBus.Array arr -> json_of_dbus_array_ty arr
	| DBus.Struct items -> Json.Array (Array.of_list (List.map (fun item -> json_of_dbus_ty item) items))
	| DBus.Variant item -> json_of_dbus_ty item
	| DBus.Unknown -> Json.Null (* use undefined instead ? *)
