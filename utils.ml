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

let ensure_trailing_slash s =
	if s = "" then "/"
	else if s.[(String.length s) - 1] = '/' then s
	else s ^ "/"

let remove_trailing_slash s =
	let slen = String.length s in
	if slen = 0 then s
	else if s.[slen - 1] = '/' then String.sub s 0 (slen - 1)
	else s
