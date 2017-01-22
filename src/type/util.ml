(*****************************************************************************)
(*  Caradoc: a PDF parser and validator                                      *)
(*  Copyright (C) 2015 ANSSI                                                 *)
(*  Copyright (C) 2015-2017 Guillaume Endignoux                              *)
(*                                                                           *)
(*  This program is free software; you can redistribute it and/or modify     *)
(*  it under the terms of the GNU General Public License version 2 as        *)
(*  published by the Free Software Foundation.                               *)
(*                                                                           *)
(*  This program is distributed in the hope that it will be useful,          *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of           *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            *)
(*  GNU General Public License for more details.                             *)
(*                                                                           *)
(*  You should have received a copy of the GNU General Public License along  *)
(*  with this program; if not, write to the Free Software Foundation, Inc.,  *)
(*  51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.              *)
(*****************************************************************************)


open Type.Type
open Boundedint

let make_type ?(allow_ind=true) kind =
  {
    kind = kind;
    allow_ind = allow_ind;
  }

let make_entry ~optional typ =
  {
    typ = typ;
    optional = optional;
  }

let make_entry_type ~optional ?(allow_ind=true) kind =
  {
    typ = make_type ~allow_ind kind;
    optional = optional;
  }


let type_class ?(allow_ind=true) typename =
  make_type ~allow_ind (Class typename)

let entry_class ?(allow_ind=true) ~optional typename =
  make_entry ~optional (type_class ~allow_ind typename)


let type_stream ?(allow_ind=true) typename =
  make_type ~allow_ind (Stream typename)

let entry_stream ?(allow_ind=true) ~optional typename =
  make_entry ~optional (type_stream ~allow_ind typename)


let type_alias ?(allow_ind=true) typename =
  make_type ~allow_ind (Alias typename)

let entry_alias ?(allow_ind=true) ~optional typename =
  make_entry ~optional (type_alias ~allow_ind typename)


let type_name_exact ?(allow_ind=true) name =
  make_type ~allow_ind (NameExact name)

let entry_name_exact ?(allow_ind=true) ~optional name =
  make_entry ~optional (type_name_exact ~allow_ind name)

let kind_name_in names =
  let x = Hashtbl.create (List.length names) in
  List.iter (fun y -> Hashtbl.add x y ()) names;
  NameIn x

let type_name_in ?(allow_ind=true) names =
  make_type ~allow_ind (kind_name_in names)

let entry_name_in ?(allow_ind=true) ~optional names =
  make_entry ~optional (type_name_in ~allow_ind names)


let type_int_exact ?(allow_ind=true) value =
  make_type ~allow_ind (IntExact ~:value)

let entry_int_exact ?(allow_ind=true) ~optional value =
  make_entry ~optional (type_int_exact ~allow_ind value)

let kind_int_in values =
  IntIn (Array.map (fun x -> ~:x) values)

let type_int_in ?(allow_ind=true) values =
  make_type ~allow_ind (kind_int_in values)

let entry_int_in ?(allow_ind=true) ~optional values =
  make_entry ~optional (type_int_in ~allow_ind values)


let type_dict ?(allow_ind=true) elemtype =
  make_type ~allow_ind (Dictionary elemtype)

let entry_dict ?(allow_ind=true) ~optional elemtype =
  make_entry ~optional (type_dict ~allow_ind elemtype)


let type_array ?(allow_ind=true) elemtype =
  make_type ~allow_ind (Array elemtype)

let entry_array ?(allow_ind=true) ~optional elemtype =
  make_entry ~optional (type_array ~allow_ind elemtype)


let type_array_or_one ?(allow_ind=true) elemtype =
  make_type ~allow_ind (ArrayOrOne elemtype)

let entry_array_or_one ?(allow_ind=true) ~optional elemtype =
  make_entry ~optional (type_array_or_one ~allow_ind elemtype)


let type_sized_array ?(allow_ind=true) len elemtype =
  make_type ~allow_ind (ArraySized (elemtype, len))

let entry_sized_array ?(allow_ind=true) ~optional len elemtype =
  make_entry ~optional (type_sized_array ~allow_ind len elemtype)


let type_array_tuples ?(allow_ind=true) types =
  make_type ~allow_ind (ArrayTuples types)

let entry_array_tuples ?(allow_ind=true) ~optional types =
  make_entry ~optional (type_array_tuples ~allow_ind types)


let type_tuple ?(allow_ind=true) types =
  make_type ~allow_ind (Tuple types)

let entry_tuple ?(allow_ind=true) ~optional types =
  make_entry ~optional (type_tuple ~allow_ind types)


let type_variant ?(allow_ind=true) options =
  make_type ~allow_ind (Variant options)

let entry_variant ?(allow_ind=true) ~optional options =
  make_entry ~optional (type_variant ~allow_ind options)


let type_flags ?(allow_ind=true) bitcount =
  make_type ~allow_ind (IntRange (Some ~:0, Some ~:((1 lsl bitcount) - 1)))

let entry_flags ?(allow_ind=true) ~optional bitcount =
  make_entry ~optional (type_flags ~allow_ind bitcount)

