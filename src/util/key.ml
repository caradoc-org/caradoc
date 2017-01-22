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


open Boundedint

type t =
  | Trailer
  | Version
  | Object of int * int


let make_0 (id : BoundedInt.t) : t =
  Object (BoundedInt.to_int id, 0)

let make_gen (id : BoundedInt.t) (gen : BoundedInt.t) : t =
  Object (BoundedInt.to_int id, BoundedInt.to_int gen)

let make_gen_i (id : int) (gen : int) : t =
  Object (id, gen)


let get_obj_ref (x : t) : int * int =
  match x with
  | Object (id, gen) -> id, gen
  | _ -> failwith "Internal error: invalid reference"

let compare (x : t) (y : t) : int =
  Pervasives.compare x y


let to_string (x : t) : string =
  match x with
  | Trailer -> "trailer"
  | Version -> "version"
  | Object (id, 0) -> string_of_int id
  | Object (id, gen) -> (string_of_int id) ^ " [" ^ (string_of_int gen) ^ "]"

let to_string_nosp (x : t) : string =
  match x with
  | Trailer -> "trailer"
  | Version -> "version"
  | Object (id, 0) -> string_of_int id
  | Object (id, gen) -> (string_of_int id) ^ "_" ^ (string_of_int gen)
