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


(* int type that raise an exception on integer overflow *)
module BoundedInt : sig

  type t
  exception IntegerError of string

  val of_int : int -> t
  val of_int64 : int64 -> t
  val to_int : t -> int
  val to_string : t -> string

  val int_of_string : string -> t
  val uint_of_string : string -> t

  val compare : t -> t -> int

  val rem : t -> t -> t

end

(* specialized operators *)
val ( ~: ) : int -> BoundedInt.t

val ( +: ) : BoundedInt.t -> BoundedInt.t -> BoundedInt.t
val ( -: ) : BoundedInt.t -> BoundedInt.t -> BoundedInt.t
val ( *: ) : BoundedInt.t -> BoundedInt.t -> BoundedInt.t
val ( /: ) : BoundedInt.t -> BoundedInt.t -> BoundedInt.t
val ( <<: ) : BoundedInt.t -> int -> BoundedInt.t

val ( <: ) : BoundedInt.t -> BoundedInt.t -> bool
val ( >: ) : BoundedInt.t -> BoundedInt.t -> bool
val ( <=: ) : BoundedInt.t -> BoundedInt.t -> bool
val ( >=: ) : BoundedInt.t -> BoundedInt.t -> bool

