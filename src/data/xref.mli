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


open Mapkey
open Key
open Boundedint

(* xref table, i.e. mapping from objects to offsets in file *)
module XRefTable : sig

  type kind_t = Free | Inuse | Compressed of BoundedInt.t
  (* TODO : check positions of object *)
  type value_t = {
    off : BoundedInt.t;
    kind : kind_t;
  }
  type t


  val create : unit -> t
  val make_value : BoundedInt.t -> kind_t -> value_t
  val print : out_channel -> t -> unit
  val add : t -> Key.t -> value_t -> unit
  val compare : t -> t -> unit
(*
val debug : out_channel -> t -> unit
*)

  val mem : t -> Key.t -> bool
  val find_list : t -> Key.t -> value_t list
  val find : t -> Key.t -> string -> value_t

  val iter : (Key.t -> (value_t list) -> unit) -> t -> unit
  val iter_all : (Key.t -> value_t -> unit) -> t -> unit
  val fold : (Key.t -> (value_t list) -> 'a -> 'a) -> t -> 'a -> 'a
  val fold_all : (Key.t -> value_t -> 'a -> bool -> 'a) -> t -> 'a -> 'a

  val cleanup_zero_offsets : t -> unit

end
