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
open Setkey
open Type

(* Directed graph of PDF objects, linked by references *)
module Graph : sig

  type node_t = Key.t
  type t

  val create : unit -> t
  val neighbors : t -> node_t -> (node_t list)
  val add_vertex : t -> node_t -> unit
  val add_edge : t -> (node_t * node_t) -> unit

  val iter_vertices : (SetKey.elt -> unit) -> t -> unit
  val iter_edges : (node_t -> node_t -> unit) -> t -> unit
  val iter_neighbors : (node_t -> unit) -> t -> node_t -> unit

  val equals : t -> t -> bool

  val print_dot : t -> string -> unit
  val print_dot_types : t -> Type.kind_t MapKey.t -> string -> unit
  val print_visjs : t -> string -> unit

end
