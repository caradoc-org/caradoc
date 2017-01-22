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

(* Associative table from intervals to a type *)
module Intervals : sig

  (* Bounds are inclusive with respect to overlap *)
  type interval = BoundedInt.t * BoundedInt.t
  type 'a overlap_t = interval * 'a * interval * 'a
  type 'a t

  val create : unit -> 'a t
  val add : 'a t -> interval -> 'a -> unit
  val check_overlaps : 'a t -> ('a overlap_t) option

  val iter : (interval -> 'a -> unit) -> 'a t -> unit
  val fold : (interval -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

end

