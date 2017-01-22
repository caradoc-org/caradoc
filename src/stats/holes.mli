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
open Intervals
open Key

(*   Get the content of a hole if it is does not only contains whitespace and comments
     Args    :
     - input file
     - start of hole
     - length of hole
     Returns :
     - hole content of not blank
*)
val dump_hole : in_channel -> BoundedInt.t -> BoundedInt.t -> string

(*   Print the intervals of objects into a file
     Args    :
     - intervals of objects in file
     - output file name
     - input file
     - input length
*)
val dump_intervals : (Key.t Intervals.t) -> string -> in_channel -> BoundedInt.t -> unit

(*   Print the content of holes into a file
     Args    :
     - intervals of objects in file
     - output file name
     - input file
     - input length
*)
val dump_holes : (Key.t Intervals.t) -> string -> in_channel -> BoundedInt.t -> unit

