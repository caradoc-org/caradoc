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


open Key
open Directobject
open Indirectobject
open Stats
open Boundedint
open Intervals
open Xref
open Document

(*   Parse a PDF file until extraction of xref sections, i.e. version, xref tables/streams, trailers
     Args    :
     - input channel
     - file statistics
     Returns :
     - length of input
     - intervals of objects in file
     - xref table
     - document
*)
val parse_until_xref : in_channel -> Stats.t -> (BoundedInt.t * Key.t Intervals.t * XRefTable.t * Document.t)

(*   Extract a given object from a PDF file
     Args    :
     - input channel
     - object key
     Returns :
     - object
*)
val extract_object : in_channel -> Key.t -> IndirectObject.t

(*   Extract the trailer(s) from a PDF file
     Args    :
     - input channel
     Returns :
     - list of trailers
*)
val extract_trailers : in_channel -> (DirectObject.dict_t list)

(*   Extract statistics from a PDF file
     Args    :
     - input file name
     - file statistics
*)
val statistics : string -> Stats.t -> unit

(*   Parse a PDF file and extract various data
     Args    :
     - input file name
     Returns :
     - parsed document
*)
val parse_file : string -> Stats.t -> Document.t

(*   Parse a PDF file and extract various data
     Args    :
     - input file name
*)
val check_file : string -> unit

(*   Write a cleaned up version of a file to stdout
     Args    :
     - input file name
     - output file name
*)
val cleanup : string -> string -> unit

