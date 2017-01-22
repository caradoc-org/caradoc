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
open Xref
open Intset
open Directobject
open Intervals
open Key
open Document
open Stats
open Errors

(*   Seek input channel to the start of xref table
     Args    :
     - input channel
     - position of the xref table
     - length of input
*)
val seek_xref : in_channel -> BoundedInt.t -> BoundedInt.t -> unit

(*   Parse one xref table section and returns the associated trailer
     Args    :
     - xref table
     - input channel
     - position of the xref section
     - intervals of objects in file
     Returns :
     - position of the trailer
     - trailer dictionary
*)
val parsexref_table : XRefTable.t -> in_channel -> BoundedInt.t -> (Key.t Intervals.t) -> (BoundedInt.t * DirectObject.dict_t)

(*   Parse one xref stream subsection
     Args    :
     - xref table
     - error context position
     - xref stream content
     - first object number of this subsection
     - number of objects in this subsection
     - offset in the stream content
     - widths of entry fields
     - width of an entry
*)
val parsexrefstm_subsection : XRefTable.t -> Errors.pos_t -> string -> BoundedInt.t -> BoundedInt.t -> (BoundedInt.t ref) -> (BoundedInt.t array) -> BoundedInt.t -> unit

(*   Parse one xref stream section and returns the associated xref stream dictionary
     Args    :
     - xref table
     - input channel
     - position of the xref section
     - length of input
     - document of retrieved objects
     Returns :
     - position of the xref stream object
     - xref stream dictionary
*)
val parsexref_stm : XRefTable.t -> in_channel -> BoundedInt.t -> BoundedInt.t -> Document.t -> (BoundedInt.t * DirectObject.dict_t)

(*   Parse one xref section and returns the associated trailer (or xref stream dictionary)
     Args    :
     - xref table
     - input channel
     - position of the xref section
     - length of input
     - set of xref positions in the file
     - intervals of objects in file
     - document of retrieved objects
     Returns :
     - error context of the trailer or xref stream object
     - trailer or xref stream dictionary
*)
val parsexref : XRefTable.t -> in_channel -> BoundedInt.t -> BoundedInt.t -> IntSet.t -> (Key.t Intervals.t) -> Document.t -> (Errors.error_ctxt * DirectObject.dict_t)

(*   Parse a trailer and all the previous tables and trailers
     Args    :
     - error context
     - trailer dictionary
     - xref table
     - input channel
     - length of input
     - set of xref positions in the file
     - intervals of objects in file
     - document of retrieved objects
     - file statistics
*)
val parsetrailer : Errors.error_ctxt -> DirectObject.dict_t -> XRefTable.t -> in_channel -> BoundedInt.t -> IntSet.t -> (Key.t Intervals.t) -> Document.t -> Stats.t -> unit

