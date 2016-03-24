(*****************************************************************************)
(*  Caradoc: a PDF parser and validator                                      *)
(*  Copyright (C) 2015 ANSSI                                                 *)
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
open Boundedint
open Pdfobject
open Errors

(*   Decode a stream content with a given filter
     Args    :
     - stream content
     - error context
     - filter
     - filter params
     Returns :
     - decoded stream
*)
val decode_filter : string -> Errors.error_ctxt -> string -> PDFObject.dict_t -> string

(*   Decode a stream content with given chained filters
     Args    :
     - stream content
     - error context
     - filters
     - params of each filter
     - index of current filter
     - number of filters
     Returns :
     - decoded stream
*)
val decode_filters : string -> Errors.error_ctxt -> (string array) -> (PDFObject.dict_t array) -> int -> int -> string

(*   Decode a stream content
     Args    :
     - stream content
     - error context
     - stream dictionary
     - relax unsupported filters (warning instead of exception)
     Returns :
     - decoded stream
     - success
*)
val decode : string -> Errors.error_ctxt -> PDFObject.dict_t -> bool -> (string * bool)

(*   Parse a stream
     Args    :
     - id of object (to use for error report)
     - offset of encoded stream
     - length of encoded stream
     - input file
     - length of input
     Returns :
     - raw stream
     - offset of end of object in file
*)
val parsestream : Key.t -> BoundedInt.t -> BoundedInt.t -> in_channel -> BoundedInt.t -> (string * BoundedInt.t)

(*   Parse and decode a stream
     Args    :
     - id of object (to use for error report)
     - stream dictionary
     - offset of encoded stream
     - length of encoded stream
     - input file
     - length of input
     - relax unsupported filters (warning instead of exception)
     Returns :
     - raw stream
     - decoded stream
     - success
     - offset of end of object in file
*)
val parsedecodestream : Key.t -> PDFObject.dict_t -> BoundedInt.t -> BoundedInt.t -> in_channel -> BoundedInt.t -> bool -> (string * string * bool * BoundedInt.t)

