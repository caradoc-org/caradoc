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


open Boundedint
open Key
open Pdfobject
open Errors

module Predictor : sig

  type t = {
    (* Predictor id *)
    p: BoundedInt.t;
    (* Number of colors *)
    colors: BoundedInt.t;
    (* Bit per color *)
    bpc: BoundedInt.t;
    (* Number of columns *)
    cols: BoundedInt.t;
    (* Early parameter for LZW *)
    early: BoundedInt.t;
  }

  (*   Read predictor parameters from a predictor dictionary
       Args    :
       - error context
       - predictor dictionary
       Returns :
       - predictor
  *)
  val extract_predictor : Errors.error_ctxt -> PDFObject.dict_t -> t

  (*   Compute the PNG Paeth predicting function
       Args    :
       - a, b, c
       Returns :
       - Paeth(a, b, c)
  *)
  val paeth : int -> int -> int -> int

  (*   Decode a stream according to a PNG predictor
       Args    :
       - stream content
       - error context
       - sample size (in bytes, according to colors)
       - width of the predicting frame
       Returns :
       - decoded stream
  *)
  val predict_png : string -> Errors.error_ctxt -> int -> BoundedInt.t -> string

  (*   Decode a stream according to a predictor
       Args    :
       - stream content
       - error context
       - predictor
       Returns :
       - decoded stream
  *)
  val decode_predictor : string -> Errors.error_ctxt -> t -> string

end
