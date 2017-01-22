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
open Errors
open Directobject

(*************************)
(* PDF reference 7.4.4.4 *)
(*************************)
module Predictor = struct

  type t = {
    p: BoundedInt.t;
    colors: BoundedInt.t;
    bpc: BoundedInt.t;
    cols: BoundedInt.t;
    early: BoundedInt.t;
  }

  let extract_predictor (ctxt_params : Errors.error_ctxt) (params : DirectObject.dict_t) : t =
    let f name def =
      DirectObject.get_positive_int
        ~default:(~:def) ()
        "Expected positive integer" (Errors.ctxt_append_name ctxt_params name)
        (DirectObject.dict_find params name)
    in

    {p = f "Predictor" 1 ; colors = f "Colors" 1 ; bpc = f "BitsPerComponent" 8 ; cols = f "Columns" 1 ; early = f "Early" 1}


  let paeth (a : int) (b : int) (c : int) : int =
    let p = a + b - c in
    let pa = abs (p - a) in
    let pb = abs (p - b) in
    let pc = abs (p - c) in

    if pa <= pb && pa <= pc then
      a
    else if pb <= pc then
      b
    else
      c


  let predict_png (content : string) (ctxt : Errors.error_ctxt) (sample_size : int) (width : BoundedInt.t) : string =
    (* TODO : check alignment *)
    let len = ~:(String.length content) in
    let wplus1 = width +: ~:1 in
    let w = BoundedInt.to_int width in
    let wp1 = BoundedInt.to_int wplus1 in
    let height = len /: wplus1 in
    if height *: wplus1 <> len then
      raise (Errors.PDFError ("Invalid input size for PNG filter predictor", ctxt));

    (* helper functions *)
    let top buf x y =
      if y = 0 then
        0
      else
        Char.code (Buffer.nth buf ((y - 1) * w + x))
    in

    let left buf x y =
      if x < sample_size then
        0
      else
        Char.code (Buffer.nth buf (y * w + x - sample_size))
    in

    let topleft buf x y =
      if x < sample_size || y = 0 then
        0
      else
        Char.code (Buffer.nth buf ((y - 1) * w + x - sample_size))
    in

    let write buf c =
      Buffer.add_char buf (Char.chr (c mod 256))
    in


    let result = Buffer.create (BoundedInt.to_int (width *: height)) in
    for y = 0 to BoundedInt.to_int (height -: ~:1) do
      let pred = Char.code content.[y * wp1] in
      match pred with
      | 0 ->
        Buffer.add_substring result content (y * wp1 + 1) w
      | 1 ->
        for x = 0 to w - 1 do
          write result ((Char.code content.[y * wp1 + x + 1]) + (left result x y))
        done
      | 2 ->
        for x = 0 to w - 1 do
          write result ((Char.code content.[y * wp1 + x + 1]) + (top result x y))
        done
      | 3 ->
        for x = 0 to w - 1 do
          let a = left result x y in
          let b = top result x y in
          let avg = (a + b) / 2 in

          write result ((Char.code content.[y * wp1 + x + 1]) + avg)
        done
      | 4 ->
        for x = 0 to w - 1 do
          let a = left result x y in
          let b = top result x y in
          let c = topleft result x y in

          write result ((Char.code content.[y * wp1 + x + 1]) + (paeth a b c))
        done
      | _ ->
        raise (Errors.PDFError ("Invalid PNG filter predictor method", ctxt));
    done;
    Buffer.contents result


  let decode_predictor (content : string) (ctxt : Errors.error_ctxt) (ctxt_params : Errors.error_ctxt) (predictor : t) : string =
    match (BoundedInt.to_int predictor.p) with
    | 1 -> content
    | 10
    | 11
    | 12
    | 13
    | 14
    | 15 ->
      if predictor.bpc <> ~:8 then
        raise (Errors.PDFError ("Not implemented filter predictor method", ctxt_params));
      let sample_size = predictor.colors in
      let width = predictor.colors *: predictor.cols in
      predict_png content ctxt (BoundedInt.to_int sample_size) width
    (* TODO : implement *)
    | 2 ->
      raise (Errors.PDFError ("Not implemented filter predictor method", Errors.ctxt_append_name ctxt_params "Predictor"));
    | _ ->
      raise (Errors.PDFError ("Invalid filter predictor method", Errors.ctxt_append_name ctxt_params "Predictor"))

end
