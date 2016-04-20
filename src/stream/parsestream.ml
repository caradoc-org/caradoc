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
open Errors
open Common
open Wrap
open Directobject
open Zlib
open Asciihex
open Ascii85
open Predictor


let decode_filter (content : string) (ctxt : Errors.error_ctxt) (filter : string) (params : DirectObject.dict_t) : string =
  match filter with
  | "FlateDecode" ->
    let predictor = Predictor.extract_predictor ctxt params in

    let decoded =
      let decoded1 = Zlib.decode content in
      let len = String.length content in

      match decoded1 with
      (* TODO : don't accept malformed streams ? *)
      | None when len > 0 && content.[len - 1] = '\x0A' ->
        Printf.eprintf "%s\n" "Warning : Flate/Zlib stream with appended newline";
        Zlib.decode (String.sub content 0 (len - 1))
      | _ ->
        decoded1
    in

    begin
      match decoded with
      | None ->
        raise (Errors.PDFError ("Error in Flate/Zlib stream", ctxt))
      | Some d ->
        Predictor.decode_predictor d ctxt predictor
    end
  | "ASCIIHexDecode" ->
    begin
      match ASCIIHex.decode content with
      | None ->
        raise (Errors.PDFError ("Error in ASCIIHex stream", ctxt))
      | Some d ->
        (* TODO : check predictor *)
        d
    end
  | "ASCII85Decode" ->
    begin
      match ASCII85.decode content with
      | None ->
        raise (Errors.PDFError ("Error in ASCII85 stream", ctxt))
      | Some d ->
        (* TODO : check predictor *)
        d
    end
  | "LZWDecode"
  | "RunLengthDecode"
  | "CCITTFaxDecode"
  | "JBIG2Decode"
  | "DCTDecode"
  | "JPXDecode"
  | "Crypt" ->
    (* TODO : implement *)
    raise (Errors.PDFError (Printf.sprintf "Not implemented filter method : %s" filter, ctxt))
  | _ ->
    raise (Errors.PDFError (Printf.sprintf "Invalid stream filter method : %s" filter, ctxt))


let rec decode_filters (content : string) (ctxt : Errors.error_ctxt) (filters : string array) (params : DirectObject.dict_t array) (i : int) (count : int) : string =
  if i < count then (
    let decoded = decode_filter content ctxt filters.(i) params.(i) in
    decode_filters decoded ctxt filters params (i + 1) count
  ) else
    content


let decode (content : string) (ctxt : Errors.error_ctxt) (stream_dict : DirectObject.dict_t) (relax : bool) : string * bool =
  let filters = DirectObject.get_array_of
      ~default:[] ~accept_one:true ()
      "Invalid value for stream /Filter" ctxt
      ~transform:(DirectObject.get_name ())
      (DirectObject.dict_find stream_dict "Filter") in

  let count = Array.length filters in
  let def = Array.to_list (Array.make count DirectObject.Null) in
  let params = DirectObject.get_array_of
      ~default:def ~length:count ~accept_one:true()
      "Invalid value for stream /DecodeParms" ctxt
      ~transform:(DirectObject.get_dict ~default:(DirectObject.dict_create ()) ())
      (DirectObject.dict_find stream_dict "DecodeParms") in

  let f = fun () ->
    let decoded = decode_filters content ctxt filters params 0 count in
    decoded, true
  in

  if relax then
    Errors.catch ~fail:(fun () -> content, false) f
  else
    f ()


let parsestream (key : Key.t) (offset : BoundedInt.t) (stream_length : BoundedInt.t) (input : in_channel) (length : BoundedInt.t) : string * BoundedInt.t =
  if offset +: stream_length >=: length then
    raise (Errors.PDFError ("Stream size is out of bounds", Errors.make_ctxt key offset));

  let rawcontent = Common.input_substr input offset stream_length in

  let lexbuf = Lexing.from_channel input in
  wrap_parser Parser.endstream (Some (offset +: stream_length)) lexbuf;
  let endstreampos = offset +: stream_length +: ~:((Lexing.lexeme_end lexbuf) - 1) in
  (* TODO : reject streams from external file *)

  rawcontent, endstreampos


let parsedecodestream (key : Key.t) (stream_dict : DirectObject.dict_t) (offset : BoundedInt.t) (stream_length : BoundedInt.t) (input : in_channel) (length : BoundedInt.t) (relax : bool) : string * string * bool * BoundedInt.t =
  let rawcontent, endstreampos = parsestream key offset stream_length input length in
  let decoded, success = decode rawcontent (Errors.make_ctxt key offset) stream_dict relax in
  rawcontent, decoded, success, endstreampos

