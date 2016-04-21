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
open Directobject
open Zlib
open Asciihex
open Ascii85
open Predictor
open Params


module PDFStream = struct

  type t = {
    dictionary : DirectObject.dict_t;
    encoded : string;
    mutable decoded : string option;
  }


  let make_encoded (d : DirectObject.dict_t) (e : string) : t = {
    dictionary = d;
    encoded = e;
    decoded = None;
  }

  let get_dict (s : t) : DirectObject.dict_t =
    s.dictionary

  let set_dict (s : t) (d : DirectObject.dict_t) : t =
    {s with dictionary = d}

  let get_encoded (s : t) : string =
    s.encoded

  let is_decoded (s : t) : bool =
    s.decoded <> None


  let to_string (s : t) : string =
    let decoded = is_decoded s in
    let c =
      match s.decoded with
      | Some x -> x
      | None -> s.encoded
    in

    let buf = Buffer.create 16 in

    let expand =
      match (Params.global.Params.expand_streams, Params.global.Params.stream_limit) with
      | true, None ->
        true
      | true, (Some limit) when (String.length c) <= limit ->
        true
      | _ ->
        false
    in

    let header = Printf.sprintf "stream <%s stream of length %d>" (if decoded then "decoded" else "encoded") (String.length c) in
    DirectObject.dict_to_string_buf buf s.dictionary;
    Buffer.add_char buf '\n';
    Buffer.add_string buf header;
    if expand then (
      Buffer.add_char buf '\n';
      Buffer.add_string buf c;
      Buffer.add_string buf "\nendstream\n"
    );

    Buffer.contents buf


  let to_pdf (s : t) : string =
    let buf = Buffer.create 16 in
    DirectObject.dict_to_pdf_buf buf s.dictionary;
    Buffer.add_string buf "stream\n";
    Buffer.add_string buf s.encoded;
    Buffer.add_string buf "\nendstream";
    Buffer.contents buf


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


  let decode (s : t) (ctxt : Errors.error_ctxt) (relax : bool) : bool =
    if is_decoded s then
      true
    else (
      let filters = DirectObject.get_array_of
          ~default:[] ~accept_one:true ()
          "Invalid value for stream /Filter" ctxt
          ~transform:(DirectObject.get_name ())
          (DirectObject.dict_find s.dictionary "Filter") in

      let count = Array.length filters in
      let def = Array.to_list (Array.make count DirectObject.Null) in
      let params = DirectObject.get_array_of
          ~default:def ~length:count ~accept_one:true()
          "Invalid value for stream /DecodeParms" ctxt
          ~transform:(DirectObject.get_dict ~default:(DirectObject.dict_create ()) ())
          (DirectObject.dict_find s.dictionary "DecodeParms") in

      let f = fun () ->
        let d = decode_filters s.encoded ctxt filters params 0 count in
        s.decoded <- Some d;
        true
      in

      if relax then
        Errors.catch ~fail:(fun () -> false) f
      else
        f ()
    )


  let get_decoded (s : t) (ctxt : Errors.error_ctxt) : string =
    let (_:bool) = decode s ctxt false in
    match s.decoded with
    | Some x ->
      x
    | None ->
      raise (Errors.UnexpectedError "Stream was not decoded after a call to function decode")

end

