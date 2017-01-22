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


open Document
open Boundedint
open Xref
open Mapkey
open Key
open Directobject
open Indirectobject
open Errors
open Intervals
open Params
open Common
open Wrap
open Pdfstream
open Crypto


module FetchCommon = struct

  type context = {
    doc : Document.t;
    crypto : Crypto.t option;
    input : in_channel;
    length : BoundedInt.t;
    xref : XRefTable.t;
    mutable traversed : bool MapKey.t;
    mutable decompressed : ((DirectObject.t * BoundedInt.t) MapKey.t) MapKey.t;
    intervals : Key.t Intervals.t;
  }

  let make_context (input : in_channel) (length : BoundedInt.t) (xref : XRefTable.t) (intervals : Key.t Intervals.t) (doc : Document.t) : context =
    {
      doc = doc;
      crypto = Document.crypto doc;
      input = input;
      length = length;
      xref = xref;
      traversed = MapKey.empty;
      decompressed = MapKey.empty;
      intervals = intervals;
    }

  let begin_traversal (ctxt : context) (key : Key.t) : unit =
    ctxt.traversed <- MapKey.add key false ctxt.traversed

  let end_traversal (ctxt : context) (key : Key.t) : unit =
    ctxt.traversed <- MapKey.add key true ctxt.traversed

  let is_traversed (ctxt : context) (key : Key.t) : bool =
    MapKey.find key ctxt.traversed

end


module type FetchT = sig
  val fetchobject : Key.t -> BoundedInt.t -> FetchCommon.context -> IndirectObject.t
  val fetchdecodestream : Key.t -> BoundedInt.t -> FetchCommon.context -> bool -> IndirectObject.t
end

module type FetchCompT = sig
  val fetchcompressed : Key.t -> BoundedInt.t -> BoundedInt.t -> FetchCommon.context -> IndirectObject.t
  val parseobjstm : string -> Key.t -> Errors.error_ctxt -> BoundedInt.t -> BoundedInt.t -> FetchCommon.context -> ((DirectObject.t * BoundedInt.t) MapKey.t)
  val fetchobjstm : Key.t -> FetchCommon.context -> ((DirectObject.t * BoundedInt.t) MapKey.t)
end


let traverse_object (key : Key.t) ~(decrypt : bool) (error_ctxt : Errors.error_ctxt) (ctxt : FetchCommon.context) (fetch : unit -> IndirectObject.t) : IndirectObject.t =
  try
    if FetchCommon.is_traversed ctxt key then
      Document.find_obj ctxt.FetchCommon.doc key
    else
      raise (Errors.PDFError ("Circular definition detected", error_ctxt))
  with Not_found ->
    (* begin to traverse object *)
    FetchCommon.begin_traversal ctxt key;
    if Params.global.Params.debug then
      Printf.eprintf "Begin object %s\n" (Key.to_string key);

    (* Special cases: do not decrypt xref tables and metadata streams *)
    let is_exception (o : IndirectObject.t) (crypto : Crypto.t) : bool =
      match IndirectObject.get_dict o with
      | None ->
        false
      | Some d ->
        let typ = DirectObject.get_name
            ~default:"" ()
            "Expected name" (Errors.make_ctxt_name key "Type")
            (DirectObject.dict_find d "Type")
        in

        typ = "XRef" || (typ = "Metadata" && not (Crypto.encrypt_meta crypto))
    in

    let o = fetch () in
    let content =
      if decrypt then (
        match ctxt.FetchCommon.crypto with
        | Some c when not (is_exception o c) ->
          IndirectObject.decrypt c key o
        | _ ->
          o
      ) else
        o
    in
    Document.add ctxt.FetchCommon.doc key content;

    (* object succesfully traversed *)
    FetchCommon.end_traversal ctxt key;
    if Params.global.Params.debug then
      Printf.eprintf "End object %s\n" (Key.to_string key);

    content


let parsestream (key : Key.t) (offset : BoundedInt.t) (stream_length : BoundedInt.t) (input : in_channel) (length : BoundedInt.t) (stream_dict : DirectObject.dict_t) : PDFStream.t * BoundedInt.t =
  let error_ctxt = Errors.make_ctxt key (Errors.make_pos_file offset) in
  if offset +: stream_length >=: length then
    raise (Errors.PDFError ("Stream size is out of bounds", Errors.ctxt_append_name error_ctxt "Length"));

  let rawcontent = Common.input_substr input offset stream_length in

  let lexbuf = Lexing.from_channel input in
  wrap_parser Parser.endstream lexbuf (Errors.ctxt_add_offset error_ctxt stream_length);
  let endstreampos = offset +: stream_length +: ~:((Lexing.lexeme_end lexbuf) - 1) in
  (* TODO : reject streams from external file *)

  PDFStream.make_encoded stream_dict rawcontent, endstreampos

