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


module FetchCommon = struct

  type context = {
    doc : Document.t;
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
  val parseobjstm : string -> Key.t -> BoundedInt.t -> BoundedInt.t -> BoundedInt.t -> FetchCommon.context -> ((DirectObject.t * BoundedInt.t) MapKey.t)
  val fetchobjstm : BoundedInt.t -> FetchCommon.context -> ((DirectObject.t * BoundedInt.t) MapKey.t)
end


let traverse_object (key : Key.t) (off : BoundedInt.t) (ctxt : FetchCommon.context) (fetch : Key.t -> BoundedInt.t -> FetchCommon.context -> IndirectObject.t) : IndirectObject.t =
  try
    let traversed = FetchCommon.is_traversed ctxt key in
    if traversed then
      Document.find ctxt.FetchCommon.doc key
    else
      raise (Errors.PDFError ("Circular definition detected", Errors.make_ctxt key off))
  with Not_found ->
    (* begin to traverse object *)
    FetchCommon.begin_traversal ctxt key;
    if Params.global.Params.debug then
      Printf.eprintf "Begin object %s\n" (Key.to_string key);

    let content = fetch key off ctxt in
    Document.add ctxt.FetchCommon.doc key content;

    (* object succesfully traversed *)
    FetchCommon.end_traversal ctxt key;
    if Params.global.Params.debug then
      Printf.eprintf "End object %s\n" (Key.to_string key);

    content


let parsestream (key : Key.t) (offset : BoundedInt.t) (stream_length : BoundedInt.t) (input : in_channel) (length : BoundedInt.t) (stream_dict : DirectObject.dict_t) : PDFStream.t * BoundedInt.t =
  let error_ctxt = Errors.make_ctxt key offset in
  if offset +: stream_length >=: length then
    raise (Errors.PDFError ("Stream size is out of bounds", Errors.ctxt_append_name error_ctxt "Length"));

  let rawcontent = Common.input_substr input offset stream_length in

  let lexbuf = Lexing.from_channel input in
  wrap_parser Parser.endstream (Some (offset +: stream_length)) lexbuf error_ctxt;
  let endstreampos = offset +: stream_length +: ~:((Lexing.lexeme_end lexbuf) - 1) in
  (* TODO : reject streams from external file *)

  PDFStream.make_encoded stream_dict rawcontent, endstreampos

