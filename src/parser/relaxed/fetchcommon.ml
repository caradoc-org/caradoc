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
open Pdfobject
open Errors
open Intervals
open Params


module FetchCommon = struct

  type context = {
    doc : Document.t;
    input : in_channel;
    length : BoundedInt.t;
    xref : XRefTable.t;
    mutable traversed : bool MapKey.t;
    mutable decompressed : ((PDFObject.t * BoundedInt.t) MapKey.t) MapKey.t;
    intervals : Key.t Intervals.t;
    mutable streamerrorfile : string option;
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
      streamerrorfile = Params.global.Params.stream_errors_filename;
    }

end


module type FetchT = sig
  val fetchobject : Key.t -> BoundedInt.t -> FetchCommon.context -> PDFObject.t
  val dereference : PDFObject.t -> FetchCommon.context -> PDFObject.t
  val fetchdecodestream : Key.t -> BoundedInt.t -> FetchCommon.context -> bool -> PDFObject.t
end

module type FetchCompT = sig
  val fetchcompressed : Key.t -> BoundedInt.t -> BoundedInt.t -> FetchCommon.context -> PDFObject.t
  val parseobjstm : string -> Key.t -> BoundedInt.t -> BoundedInt.t -> BoundedInt.t -> FetchCommon.context -> ((PDFObject.t * BoundedInt.t) MapKey.t)
  val fetchobjstm : BoundedInt.t -> FetchCommon.context -> ((PDFObject.t * BoundedInt.t) MapKey.t)
end


let traverse_object (key : Key.t) (off : BoundedInt.t) (ctxt : FetchCommon.context) (fetch : Key.t -> BoundedInt.t -> FetchCommon.context -> PDFObject.t) : PDFObject.t =
  try
    let traversed = MapKey.find key ctxt.FetchCommon.traversed in
    if traversed then
      Document.find ctxt.FetchCommon.doc key
    else
      raise (Errors.PDFError ("Circular definition detected", Errors.make_ctxt key off))
  with Not_found ->
    (* begin to traverse object *)
    ctxt.FetchCommon.traversed <- MapKey.add key false ctxt.FetchCommon.traversed;
    if Params.global.Params.debug then
      Printf.eprintf "Begin object %s\n" (Key.to_string key);

    let content = fetch key off ctxt in
    Document.add ctxt.FetchCommon.doc key content;

    (* object succesfully traversed *)
    ctxt.FetchCommon.traversed <- MapKey.add key true ctxt.FetchCommon.traversed;
    if Params.global.Params.debug then
      Printf.eprintf "End object %s\n" (Key.to_string key);

    content

