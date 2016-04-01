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


open Fetchcommon
open Boundedint
open Errors
open Wrap
open Pdfobject
open Parsestream
open Xref
open Intervals
open Mapkey
open Document


module MakeFetch (FetchComp : FetchCompT) = struct

  let rec fetchobject (key : Key.t) (off : BoundedInt.t) (ctxt : FetchCommon.context) =
    traverse_object key off ctxt (fun key off ctxt ->
        if off <: ctxt.FetchCommon.length then
          seek_in ctxt.FetchCommon.input (BoundedInt.to_int off)
        else
          raise (Errors.PDFError ("Invalid object position", Errors.make_ctxt key off));

        let lexbuf = Lexing.from_channel ctxt.FetchCommon.input in
        let k, o = wrap_parser Parser.indirectobj (Some off) lexbuf in

        let endobjpos = off +: ~:((Lexing.lexeme_end lexbuf) - 1) in

        if k <> key then
          raise (Errors.PDFError ("Object definition does not match xref table", Errors.make_ctxt key off));

        match o with
        | PDFObject.Stream (stream_dict, _, PDFObject.Offset offset) ->
          let stream_length = dereference (PDFObject.dict_find stream_dict "Length") ctxt in
          let len = PDFObject.get_nonnegative_int ()
              "Expected integer for stream /Length" (Errors.make_ctxt key off)
              stream_length in

          let raw, endstreampos =
            parsestream key (off +: offset) len ctxt.FetchCommon.input ctxt.FetchCommon.length
          in
          Intervals.add ctxt.FetchCommon.intervals (off, endstreampos) key;

          PDFObject.Stream (stream_dict, raw, PDFObject.Raw)
        | _ ->
          Intervals.add ctxt.FetchCommon.intervals (off, endobjpos) key;
          o
      )

  and dereference (obj : PDFObject.t) (ctxt : FetchCommon.context) : PDFObject.t =
    match obj with
    | PDFObject.Reference key ->
      let entry = XRefTable.find ctxt.FetchCommon.xref key "Reference to undeclared object" in

      begin
        match entry.XRefTable.kind with
        | XRefTable.Inuse ->
          (* TODO : check what to do *)
            (*
            dereference (fetchobject (id, gen) off ctxt) ctxt
            *)
          fetchobject key entry.XRefTable.off ctxt
        | XRefTable.Compressed index ->
          FetchComp.fetchcompressed key entry.XRefTable.off index ctxt
        | XRefTable.Free ->
          raise (Errors.PDFError ("Reference to free object", Errors.make_ctxt_key key))
      end
    | _ -> obj


  let fetchdecodestream (key : Key.t) (off : BoundedInt.t) (ctxt : FetchCommon.context) (relax : bool) : PDFObject.t =
    let obj = fetchobject key off ctxt in
    match obj with
    | PDFObject.Stream (stream_dict, raw, PDFObject.Raw) ->
      (* lock object *)
      ctxt.FetchCommon.traversed <- MapKey.add key false ctxt.FetchCommon.traversed;

      let decoded, _ = decode raw (Errors.make_ctxt key off) stream_dict relax in
      let result = PDFObject.Stream (stream_dict, raw, PDFObject.Content decoded) in
      Document.set ctxt.FetchCommon.doc key result;

      (* unlock object *)
      ctxt.FetchCommon.traversed <- MapKey.add key true ctxt.FetchCommon.traversed;
      result
    | _ ->
      obj

end

