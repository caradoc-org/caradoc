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
open Directobject
open Indirectobject
open Pdfstream
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
        | IndirectObject.StreamOffset (stream_dict, offset) ->
          let stream_length = dereference (DirectObject.dict_find stream_dict "Length") ctxt in
          let len = IndirectObject.get_direct_of
              "Expected integer for stream /Length" (Errors.make_ctxt key off)
              ~transform:(DirectObject.get_nonnegative_int ())
              stream_length in

          let stream, endstreampos =
            parsestream key (off +: offset) len ctxt.FetchCommon.input ctxt.FetchCommon.length stream_dict
          in
          Intervals.add ctxt.FetchCommon.intervals (off, endstreampos) key;
          IndirectObject.Stream stream

        | IndirectObject.Complete obj ->
          Intervals.add ctxt.FetchCommon.intervals (off, endobjpos) key;
          IndirectObject.Direct obj
      )

  and dereference (obj : DirectObject.t) (ctxt : FetchCommon.context) : IndirectObject.t =
    match obj with
    | DirectObject.Reference key ->
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
    | _ -> IndirectObject.Direct obj


  let fetchdecodestream (key : Key.t) (off : BoundedInt.t) (ctxt : FetchCommon.context) (relax : bool) : IndirectObject.t =
    let obj = fetchobject key off ctxt in
    match obj with
    | IndirectObject.Stream s when not (PDFStream.is_decoded s) ->
      let (_:bool) = PDFStream.decode s (Errors.make_ctxt key off) relax in
      let result = IndirectObject.Stream s in
      Document.set ctxt.FetchCommon.doc key result;

      result
    | _ ->
      obj

end

