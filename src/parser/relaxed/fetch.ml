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
        | IndirectObject.StreamOffset (original_stream_dict, offset) ->
          let stream_dict = DirectObject.dict_map_key (fun key value ->
              match key with
              | "Length"
              | "Filter"
              | "DecodeParms" ->
                dereference_rec ctxt value
              | _ ->
                value
            ) original_stream_dict in

          let len = DirectObject.get_nonnegative_int ()
              "Expected integer for stream /Length" (Errors.make_ctxt key off)
              (DirectObject.dict_find stream_dict "Length") in

          let stream, endstreampos =
            parsestream key (off +: offset) len ctxt.FetchCommon.input ctxt.FetchCommon.length stream_dict
          in
          Intervals.add ctxt.FetchCommon.intervals (off, endstreampos) key;
          IndirectObject.Stream stream

        | IndirectObject.Complete obj ->
          Intervals.add ctxt.FetchCommon.intervals (off, endobjpos) key;
          IndirectObject.Direct obj
      )

  and dereference_rec (ctxt : FetchCommon.context) (obj : DirectObject.t) : DirectObject.t =
    match obj with
    | DirectObject.Reference key ->
      begin
        match dereference_key ctxt key with
        | IndirectObject.Stream _ ->
          obj
        | IndirectObject.Direct d ->
          (* We mark the key as currently being traversed to avoid infinite recursion *)
          FetchCommon.begin_traversal ctxt key;
          let result = dereference_rec ctxt d in
          FetchCommon.end_traversal ctxt key;
          result
      end
    | DirectObject.Dictionary d ->
      DirectObject.Dictionary (DirectObject.dict_map (dereference_rec ctxt) d)
    | DirectObject.Array a ->
      DirectObject.Array (List.map (dereference_rec ctxt) a)
    | _ -> obj

  and dereference_key (ctxt : FetchCommon.context) (key : Key.t) : IndirectObject.t =
    let entry = XRefTable.find ctxt.FetchCommon.xref key "Reference to undeclared object" in

    match entry.XRefTable.kind with
    | XRefTable.Inuse ->
      fetchobject key entry.XRefTable.off ctxt
    | XRefTable.Compressed index ->
      FetchComp.fetchcompressed key entry.XRefTable.off index ctxt
    | XRefTable.Free ->
      raise (Errors.PDFError ("Reference to free object", Errors.make_ctxt_key key))


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

