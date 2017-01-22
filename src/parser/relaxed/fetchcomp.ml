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


open Fetchcommon
open Boundedint
open Errors
open Wrap
open Mapkey
open Document
open Xref
open Directobject
open Indirectobject
open Key
open Params
open Pdfstream


module MakeFetchComp (Fetch : FetchT) = struct

  (***********************)
  (* PDF reference 7.5.7 *)
  (***********************)
  let parseobjstm (content : string) (key : Key.t) (error_ctxt : Errors.error_ctxt) (first : BoundedInt.t) (n : BoundedInt.t) (ctxt : FetchCommon.context) : (DirectObject.t * BoundedInt.t) MapKey.t =
    if Params.global.Params.debug then
      Printf.eprintf "Parse object stream %s\n" (Key.to_string key);

    let nm1 = BoundedInt.to_int (n -: ~:1) in

    let content_length = ~:(String.length content) in
    if first >=: content_length then
      raise (Errors.PDFError ("First entry is beyond size of object stream", error_ctxt));

    let lexbuf = Lexing.from_string (String.sub content 0 (BoundedInt.to_int first)) in
    let l, m =
      try
        wrap_parser Parser.intpair_list lexbuf (Errors.make_ctxt_pos (Errors.make_pos_stream key ~:0))
      with _ ->
        raise (Errors.PDFError ("Parsing error in object stream", error_ctxt))
    in
    let idents = Array.of_list l in
    let offsets = Array.of_list m in

    if Params.global.Params.debug then (
      for i = 0 to (Array.length idents) - 1 do
        Printf.eprintf "In %s : object %s at %s\n" (Key.to_string key) (BoundedInt.to_string idents.(i)) (BoundedInt.to_string offsets.(i));
      done
    );

    if ~:(Array.length offsets) <> n then
      raise (Errors.PDFError ("Number of entries does not match /N in object stream", error_ctxt));

    if offsets.(0) <> ~:0 then
      raise (Errors.PDFError ("First offset must be zero in object stream", error_ctxt));
    if first +: offsets.(nm1) >=: content_length then
      raise (Errors.PDFError ("Last entry offset is beyond object stream size", error_ctxt));

    for i = 1 to nm1 do
      if offsets.(i) <= offsets.(i - 1) then
        raise (Errors.PDFError ("Offsets are not in increasing order in object stream", error_ctxt));
    done;

    let bag = ref MapKey.empty in
    for i = 0 to nm1 do
      let next =
        if i = nm1 then
          content_length
        else
          first +: offsets.(i + 1)
      in
      let me = first +: offsets.(i) in
      let len = next -: me in

      let k = Key.make_0 idents.(i) in
      let error_ctxt_me = Errors.make_ctxt k (Errors.make_pos_stream key me) in

      if MapKey.mem k !bag then
        raise (Errors.PDFError ("Object appears several times in object stream", error_ctxt_me));

      if Params.global.Params.debug then
        Printf.eprintf "In %s : parsing object %s\n" (Key.to_string key) (Key.to_string k);

      let substr = String.sub content (BoundedInt.to_int me) (BoundedInt.to_int len) in
      let lexbuf = Lexing.from_string substr in

      if Params.global.Params.debug then
        Printf.eprintf "Content of object %s : %s\n" (Key.to_string k) substr;

      (* TODO : check that object is not reference, stream, etc... *)
      let obj = wrap_parser Parser.one_object lexbuf error_ctxt_me in
      bag := MapKey.add k (obj, ~:i) !bag;
    done;

    if Params.global.Params.debug then
      Printf.eprintf "Parsed object stream %s\n" (Key.to_string key);

    Document.add_objstm ctxt.FetchCommon.doc key;
    ctxt.FetchCommon.decompressed <- MapKey.add key !bag ctxt.FetchCommon.decompressed;
    !bag


  let fetchobjstm (key : Key.t) (ctxt : FetchCommon.context) : (DirectObject.t * BoundedInt.t) MapKey.t =
    try
      MapKey.find key ctxt.FetchCommon.decompressed
    with Not_found ->
      let entry = XRefTable.find ctxt.FetchCommon.xref key "Object contains compressed objects but is not referenced in xref table" in

      begin
        match entry.XRefTable.kind with
        | XRefTable.Free
        | XRefTable.Compressed _ ->
          raise (Errors.PDFError ("Object stream must not be free or compressed", Errors.make_ctxt_key key))
        | XRefTable.Inuse -> ()
      end;

      let error_ctxt = Errors.make_ctxt key (Errors.make_pos_file entry.XRefTable.off) in

      let obj = Fetch.fetchdecodestream key entry.XRefTable.off ctxt false in
      let stream = IndirectObject.get_stream
          "Object stream must be a stream" error_ctxt
          obj in

      let dict = PDFStream.get_dict stream in

      (* TODO : handle dereference ? *)
      let first = DirectObject.get_nonnegative_int ()
          "Expected non-negative integer" (Errors.ctxt_append_name error_ctxt "First")
          (DirectObject.dict_find dict "First") in

      let n = DirectObject.get_positive_int ()
          "Expected positive integer" (Errors.ctxt_append_name error_ctxt "N")
          (DirectObject.dict_find dict "N") in

      let success = PDFStream.decode stream error_ctxt true in
      if not success then
        raise (Errors.PDFError ("Error decoding object stream", error_ctxt));

      let content = PDFStream.get_decoded stream error_ctxt in
      parseobjstm content key error_ctxt first n ctxt


  let fetchcompressed (key : Key.t) (id : BoundedInt.t) (idx : BoundedInt.t) (ctxt : FetchCommon.context) : IndirectObject.t =
    let key_objstm = Key.make_0 id in
    let error_ctxt = Errors.make_ctxt key (Errors.make_pos_objstm key_objstm idx) in

    traverse_object key ~decrypt:false error_ctxt ctxt (fun () ->
        let bag = fetchobjstm key_objstm ctxt in
        if not (MapKey.mem key bag) then
          raise (Errors.PDFError ("Object not found in object stream", error_ctxt));

        let obj, index = MapKey.find key bag in
        if index <> idx then
          raise (Errors.PDFError ("Compressed object index does not match object stream", error_ctxt));

        IndirectObject.Direct obj
      )

end

