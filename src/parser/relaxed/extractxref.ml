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
open Intset
open Wrap
open Xref
open Pdfstream
open Intervals
open Document
open Directobject
open Indirectobject
open Key
open Stats
open Params
open Fetchcommon


let seek_xref input startxref length =
  if startxref <: length then
    seek_in input (BoundedInt.to_int startxref)
  else
    raise (Errors.PDFError ("Invalid startxref", Errors.make_ctxt_pos startxref))


(***********************)
(* PDF reference 7.5.4 *)
(***********************)
let parsexref_table xref input offset intervals =
  let trail = ref false in
  let pos = ref offset in
  (* TODO : use this shortcut ?
     let ( &> ) p buf = wrap_xrefparser p (Some !pos) buf in
  *)

  while not !trail do
    try
      seek_in input (BoundedInt.to_int !pos);
      let buf = Lexing.from_channel input in

      wrap_xrefparser Xrefparser.trailer (Some !pos) buf;
      trail := true;
      pos := !pos +: ~:(Lexing.lexeme_end buf)
    with _ ->
      seek_in input (BoundedInt.to_int !pos);
      let buf = Lexing.from_channel input in

      let id, len = wrap_xrefparser Xrefparser.xrefsection (Some !pos) buf in
      for i = 0 to BoundedInt.to_int (len -: ~:1) do
        let gen, value = wrap_xrefparser Xrefparser.xrefentry (Some !pos) buf in
        XRefTable.add xref (Key.make_gen (id +: ~:i) gen) value
      done;

      pos := !pos +: ~:(Lexing.lexeme_end buf)
  done;

  seek_in input (BoundedInt.to_int !pos);
  let buf = Lexing.from_channel input in
  let trailer = wrap_parser Parser.trailerdict (Some !pos) buf (Errors.make_ctxt_key Key.Trailer) in

  Intervals.add intervals (offset, !pos +: ~:((Lexing.lexeme_end buf) - 1)) Key.Trailer;

  !pos, trailer


(*************************)
(* PDF reference 7.5.8.3 *)
(*************************)
let parsexrefstm_subsection xref offset content start count pos w sum_w =
  for i = 0 to BoundedInt.to_int (count -: ~:1) do
    let typ = Convert.int_of_bytes content !pos w.(0) ~default:(~:1) () in

    let f1 = Convert.int_of_bytes content (!pos +: w.(0)) w.(1) in
    let f2 = Convert.int_of_bytes content (!pos +: w.(0) +: w.(1)) w.(2) in

    begin
      match typ with
      | _ when typ = ~:0 ->
        let next_free = f1 () in
        let next_gen = f2 () in
        XRefTable.add xref (Key.make_gen (start +: ~:i) next_gen) (XRefTable.make_value next_free XRefTable.Free)

      | _ when typ = ~:1 ->
        let obj_offset = f1 () in
        let obj_gen = f2 ~default:(~:0) () in
        XRefTable.add xref (Key.make_gen (start +: ~:i) obj_gen) (XRefTable.make_value obj_offset XRefTable.Inuse)

      | _ when typ = ~:2 ->
        let obj_stm = f1 () in
        let obj_idx = f2 () in
        XRefTable.add xref (Key.make_0 (start +: ~:i)) (XRefTable.make_value obj_stm (XRefTable.Compressed obj_idx))
      | _ ->
        raise (Errors.PDFError ("Error in xref stream", Errors.make_ctxt (Key.make_0 (start +: ~:i)) offset))
    end;
    pos := !pos +: sum_w
  done


(***********************)
(* PDF reference 7.5.8 *)
(***********************)
let parsexref_stm xref input offset length doc =
  seek_xref input offset length;
  let lexbuf = Lexing.from_channel input in

  let key, obj = wrap_parser Parser.indirectobj (Some offset) lexbuf Errors.ctxt_none in

  let stream_dict, stream_off =
    match obj with
    | IndirectObject.StreamOffset (d, o) ->
      d, o
    | IndirectObject.Complete _ ->
      raise (Errors.PDFError ("Invalid xref", Errors.make_ctxt_pos offset))
  in

  let error_ctxt = Errors.make_ctxt key offset in

  let stream_length = DirectObject.get_nonnegative_int ()
      "Expected non-negative integer" (Errors.ctxt_append_name error_ctxt "Length")
      (DirectObject.dict_find stream_dict "Length") in

  let stream, _ = parsestream key (offset +: stream_off) stream_length input length stream_dict in
  let content = PDFStream.get_decoded stream error_ctxt in

  (*************************)
  (* PDF reference 7.5.8.2 *)
  (*************************)
  let error_ctxt_w = Errors.ctxt_append_name error_ctxt "W" in
  let w = DirectObject.get_array_of
      ~length:3 ()
      "Expected array of 3 non-negative ints" error_ctxt_w
      ~transform:(DirectObject.get_nonnegative_int ())
      (DirectObject.dict_find stream_dict "W") in

  let error_ctxt_size = Errors.ctxt_append_name error_ctxt "Size" in
  let size = DirectObject.get_nonnegative_int ()
      "Expected non-negative integer" error_ctxt_size
      (DirectObject.dict_find stream_dict "Size") in

  let error_ctxt_index = Errors.ctxt_append_name error_ctxt "Index" in
  let index = DirectObject.get_array_of
      ~default:[DirectObject.Int ~:0 ; DirectObject.Int size] ()
      "Expected array of non-negative ints" error_ctxt_index
      ~transform:(DirectObject.get_nonnegative_int ())
      (DirectObject.dict_find stream_dict "Index") in

  let idxlen = ~:(Array.length index) in
  if (BoundedInt.rem idxlen ~:2) <> ~:0 || idxlen <=: ~:0 then
    raise (Errors.PDFError ("Expected pairs of ints", error_ctxt_index));

  let total_count = ref ~:0 in
  for k = 0 to BoundedInt.to_int ((idxlen /: ~:2) -: ~:1) do
    (* TODO : check positive lengths *)
    if k > 0 && (index.(2*k) <: index.(2*k - 2) +: index.(2*k - 1)) then
      raise (Errors.PDFError ("Xref stream subsections must come in ascending order", error_ctxt_index));
    total_count := !total_count +: index.(2*k + 1)
  done;

  let sum_w = w.(0) +: w.(1) +: w.(2) in
  if !total_count *: sum_w <> ~:(String.length content) then
    raise (Errors.PDFError ("Xref stream length does not match parameters", error_ctxt_w));

  let pos = ref ~:0 in
  for k = 0 to BoundedInt.to_int ((idxlen /: ~:2) -: ~:1) do
    let start = index.(2*k) in
    let count = index.(2*k + 1) in
    parsexrefstm_subsection xref offset content start count pos w sum_w
  done;

  Document.add_xrefstm doc key;
  offset, stream_dict


let parsexref xref input offset length setpos intervals doc =
  if not (IntSet.add setpos offset) then
    raise (Errors.PDFError ("Cyclic xref tables detected", Errors.make_ctxt_pos offset));

  let ( &> ) p buf = wrap_xrefparser p (Some offset) buf in

  seek_xref input offset length;
  let buf = Lexing.from_channel input in

  let is_xref =
    try
      Xrefparser.xref &> buf;
      true
    with _ ->
      false
  in

  if is_xref then
    parsexref_table xref input (offset +: ~:(Lexing.lexeme_end buf)) intervals
  else
    parsexref_stm xref input offset length doc


let rec parsetrailer offset trailer xref input length setpos intervals doc stats =
  stats.Stats.updatecount <- stats.Stats.updatecount + 1;

  if Params.global.Params.debug then
    Printf.eprintf "Parsing trailer at offset : %d [0x%x]\n" (BoundedInt.to_int offset) (BoundedInt.to_int offset);

  (* Generic function to handle hybrid-reference files. *)
  let f name x =
    let startxref = DirectObject.get_positive_int ()
        "Expected positive integer" (Errors.make_ctxt_full_name Key.Trailer offset name)
        x in

    if Params.global.Params.debug then
      Printf.eprintf "Trailer has /%s = %d [0x%x]\n" name (BoundedInt.to_int startxref) (BoundedInt.to_int startxref);

    let newoffset, newtrailer = parsexref xref input startxref length setpos intervals doc in
    Document.add_trailer doc newtrailer;
    parsetrailer newoffset newtrailer xref input length setpos intervals doc stats
  in

  (*************************)
  (* PDF reference 7.5.8.4 *)
  (*************************)
  DirectObject.apply_not_null (DirectObject.dict_find trailer "XRefStm") (f "XRefStm");
  DirectObject.apply_not_null (DirectObject.dict_find trailer "Prev") (f "Prev")

