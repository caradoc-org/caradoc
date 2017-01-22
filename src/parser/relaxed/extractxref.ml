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


let seek_xref (input : in_channel) (startxref : BoundedInt.t) (length : BoundedInt.t) : unit =
  if startxref >=: length then
    raise (Errors.PDFError ("Invalid startxref", Errors.make_ctxt_pos (Errors.make_pos_file startxref)));
  seek_in input (BoundedInt.to_int startxref)


(***********************)
(* PDF reference 7.5.4 *)
(***********************)
let parsexref_table (xref : XRefTable.t) (input : in_channel) (offset : BoundedInt.t) (intervals : Key.t Intervals.t) : BoundedInt.t * DirectObject.dict_t =
  let trail = ref false in
  let pos = ref offset in

  while not !trail do
    let error_ctxt = Errors.make_ctxt_pos (Errors.make_pos_file !pos) in
    if Params.global.Params.debug then
      Printf.eprintf "Parsing xref table section%s\n" (Errors.ctxt_to_string error_ctxt);

    let ( &> ) p lexbuf = wrap_xrefparser p lexbuf error_ctxt in

    try
      seek_in input (BoundedInt.to_int !pos);
      let lexbuf = Lexing.from_channel input in

      Xrefparser.trailer &> lexbuf;
      trail := true;
      pos := !pos +: ~:(Lexing.lexeme_end lexbuf)
    with _ ->
      seek_in input (BoundedInt.to_int !pos);
      let lexbuf = Lexing.from_channel input in

      let id, len = Xrefparser.xrefsection &> lexbuf in
      for i = 0 to BoundedInt.to_int (len -: ~:1) do
        let gen, value = Xrefparser.xrefentry &> lexbuf in
        XRefTable.add xref (Key.make_gen (id +: ~:i) gen) value
      done;

      pos := !pos +: ~:(Lexing.lexeme_end lexbuf)
  done;

  seek_in input (BoundedInt.to_int !pos);
  let lexbuf = Lexing.from_channel input in
  let trailer = wrap_parser Parser.trailerdict lexbuf (Errors.make_ctxt Key.Trailer (Errors.make_pos_file !pos)) in

  Intervals.add intervals (offset, !pos +: ~:((Lexing.lexeme_end lexbuf) - 1)) Key.Trailer;

  !pos, trailer


(*************************)
(* PDF reference 7.5.8.3 *)
(*************************)
let parsexrefstm_subsection (xref : XRefTable.t) (error_pos : Errors.pos_t) (content : string) (start : BoundedInt.t) (count : BoundedInt.t) (pos : BoundedInt.t ref) (w : BoundedInt.t array) (sum_w : BoundedInt.t) : unit =
  for i = 0 to BoundedInt.to_int (count -: ~:1) do
    let typ = Convert.int_of_bytes content !pos w.(0) ~default:(~:1) () in

    let f1 = Convert.int_of_bytes content (!pos +: w.(0)) w.(1) in
    let f2 = Convert.int_of_bytes content (!pos +: w.(0) +: w.(1)) w.(2) in

    let obj_num = start +: ~:i in
    let error_ctxt = Errors.make_ctxt (Key.make_0 obj_num) (Errors.pos_add_offset error_pos !pos) in
    if Params.global.Params.debug then
      Printf.eprintf "Xref stream entry : type = %s%s\n" (BoundedInt.to_string typ) (Errors.ctxt_to_string error_ctxt);

    begin
      try
        match (BoundedInt.to_int typ) with
        | 0 ->
          let next_free = f1 () in
          if w.(2) = ~:0 then
            Errors.warning_or_pdf_error Params.global.Params.xref_stream_default_zero "Free object entry in xref stream has no next generation number" error_ctxt;
          let next_gen = f2 ~default:(~:0) () in
          XRefTable.add xref (Key.make_gen obj_num next_gen) (XRefTable.make_value next_free XRefTable.Free)

        | 1 ->
          let obj_offset = f1 () in
          let obj_gen = f2 ~default:(~:0) () in
          XRefTable.add xref (Key.make_gen obj_num obj_gen) (XRefTable.make_value obj_offset XRefTable.Inuse)

        | 2 ->
          let obj_stm = f1 () in
          if w.(2) = ~:0 then
            Errors.warning_or_pdf_error Params.global.Params.xref_stream_default_zero "Compressed object entry in xref stream has no index" error_ctxt;
          let obj_idx = f2 ~default:(~:0) () in
          XRefTable.add xref (Key.make_0 obj_num) (XRefTable.make_value obj_stm (XRefTable.Compressed obj_idx))

        | _ ->
          raise (Errors.PDFError ("Error in xref stream", error_ctxt))
      with Convert.ConvertError msg ->
        raise (Errors.PDFError (Printf.sprintf "Convert error in xref stream : %s" msg, error_ctxt))
    end;
    pos := !pos +: sum_w
  done


(***********************)
(* PDF reference 7.5.8 *)
(***********************)
let parsexref_stm (xref : XRefTable.t) (input : in_channel) (offset : BoundedInt.t) (length : BoundedInt.t) (doc : Document.t) : BoundedInt.t * DirectObject.dict_t =
  let error_ctxt_pos = Errors.make_ctxt_pos (Errors.make_pos_file offset) in
  if Params.global.Params.debug then
    Printf.eprintf "Parsing xref stream%s\n" (Errors.ctxt_to_string error_ctxt_pos);

  seek_xref input offset length;
  let lexbuf = Lexing.from_channel input in
  let key, obj = wrap_parser Parser.indirectobj lexbuf error_ctxt_pos in

  let error_ctxt = Errors.make_ctxt key (Errors.make_pos_file offset) in

  let stream_dict, stream_off =
    match obj with
    | IndirectObject.StreamOffset (d, o) ->
      d, o
    | IndirectObject.Complete _ ->
      raise (Errors.PDFError ("Expected stream for xref but got non-stream object", error_ctxt))
  in

  let stream_length = DirectObject.get_nonnegative_int ()
      "Expected non-negative integer" (Errors.ctxt_append_name error_ctxt "Length")
      (DirectObject.dict_find stream_dict "Length") in

  let stream, _ = parsestream key (offset +: stream_off) stream_length input length stream_dict in
  let content = PDFStream.get_decoded stream error_ctxt in

  (*************************)
  (* PDF reference 7.5.8.2 *)
  (*************************)
  if Params.global.Params.debug then
    Printf.eprintf "Decoding xref stream%s\n" (Errors.ctxt_to_string error_ctxt);

  let error_ctxt_w = Errors.ctxt_append_name error_ctxt "W" in
  let error_ctxt_size = Errors.ctxt_append_name error_ctxt "Size" in
  let error_ctxt_index = Errors.ctxt_append_name error_ctxt "Index" in

  let obj_w = DirectObject.dict_find stream_dict "W" in
  let obj_size = DirectObject.dict_find stream_dict "Size" in
  let obj_index = DirectObject.dict_find stream_dict "Index" in

  if Params.global.Params.debug then
    Printf.eprintf "\t/W = %s\n\t/Size = %s\n\t/Index = %s\n" (DirectObject.to_string obj_w) (DirectObject.to_string obj_size) (DirectObject.to_string obj_index);

  let w = DirectObject.get_array_of
      ~length:3 ()
      "Expected array of 3 non-negative ints" error_ctxt_w
      ~transform:(DirectObject.get_nonnegative_int ())
      obj_w in
  let size = DirectObject.get_nonnegative_int ()
      "Expected non-negative integer" error_ctxt_size
      obj_size in
  let index = DirectObject.get_array_of
      ~default:[DirectObject.Int ~:0 ; DirectObject.Int size] ()
      "Expected array of non-negative ints" error_ctxt_index
      ~transform:(DirectObject.get_nonnegative_int ())
      obj_index in

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
  let error_pos_stream = Errors.make_pos_stream key ~:0 in
  for k = 0 to BoundedInt.to_int ((idxlen /: ~:2) -: ~:1) do
    let start = index.(2*k) in
    let count = index.(2*k + 1) in
    parsexrefstm_subsection xref error_pos_stream content start count pos w sum_w
  done;

  Document.add_xrefstm doc key;
  offset, stream_dict


let parsexref (xref : XRefTable.t) (input : in_channel) (offset : BoundedInt.t) (length : BoundedInt.t) (setpos : IntSet.t) (intervals : Key.t Intervals.t) (doc : Document.t) : Errors.error_ctxt * DirectObject.dict_t =
  let error_ctxt = Errors.make_ctxt_pos (Errors.make_pos_file offset) in
  if Params.global.Params.debug then
    Printf.eprintf "Parsing xref table%s\n" (Errors.ctxt_to_string error_ctxt);

  if not (IntSet.add setpos offset) then
    raise (Errors.PDFError ("Cyclic xref tables detected", error_ctxt));

  let ( &> ) p lexbuf = wrap_xrefparser p lexbuf error_ctxt in

  seek_xref input offset length;
  let lexbuf = Lexing.from_channel input in

  let is_xref =
    try
      Xrefparser.xref &> lexbuf;
      true
    with _ ->
      false
  in

  let newoffset, trailer =
    if is_xref then
      parsexref_table xref input (offset +: ~:(Lexing.lexeme_end lexbuf)) intervals
    else
      parsexref_stm xref input offset length doc
  in
  Errors.make_ctxt Key.Trailer (Errors.make_pos_file newoffset), trailer


let rec parsetrailer (error_ctxt : Errors.error_ctxt) (trailer : DirectObject.dict_t) (xref : XRefTable.t) (input : in_channel) (length : BoundedInt.t) (setpos : IntSet.t) (intervals : Key.t Intervals.t) (doc : Document.t) (stats : Stats.t) : unit =
  stats.Stats.updatecount <- stats.Stats.updatecount + 1;

  if Params.global.Params.debug then
    Printf.eprintf "Parsing trailer%s\n" (Errors.ctxt_to_string error_ctxt);

  (* Generic function to handle hybrid-reference files. *)
  let f name x =
    let startxref = DirectObject.get_positive_int ()
        "Expected positive integer" (Errors.ctxt_append_name error_ctxt name)
        x in

    if Params.global.Params.debug then
      Printf.eprintf "Trailer has /%s = %d [0x%x]\n" name (BoundedInt.to_int startxref) (BoundedInt.to_int startxref);

    let new_error_ctxt, newtrailer = parsexref xref input startxref length setpos intervals doc in
    Document.add_trailer doc newtrailer;
    parsetrailer new_error_ctxt newtrailer xref input length setpos intervals doc stats
  in

  (*************************)
  (* PDF reference 7.5.8.4 *)
  (*************************)
  DirectObject.apply_not_null (DirectObject.dict_find trailer "XRefStm") (f "XRefStm");
  DirectObject.apply_not_null (DirectObject.dict_find trailer "Prev") (f "Prev")

