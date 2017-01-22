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


open Wrap
open Boundedint
open Errors
open Xref
open Document
open Intset
open Extractxref
open Directobject
open Indirectobject
open Extractobjects
open Graph
open Intervals
open Holes
open Fetchimpl
open Fetchcommon
open Typechecker
open Stats
open Mapkey
open Graphchecker
open Algo
open Params
open Pdfstream
open Cryptoparse
open Crypto
open Contentstream
open Cleanupcs
open Openfile


(*   Find version of PDF file and check that it is in [1.0, 1.7]
     Args    :
     - input channel
     - intervals of objects in file
     Returns :
     - major version
     - minor version
*)
let check_version (input : in_channel) (intervals : Key.t Intervals.t) : int * int =
  let error_ctxt = Errors.make_ctxt_pos (Errors.make_pos_file ~:0) in
  let lexbuf = Lexing.from_channel input in
  let major, minor = wrap_xrefparser Xrefparser.version lexbuf error_ctxt in

  if major <> 1 || minor < 0 || minor > 7 then
    raise (Errors.PDFError (Printf.sprintf "Invalid PDF version : %d.%d" major minor, error_ctxt));

  Intervals.add intervals (~:0, ~:((Lexing.lexeme_end lexbuf) - 1)) Key.Version;
  major, minor


(*   Find the beginning of the line
     Args    :
     - input channel
     - pos, the position of the end of the line
     Returns :
     - position of the start of the line
     - position of the end of the previous line

     pos is the beginning of an eol marker, we want to find the previous one (or
     the start of file)
*)
let line_before (input : in_channel) (pos : BoundedInt.t) : BoundedInt.t * BoundedInt.t =
  let eolbefore = ref ~:0 in
  let startline = ref ~:0 in
  let status = ref '\x00' in

  (* Find previous eol marker *)
  begin
    try
      for i = (BoundedInt.to_int pos) - 1 downto 0 do
        seek_in input i;
        let c = input_char input in
        match c with
        | '\x0D'
        | '\x0A' ->
          status := c;
          eolbefore := ~:i;
          startline := ~:i +: ~:1;
          raise Exit
        | _ -> ()
      done
    with Exit ->
      ()
  end;

  (* Handle CRLF *)
  if !status = '\x0A' && !eolbefore >: ~:0 then (
    seek_in input (BoundedInt.to_int (!eolbefore -: ~:1));
    if input_char input = '\x0D' then
      eolbefore := !eolbefore -: ~:1;
  );

  !startline, !eolbefore


(*   Find the start of the xref table
     Args    :
     - input channel
     - length of input
     - intervals of objects in file
     Returns :
     - found or not
     - position of the "startxref" keyword
*)
let find_xref (input : in_channel) (length : BoundedInt.t) (intervals : Key.t Intervals.t) : bool * BoundedInt.t =
  let result = ref ~:0 in
  let status = ref 3 in

  Errors.print (fun () ->
      try
        let eolpos = ref length in

        while true do
          let pos, eolbefore = line_before input !eolpos in

          if eolbefore >= !eolpos then
            raise Exit;

          let error_ctxt = Errors.make_ctxt_pos (Errors.make_pos_file pos) in
          seek_in input (BoundedInt.to_int pos);
          let lexbuf = Lexing.from_channel input in
          begin
            match !status with
            | 3 ->
              if wrap_xrefparser Xrefparser.eofmarker lexbuf error_ctxt then
                status := 2
            | 2 ->
              let (_:BoundedInt.t) = wrap_xrefparser Xrefparser.startxref2 lexbuf error_ctxt in
              status := 1
            | 1 ->
              let x = wrap_xrefparser Xrefparser.startxref lexbuf error_ctxt in
              result := x;
              Intervals.add intervals (pos, length -: ~:1) Key.Trailer;
              status := 0
            | _ ->
              raise Exit
          end;

          if !status = 0 then
            raise Exit;

          eolpos := eolbefore;
        done;
      with Exit ->
        ()
    );

  !status = 0, !result


(*   Print the xref table entries into a file
     Args    :
     - xref table
     - output file name
*)
let dump_xref (xref : XRefTable.t) (filename : string) : unit =
  let out = open_out_bin filename in
  XRefTable.print out xref;
  close_out out


(*   Dump all PDF objects into a file
     Args    :
     - PDF objects
     - output file name
*)
let dump_objects (doc : Document.t) (filename : string) : unit =
  let out = open_out_bin filename in
  Document.print out doc;
  close_out out


let check_header (input : in_channel) (stats : Stats.t) (intervals : Key.t Intervals.t) : unit =
  let vmajor, vminor = check_version input intervals in
  stats.Stats.version <- Stats.Version vminor;
  if Params.global.Params.verbose then
    Printf.printf "File version is : %d.%d\n" vmajor vminor


let check_signature (input : in_channel) (length : BoundedInt.t) (stats : Stats.t) : unit =
  let error_ctxt = Errors.make_ctxt_pos (Errors.make_pos_file ~:0) in
  if length <: ~:4 then
    raise (Errors.PDFError ("Invalid PDF signature", error_ctxt));

  seek_in input 0;
  if not (input_char input == '%' && input_char input == 'P' && input_char input == 'D' && input_char input == 'F') then
    raise (Errors.PDFError ("Invalid PDF signature", error_ctxt));

  stats.Stats.version <- Stats.Unknown


let fetch_encrypt_dict (input : in_channel) (length : BoundedInt.t) (xref : XRefTable.t) (encrypt : DirectObject.t) : DirectObject.dict_t * Errors.error_ctxt =
  match encrypt with
  | DirectObject.Dictionary d ->
    d, (Errors.make_ctxt_name Key.Trailer "Encrypt")
  | DirectObject.Reference key ->
    let entry = XRefTable.find xref key "/Encrypt object not found in xref table" in
    let error_ctxt = Errors.make_ctxt_key key in

    let ctxt = FetchCommon.make_context input length xref (Intervals.create ()) (Document.create ()) in
    let obj =
      match entry.XRefTable.kind with
      | XRefTable.Inuse ->
        FetchImpl.fetchobject key entry.XRefTable.off ctxt
      | XRefTable.Compressed index ->
        FetchCompImpl.fetchcompressed key entry.XRefTable.off index ctxt
      | XRefTable.Free ->
        raise (Errors.PDFError ("/Encrypt object is free", error_ctxt))
    in

    let d = IndirectObject.get_direct_of
        "/Encrypt shall be a dictionary" error_ctxt
        ~transform:(DirectObject.get_dict ())
        obj in

    d, (Errors.make_ctxt_key key)
  | _ ->
    raise (Errors.PDFError ("Expected dictionary or reference", Errors.make_ctxt_name Key.Trailer "Encrypt"))


let parse_crypto_params (doc : Document.t) (input : in_channel) (length : BoundedInt.t) (xref : XRefTable.t) (trailer : DirectObject.dict_t) (stats : Stats.t) : unit =
  DirectObject.apply_not_null (DirectObject.dict_find trailer "Encrypt") (fun enc ->
      if not Params.global.Params.decrypt then
        raise (Errors.PDFError ("Encrypted document", Errors.make_ctxt_key Key.Trailer));

      stats.Stats.encrypted <- true;
      if Params.global.Params.debug then
        Printf.eprintf "File is encrypted\n";

      let id = DirectObject.dict_find trailer "ID" in
      let docid = DirectObject.get_array_of
          ~length:2 () "Expected array of two strings" (Errors.make_ctxt_name Key.Trailer "ID")
          ~transform:(DirectObject.get_string ())
          id
      in
      if Params.global.Params.debug then
        Printf.eprintf "/ID array is: %s\n" (DirectObject.to_string id);

      let encrypt, error_ctxt = fetch_encrypt_dict input length xref enc in
      if Params.global.Params.debug then
        Printf.eprintf "∕Encrypt dictionary is: %s\n" (DirectObject.dict_to_string encrypt);

      let params = CryptoParse.parse_encrypt_dict docid.(0) encrypt error_ctxt in
      let crypto, verify_u, verify_o = Crypto.make_crypto params Params.global.Params.user_password Params.global.Params.owner_password (Errors.key_of_ctxt error_ctxt) in

      stats.Stats.verify_user <- verify_u;
      stats.Stats.verify_owner <- verify_o;

      Document.add_crypto doc crypto
    )


let parse_until_xref (input : in_channel) (stats : Stats.t) : (BoundedInt.t * Key.t Intervals.t * XRefTable.t * Document.t) =
  let length = ~:(in_channel_length input) in
  if Params.global.Params.verbose then
    Printf.printf "File has length : %d [0x%x]\n" (BoundedInt.to_int length) (BoundedInt.to_int length);

  let intervals = Intervals.create () in
  if Params.global.Params.allow_invalid_version then
    try
      check_header input stats intervals
    with
    | Errors.LexingError _
    | Errors.ParseError _
    | Errors.PDFError _ ->
      check_signature input length stats;
      Printf.eprintf "Warning : Invalid PDF version\n"
  else
    check_header input stats intervals;

  let found, startxref = find_xref input length intervals in
  if not found then
    raise (Errors.PDFError ("No startxref found", Errors.ctxt_none));

  if Params.global.Params.verbose then
    Printf.printf "startxref : %d [0x%x]\n" (BoundedInt.to_int startxref) (BoundedInt.to_int startxref);

  let xref = XRefTable.create () in
  let setpos = IntSet.create () in
  let doc = Document.create () in
  let error_ctxt, trailer = parsexref xref input startxref length setpos intervals doc in

  Document.add_trailer doc trailer;
  parsetrailer error_ctxt trailer xref input length setpos intervals doc stats;
  Document.finalize_trailers doc;

  if Params.global.Params.zero_offset_as_free then
    XRefTable.cleanup_zero_offsets xref;

  (* Fetch /Encrypt parameters *)
  if not Params.global.Params.ignore_crypt then
    parse_crypto_params doc input length xref trailer stats;

  length, intervals, xref, doc


(*   Decode an object if it is a stream
     Args    :
     - key
     - object
*)
let objdecodestream (key : Key.t) (obj : IndirectObject.t) : IndirectObject.t =
  match obj with
  | IndirectObject.Stream s ->
    let success = PDFStream.decode s (Errors.make_ctxt_key key) Params.global.Params.relax_streams in
    if success then
      IndirectObject.Stream s
    else
      obj
  | IndirectObject.Direct _ ->
    obj


(*   Reencode an object if it is a stream
     Args    :
     - filter to reencode with
     - key
     - object
*)
let objreencodestream (filter : string) (key : Key.t) (obj : IndirectObject.t) : IndirectObject.t =
  match obj with
  | IndirectObject.Stream s ->
    let stream, success = PDFStream.reencode s (Errors.make_ctxt_key key) Params.global.Params.relax_streams filter in
    if success then
      IndirectObject.Stream stream
    else
      obj
  | IndirectObject.Direct _ ->
    obj


let extract_object (input : in_channel) (key : Key.t) : IndirectObject.t =
  let length, intervals, xref, doc = parse_until_xref input (Stats.create ()) in

  let entry = XRefTable.find xref key "Object not found in xref table" in

  let ctxt = FetchCommon.make_context input length xref intervals doc in
  let obj =
    match entry.XRefTable.kind with
    | XRefTable.Inuse ->
      FetchImpl.fetchobject key entry.XRefTable.off ctxt
    | XRefTable.Compressed index ->
      FetchCompImpl.fetchcompressed key entry.XRefTable.off index ctxt
    | XRefTable.Free ->
      raise (Errors.PDFError ("Object is free", Errors.make_ctxt_key key))
  in

  close_in input;
  if Params.global.Params.decode_streams then
    objdecodestream key obj
  else
    obj


let extract_trailers (input : in_channel) : DirectObject.dict_t list =
  let _length, _intervals, _xref, doc = parse_until_xref input (Stats.create ()) in
  close_in input;

  Document.trailers doc


let apply_option (f : 'a -> 'b) (x : 'a option) : 'b =
  match x with
  | None -> ()
  | Some y -> f y


let parse_nonstrict (input : in_channel) (stats : Stats.t) : Document.t =
  let length, intervals, xref, doc = parse_until_xref input stats in

  if Params.global.Params.verbose then
    Printf.printf "Xref table(s) extracted successfully\n";
  apply_option (fun f -> dump_xref xref f) Params.global.Params.xref_filename;

  XRefTable.iter_all
    (fun key entry ->
       match entry.XRefTable.kind, key with
       | XRefTable.Compressed _, _ ->
         stats.Stats.objstm <- true
       | XRefTable.Free, Key.Object (n, _) when n != 0 ->
         stats.Stats.free <- true
       | _ -> ()
    ) xref;

  parseobjects input length xref intervals doc;
  if Params.global.Params.verbose then
    Printf.printf "Objects extracted successfully\n";

  apply_option (fun f -> dump_intervals intervals f input length) Params.global.Params.loc_filename;
  apply_option (fun f -> dump_holes intervals f input length) Params.global.Params.holes_filename;

  if not Params.global.Params.allow_overlaps then (
    let overlap = Intervals.check_overlaps intervals in
    apply_option (fun ((low1, high1), key1, (low2, high2), key2) ->
        let message = Printf.sprintf "The following objects overlap : %s at [0x%x, 0x%x] and %s at [0x%x, 0x%x]" (Key.to_string key1) (BoundedInt.to_int low1) (BoundedInt.to_int high1) (Key.to_string key2) (BoundedInt.to_int low2) (BoundedInt.to_int high2) in
        raise (Errors.PDFError (message, Errors.ctxt_none))
      ) overlap
  );

  (* TODO check content of holes in file ? *)

  if Params.global.Params.verbose then
    Printf.printf "Closing source file !\n";
  close_in input;
  doc


let parse_strict (input : in_channel) (stats : Stats.t) : Document.t =
  let lexbuf = Lexing.from_channel input in

  let version, doc = wrap_strictparser Strictparser.file lexbuf (Errors.make_ctxt_pos (Errors.make_pos_file ~:0)) in
  close_in input;
  let _vmajor, vminor = version in
  stats.Stats.version <- Stats.Version vminor;
  stats.Stats.updatecount <- 0;

  (* Check generation numbers *)
  Document.iter_objects (fun key _ ->
      let error_ctxt = Errors.make_ctxt_key key in
      match key with
      | Key.Object (_, 0) -> ()
      | Key.Object _ -> raise (Errors.PDFError ("Object has a non-zero generation number", error_ctxt))
      | _ -> raise (Errors.PDFError ("Key.Object expected", error_ctxt))
    ) doc;

  (* Check stream lengths and unallowed keywords *)
  let keywords = ["endstream" ; "endobj" ; "trailer"] in
  Document.iter_objects (fun key obj ->
      let error_ctxt = Errors.make_ctxt_key key in
      match obj with
      | IndirectObject.Stream stream ->
        let error_ctxt_length = Errors.ctxt_append_name error_ctxt "Length" in

        let stream_length, (_:Errors.error_ctxt) = Document.remove_ref doc (DirectObject.dict_find (PDFStream.get_dict stream) "Length") error_ctxt_length in
        let len = IndirectObject.get_direct_of
            "Expected non-negative integer" error_ctxt_length
            ~transform:(DirectObject.get_nonnegative_int ())
            stream_length in

        let encoded = PDFStream.get_encoded stream in
        let real_len = ~:(String.length encoded) in
        (* TODO : move this to the lexer code ? *)
        let length_match =
          (* Length match *)
          if len = real_len then
            true
            (* CR or LF after raw data *)
          else if len = real_len -: ~:1 then (
            let last = encoded.[BoundedInt.to_int len] in
            last = '\x0A' || last = '\x0D'
            (* CRLF after raw data *)
          ) else if len = real_len -: ~:2 then (
            encoded.[BoundedInt.to_int len] = '\x0D' &&
            encoded.[(BoundedInt.to_int len) + 1] = '\x0A'
            (* Does not match *)
          ) else
            false
        in

        if not length_match then
          raise (Errors.PDFError (Printf.sprintf "Length (%d) of stream does not match reported length (%d)" (BoundedInt.to_int real_len) (BoundedInt.to_int len), error_ctxt_length));

        List.iter (fun keyword ->
            if Algo.string_contains encoded keyword then
              raise (Errors.PDFError (Printf.sprintf "Stream contains unallowed keyword \"%s\"" keyword, error_ctxt));
          ) keywords;
      | _ ->
        ()
    ) doc;

  (* TODO : check unused objects *)

  doc


let parse_file (filename : string) (stats : Stats.t) : Document.t =
  let input = OpenFile.in_bin filename in
  let doc =
    if Params.global.Params.strict_parser then
      parse_strict input stats
    else
      parse_nonstrict input stats
  in

  if Params.global.Params.undefined_ref_as_null then (
    if Params.global.Params.verbose then
      Printf.printf "Replacing undefined references by null object\n";
    Document.undef_refs_to_null doc
  );

  if Params.global.Params.decode_streams then (
    if Params.global.Params.verbose then
      Printf.printf "Decoding streams\n";
    Document.map_objects objdecodestream doc
  );

  doc


let record_filter (filter_hash : (string, int) Hashtbl.t) (filter_name : string) : unit =
  try
    let old_val = Hashtbl.find filter_hash filter_name in
    Hashtbl.replace filter_hash filter_name (old_val + 1)
  with Not_found ->
    Hashtbl.replace filter_hash filter_name 1

let extract_filters (filter_hash : (string, int) Hashtbl.t) (key : Key.t) (obj : IndirectObject.t) : unit =
  match obj with
  | IndirectObject.Stream stream ->
    let filters = DirectObject.get_array_of
        ~default:[] ~accept_one:true ()
        "Expected name or array of names" (Errors.make_ctxt_name key "Filter")
        ~transform:(DirectObject.get_name ())
        (DirectObject.dict_find (PDFStream.get_dict stream) "Filter") in

    if Array.length filters = 0 then
      record_filter filter_hash "Raw"
    else
      Array.iter (record_filter filter_hash) filters
  | _ ->
    ()

let get_entry (doc : Document.t) (transform : string -> Errors.error_ctxt -> DirectObject.t -> 'a) (infodict : DirectObject.dict_t) (key : string) : 'a option =
  try
    let tmp, (_:Errors.error_ctxt) = Document.remove_ref doc (DirectObject.dict_find infodict key) Errors.ctxt_none in
    let result = IndirectObject.get_direct_of
        "" Errors.ctxt_none
        ~transform:transform
        tmp in

    Some result
  with _ ->
    None

let extract_info (doc : Document.t) (stats : Stats.t) : unit =
  let get_entry_string = get_entry doc (DirectObject.get_string ()) in
  let get_entry_int = get_entry doc (fun s e o -> BoundedInt.to_int (DirectObject.get_int () s e o)) in

  let trailer = Document.main_trailer doc in

  begin
    try
      let info, (_:Errors.error_ctxt) = Document.remove_ref doc (DirectObject.dict_find trailer "Info") Errors.ctxt_none in
      let infodict = IndirectObject.get_direct_of
          "" Errors.ctxt_none
          ~transform:(DirectObject.get_dict ())
          info in

      stats.Stats.producer <- get_entry_string infodict "Producer";
      stats.Stats.creator <- get_entry_string infodict "Creator";
      stats.Stats.creation_date <- get_entry_string infodict "CreationDate";
      stats.Stats.mod_date <- get_entry_string infodict "ModDate";
    with _ ->
      ()
  end;

  begin
    try
      let encrypt, (_:Errors.error_ctxt) = Document.remove_ref doc (DirectObject.dict_find trailer "Encrypt") Errors.ctxt_none in
      let encryptdict = IndirectObject.get_direct_of
          "" Errors.ctxt_none
          ~transform:(DirectObject.get_dict ())
          encrypt in

      stats.Stats.encrypt_v <- get_entry_int encryptdict "V";
      stats.Stats.encrypt_r <- get_entry_int encryptdict "R";
      stats.Stats.encrypt_u <- get_entry_string encryptdict "U";
      stats.Stats.encrypt_o <- get_entry_string encryptdict "O";
    with _ ->
      ()
  end;

  begin
    try
      let id = DirectObject.dict_find trailer "ID" in
      let docid = DirectObject.get_array_of
          ~length:2 () "" Errors.ctxt_none
          ~transform:(DirectObject.get_string ())
          id
      in

      stats.Stats.encrypt_id <- Some docid.(0);
    with _ ->
      ()
  end

let statistics (filename : string) (stats : Stats.t) : unit =
  let doc = parse_file filename stats in

  extract_info doc stats;

  stats.Stats.objcount <- (Document.fold_objects (fun _ _ n -> n + 1) doc 0);
  Document.iter_objects (extract_filters stats.Stats.filters) doc;

  (*
  let graph = Document.graph doc in
  TODO : graph statistics
  *)

  let types = TypeChecker.check doc stats in
  stats.Stats.knowntypes <- MapKey.cardinal types;

  GraphChecker.check doc types;
  stats.Stats.nographerror <- true;

  ContentStream.check doc types;
  stats.Stats.nocontentstreamerror <- true;
  ()


let check_file (filename : string) : unit =
  let doc = parse_file filename (Stats.create ()) in

  apply_option (fun f -> dump_objects doc f) Params.global.Params.dump_filename;

  let graph = Document.graph doc in
  apply_option (fun f -> Graph.print_dot graph f) Params.global.Params.dot_filename;
  apply_option (fun f -> Graph.print_visjs graph f) Params.global.Params.visjs_filename;

  let objcount = Document.fold_objects (fun _ _ n -> n + 1) doc 0 in
  let types = TypeChecker.check doc (Stats.create ()) in
  apply_option (fun f -> TypeChecker.dump types objcount f) Params.global.Params.types_filename;
  apply_option (fun f -> Graph.print_dot_types graph types f) Params.global.Params.dot_filename;

  GraphChecker.check doc types;

  ContentStream.check doc types;

  Printf.printf "No error detected !\n"


let cleanup (filename : string) (out_filename : string) : unit =
  let doc = parse_file filename (Stats.create ()) in

  if Params.global.Params.merge_content_streams then
    Errors.catch ~fail:(fun () ->
        Errors.warning "Could not merge content stream arrays in all pages" Errors.ctxt_none
      ) (fun () -> CleanupCS.cleanup doc);

  begin
    match Params.global.Params.reencode_streams with
    | Some filter ->
      Document.map_objects (objreencodestream filter) doc
    | None -> ()
  end;

  (* TODO : normalize streams containing keywords *)

  if Params.global.Params.verbose then
    Printf.printf "Simplifying references\n";
  let doc2 = Document.simplify_refs doc Params.global.Params.simplify_info in

  if Params.global.Params.verbose then
    Printf.printf "Sanitizing object numbers\n";
  let doc3 = Document.sanitize_nums doc2 in

  (* TODO : sanitize unknown fields in /Info dictionary *)

  if Params.global.Params.verbose then
    Printf.printf "Saving file\n";
  let output = open_out_bin out_filename in
  Document.print_pdf output doc3;
  close_out output

