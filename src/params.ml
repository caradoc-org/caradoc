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



module Params = struct

  type t = {
    (**** Parsing parameters ****)
    (* When parsing input, decode content of streams *)
    mutable decode_streams : bool;
    (* When decoding streams, do not fail for unsupported filters *)
    mutable relax_streams : bool;
    (* Use strict parser *)
    mutable strict_parser : bool;

    (**** Parsing parameters for non-conforming files ****)
    (* Allow a file to contain a non-conforming version header *)
    (* Instead, only the "%PDF" signature must be present at offset zero *)
    mutable allow_invalid_version : bool;
    (* Allow duplicates in dictionaries *)
    (* If a key is present several times in a dictionary, the last entry is kept *)
    mutable allow_dict_duplicates : bool;
    (* Treat xref table entries with an offset of zero as free objects *)
    mutable zero_offset_as_free : bool;
    (* Treat references to non-defined objects as the null object *)
    mutable undefined_ref_as_null : bool;

    (**** Type-checking parameters ****)
    (* Allow a dictionary of "info" type to contain arbitrary (custom) entries *)
    (* For example, pdfTeX can produce files with "PTEX.Fullbanner" field *)
    mutable allow_arbitrary_info : bool;

    (**** Output parameters ****)
    (* Sort dictionary by key in output *)
    mutable sort_dicts : bool;
    (* Expand the content of streams in output *)
    mutable expand_streams : bool;
    (* Limit the length of expanded streams in output *)
    mutable stream_limit : int option;

    (**** Output files ****)
    (* Xref table *)
    mutable xref_filename : string option;
    (* Objects *)
    mutable dump_filename : string option;
    (* Graph of references (dot format) *)
    mutable dot_filename : string option;
    (* Graph of references (vis.js format) *)
    mutable visjs_filename : string option;
    (* Locations of objects in file *)
    mutable loc_filename : string option;
    (* Locations of holes in file *)
    mutable holes_filename : string option;
    (* Types of objects *)
    mutable types_filename : string option;
    (* Dump of the first incorrect stream *)
    mutable stream_errors_filename : string option;

    (**** Verbosity ****)
    (* Verbose *)
    mutable verbose : bool;
    (* Debug *)
    mutable debug : bool;
  }


  let create () : t = {
    (* Parsing parameters *)
    decode_streams = false;
    relax_streams = false;
    (* TODO : which default parameter? *)
    strict_parser = false;
    (* Parsing non-conforming files *)
    allow_invalid_version = false;
    allow_dict_duplicates = false;
    zero_offset_as_free = false;
    undefined_ref_as_null = false;
    (* Type-checking parameters *)
    allow_arbitrary_info = false;
    (* Output parameters *)
    sort_dicts = false;
    expand_streams = false;
    stream_limit = None;
    (* Output files *)
    xref_filename = None;
    dump_filename = None;
    dot_filename = None;
    visjs_filename = None;
    loc_filename = None;
    holes_filename = None;
    types_filename = None;
    stream_errors_filename = None;
    (* Verbosity *)
    verbose = false;
    debug = false;
  }

  (* A global instance of parameters *)
  (* This is useful e.g. in Menhir's parsing code, because passing additional parameters does not seem possible *)
  let global = create ()

  let clear_global () : unit =
    global.decode_streams <- false;
    global.relax_streams <- false;
    global.strict_parser <- false;
    global.allow_invalid_version <- false;
    global.allow_dict_duplicates <- false;
    global.zero_offset_as_free <- false;
    global.undefined_ref_as_null <- false;
    global.allow_arbitrary_info <- false;
    global.sort_dicts <- false;
    global.expand_streams <- false;
    global.stream_limit <- None;
    global.xref_filename <- None;
    global.dump_filename <- None;
    global.dot_filename <- None;
    global.visjs_filename <- None;
    global.loc_filename <- None;
    global.holes_filename <- None;
    global.types_filename <- None;
    global.stream_errors_filename <- None;
    global.verbose <- false;
    global.debug <- false


  let load_unit_param (params : t) (p : string) : bool =
    match p with
    | "strict" ->
      params.strict_parser <- true;
      true
    | "allow-invalid-version" ->
      params.allow_invalid_version <- true;
      true
    | "allow-dict-duplicates" ->
      params.allow_dict_duplicates <- true;
      true
    | "zero-offset-as-free" ->
      params.zero_offset_as_free <- true;
      true
    | "undefined-ref-as-null" ->
      params.undefined_ref_as_null <- true;
      true
    | "allow-arbitrary-info" ->
      params.allow_arbitrary_info <- true;
      true
    | _ -> false

  let load_value_param (_params : t) (_k : string) (_v : string) : bool =
    false

  let load_line (params : t) (line : string) : bool =
    try
      (* Find '=' separator *)
      let pos = String.index line '=' in
      (* Line is 'key=value' *)
      load_value_param params (String.sub line 0 pos) (String.sub line (pos + 1) ((String.length line) - pos - 1))
    with Not_found ->
      (* Line is a parameter by itself *)
      load_unit_param params line

  let load_file (params : t) (filename : string) : unit =
    let i = ref 0 in
    let input = open_in filename in

    try
      while true do
        i := !i + 1;
        let line = input_line input in
        if line <> "" then (
          if not (load_line params line) then (
            Printf.eprintf "Invalid parameter on line %d : %s\n" !i line;
            exit 1
          )
        )
      done
    with End_of_file ->
      close_in input

end

