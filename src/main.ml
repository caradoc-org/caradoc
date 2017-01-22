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


open File
open Xref
open Directobject
open Indirectobject
open Boundedint
open Extractxref
open Type
open Typechecker
open Stats
open Params
open Errors
open Pdfstream
open Find
open Openfile


(***************)
(* Arg helpers *)
(***************)

type command_spec =
  | NoArgCmd of (Arg.key * Arg.spec * Arg.doc) list * string * (unit -> unit)
  | OneFileCmd of (Arg.key * Arg.spec * Arg.doc) list * string * (string -> unit)

let no_arg_anon_fun _ = raise (Arg.Bad "No argument expected")

let one_file_anon_fun filename_ref filename =
  if !filename_ref = None then
    filename_ref := Some filename
  else
    raise (Arg.Bad "Please provide exactly one filename")

let handle_errors f x =
  (* Catch and print exceptions defined in errors.mli *)
  let catch_f () =
    Errors.catch ~fail:(fun () -> exit 2) (fun () -> f x)
  in
  (* Catch and print unexpected exceptions *)
  try
    Printexc.print catch_f ()
  with _ -> exit 3


(************)
(* Commands *)
(************)

let command_xreftable =
  let parse_xreftable filename =
    let input = OpenFile.in_bin filename in
    let stats = Stats.create () in
    let _, _, xref, _ = File.parse_until_xref input stats in
    close_in input;
    XRefTable.print stdout xref
  in

  let options = [
    "--verbose", Arg.Unit (fun () -> Params.global.Params.verbose <- true), "verbose mode";
    "--debug", Arg.Unit (fun () -> Params.global.Params.debug <- true), "debug mode";
  ] in
  OneFileCmd (options, "Parse a PDF xref table contained in a file", parse_xreftable)


let command_types =
  let print_types () =
    let ctxt = TypeChecker.init () in
    Type.print_pool ctxt.Type.pool
  in

  NoArgCmd ([], "Print the current PDF type definitions", print_types)


let command_stats =
  let options_filename = ref "" in

  let print_stats filename =
    (* Load more options *)
    if !options_filename <> "" then
      Params.load_file Params.global !options_filename;

    let stats = Stats.create () in
    let fail = ref false in
    Errors.catch ~fail:(fun () -> fail := true) (fun () ->
        File.statistics filename stats
      );
    Stats.print stats;
    if !fail then
      exit 2
  in

  let options = [
    "--options", Arg.Set_string options_filename, "options filename";
    "--strict", Arg.Unit (fun () -> Params.global.Params.strict_parser <- true), "strict parser";
  ] in
  OneFileCmd (options, "Give statistics about a file", print_stats)


let command_extract =
  let options_filename = ref "" in

  let extract_fun filename =
    (* Load more options *)
    if !options_filename <> "" then
      Params.load_file Params.global !options_filename;

    File.check_file filename
  in

  let options = [
    "--options", Arg.Set_string options_filename, "options filename";
    "--xref", Arg.String (fun v -> Params.global.Params.xref_filename <- Some v), "file to dump the xref table";
    "--dump", Arg.String (fun v -> Params.global.Params.dump_filename <- Some v), "file to dump the objects";
    "--sort-dicts", Arg.Unit (fun () -> Params.global.Params.sort_dicts <- true), "when --dump is activated, sort dictionaries by key";
    "--expand-streams", Arg.Unit (fun () -> Params.global.Params.expand_streams <- true), "when --dump is activated, expand the full content of decoded streams";
    "--stream-limit", Arg.Int (fun v -> Params.global.Params.stream_limit <- Some v), "length when --expand-stream is activated, limit the length of the decoded streams to expand";
    "--decode-streams", Arg.Unit (fun () -> Params.global.Params.decode_streams <- true), "decode content of all streams";
    "--relax-streams", Arg.Unit (fun () -> Params.global.Params.relax_streams <- true), "relax unsupported filters (warning instead of fatal error)";
    "--dot", Arg.String (fun v -> Params.global.Params.dot_filename <- Some v), "file to dump the graph of references between objects";
    "--visjs", Arg.String (fun v -> Params.global.Params.visjs_filename <- Some v), "file to dump the graph of references between objects";
    "--loc", Arg.String (fun v -> Params.global.Params.loc_filename <- Some v), "file to dump the locations of objects in source file";
    "--holes", Arg.String (fun v -> Params.global.Params.holes_filename <- Some v), "file to dump the content of holes in source file (except whitespaces and comments)";
    "--types", Arg.String (fun v -> Params.global.Params.types_filename <- Some v), "file to dump the types of objects inferred by the typechecker";
    "--verbose", Arg.Unit (fun () -> Params.global.Params.verbose <- true), "verbose mode";
    "--strict", Arg.Unit (fun () -> Params.global.Params.strict_parser <- true), "strict parser";
    "--debug", Arg.Unit (fun () -> Params.global.Params.debug <- true), "debug mode";
  ] in
  OneFileCmd (options, "Parse a PDF file and extract various data (objects, types, ...)", extract_fun)


let command_object =
  let options_filename = ref "" in
  let raw_stream_filename = ref "" in
  let decoded_stream_filename = ref "" in
  let num = ref 0 in   (* These two fields describe the object number *)
  let gen = ref 0 in   (* => number + generation number               *)

  let parse_object filename =
    (* Load more options *)
    if !options_filename <> "" then
      Params.load_file Params.global !options_filename;

    let input = OpenFile.in_bin filename in
    let key = Key.make_gen_i !num !gen in
    let obj = extract_object input key in
    close_in input;

    Printf.printf "%s\n" (IndirectObject.to_string obj);

    if !raw_stream_filename <> "" then
      begin
        match obj with
        | IndirectObject.Stream s ->
          let out = open_out_bin !raw_stream_filename in
          Printf.fprintf out "%s" (PDFStream.get_encoded s);
          close_out out
        | _ ->
          prerr_endline "Warning: --raw-stream argument was provided but the object is not a stream."
      end;

    if !decoded_stream_filename <> "" then
      begin
        match obj with
        | IndirectObject.Stream s ->
          let out = open_out_bin !decoded_stream_filename in
          Printf.fprintf out "%s" (PDFStream.get_decoded s (Errors.make_ctxt_key key));
          close_out out
        | _ ->
          prerr_endline "Warning: --decoded-stream argument was provided but the object is not a stream."
      end;
  in

  let options = [
    "--options", Arg.Set_string options_filename, "options filename";
    "--num", Arg.Set_int num, "number of the object";
    "--gen", Arg.Set_int gen, "generation number of the object (default : 0)";
    "--sort-dicts", Arg.Unit (fun () -> Params.global.Params.sort_dicts <- true), "sort dictionaries by key";
    "--raw-stream", Arg.Set_string raw_stream_filename, "file to dump the raw stream of this object";
    "--decoded-stream", Arg.String (fun v -> decoded_stream_filename := v; Params.global.Params.decode_streams <- true), "file to dump the decoded stream of this object";
    "--debug", Arg.Unit (fun () -> Params.global.Params.debug <- true), "debug mode";
  ] in
  OneFileCmd (options, "Extract a given object from a PDF file", parse_object)


let command_trailer =
  let options_filename = ref "" in

  let parse_trailer filename =
    Params.global.Params.expand_streams <- true;
    (* Load more options *)
    if !options_filename <> "" then
      Params.load_file Params.global !options_filename;

    let input = OpenFile.in_bin filename in
    let trailers = extract_trailers input in
    List.iter (
      fun obj ->
        Printf.printf "%s\n" (DirectObject.dict_to_string obj)
    ) trailers
  in

  let options = [
    "--options", Arg.Set_string options_filename, "options filename";
    "--sort-dicts", Arg.Unit (fun () -> Params.global.Params.sort_dicts <- true), "sort dictionaries by key";
  ] in
  OneFileCmd (options, "Extract the trailer(s) from a PDF file", parse_trailer)


let command_cleanup =
  let options_filename = ref "" in
  let out_filename = ref "" in

  let cleanup_file filename =
    (* Load more options *)
    if !options_filename <> "" then
      Params.load_file Params.global !options_filename;

    File.cleanup filename !out_filename
  in

  let options = [
    "--options", Arg.Set_string options_filename, "options filename";
    "--out", Arg.Set_string out_filename, "output filename";
    "--decode-streams", Arg.Unit (fun () -> Params.global.Params.decode_streams <- true), "decode content of all streams";
    "--relax-streams", Arg.Unit (fun () -> Params.global.Params.relax_streams <- true), "relax unsupported filters (warning instead of fatal error)";
    "--simplify-info", Arg.Unit (fun () -> Params.global.Params.simplify_info <- true), "remove non-standard entries from /Info dictionary";
    "--remove-ptex", Arg.Unit (fun () -> Params.global.Params.remove_ptex <- true), "recursively remove all entries starting with /PTEX.";
    "--merge-content-streams", Arg.Unit (fun () -> Params.global.Params.merge_content_streams <- true), "merge arrays of content streams in pages";
    "--verbose", Arg.Unit (fun () -> Params.global.Params.verbose <- true), "verbose mode";
    "--debug", Arg.Unit (fun () -> Params.global.Params.debug <- true), "debug mode";
  ] in
  OneFileCmd (options, "Rewrite a cleaned up version of a file", cleanup_file)


let command_findref =
  let options_filename = ref "" in
  let num = ref 0 in   (* These two fields describe the object number *)
  let gen = ref 0 in   (* => number + generation number               *)
  let show_ctxt = ref false in
  let highlight = ref false in

  let find_reference filename =
    (* Load more options *)
    if !options_filename <> "" then
      Params.load_file Params.global !options_filename;

    let key = Key.make_gen_i !num !gen in
    Find.find_ref key filename !show_ctxt !highlight
  in

  let options = [
    "--options", Arg.Set_string options_filename, "options filename";
    "--num", Arg.Set_int num, "number of the object";
    "--gen", Arg.Set_int gen, "generation number of the object (default : 0)";
    "--show", Arg.Unit (fun () -> show_ctxt := true), "show context of each occurrence found";
    "--sort-dicts", Arg.Unit (fun () -> Params.global.Params.sort_dicts <- true), "when --show is activated, sort dictionaries by key";
    "--highlight", Arg.Unit (fun () -> highlight := true), "when --show is activated, highlight occurrences in console output";
  ] in
  OneFileCmd (options, "Find all references to an object in a PDF file", find_reference)


let command_findname =
  let options_filename = ref "" in
  let name = ref "" in
  let show_ctxt = ref false in
  let highlight = ref false in

  let find_name filename =
    (* Load more options *)
    if !options_filename <> "" then
      Params.load_file Params.global !options_filename;

    Find.find_name !name filename !show_ctxt !highlight
  in

  let options = [
    "--options", Arg.Set_string options_filename, "options filename";
    "--name", Arg.Set_string name, "name to find";
    "--show", Arg.Unit (fun () -> show_ctxt := true), "show context of each occurrence found";
    "--sort-dicts", Arg.Unit (fun () -> Params.global.Params.sort_dicts <- true), "when --show is activated, sort dictionaries by key";
    "--highlight", Arg.Unit (fun () -> highlight := true), "when --show is activated, highlight occurrences in console output";
  ] in
  OneFileCmd (options, "Find all references to an object in a PDF file", find_name)



(****************)
(* Main program *)
(****************)

let commands = [
  "xref", command_xreftable;
  "extract", command_extract;
  "object", command_object;
  "trailer", command_trailer;
  "types", command_types;
  "cleanup", command_cleanup;
  "stats", command_stats;
  "findref", command_findref;
  "findname", command_findname;
]


let main_help error_msg =
  begin
    match error_msg with
    | None -> ()
    | Some m -> prerr_endline m
  end;
  prerr_endline "Usage: caradoc <command> <options>";
  prerr_endline "Valid commands are:";
  List.iter (fun (name, _) -> prerr_endline ("  " ^ name)) commands;
  exit (if error_msg = None then 0 else 1)


let () =
  if Array.length Sys.argv < 2 then
    main_help (Some "Please provide a command.");

  let command_name = Sys.argv.(1) in
  Arg.current := 1;
  if command_name = "help" then
    main_help None;

  try
    match List.assoc command_name commands with
    | NoArgCmd (options, msg, command_fun) ->
      let usage_msg = "Usage: caradoc " ^ command_name ^ " <options>\n" ^ msg in
      Arg.parse_argv Sys.argv options no_arg_anon_fun usage_msg;
      handle_errors command_fun ()
    | OneFileCmd (options, msg, command_fun) ->
      let usage_msg = "Usage: caradoc " ^ command_name ^ " <options> <filename>\n" ^ msg in
      let filename = ref None in
      Arg.parse_argv Sys.argv options (one_file_anon_fun filename) usage_msg;

      match !filename with
      | Some f ->
        handle_errors command_fun f
      | None ->
        Arg.usage options usage_msg; exit 1
  with
  | Not_found -> main_help (Some ("Invalid command: \"" ^ command_name ^ "\"."))
  | Arg.Bad error_msg -> prerr_string error_msg; exit 1
  | Arg.Help help_msg -> print_string help_msg; exit 0

