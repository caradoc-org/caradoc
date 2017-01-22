(*****************************************************************************)
(*  Caradoc: a PDF parser and validator                                      *)
(*  Copyright (C) 2016-2017 Guillaume Endignoux                              *)
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
open Params
open Errors
open Indirectobject
open Mapkey
open Type
open Boundedint
open Pdfstream
open Wrap
open Operator
open Resources

module ContentStream = struct

  let get_tokens (s : string) =
    let lexbuf = Lexing.from_string s in
    let t = ref [] in
    begin
      try
        while true do
          let tt = Contentlexer.token lexbuf in
          if tt = Contentparser.EOF then
            raise Exit;
          let pos = Lexing.lexeme_start lexbuf in
          t := (tt, pos)::!t;
        done;
      with
      | BoundedInt.IntegerError msg ->
        let pos = ~:(Lexing.lexeme_start lexbuf) in
        raise (Errors.LexingError (Printf.sprintf "integer error in content stream : %s" msg, pos))
      | Exit -> ()
    end;
    !t

  let next_token tokens (i : int ref) (s : int) (_lexbuf : Lexing.lexbuf) =
    if !i >= s then
      Contentparser.EOF
    else (
      let t, _ = tokens.(!i) in
      i := !i + 1;
      t
    )

  let parse_commands (s : string) (error_ctxt_stream : Errors.error_ctxt) : Operator.t list =
    let tokens = Array.of_list (get_tokens s) in
    let i = ref 0 in
    let s = Array.length tokens in
    (* We use a dummy Lexing object here because we parse backwards *)
    let lexbuf = Lexing.from_string "" in
    let getpos = fun () ->
      let _, pos = tokens.(!i - 1) in
      ~:pos
    in

    List.rev (wrap_contentparser Contentparser.content (next_token tokens i s) getpos lexbuf error_ctxt_stream)

  let check_content (s : string) (is_font3 : bool) (error_ctxt : Errors.error_ctxt) (error_ctxt_stream : Errors.error_ctxt) : unit =
    let commands = parse_commands s error_ctxt_stream in
    Operator.check_commands commands is_font3 error_ctxt;
    let resources = Resources.from_commands commands in
    if Params.global.Params.debug then
      Printf.eprintf "Needed resources for content stream%s :\n%s\n" (Errors.ctxt_to_string error_ctxt) (Resources.to_string resources)


  let check_stream (doc : Document.t) (key : Key.t) (is_font3 : bool) : unit =
    if Params.global.Params.verbose then
      Printf.eprintf "Checking content stream in object %s\n" (Key.to_string key);
    let error_ctxt = Errors.make_ctxt_key key in
    let error_ctxt_stream = Errors.make_ctxt_pos (Errors.make_pos_stream key ~:0) in

    match Document.find_obj doc key with
    | IndirectObject.Stream s ->
      check_content (PDFStream.get_decoded s error_ctxt) is_font3 error_ctxt error_ctxt_stream
    | _ ->
      raise (Errors.PDFError ("Expected a stream for object of type \"content_stream\"", error_ctxt))

  let check (doc : Document.t) (types : Type.kind_t MapKey.t) : unit =
    MapKey.iter (fun key typ ->
        match typ with
        | Type.Stream s when s = "content_stream" || s = "xobject_form" ->
          check_stream doc key false
        | Type.Stream s when s = "content_stream_font3" ->
          check_stream doc key true
        | _ -> ()
      ) types

end

