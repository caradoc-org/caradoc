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


open OUnit
open Xreflexer
open Xrefparser
open Errors
open Boundedint
open Xref


let rec next_tokens f lexbuf =
  let tok = f lexbuf in
  match tok with
  | EOF ->
    []
  | _ ->
    tok::(next_tokens f lexbuf)

let get_tokens f text =
  let lexbuf = Lexing.from_string text in
  next_tokens f lexbuf

let check_tokens text f toks =
  assert_equal (get_tokens f text) toks


let check_lexing_error text f msg pos =
  assert_raises (Errors.LexingError (msg, ~:pos)) (fun () -> f text)


let tests =
  "XrefLexer" >:::
  [
    "version" >:::
    [
      "(1)" >:: (fun _ -> check_tokens "%PDF-1.5\n" Xreflexer.token
                    [VERSION (1, 5)]) ;
      "(2)" >:: (fun _ -> check_tokens "%PDF-1.3\r" Xreflexer.token
                    [VERSION (1, 3)]) ;

      "(3)" >:: (fun _ -> check_lexing_error "%PDF-2.5\n" (get_tokens Xreflexer.token)
                    "unexpected character : 0x25" 0) ;
      "(4)" >:: (fun _ -> check_lexing_error "%PDF-1.8\n" (get_tokens Xreflexer.token)
                    "unexpected character : 0x25" 0) ;
      "(5)" >:: (fun _ -> check_lexing_error "%PDF-1.7 \n" (get_tokens Xreflexer.token)
                    "unexpected character : 0x25" 0) ;
      "(6)" >:: (fun _ -> check_lexing_error "%PDF-1.7 blabla" (get_tokens Xreflexer.token)
                    "unexpected character : 0x25" 0) ;
    ] ;

    "xref" >:::
    [
      "(1)" >:: (fun _ -> check_tokens "0000000012" Xreflexer.token
                    [OFFSET ~:12]) ;
      "(2)" >:: (fun _ -> check_tokens "00000" Xreflexer.token
                    [GENERATION ~:0]) ;
      "(3)" >:: (fun _ -> check_tokens "0000000012 00000" Xreflexer.token
                    [OFFSET ~:12 ; SPACE ; GENERATION ~:0]) ;
      "(4)" >:: (fun _ -> check_tokens "2 0\n0000000000 65535 f \n0000000012 00000 n \n" Xreflexer.token
                    [OTHERNUM ~:2 ; SPACE ; OTHERNUM ~:0 ; EOL ;
                     OFFSET ~:0 ; SPACE ; GENERATION ~:65535 ; SPACE ; INUSE XRefTable.Free ; SPACE_EOL ;
                     OFFSET ~:12 ; SPACE ; GENERATION ~:0 ; SPACE ; INUSE XRefTable.Inuse ; SPACE_EOL]) ;
      "(5)" >:: (fun _ -> check_tokens "xref" Xreflexer.token
                    [XREF]) ;
    ] ;

    "trailer" >:::
    [
      "(1)" >:: (fun _ -> check_tokens "trailer" Xreflexer.token
                    [TRAILER]) ;
      "(2)" >:: (fun _ -> check_tokens "startxref" Xreflexer.token
                    [STARTXREF]) ;
      "(3)" >:: (fun _ -> check_tokens "%%EOF" Xreflexer.token
                    [EOF_MARKER]) ;
    ] ;
  ]

