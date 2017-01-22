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
open Parser
open Common
open Boundedint


let rec next_tokens f lexbuf =
  let tok = f lexbuf in
  match tok with
  | EOF ->
    []
  | _ ->
    tok::(next_tokens f lexbuf)

let test_rewind f n text =
  let lexbuf = Lexing.from_string text in
  let a = [f lexbuf] in
  Common.rewind lexbuf n;
  let b = next_tokens f lexbuf in
  a, b


let tests =
  "Common" >:::
  [
    "rewind" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (test_rewind Lexer.token 3 "123 456 789")
                    ([INT ~:123], [INT ~:123 ; INT ~:456 ; INT ~:789])) ;
      "(2)" >:: (fun _ -> assert_equal
                    (test_rewind Lexer.token 2 "123 456 789")
                    ([INT ~:123], [INT ~:23 ; INT ~:456 ; INT ~:789])) ;

      "(3)" >:: (fun _ -> assert_raises
                    (Invalid_argument ("index out of bounds"))
                    (fun () -> test_rewind Lexer.token 4 "123 456 789")) ;
    ] ;
  ]

