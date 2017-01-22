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
open Boundedint
open Xref
open Key


let parse f text =
  let lexbuf = Lexing.from_string text in
  f Strictlexer.token_test lexbuf


let tests =
  "StrictParser" >:::
  [
    "xrefsection" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (parse Strictparser.xrefsection_test "")
                    (XRefTable.create (), ~:0)) ;
      "(2)" >:: (fun _ -> assert_equal
                    (parse Strictparser.xrefsection_test (
                        "0000000001 00002 n \n" ^
                        "0000000012 00000 n \n" ^
                        "0000000123 00002 f \n")
                    )
                    (TestXref.add_all [
                        Key.make_gen ~:0 ~:2, ~:1, XRefTable.Inuse ;
                        Key.make_gen ~:1 ~:0, ~:12, XRefTable.Inuse ;
                        Key.make_gen ~:2 ~:2, ~:123, XRefTable.Free ;
                      ], ~:3)) ;
    ] ;

    (* TODO *)
  ]

