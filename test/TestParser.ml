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
open Directobject.DirectObject
open Key
open Boundedint
open Errors


let parse f text =
  let lexbuf = Lexing.from_string text in
  f Lexer.token lexbuf


let tests =
  "Parser" >:::
  [
    "one_object" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (parse Parser.one_object "null")
                    Null) ;
      "(2)" >:: (fun _ -> assert_equal
                    (parse Parser.one_object "true")
                    (Bool true)) ;
      "(3)" >:: (fun _ -> assert_equal
                    (parse Parser.one_object "123")
                    (Int ~:123)) ;
      "(4)" >:: (fun _ -> assert_equal
                    (parse Parser.one_object "(text)")
                    (String "text")) ;
      "(5)" >:: (fun _ -> assert_equal
                    (parse Parser.one_object "/text")
                    (Name "text")) ;
      "(6)" >:: (fun _ -> assert_equal
                    (parse Parser.one_object "1 2 R")
                    (Reference (Key.make_gen ~:1 ~:2))) ;

      "(7)" >:: (fun _ -> assert_raises
                    Parser.Error
                    (fun () -> parse Parser.one_object "1 2")) ;
      "(8)" >:: (fun _ -> assert_raises
                    Parser.Error
                    (fun () -> parse Parser.one_object "<</Length 9>>stream\nblablabla\nendstream")) ;
    ] ;

    "array" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (parse Parser.one_object "[123 (text)]")
                    (Array [Int ~:123 ; String "text"])) ;
      "(2)" >:: (fun _ -> assert_equal
                    (parse Parser.one_object "[123[(text)][]]")
                    (Array [Int ~:123 ; Array [String "text"] ; Array []])) ;
    ] ;

    "dictionary" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (parse Parser.one_object "<<>>")
                    (Dictionary (dict_create ()))) ;
      "(2)" >:: (fun _ -> assert_equal
                    (parse Parser.one_object "<</Key/Value>>")
                    (Dictionary (TestDict.add_all ["Key", Name "Value"]))) ;
      "(3)" >:: (fun _ -> assert_equal
                    (parse Parser.one_object "<</Key[]/Other<<>>>>")
                    (Dictionary (TestDict.add_all ["Key", Array [] ; "Other", Dictionary (dict_create ())]))) ;
    ] ;

    "reference" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (parse Parser.one_object "[1 2]")
                    (Array [Int ~:1 ; Int ~:2])) ;
      "(2)" >:: (fun _ -> assert_equal
                    (parse Parser.one_object "[1 2 R]")
                    (Array [Reference (Key.make_gen ~:1 ~:2)])) ;
      "(3)" >:: (fun _ -> assert_equal
                    (parse Parser.one_object "[1 2 3 R]")
                    (Array [Int ~:1 ; Reference (Key.make_gen ~:2 ~:3)])) ;
      "(4)" >:: (fun _ -> assert_equal
                    (parse Parser.one_object "[1 2 3 4 R]")
                    (Array [Int ~:1 ; Int ~:2 ; Reference (Key.make_gen ~:3 ~:4)])) ;
      "(5)" >:: (fun _ -> assert_equal
                    (parse Parser.one_object "[1 2 3 4 5 R]")
                    (Array [Int ~:1 ; Int ~:2 ; Int ~:3 ; Reference (Key.make_gen ~:4 ~:5)])) ;
      "(6)" >:: (fun _ -> assert_equal
                    (parse Parser.one_object "[1 2 R 3 4 R]")
                    (Array [Reference (Key.make_gen ~:1 ~:2) ; Reference (Key.make_gen ~:3 ~:4)])) ;
      "(7)" >:: (fun _ -> assert_equal
                    (parse Parser.one_object "[1 2 R 3 4 5 R]")
                    (Array [Reference (Key.make_gen ~:1 ~:2) ; Int ~:3 ; Reference (Key.make_gen ~:4 ~:5)])) ;
      "(8)" >:: (fun _ -> assert_equal
                    (parse Parser.one_object "[1 2 (blabla) 3 4 5 R (text) 6 7]")
                    (Array [Int ~:1 ; Int ~:2 ; String "blabla" ; Int ~:3 ; Reference (Key.make_gen ~:4 ~:5) ; String "text" ; Int ~:6 ; Int ~:7])) ;
      "(9)" >:: (fun _ -> assert_equal
                    (parse Parser.one_object "[(blabla) 1 2 3 (text) 4 5 R 6 7]")
                    (Array [String "blabla" ; Int ~:1 ; Int ~:2 ; Int ~:3 ; String "text" ; Reference (Key.make_gen ~:4 ~:5) ; Int ~:6 ; Int ~:7])) ;
      "(10)" >:: (fun _ -> assert_equal
                     (parse Parser.one_object "<</Key 1 0 R/Other(blabla)>>")
                     (Dictionary (TestDict.add_all ["Key", Reference (Key.make_0 ~:1) ; "Other", String "blabla"]))) ;

      "(11)" >:: (fun _ -> assert_raises
                     Parser.Error
                     (fun () -> parse Parser.one_object "[1 R]")) ;
      "(12)" >:: (fun _ -> assert_raises
                     Parser.Error
                     (fun () -> parse Parser.one_object "[1 2 R 3 R]")) ;
      "(13)" >:: (fun _ -> assert_raises
                     Parser.Error
                     (fun () -> parse Parser.one_object "[1 2 R 3 (text) 4 R]")) ;
    ] ;

    "trailerdict" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (parse Parser.trailerdict "<</Info 1 0 R>>")
                    (TestDict.add_all ["Info", Reference (Key.make_0 ~:1)])) ;
      "(2)" >:: (fun _ -> assert_equal
                    (parse Parser.trailerdict "<</Info 1 0 R>>\nstartxref\n")
                    (TestDict.add_all ["Info", Reference (Key.make_0 ~:1)])) ;
      "(3)" >:: (fun _ -> assert_equal
                    (parse Parser.trailerdict "<</Info 1 0 R>>garbage")
                    (TestDict.add_all ["Info", Reference (Key.make_0 ~:1)])) ;
    ] ;

    "endstream" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (parse Parser.endstream "endstream endobj")
                    ()) ;

      "(2)" >:: (fun _ -> assert_raises
                    Parser.Error
                    (fun () -> parse Parser.endstream "endstream")) ;
    ] ;

    "intpair_list" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (parse Parser.intpair_list "12 456 78 123 159 753")
                    ([~:12 ; ~:78 ; ~:159], [~:456 ; ~:123 ; ~:753])) ;
      "(2)" >:: (fun _ -> assert_equal
                    (parse Parser.intpair_list " 12 456 ")
                    ([~:12], [~:456])) ;

      "(3)" >:: (fun _ -> assert_raises
                    Parser.Error
                    (fun () -> parse Parser.intpair_list "12 456 78")) ;
    ] ;

    "hole" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (parse Parser.hole "\n\r\x0C\t\x00 ")
                    ()) ;

      "(2)" >:: (fun _ -> assert_raises
                    Parser.Error
                    (fun () -> parse Parser.hole " (garbage)")) ;
      "(3)" >:: (fun _ -> assert_raises
                    Parser.Error
                    (fun () -> parse Parser.hole " obj")) ;
    ] ;

    "ascii_hex_decode" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (parse Parser.ascii_hex_decode "<6 86\r56\tC6\nC6\x00F2\x0C1>")
                    "hello!") ;
      "(2)" >:: (fun _ -> assert_equal
                    (parse Parser.ascii_hex_decode "<68656C6C6F>")
                    "hello") ;

      "(3)" >:: (fun _ -> assert_raises
                    Parser.Error
                    (fun () -> parse Parser.ascii_hex_decode "(not-ascii hex)")) ;
      "(4)" >:: (fun _ -> assert_raises
                    Parser.Error
                    (fun () -> parse Parser.ascii_hex_decode "123")) ;
      "(5)" >:: (fun _ -> assert_raises
                    (Errors.LexingError ("hexadecimal string is not terminated at end of file", ~:11))
                    (fun () -> parse Parser.ascii_hex_decode "<68656C6C6F")) ;
      "(6)" >:: (fun _ -> assert_raises
                    (Errors.LexingError ("unexpected word", ~:13))
                    (fun () -> parse Parser.ascii_hex_decode "<68656C6C6F> other stuff")) ;
      "(7)" >:: (fun _ -> assert_raises
                    Parser.Error
                    (fun () -> parse Parser.ascii_hex_decode "<68656C6C6F> (other stuff)")) ;
    ] ;
  ]

