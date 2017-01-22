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
open Errors


let parse f text =
  let lexbuf = Lexing.from_string text in
  f Xreflexer.token lexbuf


let tests =
  "XrefParser" >:::
  [
    "version" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (parse Xrefparser.version "%PDF-1.7\r")
                    (1, 7)) ;
      "(2)" >:: (fun _ -> assert_equal
                    (parse Xrefparser.version "%PDF-1.5\n\r")
                    (1, 5)) ;
      "(3)" >:: (fun _ -> assert_equal
                    (parse Xrefparser.version "%PDF-1.2\ngarbage")
                    (1, 2)) ;

      "(4)" >:: (fun _ -> assert_raises
                    Xrefparser.Error
                    (fun () -> parse Xrefparser.version "xref\n")) ;
    ] ;

    "xref" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (parse Xrefparser.xref "xref\n")
                    ()) ;
      "(2)" >:: (fun _ -> assert_equal
                    (parse Xrefparser.xref "xref\ngarbage")
                    ()) ;

      "(3)" >:: (fun _ -> assert_raises
                    Xrefparser.Error
                    (fun () -> parse Xrefparser.xref "xref")) ;
      "(4)" >:: (fun _ -> assert_raises
                    Xrefparser.Error
                    (fun () -> parse Xrefparser.xref "trailer")) ;
    ] ;

    "xrefsection" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (parse Xrefparser.xrefsection "12 3\n")
                    (~:12, ~:3)) ;
      "(2)" >:: (fun _ -> assert_equal
                    (parse Xrefparser.xrefsection "12 3\rgarbage")
                    (~:12, ~:3)) ;
      "(3)" >:: (fun _ -> assert_equal
                    (parse Xrefparser.xrefsection "12 3 \n")
                    (~:12, ~:3)) ;

      "(4)" >:: (fun _ -> assert_raises
                    Xrefparser.Error
                    (fun () -> parse Xrefparser.xrefsection "12 3")) ;
      "(5)" >:: (fun _ -> assert_raises
                    Xrefparser.Error
                    (fun () -> parse Xrefparser.xrefsection "12 3 ")) ;
      "(6)" >:: (fun _ -> assert_raises
                    Xrefparser.Error
                    (fun () -> parse Xrefparser.xrefsection "12 3  \n")) ;
    ] ;

    "xrefentry" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (parse Xrefparser.xrefentry "0000000001 00002 n \n")
                    (~:2, XRefTable.make_value ~:1 XRefTable.Inuse)) ;
      "(2)" >:: (fun _ -> assert_equal
                    (parse Xrefparser.xrefentry "0000000123 65535 f \r")
                    (~:65535, XRefTable.make_value ~:123 XRefTable.Free)) ;
      "(3)" >:: (fun _ -> assert_equal
                    (parse Xrefparser.xrefentry "0000004563 00001 n\r\n")
                    (~:1, XRefTable.make_value ~:4563 XRefTable.Inuse)) ;

      "(4)" >:: (fun _ -> assert_raises
                    Xrefparser.Error
                    (fun () -> parse Xrefparser.xrefentry "0000000000 00000 n\n")) ;
      "(5)" >:: (fun _ -> assert_raises
                    (Errors.LexingError ("unexpected character : 0x2d", ~:11))
                    (fun () -> parse Xrefparser.xrefentry "0000000000 -0002 n \n")) ;
      "(6)" >:: (fun _ -> assert_raises
                    Xrefparser.Error
                    (fun () -> parse Xrefparser.xrefentry "4563 00001 n\r\n")) ;
      "(7)" >:: (fun _ -> assert_raises
                    Xrefparser.Error
                    (fun () -> parse Xrefparser.xrefentry "0000004563 1 n\r\n")) ;
    ] ;

    "trailer" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (parse Xrefparser.trailer "trailer")
                    ()) ;
      "(2)" >:: (fun _ -> assert_equal
                    (parse Xrefparser.trailer "trailer\ngarbage")
                    ()) ;
      "(3)" >:: (fun _ -> assert_equal
                    (parse Xrefparser.trailer " \n trailer\n")
                    ()) ;

      "(4)" >:: (fun _ -> assert_raises
                    Xrefparser.Error
                    (fun () -> parse Xrefparser.trailer "xref\n")) ;
    ] ;

    "startxref" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (parse Xrefparser.startxref "startxref\n12345\n%%EOF")
                    ~:12345) ;
      "(2)" >:: (fun _ -> assert_equal
                    (parse Xrefparser.startxref "startxref\r\n12345\n%%EOF")
                    ~:12345) ;

      "(3)" >:: (fun _ -> assert_raises
                    Xrefparser.Error
                    (fun () -> parse Xrefparser.startxref "startxref \n12345\n%%EOF")) ;
      "(4)" >:: (fun _ -> assert_raises
                    Xrefparser.Error
                    (fun () -> parse Xrefparser.startxref "startxref\n\r12345\n%%EOF")) ;
    ] ;

    "startxref2" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (parse Xrefparser.startxref2 "12345\n%%EOF")
                    ~:12345) ;
      "(2)" >:: (fun _ -> assert_equal
                    (parse Xrefparser.startxref2 "12345\r\n%%EOF")
                    ~:12345) ;

      "(3)" >:: (fun _ -> assert_raises
                    (Errors.LexingError ("unexpected character : 0x2d", ~:0))
                    (fun () -> parse Xrefparser.startxref2 "-12345\n%%EOF")) ;
      "(4)" >:: (fun _ -> assert_raises
                    Xrefparser.Error
                    (fun () -> parse Xrefparser.startxref2 "12345 \n%%EOF")) ;
      "(5)" >:: (fun _ -> assert_raises
                    Xrefparser.Error
                    (fun () -> parse Xrefparser.startxref2 "12345\n\r%%EOF")) ;
    ] ;

    "eofmarker" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (parse Xrefparser.eofmarker "%%EOF")
                    true) ;
      "(2)" >:: (fun _ -> assert_equal
                    (parse Xrefparser.eofmarker "%%EOF\n")
                    true) ;
      "(3)" >:: (fun _ -> assert_equal
                    (parse Xrefparser.eofmarker "%%EOF\n\r\n\n")
                    true) ;
      "(4)" >:: (fun _ -> assert_equal
                    (parse Xrefparser.eofmarker "")
                    false) ;
      "(5)" >:: (fun _ -> assert_equal
                    (parse Xrefparser.eofmarker "\n")
                    false) ;
      "(6)" >:: (fun _ -> assert_equal
                    (parse Xrefparser.eofmarker "\n\r")
                    false) ;

      "(7)" >:: (fun _ -> assert_raises
                    Xrefparser.Error
                    (fun () -> parse Xrefparser.eofmarker "xref")) ;
    ] ;
  ]

