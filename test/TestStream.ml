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


open OUnit
open Asciihex
open Ascii85
open Zlib


let tests =
  "Stream" >:::
  [
    "asciihex" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (ASCIIHex.decode "ABCDEF0123456789>")
                    (Some "\xAB\xCD\xEF\x01\x23\x45\x67\x89")) ;
      "(2)" >:: (fun _ -> assert_equal
                    (ASCIIHex.decode "68656C6C6F>")
                    (Some "hello")) ;
      "(3)" >:: (fun _ -> assert_equal
                    (ASCIIHex.decode "ABC DEF\x0001\x0A2\x0D34\x0C567\x0989 >")
                    (Some "\xAB\xCD\xEF\x01\x23\x45\x67\x89")) ;

      "(4)" >:: (fun _ -> assert_equal
                    (ASCIIHex.decode "ABCDEF0123456789")
                    None) ;
      "(5)" >:: (fun _ -> assert_equal
                    (ASCIIHex.decode "foo")
                    None) ;
    ] ;

    "ascii85" >:::
    [
      "encode" >:::
      [
        "(1)" >:: (fun _ -> assert_equal
                      (ASCII85.encode "\x00\x00\x00\x00")
                      "z~>") ;
        "(2)" >:: (fun _ -> assert_equal
                      (ASCII85.encode "\x00\x00\x00")
                      "!!!!~>") ;
        "(3)" >:: (fun _ -> assert_equal
                      (ASCII85.encode "\x00\x00")
                      "!!!~>") ;
        "(4)" >:: (fun _ -> assert_equal
                      (ASCII85.encode "\x00")
                      "!!~>") ;
        "(5)" >:: (fun _ -> assert_equal
                      (ASCII85.encode "")
                      "~>") ;
        "(6)" >:: (fun _ -> assert_equal
                      (ASCII85.encode "hello")
                      "BOu!rDZ~>") ;
        "(7)" >:: (fun _ -> assert_equal
                      (ASCII85.encode "\xFF\xFF\xFF\xFF")
                      "\x73\x38\x57\x2D\x21~>") ;
        "(8)" >:: (fun _ -> assert_equal
                      (ASCII85.encode "\xFF\xFF\xFF")
                      "\x73\x38\x57\x2A~>") ;
        "(9)" >:: (fun _ -> assert_equal
                      (ASCII85.encode "\xFF\xFF")
                      "\x73\x38\x4E~>") ;
        "(10)" >:: (fun _ -> assert_equal
                       (ASCII85.encode "\xFF")
                       "\x72\x72~>") ;
      ] ;
      "decode" >:::
      [
        "(1)" >:: (fun _ -> assert_equal
                      (ASCII85.decode "z~>")
                      (Some "\x00\x00\x00\x00")) ;
        "(2)" >:: (fun _ -> assert_equal
                      (ASCII85.decode "!!!!!~>")
                      (Some "\x00\x00\x00\x00")) ;
        "(3)" >:: (fun _ -> assert_equal
                      (ASCII85.decode "!!!!~>")
                      (Some "\x00\x00\x00")) ;
        "(4)" >:: (fun _ -> assert_equal
                      (ASCII85.decode "!!!~>")
                      (Some "\x00\x00")) ;
        "(5)" >:: (fun _ -> assert_equal
                      (ASCII85.decode "!!~>")
                      (Some "\x00")) ;
        "(6)" >:: (fun _ -> assert_equal
                      (ASCII85.decode "~>")
                      (Some "")) ;
        "(7)" >:: (fun _ -> assert_equal
                      (ASCII85.decode "BOu!rDZ~>")
                      (Some "hello")) ;
        "(8)" >:: (fun _ -> assert_equal
                      (ASCII85.decode "\x73\x38\x57\x2D\x21~>")
                      (Some "\xFF\xFF\xFF\xFF")) ;
        "(9)" >:: (fun _ -> assert_equal
                      (ASCII85.decode "\x73\x38\x57\x2A~>")
                      (Some "\xFF\xFF\xFF")) ;
        "(10)" >:: (fun _ -> assert_equal
                       (ASCII85.decode "\x73\x38\x4E~>")
                       (Some "\xFF\xFF")) ;
        "(11)" >:: (fun _ -> assert_equal
                       (ASCII85.decode "\x72\x72~>")
                       (Some "\xFF")) ;
        "(12)" >:: (fun _ -> assert_equal
                       (ASCII85.decode "B O\x00\x0Au!\x0DrD\x0CZ~\x09>")
                       (Some "hello")) ;

        "(13)" >:: (fun _ -> assert_equal
                       (ASCII85.decode "!z~>")
                       None) ;
        "(14)" >:: (fun _ -> assert_equal
                       (ASCII85.decode "uuuuu~>")
                       None) ;
        "(15)" >:: (fun _ -> assert_equal
                       (ASCII85.decode "\x73\x38\x57\x2D\x22~>")
                       None) ;
        "(16)" >:: (fun _ -> assert_equal
                       (ASCII85.decode "foo")
                       None) ;
      ] ;
    ] ;

    "zlib" >:::
    [
      (* TODO *)
    ] ;
    "adler32" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (Zlib.adler32 "hello world")
                    0x1a0b045dl) ;
      "(2)" >:: (fun _ -> assert_equal
                    (Zlib.adler32 "")
                    1l) ;
    ] ;
  ]

