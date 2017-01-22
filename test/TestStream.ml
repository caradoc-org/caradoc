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
open Asciihex
open Ascii85
open Runlength
open Zlib
open Pdfstream.PDFStream
open Directobject
open Boundedint
open Errors


let make_raw (s : string) : t =
  make_encoded (TestDict.add_all ["Length", DirectObject.Int ~:(String.length s)]) s

let make_raw_dict (d : (string * DirectObject.t) list) (s : string) : t =
  make_encoded (TestDict.add_all (("Length", DirectObject.Int ~:(String.length s)) :: d)) s

let make_decoded (s : string) : t =
  let stream = make_encoded (TestDict.add_all ["Length", DirectObject.Int ~:(String.length s)]) s in
  let (_:bool) = decode stream Errors.ctxt_none false in
  stream


let tests =
  "Stream" >:::
  [
    "asciihex" >:::
    [
      "encode" >:::
      [
        "(1)" >:: (fun _ -> assert_equal
                      (ASCIIHex.encode "")
                      ">") ;
        "(2)" >:: (fun _ -> assert_equal
                      (ASCIIHex.encode "hello")
                      "68656C6C6F>") ;
        "(3)" >:: (fun _ -> assert_equal
                      (ASCIIHex.encode "\xFE\xDC\xBA\x98\x76\x54\x32\x10")
                      "FEDCBA9876543210>") ;
      ] ;
      "decode" >:::
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
                      (ASCIIHex.decode ">")
                      (Some "")) ;

        "(5)" >:: (fun _ -> assert_equal
                      (ASCIIHex.decode "ABCDEF0123456789")
                      None) ;
        "(6)" >:: (fun _ -> assert_equal
                      (ASCIIHex.decode "foo")
                      None) ;
      ] ;
      "encode-decode" >:::
      [
        "(1)" >:: (fun _ -> assert_equal
                      (ASCIIHex.decode (ASCIIHex.encode ""))
                      (Some "")) ;
        "(2)" >:: (fun _ -> assert_equal
                      (ASCIIHex.decode (ASCIIHex.encode "foobar"))
                      (Some "foobar")) ;
      ] ;
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
      "encode-decode" >:::
      [
        "(1)" >:: (fun _ -> assert_equal
                      (ASCII85.decode (ASCII85.encode ""))
                      (Some "")) ;
        "(2)" >:: (fun _ -> assert_equal
                      (ASCII85.decode (ASCII85.encode "foobar"))
                      (Some "foobar")) ;
      ] ;
    ] ;

    "runlength" >:::
    [
      "encode" >:::
      [
        "(1)" >:: (fun _ -> assert_equal
                      (RunLength.encode "")
                      "\x80") ;
        "(2)" >:: (fun _ -> assert_equal
                      (RunLength.encode "Hello world")
                      "\x0AHello world\x80") ;
      ] ;
      "decode" >:::
      [
        "(1)" >:: (fun _ -> assert_equal
                      (RunLength.decode "\x04hello\x80")
                      (Some "hello")) ;
        "(2)" >:: (fun _ -> assert_equal
                      (RunLength.decode "\xFFa\x80")
                      (Some "aa")) ;
        "(3)" >:: (fun _ -> assert_equal
                      (RunLength.decode "\xFFa\xF0b\x80")
                      (Some "aabbbbbbbbbbbbbbbbb")) ;

        "(4)" >:: (fun _ -> assert_equal
                      (RunLength.decode "foo")
                      None) ;
        "(5)" >:: (fun _ -> assert_equal
                      (RunLength.decode "\x04hello")
                      None) ;
        "(6)" >:: (fun _ -> assert_equal
                      (RunLength.decode "\x04hello\x80foo")
                      None) ;
        "(7)" >:: (fun _ -> assert_equal
                      (RunLength.decode "\x06hello\x80")
                      None) ;
      ] ;
      "encode-decode" >:::
      [
        "(1)" >:: (fun _ -> assert_equal
                      (RunLength.decode (RunLength.encode ""))
                      (Some "")) ;
        "(2)" >:: (fun _ -> assert_equal
                      (RunLength.decode (RunLength.encode "foobar"))
                      (Some "foobar")) ;
      ] ;
    ] ;

    "zlib" >:::
    [
      "encode" >:::
      [
        "(1)" >:: (fun _ -> assert_equal
                      (Zlib.encode "")
                      "\x78\x9C\x03\x00\x00\x00\x00\x01") ;
        "(2)" >:: (fun _ -> assert_equal
                      (Zlib.encode "Hello world")
                      "\x78\x9C\xF3\x48\xCD\xC9\xC9\x57\x28\xCF\x2F\xCA\x49\x01\x00\x18\xAB\x04\x3D") ;
      ] ;
      "decode" >:::
      [
        "(1)" >:: (fun _ -> assert_equal
                      (Zlib.decode "\x78\x9C\xF3\x48\xCD\xC9\xC9\x57\x28\xCF\x2F\xCA\x49\x01\x00\x18\xAB\x04\x3D")
                      (Some "Hello world")) ;

        "(2)" >:: (fun _ -> assert_equal
                      (Zlib.decode "\x78\x9Cfoobar\x00\x00\x00\x00")
                      None) ;

        "header" >:::
        [
          "(1)" >:: (fun _ -> assert_equal
                        (Zlib.decode "\x78\x9C\x03\x00\x00\x00\x00\x01")
                        (Some "")) ;
          "(2)" >:: (fun _ -> assert_equal
                        (Zlib.decode "\x68\x81\x03\x00\x00\x00\x00\x01")
                        (Some "")) ;
          "(3)" >:: (fun _ -> assert_equal
                        (Zlib.decode "\x58\x85\x03\x00\x00\x00\x00\x01")
                        (Some "")) ;
          "(4)" >:: (fun _ -> assert_equal
                        (Zlib.decode "\x48\x89\x03\x00\x00\x00\x00\x01")
                        (Some "")) ;
          "(5)" >:: (fun _ -> assert_equal
                        (Zlib.decode "\x38\x8D\x03\x00\x00\x00\x00\x01")
                        (Some "")) ;
          "(6)" >:: (fun _ -> assert_equal
                        (Zlib.decode "\x28\x91\x03\x00\x00\x00\x00\x01")
                        (Some "")) ;
          "(7)" >:: (fun _ -> assert_equal
                        (Zlib.decode "\x18\x95\x03\x00\x00\x00\x00\x01")
                        (Some "")) ;
          "(8)" >:: (fun _ -> assert_equal
                        (Zlib.decode "\x08\x99\x03\x00\x00\x00\x00\x01")
                        (Some "")) ;

          "(9)" >:: (fun _ -> assert_equal
                        (Zlib.decode "\x78\x9D\x03\x00\x00\x00\x00\x01")
                        None) ;
          "(10)" >:: (fun _ -> assert_equal
                         (Zlib.decode "\x88\x98\x03\x00\x00\x00\x00\x01")
                         None) ;
          "(11)" >:: (fun _ -> assert_equal
                         (Zlib.decode "\x70\x9E\x03\x00\x00\x00\x00\x01")
                         None) ;
          "(12)" >:: (fun _ -> assert_equal
                         (Zlib.decode "\x78\xBB\x03\x00\x00\x00\x00\x01")
                         None) ;
        ] ;
        "adler" >:::
        [
          "(1)" >:: (fun _ -> assert_equal
                        (Zlib.decode "\x78\x9C\x03\x00\x00\x00\x00\x00")
                        None) ;
          "(2)" >:: (fun _ -> assert_equal
                        (Zlib.decode "\x78\x9C\xF3\x48\xCD\xC9\xC9\x57\x28\xCF\x2F\xCA\x49\x01\x00\x19\xAB\x04\x3D")
                        None) ;
        ] ;
        (* TODO : invalid Huffman trees, etc. *)
      ] ;
      "encode-decode" >:::
      [
        "(1)" >:: (fun _ -> assert_equal
                      (Zlib.decode (Zlib.encode ""))
                      (Some "")) ;
        "(2)" >:: (fun _ -> assert_equal
                      (Zlib.decode (Zlib.encode "foobar"))
                      (Some "foobar")) ;
      ] ;
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

