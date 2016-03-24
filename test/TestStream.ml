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
open Ascii
open Zlib


let tests =
  "Stream" >:::
  [
    "ascii" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (ASCII.decode "ABCDEF0123456789>")
                    (Some "\xAB\xCD\xEF\x01\x23\x45\x67\x89")) ;
      "(2)" >:: (fun _ -> assert_equal
                    (ASCII.decode "68656C6C6F>")
                    (Some "hello")) ;

      "(3)" >:: (fun _ -> assert_equal
                    (ASCII.decode "ABCDEF0123456789")
                    None) ;
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

