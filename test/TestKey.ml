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
open Key
open Boundedint


let tests =
  "Key" >:::
  [
    "get" >:::
    [
      "(1)" >:: (fun _ -> assert_raises (Failure "Internal error: invalid reference") (fun () -> Key.get_obj_ref Key.Trailer));
      "(2)" >:: (fun _ -> assert_equal (Key.get_obj_ref (Key.make_gen ~:15 ~:2)) (15, 2)) ;
      "(3)" >:: (fun _ -> assert_equal (Key.get_obj_ref (Key.make_0 ~:37)) (37, 0)) ;
      "(4)" >:: (fun _ -> assert_equal (Key.get_obj_ref (Key.make_gen_i 15 2)) (15, 2)) ;
    ] ;

    "compare" >:::
    [
      "(1)" >:: (fun _ -> assert_equal (Key.compare (Key.make_gen ~:15 ~:2) (Key.make_gen ~:2 ~:15)) ~cmp:(>) 0) ;
      "(2)" >:: (fun _ -> assert_equal (Key.compare (Key.make_gen ~:15 ~:2) (Key.make_gen ~:15 ~:1)) ~cmp:(>) 0) ;
      "(3)" >:: (fun _ -> assert_equal (Key.compare (Key.make_0 ~:14) (Key.make_0 ~:15)) ~cmp:(<) 0) ;
    ] ;

    "to_string" >:::
    [
      "(1)" >:: (fun _ -> assert_equal (Key.to_string (Key.make_gen ~:15 ~:2)) "15 [2]") ;
      "(2)" >:: (fun _ -> assert_equal (Key.to_string (Key.make_0 ~:8)) "8") ;
      "(3)" >:: (fun _ -> assert_equal (Key.to_string (Key.make_gen ~:0 ~:65535)) "0 [65535]") ;
      "(4)" >:: (fun _ -> assert_equal (Key.to_string Key.Trailer) "trailer") ;
      "(5)" >:: (fun _ -> assert_equal (Key.to_string Key.Version) "version") ;
    ] ;
  ]

