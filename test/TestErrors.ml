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
open Errors
open Entry


let tests =
  "Errors" >:::
  [
    "pos_add_offset" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (Errors.pos_add_offset (Errors.make_pos_file ~:123) ~:321)
                    (Errors.make_pos_file ~:444)) ;
      "(2)" >:: (fun _ -> assert_equal
                    (Errors.pos_add_offset (Errors.make_pos_stream (Key.make_0 ~:1) ~:123) ~:321)
                    (Errors.make_pos_stream (Key.make_0 ~:1) ~:444)) ;
      "(3)" >:: (fun _ -> assert_equal
                    (Errors.pos_add_offset (Errors.make_pos_objstm (Key.make_0 ~:1) ~:123) ~:321)
                    (Errors.make_pos_objstm (Key.make_0 ~:1) ~:123)) ;
    ] ;

    "ctxt_none" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (Errors.ctxt_to_string Errors.ctxt_none)
                    "") ;
    ] ;
    "make_ctxt" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (Errors.ctxt_to_string (Errors.make_ctxt Key.Trailer (Errors.make_pos_file ~:123)))
                    " in object trailer at offset 123 [0x7b] in file") ;
    ] ;
    "make_ctxt_key" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (Errors.ctxt_to_string (Errors.make_ctxt_key Key.Trailer))
                    " in object trailer") ;
    ] ;
    "make_ctxt_pos" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (Errors.ctxt_to_string (Errors.make_ctxt_pos (Errors.make_pos_file ~:123)))
                    " at offset 123 [0x7b] in file") ;
      "(2)" >:: (fun _ -> assert_equal
                    (Errors.ctxt_to_string (Errors.make_ctxt_pos (Errors.make_pos_stream (Key.make_0 ~:1) ~:123)))
                    " at offset 123 [0x7b] in stream 1") ;
      "(3)" >:: (fun _ -> assert_equal
                    (Errors.ctxt_to_string (Errors.make_ctxt_pos (Errors.make_pos_objstm (Key.make_0 ~:1) ~:123)))
                    " at index 123 in object stream 1") ;
    ] ;
    "make_ctxt_entry" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (Errors.ctxt_to_string (Errors.make_ctxt_entry Key.Trailer (Entry.make_name "foo")))
                    " in object trailer at entry /foo") ;
    ] ;
    "make_ctxt_index" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (Errors.ctxt_to_string (Errors.make_ctxt_index Key.Trailer 123))
                    " in object trailer at entry [123]") ;
      "(2)" >:: (fun _ -> assert_equal
                    (Errors.make_ctxt_index Key.Trailer 123)
                    (Errors.make_ctxt_entry Key.Trailer (Entry.make_index 123))) ;
    ] ;
    "make_ctxt_name" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (Errors.ctxt_to_string (Errors.make_ctxt_name Key.Trailer "bar"))
                    " in object trailer at entry /bar") ;
      "(2)" >:: (fun _ -> assert_equal
                    (Errors.make_ctxt_name Key.Trailer "foo")
                    (Errors.make_ctxt_entry Key.Trailer (Entry.make_name "foo"))) ;
    ] ;
    "make_ctxt_full_name" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (Errors.ctxt_to_string (Errors.make_ctxt_full_name Key.Trailer (Errors.make_pos_file ~:123) "foo"))
                    " in object trailer at entry /foo at offset 123 [0x7b] in file") ;
    ] ;

    "ctxt_append_entry" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (Errors.ctxt_append_entry (Errors.make_ctxt_key Key.Trailer) (Entry.make_name "foo"))
                    (Errors.make_ctxt_entry Key.Trailer (Entry.make_name "foo"))) ;
    ] ;
    "ctxt_append_index" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (Errors.ctxt_append_index (Errors.make_ctxt_key Key.Trailer) 123)
                    (Errors.make_ctxt_index Key.Trailer 123)) ;
    ] ;
    "ctxt_append_name" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (Errors.ctxt_append_name (Errors.make_ctxt_key Key.Trailer) "foo")
                    (Errors.make_ctxt_name Key.Trailer "foo")) ;
    ] ;
    "ctxt_set_pos" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (Errors.ctxt_set_pos Errors.ctxt_none (Errors.make_pos_file ~:123))
                    (Errors.make_ctxt_pos (Errors.make_pos_file ~:123))) ;
      "(2)" >:: (fun _ -> assert_equal
                    (Errors.ctxt_set_pos (Errors.make_ctxt_key Key.Trailer) (Errors.make_pos_file ~:123))
                    (Errors.make_ctxt Key.Trailer (Errors.make_pos_file ~:123))) ;
      "(3)" >:: (fun _ -> assert_equal
                    (Errors.ctxt_set_pos (Errors.make_ctxt Key.Trailer (Errors.make_pos_file ~:123)) (Errors.make_pos_file ~:456))
                    (Errors.make_ctxt Key.Trailer (Errors.make_pos_file ~:456))) ;
    ] ;
    "ctxt_add_offset" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (Errors.ctxt_add_offset (Errors.make_ctxt_pos (Errors.make_pos_file ~:123)) ~:321)
                    (Errors.make_ctxt_pos (Errors.make_pos_file ~:444))) ;
      "(2)" >:: (fun _ -> assert_equal
                    (Errors.ctxt_add_offset (Errors.make_ctxt_pos (Errors.make_pos_stream (Key.make_0 ~:1) ~:123)) ~:321)
                    (Errors.make_ctxt_pos (Errors.make_pos_stream (Key.make_0 ~:1) ~:444))) ;
      "(3)" >:: (fun _ -> assert_equal
                    (Errors.ctxt_add_offset (Errors.make_ctxt_pos (Errors.make_pos_objstm (Key.make_0 ~:1) ~:123)) ~:321)
                    (Errors.make_ctxt_pos (Errors.make_pos_objstm (Key.make_0 ~:1) ~:123))) ;
    ] ;
  ]

