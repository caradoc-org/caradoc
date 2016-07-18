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
open Boundedint
open Errors
open Entry


let tests =
  "Errors" >:::
  [
    "ctxt_none" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    Errors.ctxt_none
                    {Errors.key = None; Errors.pos = None; Errors.entry = Entry.empty}) ;
    ] ;
    "make_ctxt" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (Errors.make_ctxt Key.Trailer ~:123)
                    {Errors.key = Some Key.Trailer; Errors.pos = Some ~:123; Errors.entry = Entry.empty}) ;
    ] ;
    "make_ctxt_key" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (Errors.make_ctxt_key Key.Trailer)
                    {Errors.key = Some Key.Trailer; Errors.pos = None; Errors.entry = Entry.empty}) ;
    ] ;
    "make_ctxt_pos" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (Errors.make_ctxt_pos ~:123)
                    {Errors.key = None; Errors.pos = Some ~:123; Errors.entry = Entry.empty}) ;
    ] ;
    "make_ctxt_entry" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (Errors.make_ctxt_entry Key.Trailer (Entry.make_name "foo"))
                    {Errors.key = Some Key.Trailer; Errors.pos = None; Errors.entry = (Entry.make_name "foo")}) ;
    ] ;
    "make_ctxt_index" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (Errors.make_ctxt_index Key.Trailer 123)
                    {Errors.key = Some Key.Trailer; Errors.pos = None; Errors.entry = (Entry.make_index 123)}) ;
      "(2)" >:: (fun _ -> assert_equal
                    (Errors.make_ctxt_index Key.Trailer 123)
                    (Errors.make_ctxt_entry Key.Trailer (Entry.make_index 123))) ;
    ] ;
    "make_ctxt_name" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (Errors.make_ctxt_name Key.Trailer "foo")
                    {Errors.key = Some Key.Trailer; Errors.pos = None; Errors.entry = (Entry.make_name "foo")}) ;
      "(2)" >:: (fun _ -> assert_equal
                    (Errors.make_ctxt_name Key.Trailer "foo")
                    (Errors.make_ctxt_entry Key.Trailer (Entry.make_name "foo"))) ;
    ] ;
    "make_ctxt_full_name" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (Errors.make_ctxt_full_name Key.Trailer ~:123 "entry")
                    {Errors.key = Some Key.Trailer; Errors.pos = Some ~:123; Errors.entry = (Entry.make_name "entry")}) ;
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
                    (Errors.ctxt_set_pos Errors.ctxt_none ~:123)
                    (Errors.make_ctxt_pos ~:123)) ;
      "(2)" >:: (fun _ -> assert_equal
                    (Errors.ctxt_set_pos (Errors.make_ctxt_key Key.Trailer) ~:123)
                    (Errors.make_ctxt Key.Trailer ~:123)) ;
      "(3)" >:: (fun _ -> assert_equal
                    (Errors.ctxt_set_pos (Errors.make_ctxt Key.Trailer ~:123) ~:456)
                    (Errors.make_ctxt Key.Trailer ~:456)) ;
    ] ;

    "ctxt_to_string" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (Errors.ctxt_to_string Errors.ctxt_none)
                    "") ;
      "(2)" >:: (fun _ -> assert_equal
                    (Errors.ctxt_to_string (Errors.make_ctxt Key.Trailer ~:123))
                    " for object trailer at offset 123 [0x7b]") ;
      "(3)" >:: (fun _ -> assert_equal
                    (Errors.ctxt_to_string (Errors.make_ctxt_key Key.Trailer))
                    " for object trailer") ;
      "(4)" >:: (fun _ -> assert_equal
                    (Errors.ctxt_to_string (Errors.make_ctxt_pos ~:123))
                    " at offset 123 [0x7b]") ;
      "(5)" >:: (fun _ -> assert_equal
                    (Errors.ctxt_to_string (Errors.make_ctxt_entry Key.Trailer (Entry.make_name "foo")))
                    " for object trailer at entry /foo") ;
      "(6)" >:: (fun _ -> assert_equal
                    (Errors.ctxt_to_string (Errors.make_ctxt_index Key.Trailer 123))
                    " for object trailer at entry [123]") ;
      "(7)" >:: (fun _ -> assert_equal
                    (Errors.ctxt_to_string (Errors.make_ctxt_name Key.Trailer "bar"))
                    " for object trailer at entry /bar") ;
      "(8)" >:: (fun _ -> assert_equal
                    (Errors.ctxt_to_string (Errors.make_ctxt_full_name Key.Trailer ~:123 "foo"))
                    " for object trailer at entry /foo at offset 123 [0x7b]") ;
    ] ;
  ]

