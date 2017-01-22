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
open Entry


let tests =
  "Errors" >:::
  [
    "append_entry" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (Entry.append_entry Entry.empty (Entry.make_index 0))
                    (Entry.make_index 0)) ;
      "(2)" >:: (fun _ -> assert_equal
                    (Entry.append_entry (Entry.make_name "bar") Entry.empty)
                    (Entry.make_name "bar")) ;
      "(3)" >:: (fun _ -> assert_equal
                    (Entry.append_entry (Entry.make_name "bar") (Entry.make_index 0))
                    (Entry.append_index (Entry.make_name "bar") 0)) ;
    ] ;
    "is_empty" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (Entry.is_empty Entry.empty)
                    true) ;
      "(2)" >:: (fun _ -> assert_equal
                    (Entry.is_empty (Entry.make_index 0))
                    false) ;
      "(3)" >:: (fun _ -> assert_equal
                    (Entry.is_empty (Entry.make_name ""))
                    false) ;
      "(4)" >:: (fun _ -> assert_equal
                    (Entry.is_empty (Entry.make_name "foo"))
                    false) ;
      "(5)" >:: (fun _ -> assert_equal
                    (Entry.is_empty (Entry.make_name_key ""))
                    false) ;
      "(6)" >:: (fun _ -> assert_equal
                    (Entry.is_empty (Entry.make_name_key "bar"))
                    false) ;
    ] ;
    "to_string" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (Entry.to_string Entry.empty)
                    "") ;
      "(2)" >:: (fun _ -> assert_equal
                    (Entry.to_string (Entry.make_name "foo"))
                    "/foo") ;
      "(3)" >:: (fun _ -> assert_equal
                    (Entry.to_string (Entry.make_name_key "bar"))
                    "\\bar") ;
      "(4)" >:: (fun _ -> assert_equal
                    (Entry.to_string (Entry.make_index 123))
                    "[123]") ;
      "(5)" >:: (fun _ -> assert_equal
                    (Entry.to_string (Entry.append_name_key (Entry.append_name (Entry.make_index 123) "foo") "bar"))
                    "[123]/foo\\bar") ;
    ] ;

    "make_selector" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (Entry.make_selector [])
                    Entry.no_selector) ;
    ] ;
    "move_to_index" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (Entry.move_to_index Entry.no_selector 123)
                    Entry.no_selector) ;
      "(2)" >:: (fun _ -> assert_equal
                    (Entry.move_to_index (Entry.make_selector [Entry.make_index 123]) 123)
                    (Entry.make_selector [Entry.empty])) ;
      "(3)" >:: (fun _ -> assert_equal
                    (Entry.move_to_index (Entry.make_selector [Entry.make_name "foo"]) 123)
                    Entry.no_selector) ;
      "(4)" >:: (fun _ -> assert_equal
                    (Entry.move_to_index (Entry.make_selector [Entry.make_index 456]) 123)
                    Entry.no_selector) ;
      "(5)" >:: (fun _ -> assert_equal
                    (Entry.move_to_index (Entry.make_selector [Entry.append_name (Entry.make_index 123) "foo" ; Entry.make_name "bar" ; Entry.append_index (Entry.make_index 123) 456]) 123)
                    (Entry.make_selector [Entry.make_name "foo" ; Entry.make_index 456])) ;
    ] ;
    "move_to_name" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (Entry.move_to_name Entry.no_selector "foo")
                    Entry.no_selector) ;
      "(2)" >:: (fun _ -> assert_equal
                    (Entry.move_to_name (Entry.make_selector [Entry.make_index 123]) "foo")
                    Entry.no_selector) ;
      "(3)" >:: (fun _ -> assert_equal
                    (Entry.move_to_name (Entry.make_selector [Entry.make_name "foo"]) "foo")
                    (Entry.make_selector [Entry.empty])) ;
      "(4)" >:: (fun _ -> assert_equal
                    (Entry.move_to_name (Entry.make_selector [Entry.make_name "bar"]) "foo")
                    Entry.no_selector) ;
      "(5)" >:: (fun _ -> assert_equal
                    (Entry.move_to_name (Entry.make_selector [Entry.append_name (Entry.make_name "bar") "foo" ; Entry.make_name "bar" ; Entry.append_index (Entry.make_index 123) 456]) "bar")
                    (Entry.make_selector [Entry.make_name "foo" ; Entry.empty])) ;
    ] ;
    "move_to_name_key" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (Entry.move_to_name_key Entry.no_selector "foo")
                    Entry.no_selector) ;
      "(2)" >:: (fun _ -> assert_equal
                    (Entry.move_to_name_key (Entry.make_selector [Entry.make_index 123]) "foo")
                    Entry.no_selector) ;
      "(3)" >:: (fun _ -> assert_equal
                    (Entry.move_to_name_key (Entry.make_selector [Entry.make_name "foo"]) "foo")
                    Entry.no_selector) ;
      "(4)" >:: (fun _ -> assert_equal
                    (Entry.move_to_name_key (Entry.make_selector [Entry.make_name_key "bar"]) "bar")
                    (Entry.make_selector [Entry.empty])) ;
    ] ;
    "is_selected" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (Entry.is_selected Entry.no_selector)
                    false) ;
      "(2)" >:: (fun _ -> assert_equal
                    (Entry.is_selected (Entry.make_selector [Entry.empty]))
                    true) ;
    ] ;
  ]

