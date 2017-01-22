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
open Errors
open Key
open Boundedint
open Directobject


let add_all l =
  let x = DirectObject.dict_create () in
  List.iter (fun y -> DirectObject.dict_add false x y) l;
  x

let make_pages () =
  add_all ["Type", DirectObject.Name "Pages" ; "Parent", DirectObject.Reference (Key.make_0 ~:1)]


let tests =
  "Dict" >:::
  [
    "singleton" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (DirectObject.dict_singleton ("Key", DirectObject.String "Value"))
                    (add_all ["Key", DirectObject.String "Value"])) ;
      "(2)" >:: (fun _ -> assert_equal
                    (DirectObject.dict_singleton ("Key", DirectObject.String "Value"))
                    (add_all ["Key", DirectObject.String "Other"]) ~cmp:(<>)) ;
    ] ;

    "add" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (add_all ["Key", DirectObject.Bool true ; "Param", DirectObject.Bool false])
                    (add_all ["Param", DirectObject.Bool false ; "Key", DirectObject.Bool true])) ;
      "(2)" >:: (fun _ -> assert_equal
                    (add_all ["Key", DirectObject.Bool true ; "Param", DirectObject.Null])
                    (add_all ["Key", DirectObject.Bool true])) ;

      "(3)" >:: (fun _ -> assert_raises
                    (Errors.PDFError ("The same name appears several times in dictionary : /Key", Errors.ctxt_none))
                    (fun () -> add_all ["Key", DirectObject.Bool true ; "Key", DirectObject.Bool false])) ;
      "(4)" >:: (fun _ -> assert_raises
                    (Errors.PDFError ("The same name appears several times in dictionary : /Key", Errors.ctxt_none))
                    (fun () -> add_all ["Key", DirectObject.Bool true ; "Key", DirectObject.Bool true])) ;
    ] ;

    "find" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (DirectObject.dict_find (make_pages ()) "Parent")
                    (DirectObject.Reference (Key.make_0 ~:1))) ;
      "(2)" >:: (fun _ -> assert_equal
                    (DirectObject.dict_find (make_pages ()) "Kids")
                    (DirectObject.Null)) ;
      "(3)" >:: (fun _ -> assert_equal
                    (DirectObject.dict_find (make_pages ()) "Type")
                    (DirectObject.Name "Pages")) ;
    ] ;

    "simplify" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (DirectObject.dict_simplify ["Key"] (add_all ["Key", DirectObject.Bool true]))
                    (add_all ["Key", DirectObject.Bool true])) ;
      "(2)" >:: (fun _ -> assert_equal
                    (DirectObject.dict_simplify ["Key"] (add_all ["Foo", DirectObject.String "Bar"]))
                    (add_all [])) ;
      "(3)" >:: (fun _ -> assert_equal
                    (DirectObject.dict_simplify ["Foo" ; "Bar"] (add_all ["Key", DirectObject.Int ~:4 ; "Foo", DirectObject.Name "Bar"]))
                    (add_all ["Foo", DirectObject.Name "Bar"])) ;
      "(4)" >:: (fun _ -> assert_equal
                    (DirectObject.dict_simplify [] (add_all ["Key", DirectObject.Int ~:4 ; "Foo", DirectObject.Name "Bar"]))
                    (add_all [])) ;
    ] ;
  ]

