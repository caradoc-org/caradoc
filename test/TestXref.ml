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
open Xref
open Key
open Boundedint
open Errors


let add_all l =
  let x = XRefTable.create () in
  List.iter (fun (key, pos, kind) -> XRefTable.add x key (XRefTable.make_value pos kind)) l;
  x


let tests =
  "Xref" >:::
  [
    "find_list" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (XRefTable.find_list (add_all [
                         Key.make_0 ~:3, ~:5, XRefTable.Inuse ;
                       ]) (Key.make_0 ~:3))
                    [XRefTable.make_value ~:5 XRefTable.Inuse]) ;
      "(2)" >:: (fun _ -> assert_equal
                    (XRefTable.find_list (add_all [
                         Key.make_0 ~:1, ~:3, XRefTable.Compressed ~:5 ;
                         Key.make_0 ~:1, ~:2, XRefTable.Free ;
                         Key.make_0 ~:3, ~:5, XRefTable.Inuse ;
                         Key.make_0 ~:1, ~:4, XRefTable.Free ;
                         Key.make_0 ~:7, ~:0, XRefTable.Free ;
                         Key.make_0 ~:1, ~:1, XRefTable.Inuse ;
                       ]) (Key.make_0 ~:1))
                    [
                      XRefTable.make_value ~:3 (XRefTable.Compressed ~:5) ;
                      XRefTable.make_value ~:1 XRefTable.Inuse ;
                      XRefTable.make_value ~:4 XRefTable.Free ;
                      XRefTable.make_value ~:2 XRefTable.Free
                    ]) ;
    ] ;

    "find" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (XRefTable.find (add_all [
                         Key.make_0 ~:3, ~:5, XRefTable.Inuse ;
                       ]) (Key.make_0 ~:3) "dummy")
                    (XRefTable.make_value ~:5 XRefTable.Inuse)) ;
      "(2)" >:: (fun _ -> assert_equal
                    (XRefTable.find (add_all [
                         Key.make_0 ~:3, ~:2, XRefTable.Inuse ;
                         Key.make_0 ~:3, ~:4, XRefTable.Inuse ;
                         Key.make_0 ~:3, ~:7, XRefTable.Inuse ;
                       ]) (Key.make_0 ~:3) "dummy")
                    (XRefTable.make_value ~:2 XRefTable.Inuse)) ;
      "(3)" >:: (fun _ -> assert_raises
                    (Errors.PDFError ("not found message", Errors.make_ctxt_key (Key.make_0 ~:2)))
                    (fun () -> XRefTable.find (add_all [
                         Key.make_0 ~:3, ~:2, XRefTable.Inuse ;
                         Key.make_0 ~:1, ~:4, XRefTable.Inuse ;
                         Key.make_0 ~:3, ~:7, XRefTable.Inuse ;
                       ]) (Key.make_0 ~:2) "not found message")) ;
    ] ;
    (* TODO *)
  ]

