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
open Mapkey
open Type
open Key
open Boundedint
open Errors


let make_context id =
  let ctxt = Type.create_context () in
  ctxt.Type.pool <- TestType.make_pool id;
  ctxt

let make_copy_context id1 id2 =
  let ctxt = make_context id1 in
  let copy = Type.copy_context ctxt in
  copy.Type.pool <- TestType.make_pool id2;
  ctxt, copy

let make_assign_context id1 id2 =
  let ctxt1 = make_context id1 in
  let ctxt2 = make_context id2 in
  Type.assign_context ctxt1 ctxt2;
  ctxt1


let tests =
  "TypeUtil" >:::
  [
    "copy_context" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (let ctxt, _ = make_copy_context 1 2 in ctxt)
                    (make_context 1)) ;
      "(2)" >:: (fun _ -> assert_equal
                    (let _, copy = make_copy_context 1 2 in copy)
                    (make_context 2)) ;
      "(3)" >:: (fun _ -> assert_equal
                    (let ctxt, copy = make_copy_context 1 2 in
                     copy.Type.to_check <- (Key.make_0 ~:1, Errors.ctxt_none)::copy.Type.to_check;
                     ctxt)
                    (make_context 1)) ;
      "(4)" >:: (fun _ -> assert_equal
                    (let ctxt, copy = make_copy_context 1 2 in
                     ctxt.Type.to_check <- (Key.make_0 ~:1, Errors.ctxt_none)::ctxt.Type.to_check;
                     copy)
                    (make_context 2)) ;
      "(5)" >:: (fun _ -> assert_equal
                    (let ctxt, copy = make_copy_context 1 2 in
                     copy.Type.types <- MapKey.add (Key.make_0 ~:1) Type.Int copy.Type.types;
                     ctxt)
                    (make_context 1)) ;
      "(6)" >:: (fun _ -> assert_equal
                    (let ctxt, copy = make_copy_context 1 2 in
                     ctxt.Type.types <- MapKey.add (Key.make_0 ~:1) Type.String ctxt.Type.types;
                     copy)
                    (make_context 2)) ;
      (* TODO *)
    ] ;

    "assign_context" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (make_assign_context 1 2)
                    (make_context 2)) ;
      "(2)" >:: (fun _ -> assert_equal
                    (let ctxt = make_context 1 in
                     let copy = Type.copy_context ctxt in
                     ctxt.Type.types <- MapKey.add (Key.make_0 ~:1) Type.Int ctxt.Type.types;
                     Type.assign_context ctxt copy;
                     ctxt)
                    (make_context 1)) ;
      (* TODO *)
    ] ;

    (*
    "register_class" >:::
    [
      (* TODO *)
      "(0)" >:: (fun _ -> failwith "Not implemented") ;
    ] ;

    "register_alias" >:::
    [
      (* TODO : nothing *)
      "(0)" >:: (fun _ -> failwith "Not implemented") ;
    ] ;
    *)
  ]

