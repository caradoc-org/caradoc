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


open Type.Type
open Util


(***********************)
(* PDF reference 7.9.6 *)
(***********************)
let register_nametree ctxt name typ =
  register_class ctxt.pool ("nametreerootnode_" ^ name) [
    "Kids", entry_array ~optional:false (type_variant [Class ("nametreenode_" ^ name) ; Class ("nametreeleaf_" ^ name)]) ;
  ];
  register_class ctxt.pool ("nametreerootleaf_" ^ name) [
    "Names", entry_array_tuples ~optional:false [| make_type ~allow_ind:false String ; typ |] ;
  ];
  register_class ctxt.pool ("nametreenode_" ^ name) ~includes:[
    "nametreerootnode_" ^ name ;
  ] [
    "Limits", entry_sized_array ~optional:false 2 (make_type String) ;
  ];
  register_class ctxt.pool ("nametreeleaf_" ^ name) ~includes:[
    "nametreerootleaf_" ^ name ;
  ] [
    "Limits", entry_sized_array ~optional:false 2 (make_type String) ;
  ];
    (*
  type_variant [Class ("nametreerootnode_" ^ name) ; Class ("nametreerootleaf_" ^ name)]
    *)
  type_variant [Class ("nametreerootnode_" ^ name) ; Class ("nametreerootleaf_" ^ name) ; Class ("nametreenode_" ^ name) ; Class ("nametreeleaf_" ^ name)]


(***********************)
(* PDF reference 7.9.7 *)
(***********************)
let register_numtree ctxt name typ =
  register_class ctxt.pool ("numtreerootnode_" ^ name) [
    "Kids", entry_array ~optional:false (type_variant [Class ("numtreenode_" ^ name) ; Class ("numtreeleaf_" ^ name)]) ;
  ];
  register_class ctxt.pool ("numtreerootleaf_" ^ name) [
    "Nums", entry_array_tuples ~optional:false [| make_type ~allow_ind:false Int ; typ |] ;
  ];
  register_class ctxt.pool ("numtreenode_" ^ name) ~includes:[
    "numtreerootnode_" ^ name ;
  ] [
    "Limits", entry_sized_array ~optional:false 2 (make_type Int) ;
  ];
  register_class ctxt.pool ("numtreeleaf_" ^ name) ~includes:[
    "numtreerootleaf_" ^ name ;
  ] [
    "Limits", entry_sized_array ~optional:false 2 (make_type Int) ;
  ];

  type_variant [Class ("numtreerootnode_" ^ name) ; Class ("numtreerootleaf_" ^ name)]
    (*
  type_variant [Class ("numtreerootnode_" ^ name) ; Class ("numtreerootleaf_" ^ name) ; Class ("numtreenode_" ^ name) ; Class ("numtreeleaf_" ^ name)]
    *)

