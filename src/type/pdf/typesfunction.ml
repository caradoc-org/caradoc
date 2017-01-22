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


let register_function ctxt =

  (**********************)
  (* PDF reference 7.10 *)
  (**********************)
  register_class ctxt.pool "function_base_04" [
    "Domain", entry_array_tuples ~optional:false [| type_alias "number" ; type_alias "number" |] ;
    "Range", entry_array_tuples ~optional:false [| type_alias "number" ; type_alias "number" |] ;
  ];

  register_class ctxt.pool "function_base_23" [
    "Domain", entry_sized_array ~optional:true 2 (type_alias "number") ;
    "Range", entry_array_tuples ~optional:true [| type_alias "number" ; type_alias "number" |] ;
  ];

  register_alias ctxt.pool "function" (Variant [
      Stream "function_type0" ;
      Class "function_type2" ;
      Class "function_type3" ;
      Stream "function_type4" ;
      (* TODO : check dimensionality in each case *)
    ]);

  (************************)
  (* PDF reference 7.10.2 *)
  (************************)
  register_class ctxt.pool "function_type0" ~includes:[
    "stream_base" ;
    "function_base_04" ;
  ] [
    "FunctionType", entry_int_exact ~allow_ind:false ~optional:false 0 ;

    "Size", entry_array ~optional:false (type_alias "intpositive") ;
    "BitsPerSample", entry_int_in ~optional:false [| 1 ; 2 ; 4 ; 8 ; 12 ; 16 ; 24 ; 32 |] ;
    "Order", entry_int_in ~optional:true [| 1 ; 3 |] ;
    "Encode", entry_array_tuples ~optional:true [| type_alias "number" ; type_alias "number" |] ;
    "Decode", entry_array_tuples ~optional:true [| type_alias "number" ; type_alias "number" |] ;
    (* TODO : check function definition in stream *)
  ];

  (************************)
  (* PDF reference 7.10.3 *)
  (************************)
  register_class ctxt.pool "function_type2" ~includes:[
    "function_base_23" ;
  ] [
    "FunctionType", entry_int_exact ~allow_ind:false ~optional:false 2 ;

    "C0", entry_array ~optional:true (type_alias "number") ;
    "C1", entry_array ~optional:true (type_alias "number") ;
    "N", entry_alias ~optional:false "number" ;
  ];

  (************************)
  (* PDF reference 7.10.4 *)
  (************************)
  register_class ctxt.pool "function_type3" ~includes:[
    "function_base_23" ;
  ] [
    "FunctionType", entry_int_exact ~allow_ind:false ~optional:false 3 ;

    (* TODO : check dimensionality of functions *)
    "Functions", entry_array ~optional:false (type_alias "function") ;
    "Bounds", entry_array ~optional:false (type_alias "number") ;
    "Encode", entry_array_tuples ~optional:false [| type_alias "number" ; type_alias "number" |] ;
  ];

  (************************)
  (* PDF reference 7.10.5 *)
  (************************)
  register_class ctxt.pool "function_type4" ~includes:[
    "stream_base" ;
    "function_base_04" ;
  ] [
    "FunctionType", entry_int_exact ~allow_ind:false ~optional:false 4 ;
    (* TODO : check function definition in stream *)
  ];

