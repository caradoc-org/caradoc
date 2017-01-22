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


let register_stream ctxt =

  (*************************)
  (* PDF reference 7.3.8.2 *)
  (*************************)
  register_class ~strict:false ctxt.pool "stream_base" [
    "Length", entry_alias ~optional:false "intnonnegative" ;

    "Filter", entry_array_or_one ~optional:true (make_type Name) ;
    "DecodeParms", entry_array_or_one ~optional:true (type_variant [Null ; Alias "decode_parameters"]) ;
    (*
    "F", entry_alias ~optional:true "file_spec" ;
    "FFilter", entry_array_or_one ~optional:true (make_type Name) ;
    "FDecodeParms", entry_array_or_one ~optional:true (type_variant [Null ; Class "decode_parameters"]) ;
    *)
    "DL", entry_alias ~optional:true "intnonnegative" ;
  ];

  (***********************)
  (* PDF reference 7.5.7 *)
  (***********************)
  register_class ctxt.pool "object_stream" ~includes:[
    "stream_base" ;
  ] [
    "Type", entry_name_exact ~allow_ind:false ~optional:false "ObjStm" ;
    "N", entry_alias ~optional:false "intnonnegative" ;
    "First", entry_alias ~optional:false "intnonnegative" ;

    "Extends", entry_stream ~optional:true "object_stream" ;
  ];

  (***********************)
  (* PDF reference 7.4.x *)
  (***********************)
  register_alias ctxt.pool "decode_parameters" (Variant [
      Class "dp_flatelzw_none" ;
      Class "dp_flatelzw" ;
      Class "dp_ccitt" ;
      Class "dp_dct" ;

      (* TODO *)
    ]);

  (***********************)
  (* PDF reference 7.4.4 *)
  (***********************)
  register_class ctxt.pool "dp_flatelzw_base" [
    "EarlyChange", entry_int_in ~optional:true [| 0 ; 1 |] ;
  ];

  register_class ctxt.pool "dp_flatelzw_none" ~includes:[
    "dp_flatelzw_base" ;
  ] [
    "Predictor", entry_int_in ~optional:false [| 1 |] ;
  ];

  register_class ctxt.pool "dp_flatelzw" ~includes:[
    "dp_flatelzw_base" ;
  ] [
    "Predictor", entry_int_in ~optional:false [| 2 ; 10 ; 11 ; 12 ; 13 ; 14 ; 15 |] ;

    "Colors", entry_alias ~optional:true "intpositive" ;
    "BitsPerComponent", entry_int_in ~optional:true [| 1 ; 2 ; 4 ; 8 ; 16 |] ;
    "Columns", entry_alias ~optional:true "intpositive" ;
  ];

  (***********************)
  (* PDF reference 7.4.6 *)
  (***********************)
  register_class ctxt.pool "dp_ccitt" [
    "K", make_entry_type ~optional:true Int ;
    "EndOfLine", make_entry_type ~optional:true Bool ;
    "EncodedByteAlign", make_entry_type ~optional:true Bool ;
    "Columns", entry_alias ~optional:true "intpositive" ;
    "Rows", entry_alias ~optional:true "intnonnegative" ;
    "EndOfBlock", make_entry_type ~optional:true Bool ;
    "BlackIs1", make_entry_type ~optional:true Bool ;
    (*
    (* TODO : note that some writers made a typo ! *)
    "Blackls1", make_entry_type ~optional:true Bool ;
    *)
    "DamagedRowsBeforeError", entry_alias ~optional:true "intnonnegative" ;
  ];

  (***********************)
  (* PDF reference 7.4.8 *)
  (***********************)
  register_class ctxt.pool "dp_dct" [
    "ColorTransform", entry_int_in ~optional:false [| 0 ; 1 |] ;
  ];

