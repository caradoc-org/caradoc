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
open Boundedint


let register_graphic ctxt =

  (***********************)
  (* PDF reference 8.4.5 *)
  (***********************)
  register_class ~strict:false ctxt.pool "graphic_state" [
    "Type", entry_name_exact ~allow_ind:false ~optional:true "ExtGState" ;

    (* TODO : check allowed values *)
    "LW", entry_alias ~optional:true "number" ;
    (* TODO : check allowed values *)
    "LC", make_entry_type ~optional:true Int ;
    (* TODO : check allowed values *)
    "LJ", make_entry_type ~optional:true Int ;
    "ML", entry_alias ~optional:true "number" ;
    "D", entry_alias ~optional:true "dash_pattern" ;
    (* TODO : check allowed values *)
    "RI", make_entry_type ~optional:true Name ;
    "OP", make_entry_type ~optional:true Bool ;
    "op", make_entry_type ~optional:true Bool ;
    (* TODO : check allowed values *)
    "OPM", make_entry_type ~optional:true Int ;
    (* TODO : check allowed values *)
    "Font", entry_tuple ~optional:true [| type_alias "font" ; type_alias "numpositive" |] ;
    "BG", entry_alias ~optional:true "function" ;
    "BG2", entry_variant ~optional:true [Alias "function" ; NameExact "Default"] ;
    "UCR", entry_alias ~optional:true "function" ;
    "UCR2", entry_variant ~optional:true [Alias "function" ; NameExact "Default"] ;
    "TR", entry_variant ~optional:true [Alias "function" ; ArraySized (type_alias "function", 4) ; NameExact "Identity"] ;
    "TR2", entry_variant ~optional:true [Alias "function" ; ArraySized (type_alias "function", 4) ; kind_name_in ["Identity" ; "Default"]] ;
    "HT", entry_variant ~optional:true [Alias "halftone" ; NameExact "Default"] ;
    (* TODO : check allowed values *)
    "FL", entry_alias ~optional:true "number" ;
    (* TODO : check allowed values *)
    "SM", entry_alias ~optional:true "number" ;
    "SA", make_entry_type ~optional:true Bool ;
    "BM", entry_alias ~optional:true "blend_mode" ;
    "SMask", entry_alias ~optional:true "soft_mask" ;
    (* TODO : check allowed values *)
    "CA", entry_alias ~optional:true "number" ;
    (* TODO : check allowed values *)
    "ca", entry_alias ~optional:true "number" ;
    "AIS", make_entry_type ~optional:true Bool ;
    "TK", make_entry_type ~optional:true Bool ;
  ];

  (* TODO *)
  register_alias ctxt.pool "halftone" Any;
  register_alias ctxt.pool "blend_mode" Any;
  register_alias ctxt.pool "soft_mask" Any;

  (*********************)
  (* PDF reference 8.7 *)
  (*********************)
  register_alias ctxt.pool "pattern" (Variant [
      Stream "tiling_pattern" ;
      Class "shading_pattern" ;
    ]);

  register_class ctxt.pool "pattern_base" [
    "Type", entry_name_exact ~allow_ind:false ~optional:true "Pattern" ;

    "Matrix", entry_alias ~optional:true "matrix6" ;
  ];

  register_class ctxt.pool "tiling_pattern" ~includes:[
    "stream_base" ;
    "pattern_base" ;
  ] [
    "PatternType", entry_int_exact ~optional:false 1 ;
    "PaintType", make_entry_type ~optional:false Int ;
    "TilingType", make_entry_type ~optional:false Int ;
    "BBox", entry_alias ~optional:false "rectangle" ;
    "XStep", entry_alias ~optional:false "numnonzero" ;
    "YStep", entry_alias ~optional:false "numnonzero" ;
    "Resources", entry_class ~optional:false "resources" ;
  ];

  register_class ctxt.pool "shading_pattern" ~includes:[
    "pattern_base" ;
  ] [
    "PatternType", entry_int_exact ~optional:false 2 ;
    "Shading", entry_alias ~optional:false "shading" ;
    "ExtGState", entry_class ~optional:true "graphic_state" ;
  ];

  (*************************)
  (* PDF reference 8.7.4.3 *)
  (*************************)
  register_alias ctxt.pool "shading" (Variant [
      Class "shading_1" ;
      Class "shading_2" ;
      Class "shading_3" ;
      Stream "shading_4" ;
      Stream "shading_5" ;
      Stream "shading_6" ;
      Stream "shading_7" ;
    ]);

  register_class ctxt.pool "shading_base" [
    "ColorSpace", entry_alias ~optional:false "color_space" ;
    (* TODO : background *)
    "Background", entry_array ~optional:true (make_type Any) ;
    "BBox", entry_alias ~optional:true "rectangle" ;
    "AntiAlias", make_entry_type ~optional:true Bool ;
  ];

  register_class ctxt.pool "shading_1" ~includes:[
    "shading_base" ;
  ] [
    "ShadingType", entry_int_exact ~allow_ind:false ~optional:false 1 ;

    "Domain", entry_alias ~optional:true "rectangle" ;
    "Matrix", entry_alias ~optional:true "matrix6" ;
    "Function", entry_alias ~optional:false "function" ;
  ];
  register_class ctxt.pool "shading_2" ~includes:[
    "shading_base" ;
  ] [
    "ShadingType", entry_int_exact ~allow_ind:false ~optional:false 2 ;

    "Coords", entry_sized_array ~optional:false 4 (type_alias "number") ;
    "Domain", entry_sized_array ~optional:true 2 (type_alias "number") ;
    "Function", entry_alias ~optional:false "function" ;
    "Extend", entry_sized_array ~optional:true 2 (make_type Bool) ;
  ];
  register_class ctxt.pool "shading_3" ~includes:[
    "shading_base" ;
  ] [
    "ShadingType", entry_int_exact ~allow_ind:false ~optional:false 3 ;

    "Coords", entry_sized_array ~optional:false 6 (type_alias "number") ;
    "Domain", entry_sized_array ~optional:true 2 (type_alias "number") ;
    "Function", entry_alias ~optional:false "function" ;
    "Extend", entry_sized_array ~optional:true 2 (make_type Bool) ;
  ];

  register_class ctxt.pool "shading_stream_base" ~includes:[
    "shading_base" ;
    "stream_base" ;
  ] [
    "BitsPerCoordinate", entry_int_in ~optional:false [| 1 ; 2 ; 4 ; 8 ; 12 ; 16 ; 24 ; 32 |] ;
    "BitsPerComponent", entry_int_in ~optional:false [| 1 ; 2 ; 4 ; 8 ; 12 ; 16 |] ;
    "Decode", entry_array_tuples ~optional:false [| type_alias "number" ; type_alias "number" |] ;
    "Function", entry_alias ~optional:true "function" ;
  ];
  register_class ctxt.pool "shading_467" ~includes:[
    "shading_stream_base" ;
  ] [
    "BitsPerFlag", entry_int_in ~optional:false [| 2 ; 4 ; 8 |] ;
  ];

  register_class ctxt.pool "shading_4" ~includes:[
    "shading_467" ;
  ] [
    "ShadingType", entry_int_exact ~allow_ind:false ~optional:false 4 ;
  ];
  register_class ctxt.pool "shading_5" ~includes:[
    "shading_stream_base" ;
  ] [
    "ShadingType", entry_int_exact ~allow_ind:false ~optional:false 5 ;

    "VerticesPerRow", make_entry_type ~optional:false (IntRange (Some ~:2, None)) ;
  ];
  register_class ctxt.pool "shading_6" ~includes:[
    "shading_467" ;
  ] [
    "ShadingType", entry_int_exact ~allow_ind:false ~optional:false 6 ;
  ];
  register_class ctxt.pool "shading_7" ~includes:[
    "shading_467" ;
  ] [
    "ShadingType", entry_int_exact ~allow_ind:false ~optional:false 7 ;
  ];

