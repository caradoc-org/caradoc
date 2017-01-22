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


let register_xobject ctxt =

  (*********************)
  (* PDF reference 8.8 *)
  (*********************)
  register_alias ctxt.pool "xobject" (Variant [
      Stream "xobject_form" ;
      Alias "xobject_image" ;

      (* TODO : remove *)
      Stream "xobject_base" ;
    ]);

  register_class ~strict:false ctxt.pool "xobject_base" ~includes:[
    "stream_base" ;
  ] [
    "Type", entry_name_exact ~allow_ind:false ~optional:true "XObject" ;

    "Name", make_entry_type ~optional:true Name ;
    "Metadata", entry_stream ~optional:true "metadata_stream" ;
  ];

  (***********************)
  (* PDF reference 8.9.5 *)
  (***********************)
  register_alias ctxt.pool "xobject_image" (Variant [
      Stream "xobject_image_normal" ;
    ]);

  register_class ctxt.pool "xobject_image_base" ~includes:[
    "xobject_base" ;
  ] [
    "Subtype", entry_name_exact ~optional:false "Image" ;
    "Width", entry_alias ~optional:false "intpositive" ;
    "Height", entry_alias ~optional:false "intpositive" ;

    "Intent", make_entry_type ~optional:true Name ;
    "Interpolate", make_entry_type ~optional:true Bool ;

    "Name", make_entry_type ~optional:true Name ;
    "Metadata", entry_stream ~optional:true "metadata_stream" ;

    (* TODO : restrictions *)
    "Decode", entry_array_tuples ~optional:true [|type_alias "number" ; type_alias "number"|] ;
    (* TODO *)
  ];

  register_class ~strict:false ctxt.pool "xobject_image_alternate" ~includes:[
    "xobject_image_base" ;
  ] [
    "ImageMask", make_entry_type ~optional:true (BoolExact false) ;

    "ColorSpace", entry_alias ~optional:true "color_space" ;
    "BitsPerComponent", entry_int_in ~optional:true [| 1 ; 2 ; 4 ; 8 ; 16 |] ;
    (* TODO *)
    "Mask", entry_variant ~optional:true [Stream "xobject_image_mask" ; Array (make_type Any)] ;
  ];

  register_class ~strict:false ctxt.pool "xobject_image_normal" ~includes:[
    "xobject_image_alternate" ;
  ] [
    (* TODO : allowed in mask ?? *)
    "Alternates", entry_array ~optional:true (type_class "image_alternate") ;

    "SMask", entry_stream ~optional:true "xobject_image_smask" ;
  ];

  register_class ~strict:false ctxt.pool "xobject_image_mask" ~includes:[
    "xobject_image_base" ;
  ] [
    "ImageMask", make_entry_type ~optional:false (BoolExact true) ;

    "BitsPerComponent", entry_int_exact ~optional:true 1 ;
  ];

  register_class ~strict:false ctxt.pool "xobject_image_smask" ~includes:[
    "xobject_image_base" ;
  ] [
    "ImageMask", make_entry_type ~optional:true (BoolExact true) ;

    "BitsPerComponent", entry_int_in ~optional:false [| 1 ; 2 ; 4 ; 8 ; 16 |] ;
    "ColorSpace", entry_alias ~optional:false "color_space_gray" ;
    (* TODO : restrictions *)
    (*
    "Matte", ?? ;
    *)
  ];

  (*************************)
  (* PDF reference 8.9.5.4 *)
  (*************************)
  register_class ~strict:false ctxt.pool "image_alternate" [
    "Image", entry_stream ~optional:false "xobject_image_alternate" ;
    "DefaultForPrinting", make_entry_type ~optional:true Bool ;
    (*
    (* TODO *)
    "OC", entry_alias ~optional:true "optional_content" ;
    *)
  ];

  (************************)
  (* PDF reference 8.10.2 *)
  (************************)
  register_class ~strict:false ctxt.pool "xobject_form" ~includes:[
    "xobject_base" ;
  ] [
    "Subtype", entry_name_exact ~optional:false "Form" ;
    "BBox", entry_alias ~optional:false "rectangle" ;

    "FormType", entry_int_exact ~optional:true 1 ;
    "Matrix", entry_alias ~optional:true "matrix6" ;
    "Resources", entry_class ~optional:true "resources" ;
    "LastModified", make_entry_type ~optional:true Date ;
    (* TODO *)
  ];

