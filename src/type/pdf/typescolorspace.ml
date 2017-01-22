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


let register_colorspace ctxt =

  (*********************)
  (* PDF reference 8.6 *)
  (*********************)
  register_alias ctxt.pool "color_space" (Variant [
      Alias "color_space_special" ;
      (* TODO : specification is not very clear... *)
      NameExact "Pattern" ;
      Tuple [| type_name_exact ~allow_ind:false "Pattern" |] ;
      Tuple [| type_name_exact ~allow_ind:false "Pattern" ; type_alias "color_space_special" |] ;
    ]);

  register_alias ctxt.pool "color_space_special" (Variant [
      Alias "color_space_basic_sepdevicen" ;
      Tuple [| type_name_exact ~allow_ind:false "Indexed" ; type_alias "color_space_basic_sepdevicen" ; make_type (IntRange (Some ~:0, Some ~:255)) ; type_alias "stream_or_bytes" |] ;
    ]);

  register_alias ctxt.pool "color_space_basic_sepdevicen" (Variant [
      Alias "color_space_basic" ;
      Tuple [| type_name_exact ~allow_ind:false "Separation" ; make_type Name ; type_alias "color_space_basic" ; type_alias "function" |] ;
      Tuple [| type_name_exact ~allow_ind:false "DeviceN" ; make_type (Array (make_type Name)) ; type_alias "color_space_basic" ; type_alias "function" |] ;
      Tuple [| type_name_exact ~allow_ind:false "DeviceN" ; make_type (Array (make_type Name)) ; type_alias "color_space_basic" ; type_alias "function" ; type_class "cs_devicen_attr" |] ;
    ]);

  register_alias ctxt.pool "color_space_basic" (Variant [
      kind_name_in ["DeviceGray" ; "DeviceRGB" ; "DeviceCMYK"] ;
      Tuple [| type_name_in ~allow_ind:false ["DeviceGray" ; "DeviceRGB" ; "DeviceCMYK"] |] ;
      Tuple [| type_name_exact ~allow_ind:false "CalGray" ; type_class "cs_cal_gray" |] ;
      Tuple [| type_name_exact ~allow_ind:false "CalRGB" ; type_class "cs_cal_rgb" |] ;
      Tuple [| type_name_exact ~allow_ind:false "Lab" ; type_class "cs_lab" |] ;
      Tuple [| type_name_exact ~allow_ind:false "ICCBased" ; type_stream "cs_icc" |] ;
    ]);

  register_alias ctxt.pool "color_space_gray" (Variant [
      NameExact "DeviceGray" ;
      Tuple [| type_name_exact ~allow_ind:false "DeviceGray" |] ;
    ]);

  register_alias ctxt.pool "whitepoint" (Tuple [| type_alias "numpositive" ; type_alias "numone" ; type_alias "numpositive" |]);
  register_alias ctxt.pool "blackpoint" (Tuple [| type_alias "numnonnegative" ; type_alias "numnonnegative" ; type_alias "numnonnegative" |]);

  (*************************)
  (* PDF reference 8.6.5.2 *)
  (*************************)
  register_class ctxt.pool "cs_cal_gray" [
    "WhitePoint", entry_alias ~optional:false "whitepoint" ;
    "BlackPoint", entry_alias ~optional:true "blackpoint" ;
    "Gamma", entry_alias ~optional:true "numpositive" ;
  ];
  (*************************)
  (* PDF reference 8.6.5.3 *)
  (*************************)
  register_class ctxt.pool "cs_cal_rgb" [
    "WhitePoint", entry_alias ~optional:false "whitepoint" ;
    "BlackPoint", entry_alias ~optional:true "blackpoint" ;
    "Gamma", entry_sized_array ~optional:true 3 (type_alias "numpositive") ;
    "Matrix", entry_alias ~optional:true "matrix9" ;
  ];
  (*************************)
  (* PDF reference 8.6.5.4 *)
  (*************************)
  register_class ctxt.pool "cs_lab" [
    "WhitePoint", entry_alias ~optional:false "whitepoint" ;
    "BlackPoint", entry_alias ~optional:true "blackpoint" ;
    (* TODO : order constraint on values *)
    "Range", entry_sized_array ~optional:true 4 (type_alias "number") ;
  ];
  (*************************)
  (* PDF reference 8.6.5.5 *)
  (*************************)
  register_class ctxt.pool "cs_icc" ~includes:[
    "stream_base" ;
  ] [
    "N", entry_int_in ~optional:false [| 1 ; 3 ; 4 |] ;
    "Alternate", entry_alias ~optional:true "color_space_special" ;
    (* TODO : order constraint on values *)
    "Range", entry_array_tuples ~optional:true [| type_alias "number" ; type_alias "number" |] ;
    "Metadata", entry_stream ~optional:true "metadata_stream" ;
  ];
  (*************************)
  (* PDF reference 8.6.6.5 *)
  (*************************)
  register_class ~strict:false ctxt.pool "cs_devicen_attr" [
    "Subtype", entry_name_in ~optional:true ["DeviceN" ; "NChannel"] ;
    (*
    "Colorants", entry_class ~optional:true "colorants" ;
    "Process", entry_class ~optional:true "process" ;
    "MixingHints", entry_class ~optional:true "mixinghints" ;
    *)
    (* TODO *)
  ];

