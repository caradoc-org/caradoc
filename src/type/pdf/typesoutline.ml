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


let register_outline ctxt =

  (************************)
  (* PDF reference 12.3.3 *)
  (************************)
  register_class ctxt.pool "outlines" [
    "First", entry_alias ~optional:true "outlineitem" ;
    "Last", entry_alias ~optional:true "outlineitem" ;
    "Count", entry_alias ~optional:true "intnonnegative" ;

    "Type", entry_name_exact ~allow_ind:false ~optional:true "Outlines" ;
  ];

  (************************)
  (* PDF reference 12.3.3 *)
  (************************)
  register_alias ctxt.pool "outlineitem" (Variant [
      Class "outlineitem_base" ;
      Class "outlineitem_dest" ;
      Class "outlineitem_action" ;
    ]);


  register_class ctxt.pool "outlineitem_base" [
    "Title", make_entry_type ~optional:false String ;
    "Parent", entry_variant ~optional:false [Alias "outlineitem" ; Class "outlines"] ;
    "Prev", entry_alias ~optional:true "outlineitem" ;
    "Next", entry_alias ~optional:true "outlineitem" ;
    "First", entry_alias ~optional:true "outlineitem" ;
    "Last", entry_alias ~optional:true "outlineitem" ;
    "Count", make_entry_type ~optional:true Int ;
    "F", entry_flags ~optional:true 2 ;
    "SE", entry_class ~optional:true "struct_element" ;
    "C", entry_alias ~optional:true "color_rgb" ;
  ];

  register_class ctxt.pool "outlineitem_dest" ~includes:[
    "outlineitem_base" ;
  ] [
    "Dest", entry_alias ~optional:false "named_dest" ;
  ];

  register_class ctxt.pool "outlineitem_action" ~includes:[
    "outlineitem_base" ;
  ] [
    "A", entry_alias ~optional:false "action" ;
  ];

