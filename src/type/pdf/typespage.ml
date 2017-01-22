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


let register_page ctxt =

  (*************************)
  (* PDF reference 7.7.3.4 *)
  (*************************)
  register_class ctxt.pool "page_inheritable" [
    "Resources", entry_class ~optional:true "resources" ;
    "MediaBox", entry_alias ~optional:true "rectangle" ;
    "CropBox", entry_alias ~optional:true "rectangle" ;
    "Rotate", entry_alias ~optional:true "angle" ;
  ];

  (*************************)
  (* PDF reference 7.7.3.2 *)
  (*************************)
  register_class ctxt.pool "pageroot" ~includes:[
    "page_inheritable" ;
  ] [
    "Type", entry_name_exact ~allow_ind:false ~optional:false "Pages" ;

    "Count", entry_alias ~optional:false "intpositive" ;
    "Kids", entry_array ~optional:false (type_variant [Class "pagenode" ; Class "page"]) ;
  ];
  (*************************)
  (* PDF reference 7.7.3.2 *)
  (*************************)
  register_class ctxt.pool "pagenode" ~includes:[
    "pageroot" ;
  ] [
    "Parent", entry_variant ~optional:false [Class "pageroot" ; Class "pagenode"] ;
  ];

  (*************************)
  (* PDF reference 7.7.3.3 *)
  (*************************)
  register_class ~strict:false ctxt.pool "page" ~includes:[
    "page_inheritable" ;
  ] [
    "Type", entry_name_exact ~allow_ind:false ~optional:false "Page" ;

    "Parent", entry_variant ~optional:false [Class "pageroot" ; Class "pagenode"] ;
    "LastModified", make_entry_type ~optional:true Date ;
    "BleedBox", entry_alias ~optional:true "rectangle" ;
    "TrimBox", entry_alias ~optional:true "rectangle" ;
    "ArtBox", entry_alias ~optional:true "rectangle" ;

    "Contents", entry_array_or_one ~optional:true (type_stream "content_stream") ;
    "Trans", entry_alias ~optional:true "transition" ;
    "Annots", entry_array ~optional:true (type_alias "annotation") ;
    (* TODO *)
    (*
    "Group", entry_alias ~optional:true "group" ;
    *)
  ];

