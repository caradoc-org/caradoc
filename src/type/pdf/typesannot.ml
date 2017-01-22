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


let register_annot ctxt =

  (************************)
  (* PDF reference 12.5.2 *)
  (************************)
  register_class ctxt.pool "annot_base" [
    "Rect", entry_alias ~optional:false "rectangle" ;
    "Contents", make_entry_type ~optional:true Text ;
    "P", entry_class ~optional:true "page" ;
    "NM", make_entry_type ~optional:true Text ;
    "M", entry_variant ~optional:true [Text ; Date] ;
    "F", entry_flags ~optional:true 10 ;
    "AP", entry_class ~optional:true "appearance" ;
    "AS", make_entry_type ~optional:true Name ;
    "Border", entry_alias ~optional:true "border" ;
    "C", entry_alias ~optional:true "color" ;
    "StructParent", make_entry_type ~optional:true Int ;
    "OC", entry_class ~optional:true "optional" ;

    "Type", entry_name_exact ~allow_ind:false ~optional:true "Annot" ;
  ];

  register_class ctxt.pool "annot_markup" ~includes:[
    "annot_base" ;
  ] [
    (*
    "Subtype", entry_name_in ~optional:false ["Text" ; "FreeText" ; "Line" ; "Square" ; "Circle" ; "Polygon" ; "PolyLine" ; "Highlight" ; "Underline" ; "Squiggly" ; "StrikeOut" ; "Stamp" ; "Caret" ; "Ink" ; "FileAttachment" ; "Sound" ; "Redact"] ;
    *)

    "T", make_entry_type ~optional:true Text ;
    "Popup", entry_class ~optional:true "annot_popup" ;
    (* TODO *)
    "CA", make_entry_type ~optional:true Any ;
    (* TODO *)
    "RC", make_entry_type ~optional:true Any ;
    "CreationDate", make_entry_type ~optional:true Date ;
    "IRT", entry_alias ~optional:true "annotation" ;
    "Subj", make_entry_type ~optional:true Text ;
    "RT", entry_name_in ~optional:true ["R" ; "Group"] ;
    (* TODO *)
    "IT", make_entry_type ~optional:true Any ;
    (* TODO *)
    "ExData", make_entry_type ~optional:true Any ;
  ];

  register_class ctxt.pool "annot_nonmarkup" ~includes:[
    "annot_base" ;
  ] [
    (*
    "Subtype", entry_name_in ~optional:false ["Link" ; "Popup" ; "Movie" ; "Widget" ; "Screen" ; "PrinterMark" ; "TrapNet" ; "Watermark" ; "3D"] ;
    *)
  ];

  register_alias ctxt.pool "annotation" (Variant [
      Class "annot_text" ;
      Alias "annot_link" ;

      (* TODO : remove *)
    (*
    Class "annot_markup" ;
    Class "annot_nonmarkup" ;
    *)
    ]);

  (************************)
  (* PDF reference 12.5.5 *)
  (************************)
  register_class ~strict:false ctxt.pool "appearance" [
    (* TODO *)
  ];
  register_class ~strict:false ctxt.pool "optional" [
    (* TODO *)
  ];
  register_alias ctxt.pool "border" (Variant [
      Tuple [| type_alias "number" ; type_alias "number" ; type_alias "number" |] ;
      Tuple [| type_alias "number" ; type_alias "number" ; type_alias "number" ; type_alias "dash_pattern" |] ;
    ]);
  register_alias ctxt.pool "dash_pattern" Any;
  (* TODO *)

  (************************)
  (* PDF reference 12.5.4 *)
  (************************)
  register_class ctxt.pool "border_style" [
    "W", entry_alias ~optional:true "numnonnegative" ;
    "S", entry_name_in ~optional:true ["S" ; "D" ; "B" ; "I" ; "U" ; "A"] ;
    "D", entry_alias ~optional:true "dash_pattern" ;

    "Type", entry_name_exact ~allow_ind:false ~optional:true "Border" ;
  ];
  register_class ctxt.pool "border_effect_none" [
    "S", entry_name_exact ~optional:true "S" ;
  ];
  register_class ctxt.pool "border_effect_cloud" [
    "S", entry_name_exact ~optional:false "C" ;
    "I", entry_alias ~optional:true "num02" ;
  ];
  register_alias ctxt.pool "border_effect" (Variant [
      Class "border_effect_none" ;
      Class "border_effect_cloud" ;
    ]);

  (**************************)
  (* PDF reference 12.5.6.4 *)
  (**************************)
  register_class ctxt.pool "annot_text" ~includes:[
    "annot_markup" ;
  ] [
    "Subtype", entry_name_exact ~optional:false "Text" ;

    "Open", make_entry_type ~optional:true Bool ;
    (* TODO : allow extensions ? *)
    "Name", entry_name_in ~optional:true ["Comment" ; "Key" ; "Note" ; "Help" ; "NewParagraph" ; "Paragraph" ; "Insert"] ;
    (* TODO *)
    "State", make_entry_type ~optional:true Text ;
    "StateModel", make_entry_type ~optional:true Text ;
  ];

  (**************************)
  (* PDF reference 12.5.6.5 *)
  (**************************)
  register_class ctxt.pool "annot_link_base" ~includes:[
    "annot_nonmarkup" ;
  ] [
    "Subtype", entry_name_exact ~optional:false "Link" ;

    "H", entry_name_in ~optional:true ["N" ; "I" ; "O" ; "P"] ;
    "PA", entry_class ~optional:true "action_uri" ;
    "QuadPoints", entry_alias ~optional:true "quad_points" ;
    "BS", entry_class ~optional:true "border_style" ;
  ];
  register_class ctxt.pool "annot_link_action" ~includes:[
    "annot_link_base" ;
  ] [
    "A", entry_alias ~optional:false "action" ;
  ];
  register_class ctxt.pool "annot_link_dest" ~includes:[
    "annot_link_base" ;
  ] [
    "Dest", entry_alias ~optional:false "named_dest" ;
  ];

  register_alias ctxt.pool "annot_link" (Variant [
      Class "annot_link_action" ;
      Class "annot_link_dest" ;
      Class "annot_link_base" ;
    ]);

  (***************************)
  (* PDF reference 12.5.6.14 *)
  (***************************)
  register_class ~strict:false ctxt.pool "annot_popup" ~includes:[
    "annot_nonmarkup" ;
  ] [
    (* TODO *)
  ];

