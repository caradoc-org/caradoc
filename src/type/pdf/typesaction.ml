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


let register_action ctxt =

  (**********************)
  (* PDF reference 12.6 *)
  (**********************)
  register_class ~strict:false ctxt.pool "action_base" [
    "Next", entry_array_or_one ~optional:true (type_alias "action") ;
    (*
    "S", entry_name_in ~optional:false ["GoTo" ; "GoToR" ; "GoToE" ; "Launch" ; "Thread" ; "URI" ; "Sound" ; "Movie" ; "Hide" ; "Named" ; "SubmitForm" ; "ResetForm" ; "ImportData" ; "JavaScript" ; "SetOCGState" ; "Rendition" ; "Trans" ; "GoTo3DView"] ;
    *)

    "Type", entry_name_exact ~allow_ind:false ~optional:true "Action" ;
  ];

  register_alias ctxt.pool "action" (Variant [
      Class "action_goto" ;
      Class "action_uri" ;
      Class "action_named" ;

      (* TODO : remove *)
      Class "action_base" ;
    ]);

  (**************************)
  (* PDF reference 12.6.4.2 *)
  (**************************)
  register_class ctxt.pool "action_goto" ~includes:[
    "action_base" ;
  ] [
    "S", entry_name_exact ~optional:false "GoTo" ;
    "D", entry_alias ~optional:false "named_dest" ;
  ];

  (**************************)
  (* PDF reference 12.6.4.7 *)
  (**************************)
  register_class ctxt.pool "action_uri" ~includes:[
    "action_base" ;
  ] [
    "S", entry_name_exact ~optional:false "URI" ;
    "URI", make_entry_type ~optional:false Text ;
    "IsMap", make_entry_type ~optional:true Bool ;
  ];

  (**************************)
  (* PDF reference 12.6.4.2 *)
  (**************************)
  register_class ctxt.pool "action_named" ~includes:[
    "action_base" ;
  ] [
    "S", entry_name_exact ~optional:false "Named" ;
    (*
    "N", entry_name_in ~optional:false ["NextPage" ; "PrevPage" ; "FirstPage" ; "LastPage"] ;
    *)
    (* TODO : Non-standard *)
    "N", entry_name_in ~optional:false ["NextPage" ; "PrevPage" ; "FirstPage" ; "LastPage" ; "GoForward" ; "GoBack" ; "GoToPage" ; "Find"] ;
  ];

