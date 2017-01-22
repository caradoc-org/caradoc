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


let register_transition ctxt =

  (************************)
  (* PDF reference 12.4.4 *)
  (************************)
  register_alias ctxt.pool "transition" (Variant [
      Class "transition_fly" ;
      Class "transition_split" ;
      Class "transition_blinds" ;
      Class "transition_box" ;
      Class "transition_di" ;
      Class "transition_other" ;
    ]);

  register_class ctxt.pool "transition_base" [
    "Type", entry_name_exact ~allow_ind:false ~optional:true "Trans" ;

    "D", entry_alias ~optional:true "numnonnegative" ;
  ];

  register_class ctxt.pool "transition_fly" ~includes:[
    "transition_base"
  ] [
    "S", entry_name_exact ~optional:true "Fly" ;

    "M", entry_name_in ~optional:true ["I" ; "O"] ;
    (* TODO : check *)
    "Di", entry_variant ~optional:true [Alias "number" ; Name] ;
    "SS", entry_alias ~optional:true "numpositive" ;
    "B", make_entry_type ~optional:true Bool ;
  ];
  register_class ctxt.pool "transition_split" ~includes:[
    "transition_base"
  ] [
    "S", entry_name_exact ~optional:true "Split" ;

    "Dm", entry_name_in ~optional:true ["H" ; "V"] ;
    "M", entry_name_in ~optional:true ["I" ; "O"] ;
  ];
  register_class ctxt.pool "transition_blinds" ~includes:[
    "transition_base"
  ] [
    "S", entry_name_exact ~optional:true "Blinds" ;

    "Dm", entry_name_in ~optional:true ["H" ; "V"] ;
  ];
  register_class ctxt.pool "transition_box" ~includes:[
    "transition_base"
  ] [
    "S", entry_name_exact ~optional:true "Box" ;

    "M", entry_name_in ~optional:true ["I" ; "O"] ;
  ];
  register_class ctxt.pool "transition_di" ~includes:[
    "transition_base"
  ] [
    "S", entry_name_in ~optional:true ["Wipe" ; "Glitter" ; "Push" ; "Cover" ; "Uncover"] ;

    (* TODO : check *)
    "Di", entry_variant ~optional:true [Alias "number" ; Name] ;
  ];

  register_class ctxt.pool "transition_other" ~includes:[
    "transition_base"
  ] [
    "S", entry_name_in ~optional:true ["Dissolve" ; "R" ; "Fade"] ;
  ];

