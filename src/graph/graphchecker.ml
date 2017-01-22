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


open Tree
open Document
open Errors
open Key
open Boundedint
open Directobject
open Indirectobject
open Params


module GraphChecker = struct

  let check doc _types =
    let trailer = Document.main_trailer doc in

    let catalog_k = DirectObject.get_reference
        () "Catalog is mandatory and shall be indirect" (Errors.make_ctxt_name Key.Trailer "Root")
        (DirectObject.dict_find trailer "Root") in

    let catalog = IndirectObject.get_direct_of
        "Catalog shall be a dictionary" (Errors.make_ctxt_key catalog_k)
        ~transform:(DirectObject.get_dict ())
        (Document.find_obj doc catalog_k) in


    (* Page tree *)
    let pageroot = DirectObject.get_reference
        () "Page root is mandatory and shall be indirect" (Errors.make_ctxt_name catalog_k "Pages")
        (DirectObject.dict_find catalog "Pages") in

    if Params.global.Params.debug then
      Printf.eprintf "Checking page tree...\n";
    Tree.check (Some "Parent") "Kids" doc pageroot;
    if Params.global.Params.debug then
      Printf.eprintf "Page tree OK\n";


    (* Name trees *)
    DirectObject.apply_not_null (DirectObject.dict_find catalog "Names") (fun x ->
        let xx, error_ctxt = Document.remove_ref doc x (Errors.make_ctxt_name catalog_k "Names") in
        let names = IndirectObject.get_direct_of
            "Names shall be a dictionary" error_ctxt
            ~transform:(DirectObject.get_dict ())
            xx in

        (* Destination tree *)
        DirectObject.apply_not_null (DirectObject.dict_find names "Dests") (fun x ->
            let destroot = DirectObject.get_reference
                () "Dest shall be indirect" (Errors.ctxt_append_name error_ctxt "Dests")
                x in

            if Params.global.Params.debug then
              Printf.eprintf "Checking destination tree...\n";
            Tree.check None "Kids" doc destroot;
            if Params.global.Params.debug then
              Printf.eprintf "Destination tree OK\n";
          );

        (* TODO : other name trees *)
      );


    (* Outlines *)
    DirectObject.apply_not_null (DirectObject.dict_find catalog "Outlines") (fun x ->
        let outlineroot = DirectObject.get_reference
            () "Outlines shall be indirect" (Errors.make_ctxt_name catalog_k "Outlines")
            x in

        if Params.global.Params.debug then
          Printf.eprintf "Checking outline tree...\n";
        Tree.checklist "Parent" "First" "Last" "Next" "Prev" doc outlineroot;
        if Params.global.Params.debug then
          Printf.eprintf "Outline tree OK\n";
      );

    (* TODO : extend to other structures *)

end

