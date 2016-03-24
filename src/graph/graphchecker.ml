(*****************************************************************************)
(*  Caradoc: a PDF parser and validator                                      *)
(*  Copyright (C) 2015 ANSSI                                                 *)
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
open Pdfobject
open Params


module GraphChecker = struct

  let check doc _types =
    let trailer =
      match Document.trailers doc with
      | [] ->
        raise (Errors.PDFError ("No trailer", Errors.ctxt_none))
      | t::_ ->
        t
    in

    let catalog_k = PDFObject.get_reference
        () "Catalog is mandatory and shall be indirect" (Errors.make_ctxt_key Key.Trailer)
        (PDFObject.dict_find trailer "Root") in

    let catalog = PDFObject.get_dict
        () "Catalog shall be a dictionary" (Errors.make_ctxt_key catalog_k)
        (Document.find doc catalog_k) in


    (* Page tree *)
    let pageroot = PDFObject.get_reference
        () "Page root is mandatory and shall be indirect" (Errors.make_ctxt_key catalog_k)
        (PDFObject.dict_find catalog "Pages") in

    if Params.global.Params.debug then
      Printf.eprintf "Checking page tree...\n";
    Tree.check (Some "Parent") "Kids" doc pageroot;
    if Params.global.Params.debug then
      Printf.eprintf "Page tree OK\n";


    (* Name trees *)
    PDFObject.apply_not_null (PDFObject.dict_find catalog "Names") (fun x ->
        let names = PDFObject.get_dict
            () "Names shall be a dictionary" Errors.ctxt_none
            (Document.remove_ref doc x) in

        (* Destination tree *)
        PDFObject.apply_not_null (PDFObject.dict_find names "Dests") (fun x ->
            let destroot = PDFObject.get_reference
                () "Dest shall be indirect" Errors.ctxt_none
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
    PDFObject.apply_not_null (PDFObject.dict_find catalog "Outlines") (fun x ->
        let outlineroot = PDFObject.get_reference
            () "Outlines shall be indirect" Errors.ctxt_none
            x in

        if Params.global.Params.debug then
          Printf.eprintf "Checking outline tree...\n";
        Tree.checklist "Parent" "First" "Last" "Next" "Prev" doc outlineroot;
        if Params.global.Params.debug then
          Printf.eprintf "Outline tree OK\n";
      );

    (* TODO : extend to other structures *)

end

