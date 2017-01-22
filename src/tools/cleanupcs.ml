(*****************************************************************************)
(*  Caradoc: a PDF parser and validator                                      *)
(*  Copyright (C) 2016-2017 Guillaume Endignoux                              *)
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


open Document
open Directobject
open Indirectobject
open Key
open Tree
open Errors
open Params
open Boundedint
open Pdfstream


module CleanupCS = struct

  let cleanup_page (doc : Document.t) (objnum : BoundedInt.t ref) (node : DirectObject.dict_t) (node_k : Key.t) : unit =
    let tmp = DirectObject.dict_find node "Contents" in
    let contents =
      try
        let tmp2, (_:Errors.error_ctxt) = Document.remove_ref doc tmp Errors.ctxt_none in
        let tmp3 = IndirectObject.get_direct "" Errors.ctxt_none tmp2 in
        if DirectObject.is_array tmp3 then
          tmp3
        else
          tmp
      with _ ->
        tmp
    in

    let contents_array = DirectObject.get_array_of
        ~accept_one:true ()
        "Expected reference or array of references" (Errors.make_ctxt_name node_k "Contents")
        ~transform:(DirectObject.get_reference ())
        contents in

    (* Collect data in all content streams *)
    let buf = Buffer.create 16 in
    Array.iteri (fun i cs_k ->
        let ctxt_cs = Errors.make_ctxt_key cs_k in
        let stm = IndirectObject.get_stream
            "Content stream shall be a stream" ctxt_cs
            (Document.find_obj doc cs_k) in

        let str = PDFStream.get_decoded stm ctxt_cs in
        Buffer.add_string buf str;
        if i > 0 then
          Buffer.add_char buf '\n';
      ) contents_array;

    (* Create new content stream *)
    let merged_stream = PDFStream.make_contents (Buffer.contents buf) Errors.ctxt_none "FlateDecode" in

    let k = Key.make_0 !objnum in
    objnum := !objnum +: ~:1;
    Document.add doc k (IndirectObject.Stream merged_stream);

    DirectObject.dict_set node ("Contents", DirectObject.Reference k);
    Document.set doc node_k (IndirectObject.Direct (DirectObject.Dictionary node))


  let cleanup_pagenode_impl (doc : Document.t) (objnum : BoundedInt.t ref) (node : DirectObject.dict_t) (node_k : Key.t) (error_ctxt : Errors.error_ctxt) : unit =
    let typ = DirectObject.get_name
        () "Type shall be a name" (Errors.ctxt_append_name error_ctxt "Type")
        (DirectObject.dict_find node "Type") in

    match typ with
    | "Pages" ->
      ()
    | "Page" ->
      cleanup_page doc objnum node node_k
    | _ ->
      raise (Errors.PDFError ("Unexpected /Type in page tree", error_ctxt))

  let cleanup_pagenode (doc : Document.t) (objnum : BoundedInt.t ref) (node : DirectObject.dict_t) (node_k : Key.t) _ : unit =
    let error_ctxt = Errors.make_ctxt_key node_k in
    Errors.catch ~fail:(fun () ->
        Errors.warning "Could not cleanup content stream(s) for page" error_ctxt
      ) (fun () -> cleanup_pagenode_impl doc objnum node node_k error_ctxt)


  let cleanup (doc : Document.t) : unit =
    let trailer = Document.main_trailer doc in

    let catalog_k = DirectObject.get_reference
        () "Catalog is mandatory and shall be indirect" (Errors.make_ctxt_name Key.Trailer "Root")
        (DirectObject.dict_find trailer "Root") in

    let catalog = IndirectObject.get_direct_of
        "Catalog shall be a dictionary" (Errors.make_ctxt_key catalog_k)
        ~transform:(DirectObject.get_dict ())
        (Document.find_obj doc catalog_k) in

    (* Find available object numbers *)
    let objnum = ref ((Document.max_objnum doc) +: ~:1) in

    (* Page tree *)
    let pageroot = DirectObject.get_reference
        () "Page root is mandatory and shall be indirect" (Errors.make_ctxt_name catalog_k "Pages")
        (DirectObject.dict_find catalog "Pages") in

    if Params.global.Params.debug then
      Printf.eprintf "Traversing page tree...\n";
    Tree.traverse (cleanup_pagenode doc objnum) "Kids" doc pageroot;
    if Params.global.Params.debug then
      Printf.eprintf "Page tree OK\n";

end

