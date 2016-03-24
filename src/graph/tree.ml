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


open Key
open Setkey
open Boundedint
open Pdfobject
open Document
open Errors
open Params


module Tree = struct

  let check (parent_s : string option) (children_s : string) (doc : Document.t) (root_k : Key.t) : unit =

    let checkparent =
      match parent_s with
      | None ->
        (fun _ _ _ -> ())
      | Some s ->
        (fun node node_k parent_k ->
           let p = PDFObject.get_reference
               () (Printf.sprintf "Graph error : Expected a reference in /%s entry" s) (Errors.make_ctxt_key node_k)
               (PDFObject.dict_find node s) in

           if p <> parent_k then
             raise (Errors.PDFError ("Graph error : Invalid parent in tree structure", Errors.make_ctxt_key node_k))
        )
    in


    let rec checkchildren (doc : Document.t) (visited : SetKey.t ref) (node : PDFObject.dict_t) (node_k : Key.t) =
      let children = PDFObject.get_array_of
          ~default:[] () (Printf.sprintf "Graph error : Expected an array of references in /%s entry" children_s) (Errors.make_ctxt_key node_k)
          ~transform:(PDFObject.get_reference ())
          (PDFObject.dict_find node children_s) in

      Array.iter (fun child_k ->
          checksubtree doc visited child_k node_k
        ) children


    and checksubtree (doc : Document.t) (visited : SetKey.t ref) (node_k : Key.t) (parent_k : Key.t) =
      if Params.global.Params.debug then
        Printf.eprintf "Visiting node %s\n" (Key.to_string node_k);

      (* Check cycles *)
      if SetKey.mem node_k !visited then
        raise (Errors.PDFError ("Graph error : Cyclic tree structure", Errors.make_ctxt_key node_k))
      else
        visited := SetKey.add node_k !visited;

      let node = PDFObject.get_dict
          () "Graph error : Expected a dictionary" (Errors.make_ctxt_key node_k)
          (Document.find doc node_k) in

      (* Check parent *)
      checkparent node node_k parent_k;

      (* Check children *)
      checkchildren doc visited node node_k
    in

    let checktree (doc : Document.t) (root_k : Key.t) =
      if Params.global.Params.debug then
        Printf.eprintf "Visiting root node %s\n" (Key.to_string root_k);

      let visited = ref SetKey.empty in
      visited := SetKey.add root_k !visited;

      let root = PDFObject.get_dict
          () "Graph error : Expected a dictionary" (Errors.make_ctxt_key root_k)
          (Document.find doc root_k) in

      (* Check children *)
      checkchildren doc visited root root_k
    in

    checktree doc root_k


  let checklist (parent_s : string) (first_s : string) (last_s : string) (next_s : string) (prev_s : string) (doc : Document.t) (root_k : Key.t) =

    let checkparent (node : PDFObject.dict_t) (node_k : Key.t) (parent_k : Key.t) =
      let k = PDFObject.get_reference
          () (Printf.sprintf "Graph error : Expected a reference in /%s entry" parent_s) (Errors.make_ctxt_key node_k)
          (PDFObject.dict_find node parent_s) in

      if k <> parent_k then
        raise (Errors.PDFError ("Graph error : Invalid parent in tree structure", Errors.make_ctxt_key node_k))
    in

    let checkprev (node : PDFObject.dict_t) (node_k : Key.t) (prev_k : Key.t option) (first_k : Key.t) =
      let prev_entry = PDFObject.dict_find node prev_s in
      if node_k = first_k then (
        if prev_entry <> PDFObject.Null then
          raise (Errors.PDFError (Printf.sprintf "Graph error : Node is %s element but has %s element" first_s prev_s, Errors.make_ctxt_key node_k))
      ) else (
        let k = PDFObject.get_reference
            () (Printf.sprintf "Graph error : Expected a reference in /%s entry" prev_s) (Errors.make_ctxt_key node_k)
            prev_entry in

        match prev_k with
        | Some l when k = l ->
          ()
        | _ ->
          raise (Errors.PDFError ("Graph error : Invalid previous element in list structure", Errors.make_ctxt_key node_k))
      )
    in


    let rec checkchildren (doc : Document.t) (visited : SetKey.t ref) (node : PDFObject.dict_t) (node_k : Key.t) =
      let first_entry = PDFObject.dict_find node first_s in
      let last_entry = PDFObject.dict_find node last_s in

      if first_entry <> PDFObject.Null || last_entry <> PDFObject.Null then (
        let first_k = PDFObject.get_reference
            () (Printf.sprintf "Graph error : Expected a reference in /%s entry" first_s) (Errors.make_ctxt_key node_k)
            first_entry in

        let last_k = PDFObject.get_reference
            () (Printf.sprintf "Graph error : Expected a reference in /%s entry" last_s) (Errors.make_ctxt_key node_k)
            last_entry in

        checksubtree doc visited first_k last_k node_k
      )

    and checksubtree (doc : Document.t) (visited : SetKey.t ref) (first_k : Key.t) (last_k : Key.t) (parent_k : Key.t) =
      checknode doc visited first_k None first_k last_k parent_k

    and checknext (doc : Document.t) (visited : SetKey.t ref) (node : PDFObject.dict_t) (node_k : Key.t) (first_k : Key.t) (last_k : Key.t) (parent_k : Key.t) =
      let next_entry = PDFObject.dict_find node next_s in
      if node_k = last_k then (
        if next_entry <> PDFObject.Null then
          raise (Errors.PDFError (Printf.sprintf "Graph error : Node is %s element but has %s element" last_s next_s, Errors.make_ctxt_key node_k))
      ) else (
        let next_k = PDFObject.get_reference
            () (Printf.sprintf "Graph error : Expected a reference in /%s entry" next_s) (Errors.make_ctxt_key node_k)
            next_entry in
        checknode doc visited next_k (Some node_k) first_k last_k parent_k
      )


    and checknode (doc : Document.t) (visited : SetKey.t ref) (node_k : Key.t) (prev_k : Key.t option) (first_k : Key.t) (last_k : Key.t) (parent_k : Key.t) =
      if Params.global.Params.debug then
        Printf.eprintf "Visiting node %s\n" (Key.to_string node_k);

      (* Check cycles *)
      if SetKey.mem node_k !visited then
        raise (Errors.PDFError ("Graph error : Cyclic tree structure", Errors.make_ctxt_key node_k))
      else
        visited := SetKey.add node_k !visited;

      let node = PDFObject.get_dict
          () "Graph error : Expected a dictionary" (Errors.make_ctxt_key node_k)
          (Document.find doc node_k) in

      (* Check parent *)
      checkparent node node_k parent_k;
      (* Check previous *)
      checkprev node node_k prev_k first_k;

      (* Check children *)
      checkchildren doc visited node node_k;
      (* Check next *)
      checknext doc visited node node_k first_k last_k parent_k
    in

    let checktree (doc : Document.t) (root_k : Key.t) =
      if Params.global.Params.debug then
        Printf.eprintf "Visiting root node %s\n" (Key.to_string root_k);

      let visited = ref SetKey.empty in
      visited := SetKey.add root_k !visited;

      let root = PDFObject.get_dict
          () "Graph error : Expected a dictionary" (Errors.make_ctxt_key root_k)
          (Document.find doc root_k) in

      (* Check children *)
      checkchildren doc visited root root_k
    in

    checktree doc root_k

end

