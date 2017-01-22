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


open Key
open Setkey
open Boundedint
open Directobject
open Indirectobject
open Document
open Errors
open Params


module Tree = struct

  let traverse (f : DirectObject.dict_t -> Key.t -> Key.t -> unit) (children_s : string) (doc : Document.t) (root_k : Key.t) : unit =

    let rec traverse_children (doc : Document.t) (visited : SetKey.t ref) (node : DirectObject.dict_t) (node_k : Key.t) =
      let error_ctxt = Errors.make_ctxt_name node_k children_s in

      let children = DirectObject.get_array_of
          ~default:[] () "Graph error : Expected an array of references" error_ctxt
          ~transform:(DirectObject.get_reference ())
          (DirectObject.dict_find node children_s) in

      Array.iter (fun child_k ->
          traverse_subtree doc visited child_k node_k
        ) children


    and traverse_subtree (doc : Document.t) (visited : SetKey.t ref) (node_k : Key.t) (parent_k : Key.t) =
      let error_ctxt = Errors.make_ctxt_key node_k in

      if Params.global.Params.debug then
        Printf.eprintf "Visiting node %s\n" (Key.to_string node_k);

      (* Check cycles *)
      if SetKey.mem node_k !visited then
        raise (Errors.PDFError ("Graph error : Cyclic tree structure", error_ctxt))
      else
        visited := SetKey.add node_k !visited;

      let node = IndirectObject.get_direct_of
          "Graph error : Expected a dictionary" error_ctxt
          ~transform:(DirectObject.get_dict ())
          (Document.find_obj doc node_k) in

      (* Call function *)
      f node node_k parent_k;

      (* Traverse children *)
      traverse_children doc visited node node_k
    in

    let traverse_tree (doc : Document.t) (root_k : Key.t) =
      if Params.global.Params.debug then
        Printf.eprintf "Visiting root node %s\n" (Key.to_string root_k);

      let visited = ref SetKey.empty in
      visited := SetKey.add root_k !visited;

      let root = IndirectObject.get_direct_of
          "Graph error : Expected a dictionary" (Errors.make_ctxt_key root_k)
          ~transform:(DirectObject.get_dict ())
          (Document.find_obj doc root_k) in

      (* Traverse children *)
      traverse_children doc visited root root_k
    in

    traverse_tree doc root_k


  let check (parent_s : string option) (children_s : string) (doc : Document.t) (root_k : Key.t) : unit =

    let checkparent =
      match parent_s with
      | None ->
        (fun _ _ _ -> ())
      | Some s ->
        (fun node node_k parent_k ->
           let error_ctxt = Errors.make_ctxt_name node_k s in

           let p = DirectObject.get_reference
               () "Graph error : Expected a reference" error_ctxt
               (DirectObject.dict_find node s) in

           if p <> parent_k then
             raise (Errors.PDFError ("Graph error : Invalid parent in tree structure", error_ctxt))
        )
    in

    traverse checkparent children_s doc root_k


  let checklist (parent_s : string) (first_s : string) (last_s : string) (next_s : string) (prev_s : string) (doc : Document.t) (root_k : Key.t) =

    let checkparent (node : DirectObject.dict_t) (node_k : Key.t) (parent_k : Key.t) =
      let error_ctxt = Errors.make_ctxt_name node_k parent_s in

      let k = DirectObject.get_reference
          () "Graph error : Expected a reference" error_ctxt
          (DirectObject.dict_find node parent_s) in

      if k <> parent_k then
        raise (Errors.PDFError ("Graph error : Invalid parent in tree structure", error_ctxt))
    in

    let checkprev (node : DirectObject.dict_t) (node_k : Key.t) (prev_k : Key.t option) (first_k : Key.t) =
      let prev_entry = DirectObject.dict_find node prev_s in

      if node_k = first_k then (
        let error_ctxt = Errors.make_ctxt_key node_k in
        if prev_entry <> DirectObject.Null then
          raise (Errors.PDFError (Printf.sprintf "Graph error : Node is %s element but has %s element" first_s prev_s, error_ctxt))
      ) else (
        let error_ctxt = Errors.make_ctxt_name node_k prev_s in

        let k = DirectObject.get_reference
            () (Printf.sprintf "Graph error : Expected a reference") error_ctxt
            prev_entry in

        match prev_k with
        | Some l when k = l ->
          ()
        | _ ->
          raise (Errors.PDFError ("Graph error : Invalid previous element in list structure", error_ctxt))
      )
    in


    let rec checkchildren (doc : Document.t) (visited : SetKey.t ref) (node : DirectObject.dict_t) (node_k : Key.t) =
      let error_ctxt = Errors.make_ctxt_key node_k in

      let first_entry = DirectObject.dict_find node first_s in
      let last_entry = DirectObject.dict_find node last_s in

      if first_entry <> DirectObject.Null || last_entry <> DirectObject.Null then (
        let first_k = DirectObject.get_reference
            () "Graph error : Expected a reference" (Errors.ctxt_append_name error_ctxt first_s)
            first_entry in

        let last_k = DirectObject.get_reference
            () "Graph error : Expected a reference" (Errors.ctxt_append_name error_ctxt last_s)
            last_entry in

        checksubtree doc visited first_k last_k node_k
      )

    and checksubtree (doc : Document.t) (visited : SetKey.t ref) (first_k : Key.t) (last_k : Key.t) (parent_k : Key.t) =
      checknode doc visited first_k None first_k last_k parent_k

    and checknext (doc : Document.t) (visited : SetKey.t ref) (node : DirectObject.dict_t) (node_k : Key.t) (first_k : Key.t) (last_k : Key.t) (parent_k : Key.t) =
      let next_entry = DirectObject.dict_find node next_s in

      if node_k = last_k then (
        let error_ctxt = Errors.make_ctxt_key node_k in
        if next_entry <> DirectObject.Null then
          raise (Errors.PDFError (Printf.sprintf "Graph error : Node is %s element but has %s element" last_s next_s, error_ctxt))
      ) else (
        let error_ctxt = Errors.make_ctxt_name node_k next_s in

        let next_k = DirectObject.get_reference
            () (Printf.sprintf "Graph error : Expected a reference") error_ctxt
            next_entry in
        checknode doc visited next_k (Some node_k) first_k last_k parent_k
      )


    and checknode (doc : Document.t) (visited : SetKey.t ref) (node_k : Key.t) (prev_k : Key.t option) (first_k : Key.t) (last_k : Key.t) (parent_k : Key.t) =
      let error_ctxt = Errors.make_ctxt_key node_k in

      if Params.global.Params.debug then
        Printf.eprintf "Visiting node %s\n" (Key.to_string node_k);

      (* Check cycles *)
      if SetKey.mem node_k !visited then
        raise (Errors.PDFError ("Graph error : Cyclic tree structure", error_ctxt))
      else
        visited := SetKey.add node_k !visited;

      let node = IndirectObject.get_direct_of
          "Graph error : Expected a dictionary" error_ctxt
          ~transform:(DirectObject.get_dict ())
          (Document.find_obj doc node_k) in

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

      let root = IndirectObject.get_direct_of
          "Graph error : Expected a dictionary" (Errors.make_ctxt_key root_k)
          ~transform:(DirectObject.get_dict ())
          (Document.find_obj doc root_k) in

      (* Check children *)
      checkchildren doc visited root root_k
    in

    checktree doc root_k

end

