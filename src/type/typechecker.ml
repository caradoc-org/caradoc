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


open Type
open Checkobjecttype
open Document
open Errors
open Boundedint
open Pdftypes
open Mapkey
open Directobject
open Indirectobject
open Stats
open Params

module TypeChecker = struct

  let init () : Type.context =
    let ctxt = Type.create_context () in
    load_types ctxt;
    Type.check_pool ctxt.Type.pool;
    ctxt


  let check (doc : Document.t) (stats : Stats.t) : Type.kind_t MapKey.t =
    let ctxt = init () in
    let trailer = Document.main_trailer doc in

    if Params.global.Params.verbose then
      Printf.eprintf "\nChecking trailer\n";
    let (_:Type.t) = CheckObjectType.check_alias ctxt (IndirectObject.Direct (DirectObject.Dictionary trailer)) "trailer" false (Errors.make_ctxt_key Key.Trailer) in

    Document.iter_stms
      (fun key kind ->
         begin
           match kind with
           | Document.Objstm ->
             ctxt.Type.types <- MapKey.add key (Type.Alias "stream_objstm") ctxt.Type.types
           | Document.Xrefstm ->
             ctxt.Type.types <- MapKey.add key (Type.Alias "stream_xrefstm") ctxt.Type.types
         end;
         ctxt.Type.to_check <- (key, Errors.ctxt_none)::ctxt.Type.to_check
      ) doc;

    while ctxt.Type.to_check <> [] do
      let key, error_ctxt = List.hd ctxt.Type.to_check in
      ctxt.Type.to_check <- List.tl ctxt.Type.to_check;

      if Params.global.Params.verbose then
        Printf.eprintf "Remaining objects : %d -- checking object : %s " (List.length ctxt.Type.to_check) (Key.to_string key);

      if not (Document.mem_obj doc key) then
        raise (Errors.PDFError (Printf.sprintf "Reference to unknown object during type checking : %s" (Key.to_string key), error_ctxt));

      let typ = {
        Type.kind = MapKey.find key ctxt.Type.types;
        Type.allow_ind = false;
      } in

      if Params.global.Params.verbose then
        Printf.eprintf "of type : %s\n" (Type.type_to_string typ);

      let real_type = CheckObjectType.check_object ctxt (Document.find_obj doc key) typ (Errors.make_ctxt_key key) in
      if Params.global.Params.verbose then
        Printf.eprintf "Object %s has type %s\n\n" (Key.to_string key) (Type.type_to_string real_type);

      ctxt.Type.types <- MapKey.add key real_type.Type.kind ctxt.Type.types
    done;

    stats.Stats.incompletetypes <- ctxt.Type.incomplete;
    ctxt.Type.types


  let dump (types : Type.kind_t MapKey.t) (objcount : int) (filename : string) =
    let out = open_out_bin filename in

    let total = ref 0 in
    let count = Hashtbl.create 16 in

    MapKey.iter (fun key typ ->
        let strtype = Type.kind_to_string typ in
        Printf.fprintf out "%s : %s\n" (Key.to_string key) strtype;

        incr total;
        if Hashtbl.mem count strtype then
          Hashtbl.replace count strtype ((Hashtbl.find count strtype) + 1)
        else
          Hashtbl.add count strtype 1
      ) types;

    Printf.fprintf out "\n# of objects of known type : %d / %d\n\n" !total objcount;

    let l = Hashtbl.fold
        (fun key value l ->
           (key, value)::l
        ) count []
    in

    let sorted = List.sort
        (fun (key1, count1) (key2, count2) ->
           let cmp = compare count2 count1 in
           if cmp != 0 then
             cmp
           else
             String.compare key1 key2
        ) l
    in

    List.iter (fun (strtype, cnt) ->
        Printf.fprintf out "%s : %d\n" strtype cnt;
      ) sorted;

    close_out out

end

