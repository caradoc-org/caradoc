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
open Directobject
open Indirectobject
open Mapkey
open Intervals
open Boundedint
open Errors
open Algo
open Params
open Pdfstream


module CheckObjectType = struct

  exception Break of t


  let rec check_object (ctxt : context) (x : IndirectObject.t) (typ : t) (ectxt : Errors.error_ctxt) : t =
    match (x, typ.kind) with
    | (_, Alias name) ->
      check_alias ctxt x name typ.allow_ind ectxt

    | (IndirectObject.Direct (DirectObject.Reference key), _) when typ.allow_ind ->
      check_indirect ctxt key typ ectxt

    | (IndirectObject.Direct DirectObject.Null, Null)
    | (IndirectObject.Direct (DirectObject.Bool _), Bool)
    | (IndirectObject.Direct (DirectObject.Int _), Int)
    | (IndirectObject.Direct (DirectObject.Real _), Real)
    | (IndirectObject.Direct (DirectObject.String _), String)
    | (IndirectObject.Direct (DirectObject.Name _), Name) ->
      typ

    | (IndirectObject.Direct (DirectObject.String _), Text)
    | (IndirectObject.Direct (DirectObject.String _), Date) ->
      (* TODO : check *)
      typ

    | (IndirectObject.Direct (DirectObject.Bool x), BoolExact expected) ->
      if x <> expected then
        raise (Errors.TypeError (Printf.sprintf "Boolean value %B is not the expected one (%B)" x expected, ectxt));
      typ

    | (IndirectObject.Direct (DirectObject.Int x), IntRange (low, high)) ->
      begin
        match low with
        | Some l ->
          if x <: l then
            raise (Errors.TypeError (Printf.sprintf "Integer value %s is below the minimum (%s)" (BoundedInt.to_string x) (BoundedInt.to_string l), ectxt));
        | None -> ()
      end;
      begin
        match high with
        | Some h ->
          if x >: h then
            raise (Errors.TypeError (Printf.sprintf "Integer value %s is above the maximum (%s)" (BoundedInt.to_string x) (BoundedInt.to_string h), ectxt));
        | None -> ()
      end;
      typ
    | (IndirectObject.Direct (DirectObject.Int x), IntExact expected) ->
      if x <> expected then
        raise (Errors.TypeError (Printf.sprintf "Integer value %s is not the expected one (%s)" (BoundedInt.to_string x) (BoundedInt.to_string expected), ectxt));
      typ
    | (IndirectObject.Direct (DirectObject.Int x), IntIn expected) ->
      if not (Algo.array_contains expected x) then (
        let expected_str = Algo.join_string Array.fold_left BoundedInt.to_string ", " expected in
        raise (Errors.TypeError (Printf.sprintf "Integer value %s is not among the expected ones (%s)" (BoundedInt.to_string x) expected_str, ectxt))
      );
      typ

    | (IndirectObject.Direct (DirectObject.Name name), NameExact expected) ->
      if name <> expected then
        raise (Errors.TypeError (Printf.sprintf "Name value /%s is not the expected one (/%s)" name expected, ectxt));
      typ
    | (IndirectObject.Direct (DirectObject.Name name), NameIn expected) ->
      if not (Hashtbl.mem expected name) then (
        let expected_str = Algo.join_string List.fold_left (fun (x, _) -> "/" ^ x) ", " (Algo.sort_hash expected) in
        raise (Errors.TypeError (Printf.sprintf "Name value /%s is not among the expected ones (%s)" name expected_str, ectxt))
      );
      typ

    | (IndirectObject.Direct (DirectObject.Array l), Array elemtype)
    | (IndirectObject.Direct (DirectObject.Array l), ArrayOrOne elemtype) ->
      check_array ctxt l elemtype ectxt;
      {kind = Array elemtype; allow_ind = typ.allow_ind;}

    | (_, ArrayOrOne elemtype) ->
      check_object ctxt x elemtype ectxt

    | (IndirectObject.Direct (DirectObject.Array l), ArraySized (elemtype, len)) ->
      check_array_sized ctxt l elemtype len ectxt;
      typ

    | (IndirectObject.Direct (DirectObject.Array l), ArrayVariantSized (elemtype, lens)) ->
      check_array_variant_sized ctxt l elemtype lens ectxt;
      typ

    | (IndirectObject.Direct (DirectObject.Array l), ArrayTuples types) ->
      check_array_tuples ctxt l types ectxt;
      typ

    | (IndirectObject.Direct (DirectObject.Array l), ArrayDifferences) ->
      check_array_differences ctxt l ectxt;
      typ

    | (IndirectObject.Direct (DirectObject.Array l), Tuple types) ->
      check_tuple ctxt l types ectxt;
      typ

    | (_, Variant options) ->
      check_variant ctxt x options ectxt

    | (IndirectObject.Direct (DirectObject.Dictionary dict), Dictionary elemtype) ->
      check_dict ctxt dict elemtype ectxt;
      typ

    | (IndirectObject.Direct (DirectObject.Dictionary dict), Class typename) ->
      check_class ctxt dict typename ectxt;
      typ

    | (IndirectObject.Stream s, Stream typename) ->
      check_class ctxt (PDFStream.get_dict s) typename ectxt;
      typ

    | (_, Any) ->
      (* This file makes use of the "any" wildcard, thus is not fully checked*)
      ctxt.incomplete <- true;

      if Params.global.Params.verbose then
        Errors.warning "Any type specified" ectxt;
      typ

    | _ ->
      raise (Errors.TypeError (Printf.sprintf "Invalid type : expected %s" (kind_to_string typ.kind), ectxt))


  and check_object_direct (ctxt : context) (x : DirectObject.t) (typ : t) (ectxt : Errors.error_ctxt) : t =
    check_object ctxt (IndirectObject.Direct x) typ ectxt


  and check_alias (ctxt : context) (x : IndirectObject.t) (name : string) (allow_ind : bool) (ectxt : Errors.error_ctxt) : t =
    let kind =
      try
        Hashtbl.find (snd ctxt.pool) name
      with Not_found ->
        raise (Errors.UnexpectedError (Printf.sprintf "Undeclared alias type %s" name));
    in
    check_object ctxt x {kind = kind; allow_ind = allow_ind;} ectxt


  and check_dict (ctxt : context) (dict : DirectObject.dict_t) (elemtype : t) (ectxt : Errors.error_ctxt) : unit =
    DirectObject.dict_iter
      (fun name x ->
         let (_:t) = check_object_direct ctxt x elemtype (Errors.ctxt_append_name ectxt name) in
         ()
      ) dict


  and check_array (ctxt : context) (l : DirectObject.t list) (elemtype : t) (ectxt : Errors.error_ctxt) : unit =
    Algo.iteri List.fold_left
      (fun i y ->
         let (_:t) = check_object_direct ctxt y elemtype (Errors.ctxt_append_index ectxt i) in
         ()
      ) l


  and check_array_sized (ctxt : context) (l : DirectObject.t list) (elemtype : t) (len : int) (ectxt : Errors.error_ctxt) : unit =
    let list_len = List.length l in
    if list_len <> len then
      raise (Errors.TypeError (Printf.sprintf "Array size (%d) is not the expected one (%d)" list_len len, ectxt));
    check_array ctxt l elemtype ectxt


  and check_array_variant_sized (ctxt : context) (l : DirectObject.t list) (elemtype : t) (lens : int array) (ectxt : Errors.error_ctxt) : unit =
    let list_len = List.length l in
    if not (Algo.array_contains lens list_len) then (
      let expected_str = Algo.join_string Array.fold_left string_of_int ", " lens in
      raise (Errors.TypeError (Printf.sprintf "Array size (%d) is not among the expected ones (%s)" list_len expected_str, ectxt))
    );
    check_array ctxt l elemtype ectxt


  and check_array_tuples (ctxt : context) (l : DirectObject.t list) (types : t array) (ectxt : Errors.error_ctxt) : unit =
    let list_len = List.length l in
    let tuple_len = Array.length types in
    if list_len mod tuple_len <> 0 then
      raise (Errors.TypeError (Printf.sprintf "Array size (%d) is not among the expected ones (multiples of %d) for this array of tuples" list_len tuple_len, ectxt));
    Algo.iteri List.fold_left
      (fun i y ->
         let (_:t) = check_object_direct ctxt y types.(i mod (Array.length types)) (Errors.ctxt_append_index ectxt i) in
         ()
      ) l


  and check_array_differences (_ctxt : context) (l : DirectObject.t list) (ectxt : Errors.error_ctxt) : unit =
    let i = Intervals.create () in
    let (low, high) = List.fold_left
        (fun (low, high) o ->
           match o with
           | DirectObject.Int c ->
             begin
               match low, high with
               | (Some l, Some h) ->
                 Intervals.add i (l, h) ();
               | _ -> ()
             end;
             (* TODO : check *)
             (Some c, Some (c -: ~:1))
           | DirectObject.Name _ ->
             (* TODO : check character code ? *)
             begin
               match low, high with
               | (Some _, Some h) ->
                 (low, Some (h +: ~:1))
               | _ ->
                 raise (Errors.TypeError ("No code for differences", ectxt))
             end
           | _ ->
             raise (Errors.TypeError ("Invalid differences", ectxt));
        ) (None, None) l
    in
    begin
      match low, high with
      | (Some l, Some h) ->
        Intervals.add i (l, h) ();
      | _ -> ()
    end;

    if Intervals.check_overlaps i <> None then
      raise (Errors.TypeError ("Overlapping differences", ectxt))


  and check_tuple (ctxt : context) (l : DirectObject.t list) (types : t array) (ectxt : Errors.error_ctxt) : unit =
    let list_len = List.length l in
    let tuple_len = Array.length types in
    if list_len <> tuple_len then
      raise (Errors.TypeError (Printf.sprintf "Array size (%d) is not the expected one (%d) for this tuple" list_len tuple_len, ectxt));
    Algo.iteri List.fold_left
      (fun i y ->
         let (_:t) = check_object_direct ctxt y types.(i) (Errors.ctxt_append_index ectxt i) in
         ()
      ) l


  and check_variant (ctxt : context) (x : IndirectObject.t) (options : kind_t list) (ectxt : Errors.error_ctxt) : t =
    try
      List.iter
        (fun kind ->
           let typ = {kind = kind; allow_ind = true;} in
           try
             let copy = copy_context ctxt in
             let real_type = check_object copy x typ ectxt in
             assign_context ctxt copy;
             raise (Break real_type)
           with
           | Break real_type ->
             raise (Break real_type)
          (*
        | Errors.TypeError (msg, key, entry) ->
            if Params.global.Params.verbose then (
              Printf.eprintf "Type variant error : %s" msg;
              if not (Key.is_none key) then
                Printf.eprintf " for object %s" (Key.to_string key);
              if entry <> "" then
                Printf.eprintf " at entry %s" entry;
              Printf.eprintf " (not of type %s) %!\n" (kind_to_string kind);
            )
          *)
           | _ -> ()
        ) options;
      raise (Errors.TypeError (Printf.sprintf "Invalid variant type : expected %s" (kind_to_string (Variant options)), ectxt));
    with Break typ ->
      (*
    if Params.global.Params.verbose then
      Printf.eprintf "Type is : %s\n" (type_to_string typ);
      *)
      typ


  and check_indirect (ctxt : context) (key : Key.t) (typ : t) (ectxt : Errors.error_ctxt) : t =
    if not (MapKey.mem key ctxt.types) then (
      ctxt.types <- MapKey.add key typ.kind ctxt.types;
      ctxt.to_check <- (key, ectxt)::ctxt.to_check;
    (*
    Printf.eprintf "Object %s is of type %s\n" (Key.to_string key) (type_to_string typ);
    *)
      typ
    ) else (
      let intersect = type_intersection ctxt.pool typ.kind (MapKey.find key ctxt.types) (Errors.make_ctxt_key key) in
      ctxt.types <- MapKey.add key intersect ctxt.types;
      {kind = intersect; allow_ind = true;}
    )


  and check_class (ctxt : context) (dict : DirectObject.dict_t) (typename : string) (ectxt : Errors.error_ctxt) : unit =
    let entries = Hashtbl.create (DirectObject.dict_length dict) in
    DirectObject.dict_iter
      (fun name _ ->
         Hashtbl.add entries name true
      ) dict;

    let strict = check_subclass ctxt dict typename ectxt entries in
    (* This object contains unknown entries, thus is not fully checked *)
    (* Exception: if the object is the /Info dictionary and the user explicitly allowed it, no error is reported *)
    if not (typename = "info" && Params.global.Params.allow_arbitrary_info) then (
      if Hashtbl.length entries > 0 then
        ctxt.incomplete <- true
    );

    (* Iterate over additional entries *)
    Hashtbl.iter
      (fun name _ ->
         Errors.warning_or_type_error (not strict) Params.global.Params.verbose (Printf.sprintf "Unexpected entry /%s in instance of class %s" name typename) ectxt
      ) entries

  and check_subclass (ctxt : context) (dict : DirectObject.dict_t) (typename : string) (ectxt : Errors.error_ctxt) (entries : (string, bool) Hashtbl.t) : bool =
    let class_type, includes, strict =
      try
        Hashtbl.find (fst ctxt.pool) typename
      with Not_found ->
        raise (Errors.UnexpectedError (Printf.sprintf "Undeclared class type %s" typename));
    in
    (* Check entries of this class *)
    Hashtbl.iter
      (fun name entry_type ->
         if DirectObject.dict_mem dict name then (
           let obj = DirectObject.dict_find dict name in
           let (_:t) = check_object_direct ctxt obj entry_type.typ (Errors.ctxt_append_name ectxt name) in
           Hashtbl.remove entries name
         ) else if not entry_type.optional then
           raise (Errors.TypeError (Printf.sprintf "Mandatory entry /%s was not found in instance of class %s" name typename, ectxt))
      ) class_type;

    (* Check entries of included classes *)
    List.iter
      (fun include_name ->
         let (_:bool) = check_subclass ctxt dict include_name ectxt entries in
         ()
      ) includes;

    strict

end

