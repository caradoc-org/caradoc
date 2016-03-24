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


open Type.Type
open Pdfobject
open Mapkey
open Intervals
open Boundedint
open Errors
open Algo
open Params


module CheckObjectType = struct

  exception Break of t


  let rec check_object (ctxt : context) (x : PDFObject.t) (typ : t) (indobj : Key.t) (entry : string) : t =
    match (x, typ.kind) with
    | (_, Alias name) ->
      check_alias ctxt x name typ.allow_ind indobj entry

    | (PDFObject.Reference key, _) when typ.allow_ind ->
      check_indirect ctxt key typ indobj entry

    | (PDFObject.Null, Null)
    | (PDFObject.Bool _, Bool)
    | (PDFObject.Int _, Int)
    | (PDFObject.Real _, Real)
    | (PDFObject.String _, String)
    | (PDFObject.Name _, Name) ->
      typ

    | (PDFObject.String _, Text)
    | (PDFObject.String _, Date) ->
      (* TODO : check *)
      typ

    | (PDFObject.Bool x, BoolExact expected) ->
      if x <> expected then
        raise (Errors.TypeError (Printf.sprintf "Boolean value %B is not the expected one (%B)" x expected, indobj, entry));
      typ

    | (PDFObject.Int x, IntRange (low, high)) ->
      begin
        match low with
        | Some l ->
          if x <: l then
            raise (Errors.TypeError (Printf.sprintf "Integer value %s is below the minimum (%s)" (BoundedInt.to_string x) (BoundedInt.to_string l), indobj, entry));
        | None -> ()
      end;
      begin
        match high with
        | Some h ->
          if x >: h then
            raise (Errors.TypeError (Printf.sprintf "Integer value %s is above the maximum (%s)" (BoundedInt.to_string x) (BoundedInt.to_string h), indobj, entry));
        | None -> ()
      end;
      typ
    | (PDFObject.Int x, IntExact expected) ->
      if x <> expected then
        raise (Errors.TypeError (Printf.sprintf "Integer value %s is not the expected one (%s)" (BoundedInt.to_string x) (BoundedInt.to_string expected), indobj, entry));
      typ
    | (PDFObject.Int x, IntIn expected) ->
      if not (Algo.array_contains expected x) then (
        let expected_str = Algo.join_string Array.fold_left BoundedInt.to_string ", " expected in
        raise (Errors.TypeError (Printf.sprintf "Integer value %s is not among the expected ones (%s)" (BoundedInt.to_string x) expected_str, indobj, entry))
      );
      typ

    | (PDFObject.Name name, NameExact expected) ->
      if name <> expected then
        raise (Errors.TypeError (Printf.sprintf "Name value /%s is not the expected one (/%s)" name expected, indobj, entry));
      typ
    | (PDFObject.Name name, NameIn expected) ->
      if not (Hashtbl.mem expected name) then (
        let expected_str = Algo.join_string List.fold_left (fun (x, _) -> "/" ^ x) ", " (Algo.sort_hash expected) in
        raise (Errors.TypeError (Printf.sprintf "Name value /%s is not among the expected ones (%s)" name expected_str, indobj, entry))
      );
      typ

    | (PDFObject.Array l, Array elemtype)
    | (PDFObject.Array l, ArrayOrOne elemtype) ->
      check_array ctxt l elemtype indobj entry;
      {kind = Array elemtype; allow_ind = typ.allow_ind;}

    | (_, ArrayOrOne elemtype) ->
      check_object ctxt x elemtype indobj (entry ^ "[?]")

    | (PDFObject.Array l, ArraySized (elemtype, len)) ->
      check_array_sized ctxt l elemtype len indobj entry;
      typ

    | (PDFObject.Array l, ArrayVariantSized (elemtype, lens)) ->
      check_array_variant_sized ctxt l elemtype lens indobj entry;
      typ

    | (PDFObject.Array l, ArrayTuples types) ->
      check_array_tuples ctxt l types indobj entry;
      typ

    | (PDFObject.Array l, ArrayDifferences) ->
      check_array_differences ctxt l indobj entry;
      typ

    | (PDFObject.Array l, Tuple types) ->
      check_tuple ctxt l types indobj entry;
      typ

    | (_, Variant options) ->
      check_variant ctxt x options indobj entry

    | (PDFObject.Dictionary dict, Dictionary elemtype) ->
      check_dict ctxt dict elemtype indobj entry;
      typ

    | (PDFObject.Dictionary dict, Class typename)
    | (PDFObject.Stream (dict, _, _), Stream typename) ->
      check_class ctxt dict typename indobj entry;
      typ

    | (_, Any) ->
      (* This file makes use of the "any" wildcard, thus is not fully checked*)
      ctxt.incomplete <- true;

      if Params.global.Params.verbose then (
        Printf.eprintf "Warning : any type specified";
        if entry <> "" then
          Printf.eprintf " at entry %s" entry;
        Printf.eprintf " in object %s" (Key.to_string indobj);
        Printf.eprintf "\n"
      );
      typ

    | _ ->
      raise (Errors.TypeError (Printf.sprintf "Invalid type : expected %s" (kind_to_string typ.kind), indobj, entry))


  and check_alias (ctxt : context) (x : PDFObject.t) (name : string) (allow_ind : bool) (indobj : Key.t) (entry : string) : t =
    let kind =
      try
        Hashtbl.find (snd ctxt.pool) name
      with Not_found ->
        raise (Errors.UnexpectedError (Printf.sprintf "Undeclared alias type %s" name));
    in
    check_object ctxt x {kind = kind; allow_ind = allow_ind;} indobj entry


  and check_dict (ctxt : context) (dict : PDFObject.dict_t) (elemtype : t) (indobj : Key.t) (entry : string) : unit =
    PDFObject.dict_iter
      (fun name x ->
         let (_:t) = check_object ctxt x elemtype indobj (entry ^ "/" ^ name) in
         ()
      ) dict


  and check_array (ctxt : context) (l : PDFObject.t list) (elemtype : t) (indobj : Key.t) (entry : string) : unit =
    let (_:int) = List.fold_left
        (fun i y ->
           let (_:t) = check_object ctxt y elemtype indobj (Printf.sprintf "%s[%d]" entry i) in
           i + 1
        ) 0 l
    in ()


  and check_array_sized (ctxt : context) (l : PDFObject.t list) (elemtype : t) (len : int) (indobj : Key.t) (entry : string) : unit =
    let list_len = List.length l in
    if list_len <> len then
      raise (Errors.TypeError (Printf.sprintf "Array size (%d) is not the expected one (%d)" list_len len, indobj, entry));
    check_array ctxt l elemtype indobj entry


  and check_array_variant_sized (ctxt : context) (l : PDFObject.t list) (elemtype : t) (lens : int array) (indobj : Key.t) (entry : string) : unit =
    let list_len = List.length l in
    if not (Algo.array_contains lens list_len) then (
      let expected_str = Algo.join_string Array.fold_left string_of_int ", " lens in
      raise (Errors.TypeError (Printf.sprintf "Array size (%d) is not among the expected ones (%s)" list_len expected_str, indobj, entry))
    );
    check_array ctxt l elemtype indobj entry


  and check_array_tuples (ctxt : context) (l : PDFObject.t list) (types : t array) (indobj : Key.t) (entry : string) : unit =
    let list_len = List.length l in
    let tuple_len = Array.length types in
    if list_len mod tuple_len <> 0 then
      raise (Errors.TypeError (Printf.sprintf "Array size (%d) is not among the expected ones (multiples of %d) for this array of tuples" list_len tuple_len, indobj, entry));
    let (_:int) = List.fold_left
        (fun i y ->
           let (_:t) = check_object ctxt y types.(i mod (Array.length types)) indobj (Printf.sprintf "%s[%d]" entry i) in
           i + 1
        ) 0 l
    in ()


  and check_array_differences (_ctxt : context) (l : PDFObject.t list) (indobj : Key.t) (entry : string) : unit =
    let i = Intervals.create () in
    let (low, high) = List.fold_left
        (fun (low, high) o ->
           match o with
           | PDFObject.Int c ->
             begin
               match low, high with
               | (Some l, Some h) ->
                 Intervals.add i (l, h) ();
               | _ -> ()
             end;
             (* TODO : check *)
             (Some c, Some (c -: ~:1))
           | PDFObject.Name _ ->
             (* TODO : check character code ? *)
             begin
               match low, high with
               | (Some _, Some h) ->
                 (low, Some (h +: ~:1))
               | _ ->
                 raise (Errors.TypeError ("No code for differences", indobj, entry))
             end
           | _ ->
             raise (Errors.TypeError ("Invalid differences", indobj, entry));
        ) (None, None) l
    in
    begin
      match low, high with
      | (Some l, Some h) ->
        Intervals.add i (l, h) ();
      | _ -> ()
    end;

    if Intervals.check_overlaps i <> None then
      raise (Errors.TypeError ("Overlapping differences", indobj, entry))


  and check_tuple (ctxt : context) (l : PDFObject.t list) (types : t array) (indobj : Key.t) (entry : string) : unit =
    let list_len = List.length l in
    let tuple_len = Array.length types in
    if list_len <> tuple_len then
      raise (Errors.TypeError (Printf.sprintf "Array size (%d) is not the expected one (%d) for this tuple" list_len tuple_len, indobj, entry));
    let (_:int) = List.fold_left
        (fun i y ->
           let (_:t) = check_object ctxt y types.(i) indobj (Printf.sprintf "%s[%d]" entry i) in
           i + 1
        ) 0 l
    in ()


  and check_variant (ctxt : context) (x : PDFObject.t) (options : kind_t list) (indobj : Key.t) (entry : string) : t =
    try
      List.iter
        (fun kind ->
           let typ = {kind = kind; allow_ind = true;} in
           try
             let copy = copy_context ctxt in
             let real_type = check_object copy x typ indobj entry in
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
      raise (Errors.TypeError (Printf.sprintf "Invalid variant type : expected %s" (kind_to_string (Variant options)), indobj, entry));
    with
    | Break typ ->
      (*
    if Params.global.Params.verbose then
      Printf.eprintf "Type is : %s\n" (type_to_string typ);
      *)
      typ


  and check_indirect (ctxt : context) (key : Key.t) (typ : t) (_indobj : Key.t) (entry : string) : t =
    if not (MapKey.mem key ctxt.types) then (
      ctxt.types <- MapKey.add key typ.kind ctxt.types;
      ctxt.to_check <- key::ctxt.to_check;
    (*
    Printf.eprintf "Object %s is of type %s\n" (Key.to_string key) (type_to_string typ);
    *)
      typ
    ) else (
      let intersect = type_intersection ctxt.pool typ.kind (MapKey.find key ctxt.types) key entry in
      ctxt.types <- MapKey.add key intersect ctxt.types;
      {kind = intersect; allow_ind = true;}
    )


  and check_class (ctxt : context) (dict : PDFObject.dict_t) (typename : string) (indobj : Key.t) (entry : string) : unit =
    let entries = Hashtbl.create (PDFObject.dict_length dict) in
    PDFObject.dict_iter
      (fun name _ ->
         Hashtbl.add entries name true
      ) dict;

    let strict = check_subclass ctxt dict typename indobj entry entries in
    (* This object contains unknown entries, thus is not fully checked *)
    (* Exception: if the object is the /Info dictionary and the user explicitly allowed it, no error is reported *)
    if not (typename = "info" && Params.global.Params.allow_arbitrary_info) then (
      if Hashtbl.length entries > 0 then
        ctxt.incomplete <- true
    );

    (* Iterate over additional entries *)
    Hashtbl.iter
      (fun name _ ->
         if strict then
           raise (Errors.TypeError (Printf.sprintf "Unexpected entry /%s in instance of class %s" name typename, indobj, entry))
         else if Params.global.Params.verbose then (
           Printf.eprintf "Warning : unexpected entry /%s in instance of class %s" name typename;
           if entry <> "" then
             Printf.eprintf " at entry %s" entry;
           Printf.eprintf " in object %s" (Key.to_string indobj);
           Printf.eprintf "\n";
         )
      ) entries

  and check_subclass (ctxt : context) (dict : PDFObject.dict_t) (typename : string) (indobj : Key.t) (entry : string) (entries : (string, bool) Hashtbl.t) : bool =
    let class_type, includes, strict =
      try
        Hashtbl.find (fst ctxt.pool) typename
      with Not_found ->
        raise (Errors.UnexpectedError (Printf.sprintf "Undeclared class type %s" typename));
    in
    (* Check entries of this class *)
    Hashtbl.iter
      (fun name entry_type ->
         if PDFObject.dict_mem dict name then (
           let obj = PDFObject.dict_find dict name in
           let (_:t) = check_object ctxt obj entry_type.typ indobj (entry ^ "/" ^ name) in
           Hashtbl.remove entries name
         ) else if not entry_type.optional then
           raise (Errors.TypeError (Printf.sprintf "Mandatory entry /%s was not found in instance of class %s" name typename, indobj, entry))
      ) class_type;

    (* Check entries of included classes *)
    List.iter
      (fun include_name ->
         let (_:bool) = check_subclass ctxt dict include_name indobj entry entries in
         ()
      ) includes;

    strict

end

