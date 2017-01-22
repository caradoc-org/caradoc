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


open Mapkey
open Errors
open Boundedint
open Algo


module Type = struct

  type 'a kind =
    (* TODO : remove Any, that accepts any type *)
    | Any
    | Alias of string
    | Class of string
    | Stream of string
    | Null
    | Bool | Int | Real | String | Name
    | Text | Date

    | BoolExact of bool
    | IntRange of (BoundedInt.t option) * (BoundedInt.t option)
    | IntExact of BoundedInt.t
    | IntIn of BoundedInt.t array
    | NameExact of string
    | NameIn of (string, unit) Hashtbl.t

    | Array of 'a
    | ArrayOrOne of 'a
    | ArraySized of 'a * int
    | ArrayVariantSized of 'a * (int array)
    | ArrayTuples of 'a array
    | ArrayDifferences
    | Tuple of 'a array
    | Variant of 'a kind list
    | Dictionary of 'a

  type t = {
    kind : t kind;
    allow_ind : bool;
  }

  type kind_t = t kind


  type entry_t = {
    typ : t;
    optional : bool;
  }

  type class_t = ((string, entry_t) Hashtbl.t) * (string list) * bool
  type pool_t = ((string, class_t) Hashtbl.t) * ((string, kind_t) Hashtbl.t)

  type context = {
    mutable pool : pool_t;
    mutable types : kind_t MapKey.t;
    mutable to_check : (Key.t * Errors.error_ctxt) list;
    mutable incomplete : bool;
  }


  let rec kind_to_string_impl (buf : Buffer.t) (kind : kind_t) : unit =
    match kind with
    | Any -> Buffer.add_string buf "any"
    | Alias name ->
      Buffer.add_string buf "a\"";
      Buffer.add_string buf name;
      Buffer.add_string buf "\""
    | Class typename ->
      Buffer.add_string buf "c\"";
      Buffer.add_string buf typename;
      Buffer.add_string buf "\""
    | Stream typename ->
      Buffer.add_string buf "s\"";
      Buffer.add_string buf typename;
      Buffer.add_string buf "\""
    | Null -> Buffer.add_string buf "null"
    | Bool -> Buffer.add_string buf "bool"
    | Int -> Buffer.add_string buf "int"
    | Real -> Buffer.add_string buf "real"
    | String -> Buffer.add_string buf "string"
    | Name -> Buffer.add_string buf "name"
    | Text -> Buffer.add_string buf "text"
    | Date -> Buffer.add_string buf "date"

    | BoolExact true ->
      Buffer.add_string buf "true"
    | BoolExact false ->
      Buffer.add_string buf "false"
    | IntRange (low, high) ->
      Buffer.add_string buf "(int";
      begin
        match low with
        | Some l ->
          Buffer.add_string buf " >= ";
          Buffer.add_string buf (BoundedInt.to_string l)
        | None ->
          ()
      end;
      begin
        match high with
        | Some h ->
          Buffer.add_string buf " <= ";
          Buffer.add_string buf (BoundedInt.to_string h)
        | None ->
          ()
      end;
      Buffer.add_char buf ')'
    | IntExact value ->
      Buffer.add_string buf "(int = ";
      Buffer.add_string buf (BoundedInt.to_string value);
      Buffer.add_char buf ')'
    | IntIn values ->
      Buffer.add_string buf "(int = ";
      Algo.join_buffer buf Array.fold_left (fun b x -> Buffer.add_string b (BoundedInt.to_string x)) " | " values;
      Buffer.add_char buf ')'
    | NameExact name ->
      Buffer.add_string buf "(/";
      Buffer.add_string buf name;
      Buffer.add_char buf ')'
    | NameIn names ->
      Buffer.add_char buf '(';
      Algo.join_buffer buf List.fold_left (fun b (x, _) ->
          Buffer.add_char b '/';
          Buffer.add_string b x
        ) ", " (Algo.sort_hash names);
      Buffer.add_char buf ')'

    | Array typ ->
      type_to_string_impl buf typ;
      Buffer.add_string buf "[]"
    | ArrayOrOne typ ->
      type_to_string_impl buf typ;
      Buffer.add_string buf "[?]"
    | ArraySized (typ, len) ->
      type_to_string_impl buf typ;
      Buffer.add_char buf '[';
      Buffer.add_string buf (string_of_int len);
      Buffer.add_char buf ']'
    | ArrayVariantSized (typ, lens) ->
      type_to_string_impl buf typ;
      Buffer.add_char buf '[';
      Algo.join_buffer buf Array.fold_left (fun b x -> Buffer.add_string b (string_of_int x)) ", " lens;
      Buffer.add_char buf ']'
    | ArrayTuples types ->
      Buffer.add_char buf '{';
      Algo.join_buffer buf Array.fold_left type_to_string_impl ", " types;
      Buffer.add_string buf "}[]"
    | ArrayDifferences ->
      Buffer.add_string buf "differences"
    | Tuple types ->
      Buffer.add_string buf "{{";
      Algo.join_buffer buf Array.fold_left type_to_string_impl ", " types;
      Buffer.add_string buf "}}"
    | Variant options ->
      Buffer.add_char buf '(';
      Algo.join_buffer buf List.fold_left kind_to_string_impl " | " options;
      Buffer.add_char buf ')'
    | Dictionary typ ->
      type_to_string_impl buf typ;
      Buffer.add_string buf "{}"

  and type_to_string_impl (buf : Buffer.t) (typ : t) : unit =
    kind_to_string_impl buf typ.kind;
    if not typ.allow_ind then
      Buffer.add_char buf '*'


  let kind_to_string (kind : kind_t) : string =
    let buf = Buffer.create 16 in
    kind_to_string_impl buf kind;
    Buffer.contents buf

  let type_to_string (typ : t) : string =
    let buf = Buffer.create 16 in
    type_to_string_impl buf typ;
    Buffer.contents buf


  let print_pool (pool : pool_t) : unit =
    List.iter
      (fun (typename, (entries, includes, _strict)) ->
         Printf.printf "class %s\n" typename;
         List.iter
           (fun name ->
              Printf.printf "\t#include : %s\n" name
           ) (List.sort String.compare includes);
         List.iter
           (fun (name, entry) ->
              Printf.printf "\t/%s : %s%s\n" name (type_to_string entry.typ) (if entry.optional then " [optional]" else "")
           ) (Algo.sort_hash entries)
      ) (Algo.sort_hash (fst pool));
    List.iter
      (fun (typename, kind) ->
         Printf.printf "alias %s :\n\t%s\n" typename (kind_to_string kind);
      ) (Algo.sort_hash (snd pool))


  let rec check_pool_type (pool : pool_t) (typename : string) (kind : kind_t) : unit =
    match kind with
    | Alias name ->
      if not (Hashtbl.mem (snd pool) name) then
        raise (Errors.UnexpectedError (Printf.sprintf "Undeclared alias type \"%s\", used in \"%s\"" name typename));
    | Class name
    | Stream name ->
      if not (Hashtbl.mem (fst pool) name) then
        raise (Errors.UnexpectedError (Printf.sprintf "Undeclared class type \"%s\", used in \"%s\"" name typename));
    | Array typ
    | ArraySized (typ, _)
    | ArrayVariantSized (typ, _) ->
      check_pool_type pool (typename ^ "[]") typ.kind
    | ArrayOrOne typ ->
      check_pool_type pool (typename ^ "[?]") typ.kind
    | ArrayTuples types ->
      Array.iteri
        (fun i x ->
           check_pool_type pool (Printf.sprintf "%s[{%d}]" typename i) x.kind
        ) types
    | Tuple types ->
      Array.iteri
        (fun i x ->
           check_pool_type pool (Printf.sprintf "%s{%d}" typename i) x.kind
        ) types
    | Variant options ->
      Algo.iteri List.fold_left
        (fun i x ->
           check_pool_type pool (Printf.sprintf "%s<%d>" typename i) x
        ) options
    | Dictionary typ ->
      check_pool_type pool (typename ^ "{}") typ.kind
    | Any
    | Null | Bool | Int | Real | String | Name | Text | Date | ArrayDifferences
    | BoolExact _ | IntRange _ | IntExact _ | IntIn _ | NameExact _ | NameIn _ ->
      ()


  let check_pool (pool : pool_t) : unit =
    Hashtbl.iter
      (fun typename (entries, includes, _) ->
         List.iter
           (fun name ->
              if not (Hashtbl.mem (fst pool) name) then
                raise (Errors.UnexpectedError (Printf.sprintf "Undeclared class type \"%s\", included by class \"%s\"" name typename));
           ) includes;
         Hashtbl.iter
           (fun name entry ->
              check_pool_type pool (Printf.sprintf "class %s/%s" typename name) entry.typ.kind
           ) entries
      ) (fst pool);
    Hashtbl.iter
      (fun typename kind ->
         check_pool_type pool ("alias " ^ typename) kind
      ) (snd pool)


  let create_context () : context =
    {
      pool = (Hashtbl.create 16, Hashtbl.create 16);
      types = MapKey.empty;
      to_check = [];
      incomplete = false;
    }

  (* TODO : check correctness *)
  let copy_context (ctxt : context) : context =
    {
      pool = ctxt.pool;
      types = ctxt.types;
      to_check = ctxt.to_check;
      incomplete = ctxt.incomplete;
    }

  let assign_context (dst : context) (src : context) : unit =
    dst.pool <- src.pool;
    dst.types <- src.types;
    dst.to_check <- src.to_check;
    dst.incomplete <- src.incomplete


  let register_class ?(strict=true) (pool : pool_t) (typename : string) ?(includes=[]) (members : (string * entry_t) list) : unit =
    let obj = Hashtbl.create (List.length members) in
    List.iter (fun (key, value) -> Hashtbl.add obj key value) members;
    Hashtbl.add (fst pool) typename (obj, includes, strict)

  let register_alias (pool : pool_t) (typename : string) (kind : kind_t) : unit =
    Hashtbl.add (snd pool) typename kind


  let rec remove_alias (pool : pool_t) (kind : kind_t) : kind_t =
    match kind with
    | Alias name ->
      begin
        try
          let k = Hashtbl.find (snd pool) name in
          remove_alias pool k
        with Not_found ->
          raise (Errors.UnexpectedError (Printf.sprintf "Undeclared alias type %s" name));
      end
    | _ -> kind


  let rec remove_variant (pool : pool_t) (kind : kind_t) : kind_t list =
    let noalias = remove_alias pool kind in
    match noalias with
    | Variant l ->
      List.fold_left (fun res x ->
          Algo.merge_unique res (remove_variant pool x) compare
        ) [] l
    | _ -> [noalias]


  let rec basic_type_intersection (type1 : kind_t) (type2 : kind_t) : kind_t option =
    match (type1, type2) with
    | (Any, _) -> Some type2
    | (_, Any) -> Some type1

    | (ArrayOrOne x, Array y)
    | (ArrayOrOne x, ArrayOrOne y) when x = y ->
      Some type2
    | (ArrayOrOne x, _) ->
      (* Non-array case *)
      basic_type_intersection x.kind type2
    | (_, ArrayOrOne _) ->
      (* Symetric case *)
      basic_type_intersection type2 type1

    | _ when type1 = type2 ->
      Some type1
    | _ ->
      None


  let rec type_intersection (pool : pool_t) (type1 : kind_t) (type2 : kind_t) (ectxt : Errors.error_ctxt) : kind_t =
    let l1 = remove_variant pool type1 in
    let l2 = remove_variant pool type2 in

    let l = List.fold_left
        (fun res x ->
           List.fold_left
             (fun res y ->
                match basic_type_intersection x y with
                | Some z ->
                  z::res
                | None ->
                  res
             ) res l2
        ) [] l1
    in

    match l with
    | [] ->
      let msg = Printf.sprintf "Inconsistent type inference between %s and %s" (kind_to_string type1) (kind_to_string type2) in
      raise (Errors.TypeError (msg, ectxt))
    | [x] ->
      x
    | _ ->
      Variant l

end

