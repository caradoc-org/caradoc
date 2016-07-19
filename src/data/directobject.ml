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


open Boundedint
open Setkey
open Mapkey
open Errors
open Algo
open Convert
open Params
open Entry

module DirectObject = struct

  type int_t = BoundedInt.t
  type real_t = string

  type 'a dict = (string, 'a) Hashtbl.t

  type t =
    | Null
    | Bool of bool
    | Int of int_t
    | Real of real_t
    | String of string
    | Name of string
    | Array of t list
    | Dictionary of t dict
    | Reference of Key.t

  type dict_t = t dict


  let dict_create () : dict_t =
    Hashtbl.create 4

  let dict_create_len (length : int) : dict_t =
    Hashtbl.create length

  let dict_copy (x : dict_t) : dict_t =
    Hashtbl.copy x

  let dict_length (x : dict_t) : int =
    Hashtbl.length x

  let dict_singleton (key, value) : dict_t =
    let x = dict_create () in
    Hashtbl.add x key value;
    x

  let dict_set (x : dict_t) (key, value) : unit =
    if value = Null then
      Hashtbl.remove x key
    else
      Hashtbl.replace x key value

  let dict_add (allow_duplicates : bool) (x : dict_t) (key, value) : unit =
    if Hashtbl.mem x key then (
      let error_msg = Printf.sprintf "The same name appears several times in dictionary : /%s" key in
      if allow_duplicates then (
        Printf.eprintf "Warning : %s\n" error_msg;
        dict_set x (key, value)
      ) else
        raise (Errors.PDFError (error_msg, Errors.ctxt_none))
    ) else
      dict_set x (key, value)


  let dict_mem (x : dict_t) (key : string) : bool =
    Hashtbl.mem x key

  let dict_find (x : dict_t) (key : string) : t =
    try
      Hashtbl.find x key
    with Not_found ->
      Null

  let dict_iter f (x : dict_t) : unit =
    Hashtbl.iter f x

  let dict_map_key f (x : dict_t) : dict_t =
    let y = dict_create_len (dict_length x) in
    dict_iter (fun key value ->
        dict_set y (key, f key value)
      ) x;
    y

  let dict_map f (x : dict_t) : dict_t =
    dict_map_key (fun _key value -> f value) x

  let dict_fold f (x : dict_t) a =
    Hashtbl.fold f x a


  let escape_string (buf : Buffer.t) (x : string) : unit =
    for i = 0 to (String.length x) - 1 do
      match x.[i] with
      | '\x0A' -> Buffer.add_string buf "\\n"
      | '\x0D' -> Buffer.add_string buf "\\r"
      | '\x09' -> Buffer.add_string buf "\\t"
      | '\x08' -> Buffer.add_string buf "\\b"
      | '\x0C' -> Buffer.add_string buf "\\f"
      | '(' -> Buffer.add_string buf "\\("
      | ')' -> Buffer.add_string buf "\\)"
      | '\\' -> Buffer.add_string buf "\\\\"
      | _ as c -> Buffer.add_char buf c
    done

  let escape_name (buf : Buffer.t) (x : string) : unit =
    for i = 0 to (String.length x) - 1 do
      let c = x.[i] in
      match c with
    (*
    | '\x00' | '\x09' | '\x0A' | '\x0C' | '\x0D' | '\x20'
    *)
      | '(' | ')' | '<' | '>' | '[' | ']' | '{' | '}' | '/' | '%'
      | '#' ->
        Buffer.add_char buf '#';
        Buffer.add_string buf (Convert.hexa_of_char c)
      | _ when c <= '\x20' || c >= '\x7F' ->
        Buffer.add_char buf '#';
        Buffer.add_string buf (Convert.hexa_of_char c)
      | _ ->
        Buffer.add_char buf c
    done


  let rec to_string_impl (tab : string) (buf : Buffer.t) (x : t) : unit =
    match x with
    | Null -> Buffer.add_string buf "null"
    | Bool true -> Buffer.add_string buf "true"
    | Bool false -> Buffer.add_string buf "false"
    | Int i -> Buffer.add_string buf (BoundedInt.to_string i)
    | Real r -> Buffer.add_string buf r
    | String s ->
      Buffer.add_char buf '(';
      escape_string buf s;
      Buffer.add_char buf ')'
    | Name n ->
      Buffer.add_char buf '/';
      escape_name buf n
    | Array a ->
      Buffer.add_char buf '[';
      Algo.join_buffer buf List.fold_left (to_string_impl tab) " " a;
      Buffer.add_char buf ']'
    | Dictionary d ->
      dict_to_string_impl tab buf d
    | Reference key ->
      let id, gen = Key.get_obj_ref key in
      Buffer.add_string buf (string_of_int id);
      Buffer.add_char buf ' ';
      Buffer.add_string buf (string_of_int gen);
      Buffer.add_string buf " R"


  and dict_to_string_impl (tab : string) (buf : Buffer.t) (d : dict_t) : unit =
    let tabtab = tab ^ "    " in

    let entry_to_string key value =
      Buffer.add_char buf '\n';
      Buffer.add_string buf tabtab;
      Buffer.add_char buf '/';
      escape_name buf key;
      Buffer.add_char buf ' ';
      to_string_impl tabtab buf value
    in

    let content_to_buf () =
      if Params.global.Params.sort_dicts then (
        List.iter (fun (key, value) -> entry_to_string key value) (Algo.sort_hash d)
      ) else (
        dict_iter (fun key value -> entry_to_string key value) d
      )
    in

    Buffer.add_string buf "<<";
    content_to_buf ();
    Buffer.add_char buf '\n';
    Buffer.add_string buf tab;
    Buffer.add_string buf ">>"


  let to_string (x : t) : string =
    let buf = Buffer.create 16 in
    to_string_impl "" buf x;
    Buffer.contents buf

  let dict_to_string_buf (buf : Buffer.t) (x : dict_t) : unit =
    dict_to_string_impl "" buf x

  let dict_to_string (x : dict_t) : string =
    let buf = Buffer.create 16 in
    dict_to_string_buf buf x;
    Buffer.contents buf


  let need_space_before (x : t) : bool =
    match x with
    | Null
    | Bool _
    | Int _
    | Real _
    | Reference _ ->
      true
    | String _
    | Name _
    | Array _
    | Dictionary _ ->
      false

  let need_space_after (x : t) : bool =
    match x with
    | Null
    | Bool _
    | Int _
    | Real _
    | Name _
    | Reference _ ->
      true
    | String _
    | Array _
    | Dictionary _ ->
      false


  let rec to_pdf_impl (buf : Buffer.t) (x : t) : unit =
    match x with
    | Null -> Buffer.add_string buf "null"
    | Bool true -> Buffer.add_string buf "true"
    | Bool false -> Buffer.add_string buf "false"
    | Int i -> Buffer.add_string buf (BoundedInt.to_string i)
    | Real r -> Buffer.add_string buf r
    | String s ->
      Buffer.add_char buf '(';
      escape_string buf s;
      Buffer.add_char buf ')'
    | Name n ->
      Buffer.add_char buf '/';
      escape_name buf n
    | Array a ->
      Buffer.add_char buf '[';
      let (_:bool) = List.fold_left
          (fun need_space x ->
             if need_space && (need_space_before x) then
               Buffer.add_char buf ' ';
             to_pdf_impl buf x;
             need_space_after x
          ) false a in
      Buffer.add_char buf ']'
    | Dictionary d ->
      dict_to_pdf_buf buf d
    | Reference key ->
      let id, gen = Key.get_obj_ref key in
      Buffer.add_string buf (string_of_int id);
      Buffer.add_char buf ' ';
      Buffer.add_string buf (string_of_int gen);
      Buffer.add_string buf " R"

  and dict_to_pdf_buf (buf : Buffer.t) (d : dict_t) : unit =
    let content_to_buf () =
      List.iter (fun (key, value) ->
          Buffer.add_char buf '/';
          escape_name buf key;
          if need_space_before value then
            Buffer.add_char buf ' ';
          to_pdf_impl buf value
        ) (Algo.sort_hash d)
    in

    Buffer.add_string buf "<<";
    content_to_buf ();
    Buffer.add_string buf ">>"


  let to_pdf (x : t) : string =
    let buf = Buffer.create 16 in
    to_pdf_impl buf x;
    Buffer.contents buf

  let dict_to_pdf (x : dict_t) : string =
    let buf = Buffer.create 16 in
    dict_to_pdf_buf buf x;
    Buffer.contents buf



  let rec refs_impl (entry : Entry.t) (x : t) : Entry.t MapKey.t =
    match x with
    | Reference key ->
      MapKey.singleton key entry
    | Array a ->
      Algo.fold_lefti List.fold_left (fun i s o ->
          Algo.mapkey_union s (refs_impl (Entry.append_index entry i) o)
        ) MapKey.empty a
    | Dictionary d ->
      refs_dict_impl entry d
    | Null | Bool _ | Int _ | Real _ | String _ | Name _ -> MapKey.empty

  and refs_dict_impl (entry : Entry.t) (d : dict_t) : Entry.t MapKey.t =
    dict_fold (fun key obj set ->
        Algo.mapkey_union set (refs_impl (Entry.append_name entry key) obj)
      ) d MapKey.empty


  let refs (x : t) : Entry.t MapKey.t =
    refs_impl Entry.empty x

  let refs_dict (d : dict_t) : Entry.t MapKey.t =
    refs_dict_impl Entry.empty d


  let rec relink (newkeys : Key.t MapKey.t) (ctxt : Errors.error_ctxt) (x : t) : t =
    match x with
    | Null
    | Bool _
    | Int _
    | Real _
    | String _
    | Name _ ->
      x
    | Array l ->
      Array (List.mapi (fun i x -> relink newkeys (Errors.ctxt_append_index ctxt i) x) l)
    | Dictionary d ->
      Dictionary (relink_dict newkeys ctxt d)
    | Reference key ->
      begin
        try
          Reference (MapKey.find key newkeys)
        with Not_found ->
          if Params.global.Params.undefined_ref_as_null then (
            Printf.eprintf "Warning : Reference to unknown object %s%s\n" (Key.to_string key) (Errors.ctxt_to_string ctxt);
            Null
          ) else
            raise (Errors.PDFError (Printf.sprintf "Reference to unknown object : %s" (Key.to_string key), ctxt))
      end

  and relink_dict (newkeys : Key.t MapKey.t) (ctxt : Errors.error_ctxt) (d : dict_t) : dict_t =
    dict_map_key (fun key x -> relink newkeys (Errors.ctxt_append_name ctxt key) x) d


  let simple_ref (key : Key.t) (x : t) : t =
    match x with
    | Null | Bool _ | Int _ | Real _ | String _ | Name _ -> x
    | Array _ | Dictionary _ | Reference _               -> Reference key


  let apply_not_null x fn =
    if x <> Null then fn x


  let get_positive_int ?default () error_msg ctxt x =
    match x, default with
    | (Int i), _
    | Null, (Some i)
      when i >: ~:0 ->
      i
    | _ -> raise (Errors.PDFError (error_msg, ctxt))


  let get_nonnegative_int ?default () error_msg ctxt x =
    match x, default with
    | (Int i), _
    | Null, (Some i)
      when i >=: ~:0 ->
      i
    | _ -> raise (Errors.PDFError (error_msg, ctxt))


  let get_name ?default () error_msg ctxt x =
    match x, default with
    | (Name n), _
    | Null, (Some n) ->
      n
    | _ -> raise (Errors.PDFError (error_msg, ctxt))


  let get_reference ?default () error_msg ctxt x =
    match x, default with
    | (Reference k), _
    | Null, (Some k) ->
      k
    | _ -> raise (Errors.PDFError (error_msg, ctxt))


  let get_dict ?default () error_msg ctxt x =
    match x, default with
    | (Dictionary d), _
    | Null, (Some d) ->
      d
    | _ -> raise (Errors.PDFError (error_msg, ctxt))


  let get_array ?default ?(accept_one=false) ?length () error_msg ctxt x =
    let a =
      match x, default with
      | (Array l), _
      | Null, (Some l) ->
        Array.of_list l
      | _ ->
        if accept_one then
          Array.make 1 x
        else
          raise (Errors.PDFError (error_msg, ctxt))
    in

    begin
      match length with
      | Some n ->
        if (Array.length a) <> n then
          raise (Errors.PDFError (error_msg, ctxt))
      | None -> ()
    end;

    a

  let get_array_of ?default ?(accept_one=false) ?length () error_msg ctxt ~transform x =
    let a = get_array ?default ~accept_one ?length () error_msg ctxt x in
    Array.map (transform error_msg ctxt) a


end

