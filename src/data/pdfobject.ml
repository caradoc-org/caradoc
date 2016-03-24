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

module PDFObject = struct

  type int_t = BoundedInt.t
  type real_t = string

  type 'a dict = (string, 'a) Hashtbl.t

  type stream_t =
    | Raw
    | Offset of BoundedInt.t
    | Content of string

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
    | Stream of t dict * string * stream_t

  type dict_t = t dict


  let dict_create () : dict_t =
    Hashtbl.create 4

  let dict_create_len (length : int) : dict_t =
    Hashtbl.create length

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
      let error_msg = Printf.sprintf "The same name appears several times in dictionary : %s" key in
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

  let dict_fold f (x : dict_t) a =
    Hashtbl.fold f x a


  let escape_string (x : string) : string =
    let buf = Buffer.create (String.length x) in
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
    done;
    Buffer.contents buf

  let escape_name (x : string) : string =
    let buf = Buffer.create (String.length x) in
    for i = 0 to (String.length x) - 1 do
      let c = x.[i] in
      match c with
    (*
    | '\x00' | '\x09' | '\x0A' | '\x0C' | '\x0D' | '\x20'
    *)
      | '(' | ')' | '<' | '>' | '[' | ']' | '{' | '}' | '/' | '%'
      | '#' ->
        Buffer.add_string buf ("#" ^ (Convert.hexa_of_char c))
      | _ when c <= '\x20' || c >= '\x7F' ->
        Buffer.add_string buf ("#" ^ (Convert.hexa_of_char c))
      | _ ->
        Buffer.add_char buf c
    done;
    Buffer.contents buf


  let rec to_string_impl (x : t) (tab : string) : string =
    match x with
    | Null -> "null"
    | Bool true -> "true"
    | Bool false -> "false"
    | Int i -> BoundedInt.to_string i
    | Real r -> r
    | String s -> "(" ^ (escape_string s) ^ ")"
    | Name n -> "/" ^ (escape_name n)
    | Array a ->
      "[" ^ (
        Algo.join_string List.fold_left (fun x -> to_string_impl x tab) " " a
      ) ^ "]"
    | Dictionary d ->
      dict_to_string_impl d tab
    | Reference key ->
      let id, gen = Key.get_obj_ref key in
      (string_of_int id) ^ " " ^ (string_of_int gen) ^ " R"
    | Stream (d, _raw, Offset off) ->
      (dict_to_string_impl d tab) ^ "\nstream " ^ (Printf.sprintf "<lex at %d [0x%x]>" (BoundedInt.to_int off) (BoundedInt.to_int off))
    | Stream (d, raw, Raw) ->
      stream_to_string_impl d raw false tab
    | Stream (d, _, Content c) ->
      stream_to_string_impl d c true tab


  and stream_to_string_impl (d : dict_t) (c : string) (decoded : bool) (tab : string) : string =
    let content =
      let expand =
        match (Params.global.Params.expand_streams, Params.global.Params.stream_limit) with
        | true, None ->
          true
        | true, (Some limit) when (String.length c) <= limit ->
          true
        | _ ->
          false
      in

      if expand then
        "\n" ^ c ^ "\nendstream\n"
      else
        ""
    in

    let header = Printf.sprintf "stream <%s stream of length %d>" (if decoded then "decoded" else "encoded") (String.length c) in
    (dict_to_string_impl d tab) ^ "\n" ^ header ^ content


  and dict_to_string_impl (d : dict_t) (tab : string) : string =
    let tabtab = tab ^ "    " in

    let entry_to_string key value =
      "\n" ^ tabtab ^ "/" ^ (escape_name key) ^ " " ^ (to_string_impl value tabtab)
    in

    let content =
      if Params.global.Params.sort_dicts then (
        Algo.join_string List.fold_left (fun (key, value) -> entry_to_string key value) "" (Algo.sort_hash d)
      ) else (
        dict_fold
          (fun key value s ->
             s ^ (entry_to_string key value)
          ) d ""
      )
    in

    "<<" ^ content ^ "\n" ^ tab ^ ">>"


  let to_string (x : t) : string =
    to_string_impl x ""

  let dict_to_string (x : dict_t) : string =
    dict_to_string_impl x ""


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
    | Dictionary _
    | Stream _ ->
      false

  let need_space_after (x : t) : bool =
    match x with
    | Null
    | Bool _
    | Int _
    | Real _
    | Name _
    | Reference _
    | Stream _ ->
      true
    | String _
    | Array _
    | Dictionary _ ->
      false


  let rec to_pdf (x : t) : string =
    match x with
    | Null -> "null"
    | Bool true -> "true"
    | Bool false -> "false"
    | Int i -> BoundedInt.to_string i
    | Real r -> r
    | String s -> "(" ^ (escape_string s) ^ ")"
    | Name n -> "/" ^ (escape_name n)
    | Array a ->
      "[" ^ (
        let s, _ =
          List.fold_left
            (fun (s, need_space) x ->
               s ^ (if need_space && (need_space_before x) then " " else "") ^ (to_pdf x),
               need_space_after x
            )
            ("", false) a in s
      ) ^ "]"
    | Dictionary d ->
      dict_to_pdf d
    | Reference key ->
      let id, gen = Key.get_obj_ref key in
      (string_of_int id) ^ " " ^ (string_of_int gen) ^ " R"
    | Stream (_, _, Offset _) ->
      raise (Errors.UnexpectedError "Undecoded stream")
    | Stream (d, raw, Raw)
    | Stream (d, raw, Content _) ->
      Printf.sprintf "%sstream\n%s\nendstream" (dict_to_pdf d) raw

  and dict_to_pdf (d : dict_t) : string =
    let content = Algo.join_string List.fold_left (fun (key, value) ->
        "/" ^ (escape_name key) ^ (if need_space_before value then " " else "") ^ (to_pdf value)
      ) "" (Algo.sort_hash d) in

    "<<" ^ content ^ ">>"


  let rec refs (x : t) : SetKey.t =
    match x with
    | Reference key ->
      SetKey.singleton key
    | Array a ->
      List.fold_left (fun s o -> SetKey.union s (refs o)) SetKey.empty a
    | Dictionary d
    | Stream (d, _, _) ->
      dict_fold (fun _ obj set -> SetKey.union set (refs obj)) d SetKey.empty
    | Null | Bool _ | Int _ | Real _ | String _ | Name _ -> SetKey.empty


  let rec relink (newkeys : Key.t MapKey.t) (indobj : Key.t) (x : t) : t =
    match x with
    | Null
    | Bool _
    | Int _
    | Real _
    | String _
    | Name _ ->
      x
    | Array l ->
      Array (List.map (relink newkeys indobj) l)
    | Dictionary d ->
      Dictionary (relink_dict newkeys indobj d)
    | Reference key ->
      begin
        try
          Reference (MapKey.find key newkeys)
        with Not_found ->
          if Params.global.Params.undefined_ref_as_null then (
            Printf.eprintf "Warning : Reference to unknown object %s in object %s\n" (Key.to_string key) (Key.to_string indobj);
            Null
          ) else
            raise (Errors.PDFError (Printf.sprintf "Reference to unknown object : %s" (Key.to_string key), Errors.make_ctxt_key indobj))
      end
    | Stream (d, raw, s) ->
      Stream (relink_dict newkeys indobj d, raw, s)

  and relink_dict (newkeys : Key.t MapKey.t) (indobj : Key.t) (d : dict_t) : dict_t =
    let dd = dict_create_len (dict_length d) in
    dict_iter (fun name x ->
        dict_set dd (name, (relink newkeys indobj x))
      ) d;
    dd


  let simple_ref (key : Key.t) (x : t) : t =
    match x with
    | Null | Bool _ | Int _ | Real _ | String _ | Name _ -> x
    | Array _ | Dictionary _ | Reference _ | Stream _    -> Reference key

  let rec simplify_refs (objects : t MapKey.t) (indobj : Key.t) (x : t) : t =
    match x with
    | Reference key ->
      begin
        try
          simple_ref key (MapKey.find key objects)
        with Not_found ->
          if Params.global.Params.undefined_ref_as_null then (
            Printf.eprintf "Warning : Reference to unknown object %s in object %s\n" (Key.to_string key) (Key.to_string indobj);
            Null
          ) else
            raise (Errors.PDFError (Printf.sprintf "Reference to unknown object : %s" (Key.to_string key), Errors.make_ctxt_key indobj))
      end
    | Array l ->
      Array (List.map (simplify_refs objects indobj) l)
    | Dictionary d ->
      Dictionary (dict_simplify_refs objects indobj d)
    | Stream (d, raw, s) ->
      Stream (dict_simplify_refs objects indobj d, raw, s)
    | Null | Bool _ | Int _ | Real _ | String _ | Name _ ->  x

  and dict_simplify_refs (objects : t MapKey.t) (indobj : Key.t) (d : dict_t) : dict_t =
    let dd = dict_create_len (dict_length d) in
    dict_iter (fun name o ->
        dict_set dd (name, (simplify_refs objects indobj o))
      ) d;
    dd


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


  let get_stream_offset error_msg ctxt x =
    match x with
    | Stream (stream_dict, _, Offset stream_offset) ->
      (stream_dict, stream_offset)
    | _ -> raise (Errors.PDFError (error_msg, ctxt))


  let get_stream_content error_msg ctxt x =
    match x with
    | Stream (stream_dict, _, Content stream_content) ->
      (stream_dict, stream_content)
    | _ -> raise (Errors.PDFError (error_msg, ctxt))


  let get_array ?default ?(accept_one=false) ?length () error_msg ctxt x =
    let a = (
      match x, default with
      | (Array l), _
      | Null, (Some l) ->
        Array.of_list l
      | _ ->
        if accept_one then
          Array.make 1 x
        else
          raise (Errors.PDFError (error_msg, ctxt))
    ) in

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

