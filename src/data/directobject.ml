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


open Boundedint
open Setkey
open Mapkey
open Errors
open Algo
open Params
open Entry
open Console
open Crypto

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
    if Hashtbl.mem x key then
      Errors.warning_or_pdf_error allow_duplicates (Printf.sprintf "The same name appears several times in dictionary : /%s" key) Errors.ctxt_none;
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

  let dict_iter_sorted f (x : dict_t) : unit =
    List.iter (fun (key, value) -> f key value) (Algo.sort_hash x)

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

  let dict_simplify (keys : string list) (x : dict_t) : dict_t =
    let d = dict_create () in
    List.iter (fun k ->
        let v = dict_find x k in
        dict_set d (k, v);
      ) keys;
    d


  let is_array (x : t) : bool =
    match x with
    | Array _ -> true
    | _ -> false


  let rec decrypt (f : Crypto.decrypt_t) (x : t) : t =
    match x with
    | Null | Bool _ | Int _ | Real _ | Name _ | Reference _ ->
      x
    | String s ->
      String (f s)
    | Array a ->
      Array (List.map (decrypt f) a)
    | Dictionary d ->
      Dictionary (decrypt_dict f d)

  and decrypt_dict (f : Crypto.decrypt_t) (d : dict_t) : dict_t =
    dict_map (decrypt f) d


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

  let pretty_string (buf : Buffer.t) (x : string) : unit =
    let l = String.length x in

    let printable =
      try
        for i = 0 to l-1 do
          match x.[i] with
          | '\x0A' | '\x0D' | '\x09' | '\x08' | '\x0C'
          | '\x20'..'\x7E' ->
            ()
          | _ ->
            raise Exit
        done;
        true
      with Exit ->
        false
    in

    if printable then (
      Buffer.add_char buf '(';
      escape_string buf x;
      Buffer.add_char buf ')'
    ) else (
      Buffer.add_char buf '<';
      for i = 0 to l-1 do
        Convert.hexa_of_char_buf buf x.[i]
      done;
      Buffer.add_char buf '>'
    )

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
        Convert.hexa_of_char_buf buf c
      | '\x21'..'\x7E' ->
        Buffer.add_char buf c
      | _ ->
        Buffer.add_char buf '#';
        Convert.hexa_of_char_buf buf c
    done


  let rec to_string_impl (selector : Entry.select_t) (tab : string) (buf : Buffer.t) (x : t) : unit =
    if Entry.is_selected selector then (
      Buffer.add_string buf Console.highlight;
      to_string_core selector tab buf x;
      Buffer.add_string buf Console.reset
    ) else
      to_string_core selector tab buf x

  and to_string_core (selector : Entry.select_t) (tab : string) (buf : Buffer.t) (x : t) : unit =
    match x with
    | Null -> Buffer.add_string buf "null"
    | Bool true -> Buffer.add_string buf "true"
    | Bool false -> Buffer.add_string buf "false"
    | Int i -> Buffer.add_string buf (BoundedInt.to_string i)
    | Real r -> Buffer.add_string buf r
    | String s ->
      pretty_string buf s
    | Name n ->
      Buffer.add_char buf '/';
      escape_name buf n
    | Array a ->
      Buffer.add_char buf '[';
      Algo.join_bufferi buf List.fold_left (fun i -> to_string_impl (Entry.move_to_index selector i) tab) " " a;
      Buffer.add_char buf ']'
    | Dictionary d ->
      dict_to_string_impl selector tab buf d
    | Reference key ->
      let id, gen = Key.get_obj_ref key in
      Buffer.add_string buf (string_of_int id);
      Buffer.add_char buf ' ';
      Buffer.add_string buf (string_of_int gen);
      Buffer.add_string buf " R"

  and dict_to_string_impl (selector : Entry.select_t) (tab : string) (buf : Buffer.t) (d : dict_t) : unit =
    let tabtab = tab ^ "    " in

    let entry_to_string key value =
      let key_selected = Entry.is_selected (Entry.move_to_name_key selector key) in

      Buffer.add_char buf '\n';
      Buffer.add_string buf tabtab;

      if key_selected then
        Buffer.add_string buf Console.highlight;
      Buffer.add_char buf '/';
      escape_name buf key;
      if key_selected then
        Buffer.add_string buf Console.reset;

      Buffer.add_char buf ' ';
      to_string_impl (Entry.move_to_name selector key) tabtab buf value
    in

    let content_to_buf () =
      if Params.global.Params.sort_dicts then
        dict_iter_sorted entry_to_string d
      else
        dict_iter entry_to_string d
    in

    Buffer.add_string buf "<<";
    content_to_buf ();
    Buffer.add_char buf '\n';
    Buffer.add_string buf tab;
    Buffer.add_string buf ">>"


  let to_string (x : t) : string =
    let buf = Buffer.create 16 in
    to_string_impl Entry.no_selector "" buf x;
    Buffer.contents buf

  let dict_to_string_buf (buf : Buffer.t) (x : dict_t) (selector : Entry.select_t) : unit =
    dict_to_string_impl selector "" buf x

  let dict_to_string (x : dict_t) : string =
    let buf = Buffer.create 16 in
    dict_to_string_buf buf x Entry.no_selector;
    Buffer.contents buf


  let to_string_hl (x : t) (selector : Entry.select_t) : string =
    let buf = Buffer.create 16 in
    to_string_impl selector "" buf x;
    Buffer.contents buf

  let dict_to_string_hl (x : dict_t) (selector : Entry.select_t) : string =
    let buf = Buffer.create 16 in
    dict_to_string_buf buf x selector;
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


  let find (find_impl : 'a -> Entry.t list ref -> Entry.t -> t -> unit) (what : 'a) (x : t) : Entry.t list =
    let result = ref [] in
    find_impl what result Entry.empty x;
    List.rev !result

  let find_dict (find_dict_impl : 'a -> Entry.t list ref -> Entry.t -> dict_t -> unit) (what : 'a) (d : dict_t) : Entry.t list =
    let result = ref [] in
    find_dict_impl what result Entry.empty d;
    List.rev !result


  let rec find_ref_impl (k : Key.t) (l : Entry.t list ref) (entry : Entry.t) (x : t) : unit =
    match x with
    | Reference key ->
      if key = k then
        l := entry::(!l)
    | Array a ->
      Algo.iteri List.fold_left (fun i o ->
          find_ref_impl k l (Entry.append_index entry i) o
        ) a
    | Dictionary d ->
      find_ref_dict_impl k l entry d
    | Null | Bool _ | Int _ | Real _ | String _ | Name _ -> ()

  and find_ref_dict_impl (k : Key.t) (l : Entry.t list ref) (entry : Entry.t) (d : dict_t) : unit =
    dict_iter_sorted (fun key obj ->
        find_ref_impl k l (Entry.append_name entry key) obj
      ) d


  let rec find_name_impl (n : string) (l : Entry.t list ref) (entry : Entry.t) (x : t) : unit =
    match x with
    | Name name ->
      if name = n then
        l := entry::(!l)
    | Array a ->
      Algo.iteri List.fold_left (fun i o ->
          find_name_impl n l (Entry.append_index entry i) o
        ) a
    | Dictionary d ->
      find_name_dict_impl n l entry d
    | Null | Bool _ | Int _ | Real _ | String _ | Reference _ -> ()

  and find_name_dict_impl (n : string) (l : Entry.t list ref) (entry : Entry.t) (d : dict_t) : unit =
    dict_iter_sorted (fun key obj ->
        if key = n then
          l := (Entry.append_name_key entry key)::(!l);
        find_name_impl n l (Entry.append_name entry key) obj
      ) d


  let find_ref : Key.t -> t -> Entry.t list =
    find find_ref_impl

  let find_ref_dict : Key.t -> dict_t -> Entry.t list =
    find_dict find_ref_dict_impl

  let find_name : string -> t -> Entry.t list =
    find find_name_impl

  let find_name_dict : string -> dict_t -> Entry.t list =
    find_dict find_name_dict_impl


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


  let rec undef_refs_to_null (defined : 'a MapKey.t) (warnings : (Key.t * Errors.error_ctxt) list ref) (ctxt : Errors.error_ctxt) (x : t) : t =
    match x with
    | Null | Bool _ | Int _ | Real _ | String _ | Name _ ->
      x
    | Array l ->
      Array (List.mapi (fun i x -> undef_refs_to_null defined warnings (Errors.ctxt_append_index ctxt i) x) l)
    | Dictionary d ->
      Dictionary (undef_refs_to_null_dict defined warnings ctxt d)
    | Reference key ->
      if MapKey.mem key defined then
        Reference key
      else (
        warnings := (key, ctxt)::(!warnings);
        Null
      )

  and undef_refs_to_null_dict (defined : 'a MapKey.t) (warnings : (Key.t * Errors.error_ctxt) list ref) (ctxt : Errors.error_ctxt) (d : dict_t) : dict_t =
    dict_map_key (fun key x -> undef_refs_to_null defined warnings (Errors.ctxt_append_name ctxt key) x) d


  let rec relink (newkeys : Key.t MapKey.t) (ctxt : Errors.error_ctxt) (x : t) : t =
    match x with
    | Null | Bool _ | Int _ | Real _ | String _ | Name _ ->
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


  let get_bool ?default () error_msg ctxt x =
    match x, default with
    | (Bool b), _
    | Null, (Some b) ->
      b
    | _ -> raise (Errors.PDFError (error_msg, ctxt))


  let get_int ?default () error_msg ctxt x =
    match x, default with
    | (Int i), _
    | Null, (Some i) ->
      i
    | _ -> raise (Errors.PDFError (error_msg, ctxt))


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


  let get_string ?default () error_msg ctxt x =
    match x, default with
    | (String s), _
    | Null, (Some s) ->
      s
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

