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
open Directobject
open Params
open Setkey
open Mapkey
open Errors

module IndirectObject = struct

  type stream_t =
    | Raw
    | Content of string

  type t =
    | Direct of DirectObject.t
    | Stream of DirectObject.dict_t * string * stream_t

  type partial_t =
    | Complete of DirectObject.t
    | StreamOffset of DirectObject.dict_t * BoundedInt.t


  let stream_to_string (d : DirectObject.dict_t) (c : string) (decoded : bool) : string =
    let buf = Buffer.create 16 in

    let expand =
      match (Params.global.Params.expand_streams, Params.global.Params.stream_limit) with
      | true, None ->
        true
      | true, (Some limit) when (String.length c) <= limit ->
        true
      | _ ->
        false
    in

    let header = Printf.sprintf "stream <%s stream of length %d>" (if decoded then "decoded" else "encoded") (String.length c) in
    DirectObject.dict_to_string_buf buf d;
    Buffer.add_char buf '\n';
    Buffer.add_string buf header;
    if expand then (
      Buffer.add_char buf '\n';
      Buffer.add_string buf c;
      Buffer.add_string buf "\nendstream\n"
    );

    Buffer.contents buf


  let to_string (x : t) : string =
    match x with
    | Direct y ->
      DirectObject.to_string y
    | Stream (d, raw, Raw) ->
      stream_to_string d raw false
    | Stream (d, _, Content c) ->
      stream_to_string d c true


  let need_space_before (x : t) : bool =
    match x with
    | Direct y ->
      DirectObject.need_space_before y
    | Stream _ ->
      false

  let need_space_after (x : t) : bool =
    match x with
    | Direct y ->
      DirectObject.need_space_after y
    | Stream _ ->
      true


  let to_pdf (x : t) : string =
    match x with
    | Direct y ->
      DirectObject.to_pdf y
    | Stream (d, raw, _) ->
      let buf = Buffer.create 16 in
      DirectObject.dict_to_pdf_buf buf d;
      Buffer.add_string buf "stream\n";
      Buffer.add_string buf raw;
      Buffer.add_string buf "\nendstream";
      Buffer.contents buf


  let refs (x : t) : SetKey.t =
    match x with
    | Direct y ->
      DirectObject.refs y
    | Stream (d, _, _) ->
      DirectObject.refs_dict d


  let rec relink (newkeys : Key.t MapKey.t) (indobj : Key.t) (x : t) : t =
    match x with
    | Direct y ->
      Direct (DirectObject.relink newkeys indobj y)
    | Stream (d, raw, s) ->
      Stream (DirectObject.relink_dict newkeys indobj d, raw, s)


  let simple_ref (key : Key.t) (x : t) : DirectObject.t =
    match x with
    | Direct y ->
      DirectObject.simple_ref key y
    | Stream _ ->
      DirectObject.Reference key

  let rec simplify_refs (objects : t MapKey.t) (indobj : Key.t) (x : t) : t =
    match x with
    | Direct y ->
      Direct (simplify_refs_direct objects indobj y)
    | Stream (d, raw, s) ->
      Stream (simplify_refs_dict objects indobj d, raw, s)

  and simplify_refs_direct (objects : t MapKey.t) (indobj : Key.t) (x : DirectObject.t) : DirectObject.t =
    match x with
    | DirectObject.Reference key ->
      begin
        try
          simple_ref key (MapKey.find key objects)
        with Not_found ->
          if Params.global.Params.undefined_ref_as_null then (
            Printf.eprintf "Warning : Reference to unknown object %s in object %s\n" (Key.to_string key) (Key.to_string indobj);
            DirectObject.Null
          ) else
            raise (Errors.PDFError (Printf.sprintf "Reference to unknown object : %s" (Key.to_string key), Errors.make_ctxt_key indobj))
      end
    | DirectObject.Array l ->
      DirectObject.Array (List.map (simplify_refs_direct objects indobj) l)
    | DirectObject.Dictionary d ->
      DirectObject.Dictionary (simplify_refs_dict objects indobj d)
    | DirectObject.Null | DirectObject.Bool _ | DirectObject.Int _ | DirectObject.Real _ | DirectObject.String _ | DirectObject.Name _ ->  x

  and simplify_refs_dict (objects : t MapKey.t) (indobj : Key.t) (d : DirectObject.dict_t) : DirectObject.dict_t =
    let dd = DirectObject.dict_create_len (DirectObject.dict_length d) in
    DirectObject.dict_iter (fun name o ->
        DirectObject.dict_set dd (name, (simplify_refs_direct objects indobj o))
      ) d;
    dd


  let get_direct error_msg ctxt x =
    match x with
    | Direct y ->
      y
    | _ -> raise (Errors.PDFError (error_msg, ctxt))


  let get_direct_of error_msg ctxt ~transform x =
    transform error_msg ctxt (get_direct error_msg ctxt x)


  let get_stream_content error_msg ctxt x =
    match x with
    | Stream (stream_dict, _, Content stream_content) ->
      (stream_dict, stream_content)
    | _ -> raise (Errors.PDFError (error_msg, ctxt))

end

