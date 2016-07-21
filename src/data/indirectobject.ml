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
open Pdfstream
open Entry

module IndirectObject = struct

  type t =
    | Direct of DirectObject.t
    | Stream of PDFStream.t

  type partial_t =
    | Complete of DirectObject.t
    | StreamOffset of DirectObject.dict_t * BoundedInt.t


  let to_string (x : t) : string =
    match x with
    | Direct y ->
      DirectObject.to_string y
    | Stream s ->
      PDFStream.to_string s

  let to_string_hl (x : t) (selector : Entry.select_t) : string =
    match x with
    | Direct y ->
      DirectObject.to_string_hl y selector
    | Stream s ->
      PDFStream.to_string_hl s selector


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
    | Stream s ->
      PDFStream.to_pdf s


  let find_ref (k : Key.t) (x : t) : Entry.t list =
    match x with
    | Direct y ->
      DirectObject.find_ref k y
    | Stream s ->
      DirectObject.find_ref_dict k (PDFStream.get_dict s)


  let refs (x : t) : Entry.t MapKey.t =
    match x with
    | Direct y ->
      DirectObject.refs y
    | Stream s ->
      DirectObject.refs_dict (PDFStream.get_dict s)


  let rec relink (newkeys : Key.t MapKey.t) (ctxt : Errors.error_ctxt) (x : t) : t =
    match x with
    | Direct y ->
      Direct (DirectObject.relink newkeys ctxt y)
    | Stream s ->
      let d = (DirectObject.relink_dict newkeys ctxt (PDFStream.get_dict s)) in
      Stream (PDFStream.set_dict s d)


  let simple_ref (key : Key.t) (x : t) : DirectObject.t =
    match x with
    | Direct y ->
      DirectObject.simple_ref key y
    | Stream _ ->
      DirectObject.Reference key

  let rec simplify_refs_direct (objects : t MapKey.t) (ctxt : Errors.error_ctxt) (x : DirectObject.t) : DirectObject.t =
    match x with
    | DirectObject.Reference key ->
      begin
        try
          simple_ref key (MapKey.find key objects)
        with Not_found ->
          if Params.global.Params.undefined_ref_as_null then (
            Printf.eprintf "Warning : Reference to unknown object %s%s\n" (Key.to_string key) (Errors.ctxt_to_string ctxt);
            DirectObject.Null
          ) else
            raise (Errors.PDFError (Printf.sprintf "Reference to unknown object : %s" (Key.to_string key), ctxt))
      end
    | DirectObject.Array l ->
      DirectObject.Array (List.mapi (fun i x -> simplify_refs_direct objects (Errors.ctxt_append_index ctxt i) x) l)
    | DirectObject.Dictionary d ->
      DirectObject.Dictionary (simplify_refs_dict objects ctxt d)
    | DirectObject.Null | DirectObject.Bool _ | DirectObject.Int _ | DirectObject.Real _ | DirectObject.String _ | DirectObject.Name _ ->  x

  and simplify_refs_dict (objects : t MapKey.t) (ctxt : Errors.error_ctxt) (d : DirectObject.dict_t) : DirectObject.dict_t =
    DirectObject.dict_map_key (fun key x -> simplify_refs_direct objects (Errors.ctxt_append_name ctxt key) x) d

  let simplify_refs (objects : t MapKey.t) (ctxt : Errors.error_ctxt) (x : t) : t =
    match x with
    | Direct y ->
      Direct (simplify_refs_direct objects ctxt y)
    | Stream s ->
      let d = simplify_refs_dict objects ctxt (PDFStream.get_dict s) in
      Stream (PDFStream.set_dict s d)


  let get_direct error_msg ctxt x =
    match x with
    | Direct y ->
      y
    | _ -> raise (Errors.PDFError (error_msg, ctxt))


  let get_direct_of error_msg ctxt ~transform x =
    transform error_msg ctxt (get_direct error_msg ctxt x)


  let get_stream error_msg ctxt x =
    match x with
    | Stream s ->
      s
    | _ -> raise (Errors.PDFError (error_msg, ctxt))

end

