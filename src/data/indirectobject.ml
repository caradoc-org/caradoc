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


  let make_fun (fun_direct : DirectObject.t -> 'a) (fun_stream : PDFStream.t -> 'a) (x : t) : 'a =
    match x with
    | Direct y ->
      fun_direct y
    | Stream s ->
      fun_stream s

  let make_fun_dict (fun_direct : DirectObject.t -> 'a) (fun_dict : DirectObject.dict_t -> 'a) : t -> 'a =
    make_fun fun_direct (fun s -> fun_dict (PDFStream.get_dict s))


  let to_string : t -> string =
    make_fun DirectObject.to_string PDFStream.to_string

  let to_string_hl : t -> Entry.select_t -> string =
    make_fun DirectObject.to_string_hl PDFStream.to_string_hl


  let need_space_before : t -> bool =
    make_fun DirectObject.need_space_before (fun _ -> false)

  let need_space_after : t -> bool =
    make_fun DirectObject.need_space_after (fun _ -> true)


  let to_pdf : t -> string =
    make_fun DirectObject.to_pdf PDFStream.to_pdf


  let find_ref (k : Key.t) : t -> Entry.t list =
    make_fun_dict (DirectObject.find_ref k) (DirectObject.find_ref_dict k)

  let find_name (n : string) : t -> Entry.t list =
    make_fun_dict (DirectObject.find_name n) (DirectObject.find_name_dict n)


  let refs : t -> Entry.t MapKey.t =
    make_fun_dict DirectObject.refs DirectObject.refs_dict


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

