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
open Directobject
open Params
open Setkey
open Mapkey
open Errors
open Pdfstream
open Entry
open Crypto
open Algo

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

  let make_map (fun_direct : DirectObject.t -> DirectObject.t) (fun_stream : PDFStream.t -> PDFStream.t) (x : t) : t =
    match x with
    | Direct y ->
      Direct (fun_direct y)
    | Stream s ->
      Stream (fun_stream s)

  let make_map_dict (fun_direct : DirectObject.t -> DirectObject.t) (fun_dict : DirectObject.dict_t -> DirectObject.dict_t) : t -> t =
    make_map fun_direct (fun s -> PDFStream.set_dict s (fun_dict (PDFStream.get_dict s)))


  let decrypt (crypto : Crypto.t) (key : Key.t) : t -> t =
    make_map (DirectObject.decrypt (Crypto.decrypt_for_object crypto true key)) (PDFStream.decrypt crypto key)


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


  let undef_refs_to_null (defined : 'a MapKey.t) (warnings : (Key.t * Errors.error_ctxt) list ref) (ctxt : Errors.error_ctxt) : t -> t =
    make_map_dict (DirectObject.undef_refs_to_null defined warnings ctxt) (DirectObject.undef_refs_to_null_dict defined warnings ctxt)


  let relink (newkeys : Key.t MapKey.t) (ctxt : Errors.error_ctxt) : t -> t =
    make_map_dict (DirectObject.relink newkeys ctxt) (DirectObject.relink_dict newkeys ctxt)


  let simple_ref (key : Key.t) : t -> DirectObject.t =
    make_fun (DirectObject.simple_ref key) (fun _ -> DirectObject.Reference key)

  let rec simplify_refs_direct (objects : t MapKey.t) (ctxt : Errors.error_ctxt) (x : DirectObject.t) : DirectObject.t =
    match x with
    | DirectObject.Reference key ->
      begin
        try
          simple_ref key (MapKey.find key objects)
        with Not_found ->
          raise (Errors.PDFError (Printf.sprintf "Reference to unknown object : %s" (Key.to_string key), ctxt))
      end
    | DirectObject.Array l ->
      DirectObject.Array (List.mapi (fun i x -> simplify_refs_direct objects (Errors.ctxt_append_index ctxt i) x) l)
    | DirectObject.Dictionary d ->
      DirectObject.Dictionary (simplify_refs_dict objects ctxt d)
    | DirectObject.Null | DirectObject.Bool _ | DirectObject.Int _ | DirectObject.Real _ | DirectObject.String _ | DirectObject.Name _ ->  x

  and simplify_refs_dict (objects : t MapKey.t) (ctxt : Errors.error_ctxt) (d : DirectObject.dict_t) : DirectObject.dict_t =
    DirectObject.dict_map_key (fun key x ->
        if Params.global.Params.remove_ptex && Algo.string_starts_with key "PTEX." then (
          Errors.warning "Removing PTEX entry" (Errors.ctxt_append_name ctxt key);
          DirectObject.Null
        ) else
          simplify_refs_direct objects (Errors.ctxt_append_name ctxt key) x
      ) d

  let simplify_refs (objects : t MapKey.t) (ctxt : Errors.error_ctxt) : t -> t =
    make_map_dict (simplify_refs_direct objects ctxt) (simplify_refs_dict objects ctxt)


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


  let get_dict x =
    match x with
    | Direct (DirectObject.Dictionary d) ->
      Some d
    | Stream s ->
      Some (PDFStream.get_dict s)
    | _ ->
      None

end

