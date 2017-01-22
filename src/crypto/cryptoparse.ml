(*****************************************************************************)
(*  Caradoc: a PDF parser and validator                                      *)
(*  Copyright (C) 2016-2017 Guillaume Endignoux                              *)
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


open Crypto
open Directobject
open Errors
open Boundedint

module CryptoParse = struct

  (***********************)
  (* PDF reference 7.6.5 *)
  (***********************)
  let fetch_cfm (f : string) (cf : DirectObject.dict_t) (cf_ctxt : Errors.error_ctxt) : Crypto.cipher_t =
    if f = "Identity" then
      Crypto.NONE
    else (
      let cf_dict_ctxt = Errors.ctxt_append_name cf_ctxt f in
      let cf_dict = DirectObject.get_dict
          ~default:(DirectObject.dict_create ()) () "Expected dictionary" cf_dict_ctxt
          (DirectObject.dict_find cf f)
      in

      let cfm_ctxt = Errors.ctxt_append_name cf_dict_ctxt "CFM" in
      let cfm = DirectObject.get_name
          ~default:"None" () "Expected name" cfm_ctxt
          (DirectObject.dict_find cf_dict "CFM")
      in

      match cfm with
      | "None" -> Crypto.NONE
      | "V2" -> Crypto.RC4
      | "AESV2" -> Crypto.AES_CBC
      | _ ->
        raise (Errors.PDFError ("Invalid encryption algorithm", cfm_ctxt))
    )

  let parse_crypt_filters (encrypt : DirectObject.dict_t) (ctxt : Errors.error_ctxt) : Crypto.algo4_t =
    (* /CF *)
    let cf_ctxt = Errors.ctxt_append_name ctxt "CF" in
    let cf = DirectObject.get_dict
        ~default:(DirectObject.dict_create ()) () "Expected dictionary" cf_ctxt
        (DirectObject.dict_find encrypt "CF")
    in

    (* /StmF *)
    let stmF = DirectObject.get_name
        ~default:"Identity" () "Expected name" (Errors.ctxt_append_name ctxt "StmF")
        (DirectObject.dict_find encrypt "StmF")
    in
    let stm_cipher = fetch_cfm stmF cf cf_ctxt in

    (* /StrF *)
    let strF = DirectObject.get_name
        ~default:"Identity" () "Expected name" (Errors.ctxt_append_name ctxt "StrF")
        (DirectObject.dict_find encrypt "StrF")
    in
    let str_cipher = fetch_cfm strF cf cf_ctxt in

    {
      Crypto.stm = stm_cipher;
      Crypto.str = str_cipher;
    }


  let parse_encrypt_dict (docid : string) (encrypt : DirectObject.dict_t) (ctxt : Errors.error_ctxt) : Crypto.param_t =
    (* /Filter *)
    let filter = DirectObject.get_name
        () "Expected name" (Errors.ctxt_append_name ctxt "Filter")
        (DirectObject.dict_find encrypt "Filter")
    in
    if filter <> "Standard" then
      raise (Errors.PDFError ("Only /Standard encryption filter is supported", Errors.ctxt_append_name ctxt "Filter"));

    (* /V *)
    let v = DirectObject.get_nonnegative_int
        () "Expected integer" (Errors.ctxt_append_name ctxt "V")
        (DirectObject.dict_find encrypt "V")
    in
    let algorithm =
      match BoundedInt.to_int v with
      | 1 -> Crypto.Algo1
      | 2 -> Crypto.Algo2
      | 4 -> Crypto.Algo4 (parse_crypt_filters encrypt ctxt)
      | _ ->
        raise (Errors.PDFError ("Only encryption algorithms 1, 2 and 4 are supported", Errors.ctxt_append_name ctxt "V"));
    in

    (* /Length *)
    let l = DirectObject.get_nonnegative_int
        ~default:(~:40) () "Expected integer" (Errors.ctxt_append_name ctxt "Length")
        (DirectObject.dict_find encrypt "Length")
    in
    let length = BoundedInt.to_int l in
    if length < 40 || length > 128 || length mod 8 != 0 then
      raise (Errors.PDFError ("Encryption key length must be a multiple of 8 in range [40, 128]", Errors.ctxt_append_name ctxt "Length"));

    (* /R *)
    let r = DirectObject.get_nonnegative_int
        () "Expected integer" (Errors.ctxt_append_name ctxt "R")
        (DirectObject.dict_find encrypt "R")
    in
    let rev = BoundedInt.to_int r in
    if rev < 2 || rev > 4 then
      raise (Errors.PDFError ("Only revisions 2-4 of encryption standard security handler are supported", Errors.ctxt_append_name ctxt "R"));

    (* /O *)
    let owner = DirectObject.get_string
        () "Expected string" (Errors.ctxt_append_name ctxt "O")
        (DirectObject.dict_find encrypt "O")
    in
    if String.length owner <> 32 then
      raise (Errors.PDFError ("Owner string must be 32 bytes", Errors.ctxt_append_name ctxt "O"));

    (* /U *)
    let user = DirectObject.get_string
        () "Expected string" (Errors.ctxt_append_name ctxt "U")
        (DirectObject.dict_find encrypt "U")
    in
    if String.length user <> 32 then
      raise (Errors.PDFError ("User string must be 32 bytes", Errors.ctxt_append_name ctxt "U"));

    (* /P *)
    let permissions = DirectObject.get_int
        () "Expected integer" (Errors.ctxt_append_name ctxt "P")
        (DirectObject.dict_find encrypt "P")
    in

    (* /EncryptMetadata *)
    let encrypt_meta = DirectObject.get_bool
        ~default:true () "Expected boolean" (Errors.ctxt_append_name ctxt "EncryptMetadata")
        (DirectObject.dict_find encrypt "EncryptMetadata")
    in

    {
      Crypto.algorithm = algorithm;
      Crypto.keylen_bytes = length / 8;
      Crypto.rev = rev;
      Crypto.owner = owner;
      Crypto.user = user;
      Crypto.permissions = BoundedInt.to_int permissions;
      Crypto.encrypt_meta = encrypt_meta;
      Crypto.docid = docid;
    }

end

