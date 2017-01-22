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


open Errors
open Rc4
open Md5
open Aes
open Params

module Crypto = struct

  type cipher_t = NONE | RC4 | AES_CBC

  type algo4_t = {
    stm : cipher_t;
    str : cipher_t;
  }

  type algo_t = Algo1 | Algo2 | Algo4 of algo4_t

  type param_t = {
    algorithm : algo_t;
    keylen_bytes : int;
    rev : int;
    owner : string;
    user : string;
    permissions : int;
    encrypt_meta : bool;
    docid : string;
  }

  type pass_t = {p : string}
  type key_t = {k : string}

  type t = {
    params : param_t;
    filekey : key_t;
    encrypt_dict : Key.t option;
  }

  type decrypt_t = string -> string


  let encrypt_meta (crypto : t) : bool =
    crypto.params.encrypt_meta


  (***********************)
  (* PDF reference 7.6.2 *)
  (***********************)
  let get_cipher (crypto : t) (is_string : bool) (key : Key.t) : cipher_t =
    match crypto.encrypt_dict with
    | Some k when k = key ->
      NONE
    | _ ->
      begin
        match crypto.params.algorithm with
        (* TODO : The spec is not very clear, assume RC4 for these *)
        | Algo1 | Algo2 ->
          RC4
        | Algo4 a4 ->
          if is_string then
            a4.str
          else
            a4.stm
      end

  (* aka Algorithm 1 *)
  let decrypt_for_object (crypto : t) (is_string : bool) (key : Key.t) : decrypt_t =
    let c = get_cipher crypto is_string key in

    let buf = Buffer.create 25 in
    Buffer.add_string buf crypto.filekey.k;

    (* TODO : key must not be Trailer or Version *)
    let num, gen = Key.get_obj_ref key in
    Buffer.add_char buf (Char.chr (num land 0xFF));
    Buffer.add_char buf (Char.chr ((num lsr 8) land 0xFF));
    Buffer.add_char buf (Char.chr ((num lsr 16) land 0xFF));
    Buffer.add_char buf (Char.chr (gen land 0xFF));
    Buffer.add_char buf (Char.chr ((gen lsr 8) land 0xFF));

    if c = AES_CBC then
      Buffer.add_string buf "\x73\x41\x6C\x54";

    let md5 = MD5.hash (Buffer.contents buf) in
    let l =
      if crypto.params.keylen_bytes >= 11 then
        16
      else
        crypto.params.keylen_bytes + 5
    in
    let k = String.sub md5 0 l in

    match c with
    | NONE ->
      (fun x -> x)
    | RC4 ->
      (RC4.crypt k)
    | AES_CBC ->
      (AES.decrypt_cbc k (Errors.make_ctxt_key key))


  (*************************)
  (* PDF reference 7.6.3.3 *)
  (*************************)
  let padding : string =
    "\x28\xBF\x4E\x5E\x4E\x75\x8A\x41" ^
    "\x64\x00\x4E\x56\xFF\xFA\x01\x08" ^
    "\x2E\x2E\x00\xB6\xD0\x68\x3E\x80" ^
    "\x2F\x0C\xA9\xFE\x64\x53\x69\x7A"

  let pad_to_buf (buf : Buffer.t) (pass : pass_t) : unit =
    let l = String.length pass.p in
    if l < 32 then (
      Buffer.add_string buf pass.p;
      Buffer.add_substring buf padding 0 (32 - l)
    ) else
      Buffer.add_substring buf pass.p 0 32


  (*************************)
  (* PDF reference 7.6.3.3 *)
  (*************************)
  (* aka Algorithm 2 *)
  let make_user_key (params : param_t) (user_pass : pass_t) : key_t =
    let md5trim x =
      String.sub (MD5.hash x) 0 params.keylen_bytes
    in

    let buf = Buffer.create 128 in
    (* step a *)
    pad_to_buf buf user_pass;

    (* step c *)
    Buffer.add_string buf params.owner;

    (* step d *)
    Buffer.add_char buf (Char.chr (params.permissions land 0xFF));
    Buffer.add_char buf (Char.chr ((params.permissions lsr 8) land 0xFF));
    Buffer.add_char buf (Char.chr ((params.permissions lsr 16) land 0xFF));
    Buffer.add_char buf (Char.chr ((params.permissions lsr 24) land 0xFF));

    (* step e *)
    Buffer.add_string buf params.docid;

    (* step f *)
    if params.rev >= 4 && not params.encrypt_meta then
      Buffer.add_string buf "\xFF\xFF\xFF\xFF";

    (* step g *)
    let k = ref (md5trim (Buffer.contents buf)) in
    (* step h *)
    if params.rev >= 3 then
      for i = 1 to 50 do
        k := md5trim !k
      done;

    (* step i *)
    {k = !k;}


  (*************************)
  (* PDF reference 7.6.3.4 *)
  (*************************)
  (* aka Algorithm 3 *)
  let make_owner_key (params : param_t) (owner_pass : pass_t option) (user_pass : pass_t) : key_t =
    let buf = Buffer.create 32 in
    (* step a *)
    pad_to_buf buf (match owner_pass with
        | Some pass ->
          pass
        | None ->
          user_pass
      );

    (* step b *)
    let k = ref (MD5.hash (Buffer.contents buf)) in
    (* step c *)
    if params.rev >= 3 then
      for i = 1 to 50 do
        k := MD5.hash !k
      done;

    (* step d *)
    {k = String.sub !k 0 params.keylen_bytes;}

  let owner_checksum (params : param_t) (owner_pass : pass_t option) (user_pass : pass_t) : string =
    let key = make_owner_key params owner_pass user_pass in

    (* step e *)
    let buf2 = Buffer.create 32 in
    pad_to_buf buf2 user_pass;
    (* step f *)
    let x = ref (RC4.crypt key.k (Buffer.contents buf2)) in

    (* step g *)
    if params.rev >= 3 then
      for i = 1 to 19 do
        let ki = String.map (fun c -> Char.chr ((Char.code c) lxor i)) key.k in
        x := RC4.crypt ki !x
      done;

    (* step h *)
    !x


  (* aka Algorithms 4 & 5 *)
  let user_checksum (params : param_t) (user_key : key_t) : string =
    if params.rev = 2 then
      (* step 4b *)
      RC4.crypt user_key.k padding
    else (
      let buf = Buffer.create 128 in
      (* step 5b *)
      Buffer.add_string buf padding;
      (* step 5c *)
      Buffer.add_string buf params.docid;

      (* step 5d *)
      let x = ref (RC4.crypt user_key.k (MD5.hash (Buffer.contents buf))) in
      (* step 5e *)
      for i = 1 to 19 do
        let ki = String.map (fun c -> Char.chr ((Char.code c) lxor i)) user_key.k in
        x := RC4.crypt ki !x
      done;

      (* step 5f *)
      (* 16 bytes of arbitrary padding *)
      !x ^ "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"
    )


  (* aka Algorithm 6 *)
  let verify_user (params : param_t) (user_key : key_t) : bool =
    (* step a *)
    let u = user_checksum params user_key in
    (* step b *)
    if params.rev = 2 then
      params.user = u
    else
      (String.sub params.user 0 16) = (String.sub u 0 16)


  (* aka Algorithm 7 *)
  let user_of_owner (params : param_t) (owner_pass : pass_t) : pass_t =
    (* step a *)
    let key = make_owner_key params (Some owner_pass) {p = "";} in

    (* step b *)
    let u = ref params.owner in
    if params.rev = 2 then
      u := RC4.crypt key.k !u
    else
      for i = 19 downto 0 do
        let ki = String.map (fun c -> Char.chr ((Char.code c) lxor i)) key.k in
        u := RC4.crypt ki !u
      done;

    {p = !u;}

  let verify_owner (params : param_t) (owner_pass : pass_t) : bool =
    let u = user_of_owner params owner_pass in

    (* step c *)
    verify_user params (make_user_key params u)


  let make_crypto (params : param_t) (user_pass : string) (owner_pass : string) (encrypt_dict : Key.t option) : t * bool * bool =
    let user_key1 = make_user_key params {p = user_pass;} in
    let user_key2 = make_user_key params (user_of_owner params {p = owner_pass;}) in

    let verify1 = verify_user params user_key1 in
    let verify2 = verify_user params user_key2 in
    if Params.global.Params.debug then (
      if verify1 then
        Printf.eprintf "User password is valid!\n"
      else
        Printf.eprintf "User password is invalid...\n";
      if verify2 then
        Printf.eprintf "Owner password is valid!\n"
      else
        Printf.eprintf "Owner password is invalid...\n";
    );

    let filekey =
      if verify1 then
        user_key1
      else if verify2 then
        user_key2
      else
        raise (Errors.PDFError ("Invalid user and owner passwords for encrypted document", Errors.ctxt_none))
    in

    {
      params = params;
      filekey = filekey;
      encrypt_dict = encrypt_dict;
    }, verify1, verify2

end

