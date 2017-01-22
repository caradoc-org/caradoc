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

module AES = struct

  let remove_pkcs5 (data : string) : string option =
    let len = String.length data in
    let padc = data.[len - 1] in
    let pad = Char.code padc in

    try
      (* Check that padding is correct *)
      if pad = 0 || pad > 16 then
        raise Exit;
      for i = 2 to pad do
        if data.[len - i] <> padc then
          raise Exit
      done;

      (* Remove IV and padding *)
      Some (String.sub data 16 (len - 16 - pad))
    with Exit ->
      None

  let decrypt_cbc (key : string) (ctxt : Errors.error_ctxt) (data : string) : string =
    let len = String.length data in
    if len mod 16 <> 0 then
      raise (Errors.PDFError ("Expected a multiple of 16 bytes for encrypted data in AES-CBC mode", ctxt));
    if len < 32 then
      raise (Errors.PDFError ("Expected at least two blocks for encrypted data in AES-CBC mode", ctxt));

    let cipher = (Cryptokit.Cipher.aes ~mode:Cryptokit.Cipher.CBC) key Cryptokit.Cipher.Decrypt in
    let plaintext = Cryptokit.transform_string cipher data in

    match remove_pkcs5 plaintext with
    | Some s ->
      s
    | None ->
      raise (Errors.PDFError ("Invalid PKCS#5 padding for encrypted data in AES-CBC mode", ctxt))

end

