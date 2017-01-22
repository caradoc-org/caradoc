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
open Key

module Crypto : sig

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

  type t

  type decrypt_t = string -> string

  (*   Check if metadata is encrypted
       Args    :
       - encryption parameters
       Returns :
       - whether metadata is encrypted
  *)
  val encrypt_meta : t -> bool

  (*   Verify user key
       Args    :
       - encryption parameters
       - user password
       - owner password
       - /Encrypt dictionary
       Returns :
       - encryption object
       - whether user password is valid
       - whether owner password is valid
  *)
  val make_crypto : param_t -> string -> string -> Key.t option -> t * bool * bool

  (*   Compute the encryption key of an object and return the associated decryption function
       Args    :
       - encryption parameters
       - whether the object is a string
       - object identifier
       Returns :
       - decryption function for this object
  *)
  val decrypt_for_object : t -> bool -> Key.t -> decrypt_t

end

