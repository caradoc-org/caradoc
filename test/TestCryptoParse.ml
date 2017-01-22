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


open OUnit
open Cryptoparse
open Crypto.Crypto
open Directobject.DirectObject
open Boundedint
open Errors


let tests =
  "CryptoParse" >:::
  [
    "parse_encrypt_dict" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (CryptoParse.parse_encrypt_dict "0123456789ABCDEF" (TestDict.add_all ["Filter", Name "Standard" ; "V", Int ~:1 ; "R", Int ~:2 ; "O", String "0123456789ABCDEFFEDCBA9876543210" ; "U", String "FEDCBA98765432100123456789ABCDEF" ; "P", Int ~:123456]) Errors.ctxt_none)
                    {algorithm = Algo1; keylen_bytes = 5; rev = 2; owner = "0123456789ABCDEFFEDCBA9876543210"; user = "FEDCBA98765432100123456789ABCDEF"; permissions = 123456; encrypt_meta = true; docid = "0123456789ABCDEF";}) ;
      "(2)" >:: (fun _ -> assert_equal
                    (CryptoParse.parse_encrypt_dict "0123456789ABCDEF" (TestDict.add_all ["Filter", Name "Standard" ; "V", Int ~:4 ; "R", Int ~:4 ; "O", String "0123456789ABCDEFFEDCBA9876543210" ; "U", String "FEDCBA98765432100123456789ABCDEF" ; "P", Int ~:123456 ; "CF", Dictionary (TestDict.add_all ["A", Dictionary (TestDict.add_all ["CFM", Name "AESV2"]) ; "B", Dictionary (TestDict.add_all ["CFM", Name "V2"])]) ; "StmF", Name "A" ; "StrF", Name "B"]) Errors.ctxt_none)
                    {algorithm = Algo4 {stm = AES_CBC; str = RC4;}; keylen_bytes = 5; rev = 4; owner = "0123456789ABCDEFFEDCBA9876543210"; user = "FEDCBA98765432100123456789ABCDEF"; permissions = 123456; encrypt_meta = true; docid = "0123456789ABCDEF";}) ;
    ] ;
  ]

