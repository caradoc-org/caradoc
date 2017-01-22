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
open Aes
open Errors


let tests =
  "AES" >:::
  [
    "decrypt_cbc" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (AES.decrypt_cbc "0123456789abcdef" Errors.ctxt_none ("fedcba9876543210" ^ "\x3c\x42\x53\xa6\x91\x7f\x75\x47\x26\xa5\x6a\x20\xd2\x06\x48\x45"))
                    "Hello world") ;
      "(2)" >:: (fun _ -> assert_raises
                    (Errors.PDFError ("Expected a multiple of 16 bytes for encrypted data in AES-CBC mode", Errors.ctxt_none))
                    (fun () -> AES.decrypt_cbc "0123456789abcdef" Errors.ctxt_none ("0123456789abcdef0123456789abcdef0"))) ;
      "(3)" >:: (fun _ -> assert_raises
                    (Errors.PDFError ("Expected at least two blocks for encrypted data in AES-CBC mode", Errors.ctxt_none))
                    (fun () -> AES.decrypt_cbc "0123456789abcdef" Errors.ctxt_none ("fedcba9876543210"))) ;
      "(4)" >:: (fun _ -> assert_raises (* Encryption of "Hello world\x05\x04\x05\x05\x05" *)
                    (Errors.PDFError ("Invalid PKCS#5 padding for encrypted data in AES-CBC mode", Errors.ctxt_none))
                    (fun () -> AES.decrypt_cbc "0123456789abcdef" Errors.ctxt_none ("fedcba9876543210" ^ "\x54\x29\x6b\xbe\x1a\x79\xe2\xd2\x47\xe1\xb2\x40\xe5\xa7\x4e\x53"))) ;
      "(5)" >:: (fun _ -> assert_raises
                    (Errors.PDFError ("Invalid PKCS#5 padding for encrypted data in AES-CBC mode", Errors.ctxt_none))
                    (fun () -> AES.decrypt_cbc "0123456789abcdef" Errors.ctxt_none ("0123456789abcdef0123456789abcdef"))) ;
    ] ;
  ]

