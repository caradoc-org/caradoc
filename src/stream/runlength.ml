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


(***********************)
(* PDF reference 7.4.5 *)
(***********************)
module RunLength = struct

  let decode (content : string) : string option =
    try
      let l = String.length content in
      let buf = Buffer.create l in
      let i = ref 0 in
      let eod = ref false in

      while !i < l do
        let code = Char.code content.[!i] in
        i := !i + 1;
        if code = 0x80 then (
          if !i != l then
            raise Exit
          else
            eod := true
        ) else if code < 0x80 then (
          let ll = code + 1 in
          if !i + ll > l then
            raise Exit;
          Buffer.add_substring buf content !i ll;
          i := !i + ll
        ) else (
          if !i >= l then
            raise Exit;
          let c = content.[!i] in
          for j = 1 to 257 - code do
            Buffer.add_char buf c
          done;
          i := !i + 1
        )
      done;

      if !eod then
        Some (Buffer.contents buf)
      else
        None
    with Exit ->
      None


  let encode (content : string) : string =
    let l = String.length content in
    let a = l / 0x80 in
    let b = l mod 0x80 in
    let buf = Buffer.create (a * 0x81 + b + 2) in
    let i = ref 0 in

    (* Basic encoder that does not use run lengths *)
    (* TODO : optimize the encoder to look for run lengths *)
    while !i < l do
      let ll = ref (l - !i) in
      if !ll > 0x80 then
        ll := 0x80;

      Buffer.add_char buf (Char.chr (!ll - 1));
      Buffer.add_substring buf content !i !ll;
      i := !i + !ll
    done;

    Buffer.add_char buf (Char.chr 0x80);
    Buffer.contents buf

end
