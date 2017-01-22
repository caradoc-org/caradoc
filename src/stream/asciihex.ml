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


open Algo

(***********************)
(* PDF reference 7.4.2 *)
(***********************)
module ASCIIHex = struct

  let decode (content : string) : string option =
    let s = Algo.remove_if (fun c ->
        c = '\x00' (* NUL *) || c = '\x09' (* HT *) || c = '\x0C' (* FF *) || c = '\x20' (* SP *) || c = '\x0A' (* LF *) || c = '\x0D' (* CR *)
      ) content in
    try
      let l = String.length s in
      if l < 1 then
        raise Exit;
      if s.[l-1] <> '>' then
        raise Exit;
      let ll = l-1 in

      let i = ref 0 in
      let buf = Buffer.create (l / 2) in

      while !i < ll do
        if !i + 1 < ll then
          Buffer.add_char buf (Convert.char_of_hexa s.[!i] s.[!i + 1])
        else
          Buffer.add_char buf (Convert.char_of_hexa s.[!i] '0');
        i := !i + 2
      done;

      Some (Buffer.contents buf)
    with
    | Exit -> None
    | Convert.ConvertError _ -> None


  let encode (content : string) : string =
    let l = String.length content in
    let buf = Buffer.create (2*l + 1) in

    for i = 0 to l-1 do
      Convert.hexa_of_char_buf buf content.[i]
    done;

    Buffer.add_char buf '>';
    Buffer.contents buf

end
