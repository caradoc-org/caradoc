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


open Errors

module ASCII85 = struct

  let decode_char (c : char) : int64 =
    let i = Char.code c in
    if i < 0x21 || i > 0x75 then
      raise Exit;
    Int64.of_int (i - 0x21)

  let decode_block_impl (dst : Buffer.t) (a : char) (b : char) (c : char) (d : char) (e : char) (l : int) : unit =
    let ai = decode_char a in
    let bi = decode_char b in
    let ci = decode_char c in
    let di = decode_char d in
    let ei = decode_char e in
    let x =
      Int64.add ei
        (Int64.mul 85L
           (Int64.add di
              (Int64.mul 85L
                 (Int64.add ci
                    (Int64.mul 85L
                       (Int64.add bi
                          (Int64.mul ai 85L)
                       )
                    )
                 )
              )
           )
        )
    in
    if x >= 0x1_0000_0000L then
      raise Exit;

    if l >= 1 then
      Buffer.add_char dst (Char.chr (Int64.to_int (Int64.rem (Int64.shift_right x 24) 256L)));
    if l >= 2 then
      Buffer.add_char dst (Char.chr (Int64.to_int (Int64.rem (Int64.shift_right x 16) 256L)));
    if l >= 3 then
      Buffer.add_char dst (Char.chr (Int64.to_int (Int64.rem (Int64.shift_right x  8) 256L)));
    if l >= 4 then
      Buffer.add_char dst (Char.chr (Int64.to_int (Int64.rem                    x     256L)))

  let decode_block (dst : Buffer.t) (s : string) (i : int) (l : int) : unit =
    let d = l-i in
    if d = 1 then
      raise Exit
    else if d = 2 then
      decode_block_impl dst s.[i] s.[i+1] '\x75' '\x75' '\x75' 1
    else if d = 3 then
      decode_block_impl dst s.[i] s.[i+1] s.[i+2] '\x75' '\x75' 2
    else if d = 4 then
      decode_block_impl dst s.[i] s.[i+1] s.[i+2] s.[i+3] '\x75' 3
    else
      decode_block_impl dst s.[i] s.[i+1] s.[i+2] s.[i+3] s.[i+4] 4


  let decode (content : string) : string option =
    try
      let l = String.length content in
      if l < 2 then
        raise Exit;
      if content.[l-2] <> '\x7E' || content.[l-1] <> '\x3E' then
        raise Exit;
      let ll = l-2 in

      let i = ref 0 in
      let buf = Buffer.create 1 in

      while !i < ll do
        if content.[!i] = 'z' then (
          Buffer.add_string buf "\x00\x00\x00\x00";
          i := !i + 1
        ) else (
          decode_block buf content !i ll;
          let d = ll - !i in
          if d < 5 then
            i := ll
          else
            i := !i + 5
        )
      done;

      Some (Buffer.contents buf)
    with
    | Exit -> None

end
