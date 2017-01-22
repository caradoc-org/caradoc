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
(* PDF reference 7.4.3 *)
(***********************)
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
    let s = Algo.remove_if (fun c ->
        c = '\x00' (* NUL *) || c = '\x09' (* HT *) || c = '\x0C' (* FF *) || c = '\x20' (* SP *) || c = '\x0A' (* LF *) || c = '\x0D' (* CR *)
      ) content in
    try
      let l = String.length s in
      if l < 2 then
        raise Exit;
      if s.[l-2] <> '\x7E' || s.[l-1] <> '\x3E' then
        raise Exit;
      let ll = l-2 in

      let i = ref 0 in
      let buf = Buffer.create 1 in

      while !i < ll do
        if s.[!i] = 'z' then (
          Buffer.add_string buf "\x00\x00\x00\x00";
          i := !i + 1
        ) else (
          decode_block buf s !i ll;
          let d = ll - !i in
          if d < 5 then
            i := ll
          else
            i := !i + 5
        )
      done;

      Some (Buffer.contents buf)
    with Exit ->
      None


  let encode_char (x : int) : char =
    Char.chr (x + 0x21)

  let encode_block_impl (dst : Buffer.t) (a : char) (b : char) (c : char) (d : char) (l : int) : unit =
    let al = Int64.of_int (Char.code a) in
    let bl = Int64.of_int (Char.code b) in
    let cl = Int64.of_int (Char.code c) in
    let dl = Int64.of_int (Char.code d) in
    let x =
      Int64.add dl
        (Int64.mul 256L
           (Int64.add cl
              (Int64.mul 256L
                 (Int64.add bl
                    (Int64.mul al 256L)
                 )
              )
           )
        )
    in

    if x = 0L && l = 4 then
      Buffer.add_char dst 'z'
    else (
      Buffer.add_char dst (encode_char (Int64.to_int (Int64.rem (Int64.div x 52200625L) 85L)));
      if l >= 1 then
        Buffer.add_char dst (encode_char (Int64.to_int (Int64.rem (Int64.div x 614125L) 85L)));
      if l >= 2 then
        Buffer.add_char dst (encode_char (Int64.to_int (Int64.rem (Int64.div x   7225L) 85L)));
      if l >= 3 then
        Buffer.add_char dst (encode_char (Int64.to_int (Int64.rem (Int64.div x     85L) 85L)));
      if l >= 4 then
        Buffer.add_char dst (encode_char (Int64.to_int (Int64.rem            x          85L)))
    )

  let encode_block (dst : Buffer.t) (s : string) (i : int) (l : int) : unit =
    let d = l-i in
    if d = 1 then
      encode_block_impl dst s.[i] '\x00' '\x00' '\x00' 1
    else if d = 2 then
      encode_block_impl dst s.[i] s.[i+1] '\x00' '\x00' 2
    else if d = 3 then
      encode_block_impl dst s.[i] s.[i+1] s.[i+2] '\x00' 3
    else
      encode_block_impl dst s.[i] s.[i+1] s.[i+2] s.[i+3] 4


  let encode (content : string) : string =
    let l = String.length content in
    let i = ref 0 in
    let buf = Buffer.create 1 in

    while !i < l do
      encode_block buf content !i l;
      let d = l - !i in
      if d < 4 then
        i := l
      else
        i := !i + 4
    done;

    Buffer.add_string buf "\x7E\x3E";
    Buffer.contents buf

end
