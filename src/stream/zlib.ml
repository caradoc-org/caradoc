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


(*************************)
(* PDF reference 7.4.4.1 *)
(*************************)
module Zlib = struct

  let adler32 (block : string) : int32 =
    let s1 = ref 1l in
    let s2 = ref 0l in
    for i = 0 to (String.length block) - 1 do
      s1 := Int32.rem (Int32.add !s1 (Int32.of_int (Char.code block.[i]))) 65521l;
      s2 := Int32.rem (Int32.add !s2 !s1) 65521l;
    done;
    Int32.logor (Int32.shift_left !s2 16) !s1


  let decode (content : string) : string option =
    let len = String.length content in
    if len < 6 then
      None
    else (
      let x1 = Char.code content.[0] in
      let x2 = Char.code content.[1] in
      let meth = x1 land 0x0F in
      let info = x1 lsr 4 in
      let fdict = (x2 lsr 5) land 0x01 in
    (*
    let flevel = (x2 lsr 6) land 0x03 in
    *)

      if (
        ((x1 * 256 + x2) mod 31) <> 0 ||
        meth <> 8 ||
        info > 7 ||
        fdict <> 0
      ) then
        None
      else
        try
          let transform = Cryptokit.Zlib.uncompress () in
          transform#put_substring content 2 (len - 6);
          transform#finish;
          let result = transform#get_string in

          let h1 = (Int32.shift_left (Int32.of_int (Char.code content.[len - 4])) 24) in
          let h2 = (Int32.shift_left (Int32.of_int (Char.code content.[len - 3])) 16) in
          let h3 = (Int32.shift_left (Int32.of_int (Char.code content.[len - 2]))  8) in
          let h4 =                   (Int32.of_int (Char.code content.[len - 1]))     in
          let hash = Int32.logor (Int32.logor (Int32.logor h1 h2) h3) h4 in

          if hash <> adler32 result then
            None
          else
            Some result
        with _ ->
          None
    )


  let encode (content : string) : string =
    (* TODO : compression level? *)
    let transform = Cryptokit.Zlib.compress () in
    transform#put_string content;
    transform#finish;
    let result = transform#get_string in

    let buf = Buffer.create ((String.length result) + 6) in
    (* Deflate, 32K window size, no preset dictionary, default compression level *)
    Buffer.add_string buf "\x78\x9C";
    Buffer.add_string buf result;

    let hash = adler32 content in
    Buffer.add_char buf (Char.chr (Int32.to_int (Int32.logand 0xFFl (Int32.shift_right hash 24))));
    Buffer.add_char buf (Char.chr (Int32.to_int (Int32.logand 0xFFl (Int32.shift_right hash 16))));
    Buffer.add_char buf (Char.chr (Int32.to_int (Int32.logand 0xFFl (Int32.shift_right hash  8))));
    Buffer.add_char buf (Char.chr (Int32.to_int (Int32.logand 0xFFl                    hash    )));

    Buffer.contents buf

end
