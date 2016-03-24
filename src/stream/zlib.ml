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


module Zlib = struct

  let adler32 (block : string) : int32 =
    let s1 = ref 1l in
    let s2 = ref 0l in
    for i = 0 to (String.length block) - 1 do
      s1 := Int32.rem (Int32.add !s1 (Int32.of_int (int_of_char block.[i]))) 65521l;
      s2 := Int32.rem (Int32.add !s2 !s1) 65521l;
    done;
    Int32.logor (Int32.shift_left !s2 16) !s1


  let decode (content : string) : string option =
    let len = String.length content in
    if len < 6 then
      None
    else (
      let x1 = int_of_char content.[0] in
      let x2 = int_of_char content.[1] in
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
      else (
        try
          let transform = Cryptokit.Zlib.uncompress () in
          transform#put_substring content 2 (len - 6);
          transform#finish;
          let result = transform#get_string in

          let h1 = (Int32.shift_left (Int32.of_int (int_of_char content.[len - 4])) 24) in
          let h2 = (Int32.shift_left (Int32.of_int (int_of_char content.[len - 3])) 16) in
          let h3 = (Int32.shift_left (Int32.of_int (int_of_char content.[len - 2]))  8) in
          let h4 =                   (Int32.of_int (int_of_char content.[len - 1]))     in
          let hash = Int32.logor (Int32.logor (Int32.logor h1 h2) h3) h4 in

          if hash <> adler32 result then
            None
          else
            Some result
        with
        | _ -> None
      )
    )

end
