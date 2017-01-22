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


open Boundedint

exception ConvertError of string

(* Convert integer written in a binary string *)
let int_of_bytes (str : string) (pos : BoundedInt.t) (len : BoundedInt.t) ?default () : BoundedInt.t =
  if len = ~:0 then (
    match default with
    | None -> raise (ConvertError "Binary integer has length zero")
    | Some x -> x
  ) else (
    let result = ref ~:0 in
    for i = 0 to BoundedInt.to_int (len -: ~:1) do
      result := (!result <<: 8) +: ~:(Char.code str.[BoundedInt.to_int (pos +: ~:i)])
    done;
    !result
  )

(* Functions to convert octal to character *)
let int_of_octal_digit (n : char) : int =
  let x = (Char.code n) - (Char.code '0') in
  if x < 0 || x >= 8 then
    raise (ConvertError "Not an octal digit");
  x

let int_of_octal (n1 : char) (n2 : char) (n3 : char) : int =
  ((int_of_octal_digit n1) * 8 + (int_of_octal_digit n2)) * 8 + (int_of_octal_digit n3)

let char_of_octal3 (n1 : char) (n2 : char) (n3 : char) : char =
  let x = int_of_octal n1 n2 n3 in
  if x > 255 then
    raise (ConvertError "Octal character is out of bounds");
  Char.chr x

let char_of_octal2 (n1 : char) (n2 : char) : char =
  char_of_octal3 '0' n1 n2

let char_of_octal1 (n1 : char) : char =
  char_of_octal3 '0' '0' n1


(* Functions to convert hexadecimal to character *)
let int_of_hexa_digit (n : char) : int =
  let i = Char.code n in
  let x = i - (Char.code '0') in
  if x >= 0 && x < 10 then
    x
  else (
    let y = 10 + i - (Char.code 'a') in
    if y >= 10 && y < 16 then
      y
    else (
      let z = 10 + i - (Char.code 'A') in
      if z >= 10 && z < 16 then
        z
      else
        raise (ConvertError "Not a hexadecimal digit");
    )
  )

let int_of_hexa (n1 : char) (n2 : char) : int =
  (int_of_hexa_digit n1) * 16 + (int_of_hexa_digit n2)

let char_of_hexa (n1 : char) (n2 : char) : char =
  let x = int_of_hexa n1 n2 in
  Char.chr x

let hexa_digit_of_int (n : int) : char =
  if n < 0 || n >= 16 then
    raise (ConvertError "Hexadecimal digit is out of bounds")
  else if n < 10 then
    Char.chr (n + (Char.code '0'))
  else
    Char.chr (n - 10 + (Char.code 'A'))

let hexa_of_char (c : char) : string =
  let x = Char.code c in
  let s = "00" in
  s.[0] <- hexa_digit_of_int (x / 16);
  s.[1] <- hexa_digit_of_int (x mod 16);
  s

let hexa_of_char_buf (buf : Buffer.t) (c : char) : unit =
  let x = Char.code c in
  Buffer.add_char buf (hexa_digit_of_int (x / 16));
  Buffer.add_char buf (hexa_digit_of_int (x mod 16))

