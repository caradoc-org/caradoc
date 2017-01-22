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


module BoundedInt = struct

  type t = int64
  exception IntegerError of string

  let min = -0x8000_0000L
  let max = 0x7fff_ffffL
  let max_abs = 0x8000_0000L

  let my_int_of_char (c : char) : t =
    let i = Char.code c in
    if i >= (Char.code '0') && i <= (Char.code '9') then
      Int64.of_int (i - (Char.code '0'))
    else
      raise (IntegerError "string contains a non-digit character")

  let sign_of_string (s : string) : (int * int * int) =
    let l = String.length s in
    match l with
    | 0 -> (1, 0, l)
    | _ when s.[0] = '-' -> (-1, 1, l)
    | _ when s.[0] = '+' -> (1, 1, l)
    | _ -> (1, 0, l)

  let check (i : t) : t =
    if i < min || i > max then
      raise (IntegerError "integer overflow")
    else
      i

  let check_abs (i : t) : t =
    if Int64.abs i > max_abs then
      raise (IntegerError "integer overflow")
    else
      i

  let of_int (i : int) : t =
    check (Int64.of_int i)

  let of_int64 (i : int64) : t =
    check i

  let to_int (i : t) : int =
    Int64.to_int (check i)

  let to_string (i : t) : string =
    if i < min || i > max then
      "NaN"
    else
      Int64.to_string i

  let int_of_string (s : string) : t =
    let result = ref Int64.zero in
    let sign, start, len = sign_of_string s in

    for i = start to len - 1 do
      result := check_abs (Int64.add (Int64.mul !result 10L) (my_int_of_char s.[i]))
    done;
    check (Int64.mul !result (Int64.of_int sign))

  let uint_of_string (s : string) : t =
    let result = ref Int64.zero in
    let len = String.length s in

    for i = 0 to len - 1 do
      result := check (Int64.add (Int64.mul !result 10L) (my_int_of_char s.[i]))
    done;
    !result


  let compare (x : t) (y : t) : int =
    Int64.compare x y


  let rem (x : t) (y : t) : t =
    check (Int64.rem x y)

end

let ( ~: ) x =
  BoundedInt.of_int x


let ( +: ) x y =
  BoundedInt.check (Int64.add x y)
let ( -: ) x y =
  BoundedInt.check (Int64.sub x y)
let ( *: ) x y =
  BoundedInt.check (Int64.mul x y)
let ( /: ) x y =
  BoundedInt.check (Int64.div x y)
let rec ( <<: ) x y =
  match y with
  | 0 -> x
  | _ when y > 0 ->
    (BoundedInt.check (Int64.shift_left x 1)) <<: (y - 1)
  | _ ->
    raise (BoundedInt.IntegerError "integer overflow")

let ( <: ) x y =
  (Int64.compare x y) < 0
let ( >: ) x y =
  (Int64.compare x y) > 0
let ( <=: ) x y =
  (Int64.compare x y) <= 0
let ( >=: ) x y =
  (Int64.compare x y) >= 0

