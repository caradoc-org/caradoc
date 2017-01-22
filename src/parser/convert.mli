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

(*   Convert a token string to an integer (big-endian)
     Args    :
     - input string
     - starting offset in input
     - length of the token (must be >= 0)
     - [optional] default value
     Returns :
     - integer value
*)
val int_of_bytes : string -> BoundedInt.t -> BoundedInt.t -> ?default:BoundedInt.t -> unit -> BoundedInt.t

(*   Convert an octal character to the corresponding digit
     Args    :
     - octal character (in '0'-'7')
     Returns :
     - integer value
*)
val int_of_octal_digit : char -> int

(*   Convert an octal string to the corresponding number
     Args    :
     - octal characters
     Returns :
     - integer value
*)
val int_of_octal : char -> char -> char -> int

(*   Convert an octal string to the character corresponding to its code
     Args    :
     - octal characters
     Returns :
     - corresponding character
*)
val char_of_octal3 : char -> char -> char -> char
val char_of_octal2 : char -> char -> char
val char_of_octal1 : char -> char

(*   Convert a hexadecimal character to the corresponding digit
     Args    :
     - hexadecimal character (in '0'-'9''a'-'f''A'-'F')
     Returns :
     - integer value
*)
val int_of_hexa_digit : char -> int

(*   Convert a hexadecimal string to the corresponding number
     Args    :
     - hexadecimal characters
     Returns :
     - integer value
*)
val int_of_hexa : char -> char -> int

(*   Convert a hexadecimal string to the character corresponding to its code
     Args    :
     - hexadecimal characters
     Returns :
     - corresponding character
*)
val char_of_hexa : char -> char -> char

(*   Convert a hexadecimal digit to the corresponding character
     Args    :
     - integer value (between 0 and 15)
     Returns :
     - hexadecimal character (in '0'-'9''A'-'F')
*)
val hexa_digit_of_int : int -> char

(*   Convert a character to the corresponding hexadecimal code
     Args    :
     - character
     Returns :
     - code in hexadecimal form
*)
val hexa_of_char : char -> string

(*   Convert a character to the corresponding hexadecimal code
     Args    :
     - output buffer
     - character
*)
val hexa_of_char_buf : Buffer.t -> char -> unit

