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

module Common : sig

  (*   Prints a lexbuf
       Args    :
       - lexbuf name
       - lexbuf
  *)
  val print_lexbuf : string -> Lexing.lexbuf -> unit

  (*   Read a sub-string from a channel
       Args    :
       - input
       - position
       - length
       Returns :
       - string of lengths characters starting at position
  *)
  val input_substr : in_channel -> BoundedInt.t -> BoundedInt.t -> string
  (*   Rewinds a lexbuf n characters back
       Args    :
       - lexbuf
       - number of characters to rewind
  *)
  val rewind : Lexing.lexbuf -> int -> unit

end

