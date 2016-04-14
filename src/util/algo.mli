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


(* Generic algorithms *)
module Algo : sig

  (*   Search an element in an array
       Args    :
       - array
       - element
       Returns :
       - whether array contains element
  *)
  val array_contains : 'a array -> 'a -> bool

  (*   Sort a hash table by key
       Args    :
       - hash table
       Returns :
       - list of entries sorted by key
  *)
  val sort_hash : (string, 'a) Hashtbl.t -> (string * 'a) list

  (*   Merge sorted lists
       Args    :
       - list 1
       - list 2
       - comparison function of elements
       Returns :
       - merged list
  *)
  val merge_unique : 'a list -> 'a list-> ('a -> 'a -> int) -> 'a list

  (*   Test wether a string contains a substring
       Args    :
       - string
       - substring
       Returns :
       - whether string contains substring
  *)
  val string_contains : string -> string -> bool

  (*   Join into a buffer an iterable object with separators
       Args    :
       - buffer
       - fold_left function of the iterable object
       - function to append an element of the iterable to a buffer
       - separator
       - iterable object (e.g. list, array)
  *)
  val join_buffer : Buffer.t -> ((bool -> 'b -> bool) -> bool -> 'a -> bool) -> (Buffer.t -> 'b -> unit) -> string -> 'a -> unit

  (*   Join an iterable object with separators
       Args    :
       - fold_left function of the iterable object
       - function to convert an element of the iterable to a string
       - separator
       - iterable object (e.g. list, array)
       Returns :
       - joined string
  *)
  val join_string : ((bool -> 'b -> bool) -> bool -> 'a -> bool) -> ('b -> string) -> string -> 'a -> string

  (*   Remove from a string the characters verifying a prediate
       Args    :
       - predicate on characters to remove
       - string to transform
       Returns :
       - transformed string
  *)
  val remove_if : (char -> bool) -> string -> string

end

