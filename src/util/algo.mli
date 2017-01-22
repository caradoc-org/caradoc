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


open Mapkey

(* Generic algorithms *)
module Algo : sig

  (*   Implement iteri using fold_left
       Args    :
       - fold_left function of the iterable object
       - iteration function to apply
       - iterable object (e.g. list, array)
  *)
  val iteri : ((int -> 'b -> int) -> int -> 'a -> int) -> (int -> 'b -> unit) -> 'a -> unit

  (*   Same as iter but with indication of the first round
       Args    :
       - fold_left function of the iterable object
       - iteration function to apply
       - iterable object (e.g. list, array)
  *)
  val iter_start : ((bool -> 'b -> bool) -> bool -> 'a -> bool) -> (bool -> 'b -> unit) -> 'a -> unit

  (*   Same as fold_left but with an additional counter
       Args    :
       - fold_left function of the iterable object
       - fold function to apply
       - initial argument
       - iterable object (e.g. list, array)
       Returns :
       - result of the folding operation
  *)
  val fold_lefti : ((int * 'c -> 'b -> int * 'c) -> int * 'c -> 'a -> int * 'c) -> (int -> 'c -> 'b -> 'c) -> 'c -> 'a -> 'c

  (*   Same as fold_left but with indication of the first round
       Args    :
       - fold_left function of the iterable object
       - fold function to apply
       - initial argument
       - iterable object (e.g. list, array)
       Returns :
       - result of the folding operation
  *)
  val fold_left_start : ((bool * 'c -> 'b -> bool * 'c) -> bool * 'c -> 'a -> bool * 'c) -> (bool -> 'c -> 'b -> 'c) -> 'c -> 'a -> 'c

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

  (*   Compute the union of two maps
       If a key is present in both maps, the value of the first map is retained
       Args    :
       - first map
       - second map
       Returns :
       - union of the two maps
  *)
  val mapkey_union : 'a MapKey.t -> 'a MapKey.t -> 'a MapKey.t

  (*   Test whether a string contains a substring
       Args    :
       - string
       - substring
       Returns :
       - whether string contains substring
  *)
  val string_contains : string -> string -> bool

  (*   Test whether a string starts with a substring
       Args    :
       - string
       - substring
       Returns :
       - whether string starts with substring
  *)
  val string_starts_with : string -> string -> bool

  (*   Join into a buffer an iterable object with separators
       Args    :
       - buffer
       - fold_left function of the iterable object
       - function to append an element of the iterable to a buffer
       - separator
       - iterable object (e.g. list, array)
  *)
  val join_buffer : Buffer.t -> ((bool -> 'b -> bool) -> bool -> 'a -> bool) -> (Buffer.t -> 'b -> unit) -> string -> 'a -> unit

  (*   Join into a buffer an iterable object with separators
       Args    :
       - buffer
       - fold_left function of the iterable object
       - function to append an element of the iterable to a buffer
       - separator
       - iterable object (e.g. list, array)
  *)
  val join_bufferi : Buffer.t -> ((int -> 'b -> int) -> int -> 'a -> int) -> (int -> Buffer.t -> 'b -> unit) -> string -> 'a -> unit

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

