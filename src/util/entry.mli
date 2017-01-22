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


module Entry : sig

  (*   An entry represents a position inside an object  *)
  type t

  (*   An empty entry represents the full object  *)
  val empty : t

  (*   Create an entry from an array index
       Args    :
       - index
       Returns :
       - entry
  *)
  val make_index : int -> t

  (*   Create an entry from a dictionary key binding
       Args    :
       - dictionary key (without leading slash)
       Returns :
       - entry
  *)
  val make_name : string -> t

  (*   Create an entry from a dictionary key
       Args    :
       - dictionary key (without leading slash)
       Returns :
       - entry
  *)
  val make_name_key : string -> t

  (*   Append an entry to another entry
       Args    :
       - first entry
       - second entry
       Returns :
       - concatenation of the first followed by the second entry
  *)
  val append_entry : t -> t -> t

  (*   Append an array index to an entry
       Args    :
       - entry
       - index
       Returns :
       - concatenation of the entry followed by the index
  *)
  val append_index : t -> int -> t

  (*   Append a dictionary key binding to an entry
       Args    :
       - entry
       - dictionary key (without leading slash)
       Returns :
       - concatenation of the entry followed by the name
  *)
  val append_name : t -> string -> t

  (*   Append a dictionary key to an entry
       Args    :
       - entry
       - dictionary key (without leading slash)
       Returns :
       - concatenation of the entry followed by the name
  *)
  val append_name_key : t -> string -> t

  (*   Check if an entry is empty
       Args    :
       - entry
       Returns :
       - whether the entry is empty
  *)
  val is_empty : t -> bool

  (*   Convert an entry to a string
       Args    :
       - entry
       Returns :
       - string representation of this entry
  *)
  val to_string : t -> string


  (*   A selector represents a set of entries in an object  *)
  type select_t

  (*   A selector that selects nothing  *)
  val no_selector : select_t

  (*   Create a selector from a set of entries
       Args    :
       - list of entries
       Returns :
       - selector
  *)
  val make_selector : t list -> select_t

  (*   Move to an array index in the current object and update the selector accordingly
       Args    :
       - selector
       - index to move the current entry to
       Returns :
       - intersection of the selector with the obtained entry
  *)
  val move_to_index : select_t -> int -> select_t

  (*   Move to a dictionary key binding in the current object and update the selector accordingly
       Args    :
       - selector
       - name to move the current entry to
       Returns :
       - intersection of the selector with the obtained entry
  *)
  val move_to_name : select_t -> string -> select_t

  (*   Move to a dictionary key in the current object and update the selector accordingly
       Args    :
       - selector
       - name to move the current entry to
       Returns :
       - intersection of the selector with the obtained entry
  *)
  val move_to_name_key : select_t -> string -> select_t

  (*   Check if a selector selects the current entry in an object
       Args    :
       - selector
       Returns :
       - whether the current entry is selected
  *)
  val is_selected : select_t -> bool

end

