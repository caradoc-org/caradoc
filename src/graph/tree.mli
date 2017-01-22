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


open Document
open Key
open Directobject


module Tree : sig

  (*   Traverse a tree structure
       Args    :
       - traversing function
       - field that defines an array of children
       - document
       - root of the tree
  *)
  val traverse : (DirectObject.dict_t -> Key.t -> Key.t -> unit) -> string -> Document.t -> Key.t -> unit

  (*   Check a tree structure
       Args    :
       - field that defines the parent
       - field that defines an array of children
       - document
       - root of the tree
  *)
  val check : string option -> string -> Document.t -> Key.t -> unit

  (*   Check a list-tree structure
       Args    :
       - field that defines the parent
       - field that defines the first child
       - field that defines the last child
       - field that defines the next node
       - field that defines the previous node
       - document
       - root of the tree
  *)
  val checklist : string -> string -> string -> string -> string -> Document.t -> Key.t -> unit

end
