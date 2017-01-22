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
open Key
open Boundedint
open Errors


module Type : sig

  type 'a kind =
    (* TODO : remove Any, that accepts any type *)
    | Any
    (* Alias and class *)
    | Alias of string
    | Class of string
    | Stream of string
    (* Basic PDF types *)
    | Null
    | Bool | Int | Real | String | Name
    (* Types based on string *)
    | Text | Date
    (* Bool exactly equal to *)
    | BoolExact of bool
    (* Integer within a range *)
    | IntRange of (BoundedInt.t option) * (BoundedInt.t option)
    (* Integer exactly equal to *)
    | IntExact of BoundedInt.t
    (* Integer in a set *)
    | IntIn of BoundedInt.t array
    (* Name exactly equal to *)
    | NameExact of string
    (* Name in a set *)
    | NameIn of (string, unit) Hashtbl.t
    (* Homogeneous array *)
    | Array of 'a
    (* Array or element (equivalent to array of length 1) *)
    | ArrayOrOne of 'a
    (* Homogeneous array of fixed length *)
    | ArraySized of 'a * int
    (* Homogeneous array whose length is in a set *)
    | ArrayVariantSized of 'a * (int array)
    (* Array that contains an array of tuples *)
    | ArrayTuples of 'a array
    (* Array of differences *)
    | ArrayDifferences
    (* Array that contains a tuple *)
    | Tuple of 'a array
    (* Variant of types *)
    | Variant of 'a kind list
    (* Dictionary with values of a type *)
    | Dictionary of 'a

  type t = {
    (* Actual type *)
    kind : t kind;
    (* Allow indirect reference *)
    allow_ind : bool;
  }

  type kind_t = t kind

  (* Class entry *)
  type entry_t = {
    (* Type of entry *)
    typ : t;
    (* Entry is optional *)
    optional : bool;
  }

  (* A class is a set of keys associated to value types *)
  (* The bool parameter is for strictness *)
  type class_t = ((string, entry_t) Hashtbl.t) * (string list) * bool
  (* A pool associates names to classes and aliases *)
  type pool_t = ((string, class_t) Hashtbl.t) * ((string, kind_t) Hashtbl.t)

  type context = {
    (* Definitions of aliases and classes *)
    mutable pool : pool_t;
    (* Types of objects *)
    mutable types : kind_t MapKey.t;
    (* Queue of objects to traverse *)
    mutable to_check : (Key.t * Errors.error_ctxt) list;
    (* Incomplete types were encountered *)
    mutable incomplete : bool;
  }


  (*   Convert a type to a string
       Args    :
       - actual type
       Returns :
       - string representation
  *)
  val kind_to_string : kind_t -> string
  (*   Convert a type to a string
       Args    :
       - type
       Returns :
       - string representation
  *)
  val type_to_string : t -> string

  (*   Print a pool of classes and aliases
       Args    :
       - pool
  *)
  val print_pool : pool_t -> unit

  (*   Check that classes and aliases used in a type are declared in a pool
       Args    :
       - pool
       - string representation of the type
       - type to check
  *)
  val check_pool_type : pool_t -> string -> kind_t -> unit
  (*   Check that all classes and aliases used in a pool are declared
       Args    :
       - pool
  *)
  val check_pool : pool_t -> unit

  (*   Create an empty context
       Returns :
       - a new context
  *)
  val create_context : unit -> context
  (*   Copy a context
       Args    :
       - context
       Returns :
       - copy of context
  *)
  val copy_context : context -> context
  (*   Assign a context to another
       Args    :
       - destination
       - source
  *)
  val assign_context : context -> context -> unit

  (*   Register a class in a pool
       Args    :
       - class requires strict checking
       - pool
       - name of the class
       - classes included by the class
       - members of the class
  *)
  val register_class : ?strict:bool -> pool_t -> string -> ?includes:(string list) -> (string * entry_t) list -> unit
  (*   Register an alias in a pool
       Args    :
       - pool
       - name of the alias
       - actual type
  *)
  val register_alias : pool_t -> string -> kind_t -> unit

  (*   Remove (recursively) the top-level alias of a type
       Args    :
       - pool of classes and aliases
       - type
       Returns :
       - type that is not an alias
  *)
  val remove_alias : pool_t -> kind_t -> kind_t

  (*   Get a sorted list of all variant types contained in a type
       Args    :
       - pool of classes and aliases
       - type
       Returns :
       - actual type
  *)
  val remove_variant : pool_t -> kind_t -> kind_t list

  (*   Compute the intersection of two types
       Args    :
       - pool of classes and aliases
       - type 1
       - type 2
       - error context
       Returns :
       - intersection of the two types, without alias
  *)
  val type_intersection : pool_t -> kind_t -> kind_t -> Errors.error_ctxt -> kind_t

end

