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
open Setkey
open Key
open Mapkey
open Errors
open Entry
open Crypto

module DirectObject : sig

  type int_t = BoundedInt.t
  type real_t = string

  type 'a dict

  type t =
    | Null
    | Bool of bool
    | Int of int_t
    | Real of real_t
    | String of string
    | Name of string
    | Array of t list
    | Dictionary of t dict
    | Reference of Key.t

  type dict_t = t dict


  val dict_create : unit -> dict_t
  val dict_copy : dict_t -> dict_t
  val dict_length : dict_t -> int
  val dict_singleton : (string * t) -> dict_t
  val dict_add : bool -> dict_t -> (string * t) -> unit
  val dict_set : dict_t -> (string * t) -> unit

  val dict_mem : dict_t -> string -> bool
  val dict_find : dict_t -> string -> t
  val dict_iter : (string -> t -> unit) -> dict_t -> unit
  val dict_iter_sorted : (string -> t -> unit) -> dict_t -> unit
  val dict_map_key : (string -> t -> t) -> dict_t -> dict_t
  val dict_map : (t -> t) -> dict_t -> dict_t
  val dict_fold : (string -> t -> 'a -> 'a) -> dict_t -> 'a -> 'a

  (*   Keep only keys that belong to a list
       Args    :
       - list of keys to keep
       - dictionary
       Returns :
       - simplified dictionary
  *)
  val dict_simplify : string list -> dict_t -> dict_t


  val is_array : t -> bool

  (*   Decrypt an object
       Args    :
       - decryption function
       - object
       Returns :
       - decrypted object
  *)
  val decrypt : Crypto.decrypt_t -> t -> t
  (*   Decrypt a dictionary
       Args    :
       - decryption function
       - dictionary
       Returns :
       - decrypted dictionary
  *)
  val decrypt_dict : Crypto.decrypt_t -> dict_t -> dict_t

  (*   Convert an object to a string
       Args    :
       - object
       Returns :
       - string representation of this object
  *)
  val to_string : t -> string
  (*   Convert a dictionary to a string
       Args    :
       - dictionary
       Returns :
       - string representation of this object
  *)
  val dict_to_string : dict_t -> string

  (*   Convert an object to a string with selection highlighted in console
       Args    :
       - object
       - selection to highlight
       Returns :
       - string representation of this object
  *)
  val to_string_hl : t -> Entry.select_t -> string
  (*   Convert a dictionary to a string with selection highlighted in console
       Args    :
       - dictionary
       - selection to highlight
       Returns :
       - string representation of this object
  *)
  val dict_to_string_hl : dict_t -> Entry.select_t -> string
  (*   Convert a dictionary to a string with highlighted selection and append it to a buffer
       Args    :
       - buffer
       - dictionary
       - selection to highlight
  *)
  val dict_to_string_buf : Buffer.t -> dict_t -> Entry.select_t -> unit

  (*   Check if an object needs a space before it in a PDF file
       Args    :
       - object
       Returns ;
       - whether object needs a space before
  *)
  val need_space_before : t -> bool
  (*   Check if an object needs a space after it in a PDF file
       Args    :
       - object
       Returns ;
       - whether object needs a space after
  *)
  val need_space_after : t -> bool

  (*   Convert an object to its representation in PDF syntax
       Args    :
       - object
       Returns :
       - string representation of this object
  *)
  val to_pdf : t -> string
  (*   Convert a dictionary to its representation in PDF syntax
       Args    :
       - dictionary
       Returns :
       - string representation of this object
  *)
  val dict_to_pdf : dict_t -> string
  (*   Convert a dictionary to its representation in PDF syntax and append it to a buffer
       Args    :
       - buffer
       - dictionary
       Returns :
  *)
  val dict_to_pdf_buf : Buffer.t -> dict_t -> unit

  (*   Find a reference in an object
       Args    :
       - reference to find
       - object
       Returns :
       - list of occurrences in object
  *)
  val find_ref : Key.t -> t -> Entry.t list
  (*   Find a reference in an object
       Args    :
       - reference to find
       - dictionary
       Returns :
       - list of occurrences in object
  *)
  val find_ref_dict : Key.t -> dict_t -> Entry.t list
  (*   Find a name in an object
       Args    :
       - name to find (without leading slash)
       - object
       Returns :
       - list of occurrences in object
  *)
  val find_name : string -> t -> Entry.t list
  (*   Find a name in an object
       Args    :
       - name to find (without leading slash)
       - dictionary
       Returns :
       - list of occurrences in object
  *)
  val find_name_dict : string -> dict_t -> Entry.t list

  (*   Get objects referenced by an object
       Args    :
       - object
       Returns :
       - mapping of references to entries
  *)
  val refs : t -> Entry.t MapKey.t
  (*   Get objects referenced by a dictionary
       Args    :
       - dictionary
       Returns :
       - mapping of references to entries
  *)
  val refs_dict : dict_t -> Entry.t MapKey.t

  (*   Replace all undefined references by the null object
       Args    :
       - set of defined references
       - list of warning contexts
       - error context
       - object
       Returns :
       - converted object
  *)
  val undef_refs_to_null : 'a MapKey.t -> (Key.t * Errors.error_ctxt) list ref -> Errors.error_ctxt -> t -> t
  (*   Change all references according to a conversion table
       Args    :
       - set of defined references
       - list of warning contexts
       - error context
       - dictionary
       Returns :
       - converted dictionary
  *)
  val undef_refs_to_null_dict : 'a MapKey.t -> (Key.t * Errors.error_ctxt) list ref -> Errors.error_ctxt -> dict_t -> dict_t

  (*   Change all references according to a conversion table
       Args    :
       - conversion table
       - error context
       - object
       Returns :
       - converted object
  *)
  val relink : Key.t MapKey.t -> Errors.error_ctxt -> t -> t
  (*   Change all references according to a conversion table
       Args    :
       - conversion table
       - error context
       - dictionary
       Returns :
       - converted dictionary
  *)
  val relink_dict : Key.t MapKey.t -> Errors.error_ctxt -> dict_t -> dict_t

  (*   Transform a reference into a direct object if it is simple
       Args    :
       - indirect key
       - object
       Returns :
       - simplified reference
  *)
  val simple_ref : Key.t -> t -> t


  (*   Apply function if object is non-null
       Args    :
       - object
       - function to apply on non-null object
  *)
  val apply_not_null : t -> (t -> unit) -> unit

  (*   Check and extract a boolean from an object, or raise an exception
       Args    :
       - [optional] default value for null object
       - error message
       - error context
       - object
       Returns :
       - boolean value of object
  *)
  val get_bool :
    ?default:bool -> unit ->
    string -> Errors.error_ctxt ->
    t -> bool

  (*   Check and extract an integer from an object, or raise an exception
       Args    :
       - [optional] default value for null object
       - error message
       - error context
       - object
       Returns :
       - integer value of object
  *)
  val get_int :
    ?default:BoundedInt.t -> unit ->
    string -> Errors.error_ctxt ->
    t -> BoundedInt.t

  (*   Check and extract a (strictly) positive integer from an object, or raise an exception
       Args    :
       - [optional] default value for null object
       - error message
       - error context
       - object
       Returns :
       - integer value of object
  *)
  val get_positive_int :
    ?default:BoundedInt.t -> unit ->
    string -> Errors.error_ctxt ->
    t -> BoundedInt.t

  (*   Check and extract a non-negative integer from an object, or raise an exception
       Args    :
       - [optional] default value for null object
       - error message
       - error context
       - object
       Returns :
       - integer value of object
  *)
  val get_nonnegative_int :
    ?default:BoundedInt.t -> unit ->
    string -> Errors.error_ctxt ->
    t -> BoundedInt.t

  (*   Check and extract a string from an object, or raise an exception
       Args    :
       - [optional] default value for null object
       - error message
       - error context
       - object
       Returns :
       - string value of object
  *)
  val get_string :
    ?default:string -> unit ->
    string -> Errors.error_ctxt ->
    t -> string

  (*   Check and extract a name from an object, or raise an exception
       Args    :
       - [optional] default value for null object
       - error message
       - error context
       - object
       Returns :
       - name value of object
  *)
  val get_name :
    ?default:string -> unit ->
    string -> Errors.error_ctxt ->
    t -> string

  (*   Check and extract a reference from an object, or raise an exception
       Args    :
       - [optional] default value for null object
       - error message
       - error context
       - object
       Returns :
       - key value of reference
  *)
  val get_reference :
    ?default:Key.t -> unit ->
    string -> Errors.error_ctxt ->
    t -> Key.t

  (*   Check and extract a dictionary from an object, or raise an exception
       Args    :
       - [optional] default value for null object
       - error message
       - error context
       - object
       Returns :
       - dictionary value of object
  *)
  val get_dict :
    ?default:dict_t -> unit ->
    string -> Errors.error_ctxt ->
    t -> dict_t

  (*   Check and extract an array from an object, or raise an exception
       Args    :
       - [optional] default value for null object
       - [optional] accept an object as a 1-element array
       - [optional] required length of the array
       - error message
       - error context
       - object
       Returns :
       - array value of object
  *)
  val get_array :
    ?default:(t list) -> ?accept_one:bool -> ?length:int -> unit ->
    string -> Errors.error_ctxt ->
    t -> (t array)

  (*   Check and extract an array from an object, or raise an exception
       Args    :
       - [optional] default value for null object
       - [optional] accept an object as a 1-element array
       - [optional] required length of the array
       - error message
       - error context
       - function to transform the values of the array (e.g. getInt for an array of ints)
       - object
       Returns :
       - transformed array value of object
  *)
  val get_array_of :
    ?default:(t list) -> ?accept_one:bool -> ?length:int -> unit ->
    string -> Errors.error_ctxt ->
    transform:(string -> Errors.error_ctxt -> t -> 'a) ->
    t -> ('a array)

end

