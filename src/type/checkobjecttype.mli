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


open Type.Type
open Directobject
open Indirectobject
open Errors


module CheckObjectType : sig

  (*   Check type of an object
       Args    :
       - context
       - object
       - expected type
       - error context
       Returns :
       - real type of object (modified if object is an alias, variant or indirect object)
  *)
  val check_object : context -> IndirectObject.t -> t -> Errors.error_ctxt -> t

  (*   Check type of an object
       Args    :
       - context
       - object
       - expected type
       - error context
       Returns :
       - real type of object (modified if object is an alias, variant or indirect object)
  *)
  val check_object_direct : context -> DirectObject.t -> t -> Errors.error_ctxt -> t

  (*   Check type (alias) of an object
       Args    :
       - context
       - object
       - alias name
       - allow indirect object
       - error context
       Returns :
       - real type of object
  *)
  val check_alias : context -> IndirectObject.t -> string -> bool -> Errors.error_ctxt -> t

  (*   Check type of a dictionary
       Args    :
       - context
       - dictionary object
       - expected value type
       - error context
  *)
  val check_dict : context -> DirectObject.dict_t -> t -> Errors.error_ctxt -> unit

  (*   Check type of an array
       Args    :
       - context
       - array content
       - expected element type
       - error context
  *)
  val check_array : context -> DirectObject.t list -> t -> Errors.error_ctxt -> unit

  (*   Check type of a sized array
       Args    :
       - context
       - array content
       - expected element type
       - expected size
       - error context
  *)
  val check_array_sized : context -> DirectObject.t list -> t -> int -> Errors.error_ctxt -> unit

  (*   Check type of a sized array
       Args    :
       - context
       - array content
       - expected element type
       - set of expected sizes
       - error context
  *)
  val check_array_variant_sized : context -> DirectObject.t list -> t -> int array -> Errors.error_ctxt -> unit

  (*   Check type of a tuple array
       Args    :
       - context
       - array content
       - expected element types
       - error context
  *)
  val check_array_tuples : context -> DirectObject.t list -> t array -> Errors.error_ctxt -> unit

  (*   Check type of a difference array
       Args    :
       - context
       - array content
       - error context
  *)
  val check_array_differences : context -> DirectObject.t list -> Errors.error_ctxt -> unit

  (*   Check type of a tuple
       Args    :
       - context
       - array content
       - expected tuple element types
       - error context
  *)
  val check_tuple : context -> DirectObject.t list -> t array -> Errors.error_ctxt -> unit

  (*   Check type (variant) of an object
       Args    :
       - context
       - object
       - set of expected types
       - error context
       Returns :
       - real type of object
  *)
  val check_variant : context -> IndirectObject.t -> kind_t list -> Errors.error_ctxt -> t

  (*   Check type of an indirect object
       Args    :
       - context
       - indirect reference
       - expected type
       - error context
       Returns :
       - real type of object
  *)
  val check_indirect : context -> Key.t -> t -> Errors.error_ctxt -> t

  (*   Check type of a class
       Args    :
       - context
       - object entries
       - class name
       - error context
  *)
  val check_class : context -> DirectObject.dict_t -> string -> Errors.error_ctxt -> unit

  (*   Check all entries of a class (and included subclasses)
       Args    :
       - context
       - object entries
       - class name
       - error context
       - set of checked entries
       Returns :
       - whether subclass is strict or not
  *)
  val check_subclass : context -> DirectObject.dict_t -> string -> Errors.error_ctxt -> (string, bool) Hashtbl.t -> bool

end

