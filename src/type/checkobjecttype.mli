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


open Type.Type
open Pdfobject


module CheckObjectType : sig

  (*   Check type of an object
       Args    :
       - context
       - object
       - expected type
       - object number
       - entry in object
       Returns :
       - real type of object (modified if object is an alias, variant or indirect object)
  *)
  val check_object : context -> PDFObject.t -> t -> Key.t -> string -> t

  (*   Check type (alias) of an object
       Args    :
       - context
       - object
       - alias name
       - allow indirect object
       - object number
       - entry in object
       Returns :
       - real type of object
  *)
  val check_alias : context -> PDFObject.t -> string -> bool -> Key.t -> string -> t

  (*   Check type of a dictionary
       Args    :
       - context
       - dictionary object
       - expected value type
       - object number
       - entry in object
  *)
  val check_dict : context -> PDFObject.dict_t -> t -> Key.t -> string -> unit

  (*   Check type of an array
       Args    :
       - context
       - array content
       - expected element type
       - object number
       - entry in object
  *)
  val check_array : context -> PDFObject.t list -> t -> Key.t -> string -> unit

  (*   Check type of a sized array
       Args    :
       - context
       - array content
       - expected element type
       - expected size
       - object number
       - entry in object
  *)
  val check_array_sized : context -> PDFObject.t list -> t -> int -> Key.t -> string -> unit

  (*   Check type of a sized array
       Args    :
       - context
       - array content
       - expected element type
       - set of expected sizes
       - object number
       - entry in object
  *)
  val check_array_variant_sized : context -> PDFObject.t list -> t -> int array -> Key.t -> string -> unit

  (*   Check type of a tuple array
       Args    :
       - context
       - array content
       - expected element types
       - object number
       - entry in object
  *)
  val check_array_tuples : context -> PDFObject.t list -> t array -> Key.t -> string -> unit

  (*   Check type of a difference array
       Args    :
       - context
       - array content
       - object number
       - entry in object
  *)
  val check_array_differences : context -> PDFObject.t list -> Key.t -> string -> unit

  (*   Check type of a tuple
       Args    :
       - context
       - array content
       - expected tuple element types
       - object number
       - entry in object
  *)
  val check_tuple : context -> PDFObject.t list -> t array -> Key.t -> string -> unit

  (*   Check type (variant) of an object
       Args    :
       - context
       - object
       - set of expected types
       - object number
       - entry in object
       Returns :
       - real type of object
  *)
  val check_variant : context -> PDFObject.t -> kind_t list -> Key.t -> string -> t

  (*   Check type of an indirect object
       Args    :
       - context
       - indirect reference
       - expected type
       - object number
       - entry in object
       Returns :
       - real type of object
  *)
  val check_indirect : context -> Key.t -> t -> Key.t -> string -> t

  (*   Check type of a class
       Args    :
       - context
       - object entries
       - class name
       - object number
       - entry in object
  *)
  val check_class : context -> PDFObject.dict_t -> string -> Key.t -> string -> unit

  (*   Check all entries of a class (and included subclasses)
       Args    :
       - context
       - object entries
       - class name
       - object number
       - entry in object
       - set of checked entries
       Returns :
       - whether subclass is strict or not
  *)
  val check_subclass : context -> PDFObject.dict_t -> string -> Key.t -> string -> (string, bool) Hashtbl.t -> bool

end

