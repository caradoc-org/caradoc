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
open Pdfstream
open Directobject
open Entry
open Crypto

module IndirectObject : sig

  type t =
    | Direct of DirectObject.t
    | Stream of PDFStream.t

  (* Partial object obtained after first step of parsing in relaxed mode *)
  type partial_t =
    (* Complete object *)
    | Complete of DirectObject.t
    (* Incomplete stream (parsed dictionary + offset of data) *)
    | StreamOffset of DirectObject.dict_t * BoundedInt.t


  (*   Decrypt an object
       Args    :
       - encryption parameters
       - object identifier
       - object
       Returns :
       - decrypted object
  *)
  val decrypt : Crypto.t -> Key.t -> t -> t

  (*   Convert an object to a string
       Args    :
       - object
       Returns :
       - string representation of this object
  *)
  val to_string : t -> string
  (*   Convert an object to a string with selection highlighted in console
       Args    :
       - object
       - selection to highlight
       Returns :
       - string representation of this object
  *)
  val to_string_hl : t -> Entry.select_t -> string

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

  (*   Find a reference in an object
       Args    :
       - reference to find
       - object
       Returns :
       - list of occurrences in object
  *)
  val find_ref : Key.t -> t -> Entry.t list
  (*   Find a name in an object
       Args    :
       - name to find (without leading slash)
       - object
       Returns :
       - list of occurrences in object
  *)
  val find_name : string -> t -> Entry.t list

  (*   Get objects referenced by an object
       Args    :
       - object
       Returns :
       - mapping of references to entries
  *)
  val refs : t -> Entry.t MapKey.t

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
       - conversion table
       - error context
       - object
       Returns :
       - converted object
  *)
  val relink : Key.t MapKey.t -> Errors.error_ctxt -> t -> t

  (*   Transform a reference into a direct object if it is simple
       Args    :
       - indirect key
       - object
       Returns :
       - simplified reference
  *)
  val simple_ref : Key.t -> t -> DirectObject.t
  (*   Simplify all references in an object
       Args    :
       - table of objects
       - error context
       - object
       Returns :
       - simplified object
  *)
  val simplify_refs : t MapKey.t -> Errors.error_ctxt -> t -> t
  (*   Simplify all references in a dictionary
       Args    :
       - table of objects
       - error context
       - object
       Returns :
       - simplified object
  *)
  val simplify_refs_dict : t MapKey.t -> Errors.error_ctxt -> DirectObject.dict_t -> DirectObject.dict_t


  (*   Check and extract a direct object, or raise an exception
       Args    :
       - error message
       - error context
       - object
       Returns :
       - integer value of object
  *)
  val get_direct :
    string -> Errors.error_ctxt ->
    t -> DirectObject.t

  (*   Check and extract a direct object, or raise an exception
       Args    :
       - error message
       - error context
       - function to transform the direct object
       - object
       Returns :
       - integer value of object
  *)
  val get_direct_of :
    string -> Errors.error_ctxt ->
    transform:(string -> Errors.error_ctxt -> DirectObject.t -> 'a) ->
    t -> 'a

  (*   Check and extract a stream from an object, or raise an exception
       Args    :
       - error message
       - error context
       - object
       Returns :
       - stream
  *)
  val get_stream :
    string -> Errors.error_ctxt ->
    t -> PDFStream.t

  (*   Extract a dictionary from an object (direct dictionary or stream dictionary)
       Args    :
       - object
       Returns :
       - dictionary if found
  *)
  val get_dict : t -> DirectObject.dict_t option

end

