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


open Document
open Boundedint
open Xref
open Mapkey
open Key
open Pdfobject
open Intervals


module FetchCommon : sig

  type context = {
    (* Document of retrieved objects *)
    doc : Document.t;
    (* Input file *)
    input : in_channel;
    (* Length of input *)
    length : BoundedInt.t;
    (* Xref table *)
    xref : XRefTable.t;
    (* Set of traversed objects *)
    mutable traversed : bool MapKey.t;
    (* Set of objects inside object streams *)
    mutable decompressed : ((PDFObject.t * BoundedInt.t) MapKey.t) MapKey.t;
    (* Intervals of objects in file *)
    intervals : Key.t Intervals.t;
  }

  (*   Create a context
       Args    :
       - input file
       - length of input
       - xref table
       - intervals of objects in file
       - document of retrieved objects
       Returns :
       - context
  *)
  val make_context : in_channel -> BoundedInt.t -> XRefTable.t -> (Key.t Intervals.t) -> Document.t -> context

end


module type FetchT = sig

  (*   Fetch a normal indirect object
       Args    :
       - id of object
       - offset of object
       - traversal context
       Returns :
       - object
  *)
  val fetchobject : Key.t -> BoundedInt.t -> FetchCommon.context -> PDFObject.t

  (*   Dereference an object if it is a reference, or do nothing
       Args    :
       - input object
       - traversal context
       Returns :
       - dereferenced object
  *)
  val dereference : PDFObject.t -> FetchCommon.context -> PDFObject.t

  (*   Fetch an object and decode it if it is a stream
       Args    :
       - id of object
       - offset of object
       - traversal context
       - relax unsupported filters (warning instead of exception)
       Returns :
       - object
  *)
  val fetchdecodestream : Key.t -> BoundedInt.t -> FetchCommon.context -> bool -> PDFObject.t

end

module type FetchCompT = sig

  (*   Fetch a compressed indirect object in an object stream
       Args    :
       - key of object
       - offset of object stream
       - index in object stream
       - traversal context
       Returns :
       - object
  *)
  val fetchcompressed : Key.t -> BoundedInt.t -> BoundedInt.t -> FetchCommon.context -> PDFObject.t

  (*   Extract all objects from an object stream
       Args    :
       - content of object stream
       - id of object (to use for error report)
       - offset of object (to use for error report)
       - offset of first object inside the stream
       - number of objects inside the stream
       - traversal context
       Returns :
       - bag of objects contained in the stream
  *)
  val parseobjstm : string -> Key.t -> BoundedInt.t -> BoundedInt.t -> BoundedInt.t -> FetchCommon.context -> ((PDFObject.t * BoundedInt.t) MapKey.t)

  (*   Fetch an object stream and extract its objects
       Args    :
       - id of object
       - traversal context
       Returns :
       - bag of objects contained in the stream
  *)
  val fetchobjstm : BoundedInt.t -> FetchCommon.context -> ((PDFObject.t * BoundedInt.t) MapKey.t)

end


(*   Traverse an object and retrieve its content
     Args    :
     - id of object
     - offset of object
     - traversal context
     - fetch function
     Returns :
     - content of object
*)
val traverse_object : Key.t -> BoundedInt.t -> FetchCommon.context -> (Key.t -> BoundedInt.t -> FetchCommon.context -> PDFObject.t) -> PDFObject.t

