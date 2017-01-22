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
open Setkey
open Directobject
open Indirectobject
open Graph
open Errors
open Entry
open Crypto
open Boundedint

(* Set of objects and trailers extracted from a file *)
module Document : sig

  type kind_t = Objstm | Xrefstm
  type t

  val create : unit -> t
  val mem_obj : t -> Key.t -> bool
  val find_obj : t -> Key.t -> IndirectObject.t
  val remove_ref : t -> DirectObject.t -> Errors.error_ctxt -> (IndirectObject.t * Errors.error_ctxt)

  val finalize_trailers : t -> unit
  val trailers : t -> (DirectObject.dict_t list)
  val main_trailer : t -> DirectObject.dict_t
  val add : t -> Key.t -> IndirectObject.t -> unit
  val set : t -> Key.t -> IndirectObject.t -> unit
  val add_trailer : t -> DirectObject.dict_t -> unit
  val add_objstm : t -> Key.t -> unit
  val add_xrefstm : t -> Key.t -> unit
  val add_crypto : t -> Crypto.t -> unit
  val crypto : t -> Crypto.t option

  val iter_objects : (Key.t -> IndirectObject.t -> unit) -> t -> unit
  val fold_objects : (Key.t -> IndirectObject.t -> 'a -> 'a) -> t -> 'a -> 'a
  val map_objects : (Key.t -> IndirectObject.t -> IndirectObject.t) -> t -> unit
  val iter_stms : (Key.t -> kind_t -> unit) -> t -> unit

  (*   Find the largest object number used in a document
       Args    :
       - document
       Returns :
       - maximal object number
  *)
  val max_objnum : t -> BoundedInt.t

  (*   Find all references to an object
       Args    :
       - reference to find
       - document
       Returns :
       - list of occurrences
  *)
  val find_ref : Key.t -> t -> Entry.t list MapKey.t
  (*   Find all occurrences of a PDF name
       Args    :
       - name to find (without leading slash)
       - document
       Returns :
       - list of occurrences
  *)
  val find_name : string -> t -> Entry.t list MapKey.t

  (*   Replace all undefined references by the null object
       Args    :
       - document
  *)
  val undef_refs_to_null : t -> unit

  (*   Get the closure of objects referenced by an object
       Args    :
       - document
       - object
       - current indirect object
       Returns :
       - set of references
  *)
  val ref_closure : t -> IndirectObject.t -> Key.t -> SetKey.t

  (*   Get the graph of references of a document
       Args    :
       - document
       Returns :
       - graph of references
  *)
  val graph : t -> Graph.t

  (*   Print the content of a document in a reader-friendly format
       Args    :
       - output channel
       - document
  *)
  val print : out_channel -> t -> unit

  val print_pdf : out_channel -> t -> unit

  (*   Clean up a trailer (i.e. keep only /Root /Info /ID and /Size)
       Args    :
       - new assignment of object numbers
       - trailer
       Returns :
       - sanitized version of the trailer
  *)
  val sanitize_trailer : Key.t MapKey.t -> DirectObject.dict_t -> DirectObject.dict_t

  (*   Remove references to simple direct objects
       Args    :
       - document
       - whether to simplify the /Info dictionary as well
       Returns :
       - sanitized version of the document
  *)
  val simplify_refs : t -> bool -> t

  (*   Reassign object numbers to a continuous range starting at 1
       Args    :
       - document
       Returns :
       - sanitized version of the document
  *)
  val sanitize_nums : t -> t

end

