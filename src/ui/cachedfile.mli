(*****************************************************************************)
(*  Caradoc: a PDF parser and validator                                      *)
(*  Copyright (C) 2017 Guillaume Endignoux                                   *)
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


open Directobject
open Indirectobject
open Key
open Errors
open Stats

module CachedFile : sig

  type t

  val make : unit -> t
  val make_file : string -> t

  val filename : t -> string
  val contents : t -> string

  val fetch_stats : t -> Stats.t
  val fetch_trailers : t -> DirectObject.dict_t list
  val fetch_object : t -> Key.t -> IndirectObject.t
  val decode_stream : t -> Key.t -> string
  val fetch_refs : t -> Key.t -> Errors.error_ctxt array

end

