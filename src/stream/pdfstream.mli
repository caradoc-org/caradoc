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


open Key
open Boundedint
open Directobject
open Errors
open Entry
open Crypto

module PDFStream : sig

  type t

  (*   Create a stream from encoded data
       Args    :
       - stream dictionary
       - encoded data
       Returns :
       - stream
  *)
  val make_encoded : DirectObject.dict_t -> string -> t

  (*   Extract the dictionary from a stream
       Args    :
       - stream
       Returns :
       - stream dictionary
  *)
  val get_dict : t -> DirectObject.dict_t

  (*   Set the dictionary of a stream
       Args    :
       - stream
       Returns :
       - stream dictionary
  *)
  val set_dict : t -> DirectObject.dict_t -> t

  (*   Get the encoded data of a stream
       Args    :
       - stream
       Returns :
       - encoded data
  *)
  val get_encoded : t -> string

  (*   Tell if a stream is decoded
       Args    :
       - stream
       Returns :
       - decoded status
  *)
  val is_decoded : t -> bool

  (*   Decrypt a stream
       Args    :
       - encryption parameters
       - object identifier
       - stream
       Returns :
       - decrypted stream
  *)
  val decrypt : Crypto.t -> Key.t -> t -> t

  (*   Convert a stream to a string
       Args    :
       - stream
       Returns :
       - string representation of this stream
  *)
  val to_string : t -> string
  (*   Convert a stream to a string with selection highlighted in console
       Args    :
       - stream
       - selection to highlight
       Returns :
       - string representation of this stream
  *)
  val to_string_hl : t -> Entry.select_t -> string

  (*   Convert a stream to its representation in PDF syntax
       Args    :
       - stream
       Returns :
       - string representation of this stream
  *)
  val to_pdf : t -> string

  (*   Try to decode a stream
       Args    :
       - stream
       - error context
       - relax unsupported filters (warning instead of exception)
       Returns :
       - success
  *)
  val decode : t -> Errors.error_ctxt -> bool -> bool

  (*   Get the decoded data of a stream (or raise an exception)
       Args    :
       - stream
       - error context
       Returns :
       - decoded data
  *)
  val get_decoded : t -> Errors.error_ctxt -> string

  (*   Create a stream with empty dictionary from decoded data
       Args    :
       - decoded data
       - error context
       - filter to encode with (empty string for no filter)
       Returns :
       - stream
  *)
  val make_contents : string -> Errors.error_ctxt -> string -> t

  (*   Try to reencode a stream
       Args    :
       - stream
       - error context
       - relax unsupported filters (warning instead of exception)
       - filter to reencode with (empty string for no filter)
       Returns :
       - reencoded stream
       - success
  *)
  val reencode : t -> Errors.error_ctxt -> bool -> string -> (t * bool)

end

