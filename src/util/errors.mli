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
open Key
open Entry

module Errors : sig

  type pos_t
  type error_ctxt

  val make_pos_file : BoundedInt.t -> pos_t
  val make_pos_stream : Key.t -> BoundedInt.t -> pos_t
  val make_pos_objstm : Key.t -> BoundedInt.t -> pos_t
  val pos_add_offset : pos_t -> BoundedInt.t -> pos_t

  val ctxt_none : error_ctxt
  val make_ctxt : Key.t -> pos_t -> error_ctxt
  val make_ctxt_key : Key.t -> error_ctxt
  val make_ctxt_pos : pos_t -> error_ctxt
  val make_ctxt_entry : Key.t -> Entry.t -> error_ctxt
  val make_ctxt_index : Key.t -> int -> error_ctxt
  val make_ctxt_name : Key.t -> string -> error_ctxt
  val make_ctxt_full_name : Key.t -> pos_t -> string -> error_ctxt

  val ctxt_append_entry : error_ctxt -> Entry.t -> error_ctxt
  val ctxt_append_index : error_ctxt -> int -> error_ctxt
  val ctxt_append_name : error_ctxt -> string -> error_ctxt
  val ctxt_set_pos : error_ctxt -> pos_t -> error_ctxt
  val ctxt_add_offset : error_ctxt -> BoundedInt.t -> error_ctxt

  val ctxt_to_string : error_ctxt -> string
  val key_of_ctxt : error_ctxt -> Key.t option

  exception FileError of string * string
  exception LexingError of string * BoundedInt.t
  exception ParseError of string
  exception PDFError of string * error_ctxt
  exception TypeError of string * error_ctxt
  exception UnexpectedError of string


  (*   Print a warning
       Args    :
       - error message
       - error context
  *)
  val warning : string -> error_ctxt -> unit
  (*   Print a warning if a predicate is true, raise a PDFError otherwise
       Args    :
       - whether to issue a warning
       - error message
       - error context
  *)
  val warning_or_pdf_error : bool -> string -> error_ctxt -> unit
  (*   Print a warning if a predicate is true, raise a TypeError otherwise
       Args    :
       - whether to issue a warning
       - verbose
       - error message
       - error context
  *)
  val warning_or_type_error : bool -> bool -> string -> error_ctxt -> unit
  (*   Print a warning if a predicate is true, raise a LexingError otherwise
       Args    :
       - whether to issue a warning
       - error message
       - error context
  *)
  val warning_or_lexing_error : bool -> string -> BoundedInt.t -> unit

  (*   Call a function and call fail if an exception is caught
       Args    :
       - function to call in case of failure
       - function to call
  *)
  val catch : fail:(unit -> 'a) -> (unit -> 'a) -> 'a

  (*   Call a function and print the error to stderr if an exception is caught
       Args    :
       - function to call
  *)
  val print : (unit -> unit) -> unit

end

