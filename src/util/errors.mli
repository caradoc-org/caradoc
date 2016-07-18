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


open Boundedint
open Key
open Entry

module Errors : sig

  type error_ctxt = {
    key : Key.t option;
    pos : BoundedInt.t option;
    entry : Entry.t;
  }

  val ctxt_none : error_ctxt
  val make_ctxt : Key.t -> BoundedInt.t -> error_ctxt
  val make_ctxt_key : Key.t -> error_ctxt
  val make_ctxt_pos : BoundedInt.t -> error_ctxt
  val make_ctxt_entry : Key.t -> Entry.t -> error_ctxt
  val make_ctxt_index : Key.t -> int -> error_ctxt
  val make_ctxt_name : Key.t -> string -> error_ctxt
  val make_ctxt_full_name : Key.t -> BoundedInt.t -> string -> error_ctxt

  val ctxt_append_entry : error_ctxt -> Entry.t -> error_ctxt
  val ctxt_append_index : error_ctxt -> int -> error_ctxt
  val ctxt_append_name : error_ctxt -> string -> error_ctxt
  val ctxt_set_pos : error_ctxt -> BoundedInt.t -> error_ctxt

  val ctxt_to_string : error_ctxt -> string

  exception LexingError of string * BoundedInt.t
  exception ParseError of string
  exception PDFError of string * error_ctxt
  exception TypeError of string * error_ctxt
  exception UnexpectedError of string


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

