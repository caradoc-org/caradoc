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

module Errors = struct

  type error_ctxt = {
    key : Key.t option;
    pos : BoundedInt.t option;
  }

  let make_ctxt (k : Key.t) (p : BoundedInt.t) : error_ctxt =
    {key = Some k; pos = Some p}

  let make_ctxt_key (k : Key.t) : error_ctxt =
    {key = Some k; pos = None}

  let make_ctxt_pos (p : BoundedInt.t) : error_ctxt =
    {key = None; pos = Some p}

  let ctxt_none : error_ctxt =
    {key = None; pos = None}

  exception LexingError of string * BoundedInt.t
  exception ParseError of string
  exception PDFError of string * error_ctxt
  exception TypeError of string * Key.t * string
  exception UnexpectedError of string


  let catch ~fail (f : unit -> 'a) : 'a =
    try
      f ()
    with
    | LexingError (msg, pos) ->
      Printf.eprintf "Lexing error at offset %d [0x%x] : %s !\n" (BoundedInt.to_int pos) (BoundedInt.to_int pos) msg;
      fail ()
    | ParseError msg ->
      Printf.eprintf "Parse error : %s !\n" msg;
      fail ()

    | BoundedInt.IntegerError msg ->
      Printf.eprintf "Integer error : %s !\n" msg;
      fail ()
    | Convert.ConvertError msg ->
      Printf.eprintf "Convert error : %s !\n" msg;
      fail ()
    | PDFError (msg, ctxt) ->
      Printf.eprintf "PDF error : %s" msg;
      begin
        match ctxt.key with
        | Some k ->
          Printf.eprintf " for object %s" (Key.to_string k)
        | _ -> ()
      end;
      begin
        match ctxt.pos with
        | Some p ->
          Printf.eprintf " at offset %d [0x%x]" (BoundedInt.to_int p) (BoundedInt.to_int p);
        | _ -> ()
      end;
      Printf.eprintf " !\n";
      fail ()

    | TypeError (msg, key, entry) ->
      Printf.eprintf "Type error : %s for object %s" msg (Key.to_string key);
      if entry <> "" then
        Printf.eprintf " at entry %s" entry;
      Printf.eprintf " !\n";
      fail ()

    | UnexpectedError msg ->
      Printf.eprintf "UNEXPECTED ERROR : %s !\n" msg;
      fail ()


  let print (f : unit -> unit) : unit =
    catch ~fail:(fun () -> ()) f

end

