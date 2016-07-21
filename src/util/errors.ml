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

module Errors = struct

  type error_ctxt = {
    key : Key.t option;
    pos : BoundedInt.t option;
    entry : Entry.t;
  }


  let ctxt_none : error_ctxt =
    {key = None; pos = None; entry = Entry.empty}

  let make_ctxt (k : Key.t) (p : BoundedInt.t) : error_ctxt =
    {key = Some k; pos = Some p; entry = Entry.empty}

  let make_ctxt_key (k : Key.t) : error_ctxt =
    {key = Some k; pos = None; entry = Entry.empty}

  let make_ctxt_pos (p : BoundedInt.t) : error_ctxt =
    {key = None; pos = Some p; entry = Entry.empty}

  let make_ctxt_entry (k : Key.t) (e : Entry.t) : error_ctxt =
    {key = Some k; pos = None; entry = e}

  let make_ctxt_index (k : Key.t) (i : int) : error_ctxt =
    {key = Some k; pos = None; entry = Entry.make_index i}

  let make_ctxt_name (k : Key.t) (n : string) : error_ctxt =
    {key = Some k; pos = None; entry = Entry.make_name n}

  let make_ctxt_full_name (k : Key.t) (p : BoundedInt.t) (n : string) : error_ctxt =
    {key = Some k; pos = Some p; entry = Entry.make_name n}


  let ctxt_append_entry (c : error_ctxt) (e : Entry.t) : error_ctxt =
    {c with entry = Entry.append_entry c.entry e}

  let ctxt_append_index (c : error_ctxt) (i : int) : error_ctxt =
    {c with entry = Entry.append_index c.entry i}

  let ctxt_append_name (c : error_ctxt) (n : string) : error_ctxt =
    {c with entry = Entry.append_name c.entry n}

  let ctxt_set_pos (c : error_ctxt) (p : BoundedInt.t) : error_ctxt =
    {c with pos = Some p}


  let ctxt_to_string (ctxt : error_ctxt) : string =
    let buf = Buffer.create 16 in
    begin
      match ctxt.key with
      | Some k ->
        Buffer.add_string buf " in object ";
        Buffer.add_string buf (Key.to_string k)
      | None -> ()
    end;
    if not (Entry.is_empty ctxt.entry) then (
      Buffer.add_string buf " at entry ";
      Buffer.add_string buf (Entry.to_string ctxt.entry)
    );
    begin
      match ctxt.pos with
      | Some p ->
        Buffer.add_string buf (Printf.sprintf " at offset %d [0x%x]" (BoundedInt.to_int p) (BoundedInt.to_int p));
      | None -> ()
    end;
    Buffer.contents buf


  exception LexingError of string * BoundedInt.t
  exception ParseError of string
  exception PDFError of string * error_ctxt
  exception TypeError of string * error_ctxt
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
      Printf.eprintf "PDF error : %s%s !\n" msg (ctxt_to_string ctxt);
      fail ()

    | TypeError (msg, ctxt) ->
      Printf.eprintf "Type error : %s%s !\n" msg (ctxt_to_string ctxt);
      fail ()

    | UnexpectedError msg ->
      Printf.eprintf "UNEXPECTED ERROR : %s !\n" msg;
      fail ()


  let print (f : unit -> unit) : unit =
    catch ~fail:(fun () -> ()) f

end

