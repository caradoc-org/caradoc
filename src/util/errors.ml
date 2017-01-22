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

module Errors = struct

  type pos_t =
    | File of BoundedInt.t
    | Stream of Key.t * BoundedInt.t
    | Objstm of Key.t * BoundedInt.t

  type error_ctxt = {
    key : Key.t option;
    pos : pos_t option;
    entry : Entry.t;
  }


  let make_pos_file (o : BoundedInt.t) : pos_t =
    File o

  let make_pos_stream (k : Key.t) (o : BoundedInt.t) : pos_t =
    Stream (k, o)

  let make_pos_objstm (k : Key.t) (id : BoundedInt.t) : pos_t =
    Objstm (k, id)

  let pos_add_offset (p : pos_t) (o : BoundedInt.t) : pos_t =
    match p with
    | File offset ->
      File (offset +: o)
    | Stream (k, offset) ->
      Stream (k, offset +: o)
    | Objstm _ ->
      p


  let ctxt_none : error_ctxt =
    {key = None; pos = None; entry = Entry.empty}

  let make_ctxt (k : Key.t) (p : pos_t) : error_ctxt =
    {key = Some k; pos = Some p; entry = Entry.empty}

  let make_ctxt_key (k : Key.t) : error_ctxt =
    {key = Some k; pos = None; entry = Entry.empty}

  let make_ctxt_pos (p : pos_t) : error_ctxt =
    {key = None; pos = Some p; entry = Entry.empty}

  let make_ctxt_entry (k : Key.t) (e : Entry.t) : error_ctxt =
    {key = Some k; pos = None; entry = e}

  let make_ctxt_index (k : Key.t) (i : int) : error_ctxt =
    {key = Some k; pos = None; entry = Entry.make_index i}

  let make_ctxt_name (k : Key.t) (n : string) : error_ctxt =
    {key = Some k; pos = None; entry = Entry.make_name n}

  let make_ctxt_full_name (k : Key.t) (p : pos_t) (n : string) : error_ctxt =
    {key = Some k; pos = Some p; entry = Entry.make_name n}


  let ctxt_append_entry (c : error_ctxt) (e : Entry.t) : error_ctxt =
    {c with entry = Entry.append_entry c.entry e}

  let ctxt_append_index (c : error_ctxt) (i : int) : error_ctxt =
    {c with entry = Entry.append_index c.entry i}

  let ctxt_append_name (c : error_ctxt) (n : string) : error_ctxt =
    {c with entry = Entry.append_name c.entry n}

  let ctxt_set_pos (c : error_ctxt) (p : pos_t) : error_ctxt =
    {c with pos = Some p}

  let ctxt_add_offset (c : error_ctxt) (o : BoundedInt.t) : error_ctxt =
    match c.pos with
    | Some p ->
      {c with pos = Some (pos_add_offset p o)}
    | None ->
      c


  let pos_to_buf (p : pos_t) (buf : Buffer.t) : unit =
    let offset_to_buf offset buf =
      let i = BoundedInt.to_int offset in
      Buffer.add_string buf (Printf.sprintf " at offset %d [0x%x]" i i)
    in

    match p with
    | File o ->
      offset_to_buf o buf;
      Buffer.add_string buf " in file"
    | Stream (k, o) ->
      offset_to_buf o buf;
      Buffer.add_string buf (Printf.sprintf " in stream %s" (Key.to_string k))
    | Objstm (k, id) ->
      Buffer.add_string buf (Printf.sprintf " at index %s" (BoundedInt.to_string id));
      Buffer.add_string buf (Printf.sprintf " in object stream %s" (Key.to_string k))

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
        pos_to_buf p buf
      | None -> ()
    end;
    Buffer.contents buf

  let key_of_ctxt (ctxt : error_ctxt) : Key.t option =
    ctxt.key


  exception FileError of string * string
  exception LexingError of string * BoundedInt.t
  exception ParseError of string
  exception PDFError of string * error_ctxt
  exception TypeError of string * error_ctxt
  exception UnexpectedError of string


  let warning (msg : string) (ctxt : error_ctxt) : unit =
    Printf.eprintf "Warning : %s%s\n" msg (ctxt_to_string ctxt)

  let warning_or_pdf_error (warn : bool) (msg : string) (ctxt : error_ctxt) : unit =
    if warn then
      warning msg ctxt
    else
      raise (PDFError (msg, ctxt))

  let warning_or_type_error (warn : bool) (verbose : bool) (msg : string) (ctxt : error_ctxt) : unit =
    if not warn then
      raise (TypeError (msg, ctxt))
    else if verbose then
      warning msg ctxt

  let warning_or_lexing_error (warn : bool) (msg : string) (ctxt : BoundedInt.t) : unit =
    if warn then
      warning msg ctxt_none
    else
      raise (LexingError (msg, ctxt))


  let catch ~fail (f : unit -> 'a) : 'a =
    try
      f ()
    with
    | FileError (filename, msg) ->
      Printf.eprintf "Invalid file name \"%s\" : %s !\n" filename msg;
      fail ()

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

