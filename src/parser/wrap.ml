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
open Errors


let raise_syntax (lexbuf : Lexing.lexbuf) (offset : BoundedInt.t option) =
  let pos = ~:(Lexing.lexeme_start lexbuf) in
  match offset with
  | Some o ->
    raise (Errors.ParseError (Printf.sprintf "syntax error at offset %d [0x%x]" (BoundedInt.to_int (pos +: o)) (BoundedInt.to_int (pos +: o))))
  | None ->
    raise (Errors.ParseError (Printf.sprintf "syntax error at offset %d [0x%x]" (BoundedInt.to_int pos) (BoundedInt.to_int pos)))

let raise_lexer (msg : string) (pos : BoundedInt.t) (offset : BoundedInt.t option) =
  match offset with
  | Some o ->
    raise (Errors.LexingError (msg, pos +: o))
  | None ->
    raise (Errors.LexingError (msg, pos))

let raise_integer (msg : string) (lexbuf : Lexing.lexbuf) (offset : BoundedInt.t option) =
  let pos = ~:(Lexing.lexeme_start lexbuf) in
  match offset with
  | Some o ->
    raise (Errors.LexingError (Printf.sprintf "integer error : %s" msg, pos +: o))
  | None ->
    raise (Errors.LexingError (Printf.sprintf "integer error : %s" msg, pos))

let raise_pdf (msg : string) (offset : BoundedInt.t option) (error_ctxt : Errors.error_ctxt) =
  match offset with
  | Some o ->
    raise (Errors.PDFError (msg, Errors.ctxt_set_pos error_ctxt o))
  | None ->
    raise (Errors.PDFError (msg, error_ctxt))


let wrap_parser f (offset : BoundedInt.t option) (lexbuf : Lexing.lexbuf) (error_ctxt : Errors.error_ctxt) =
  try
    f Lexer.token lexbuf
  with
  | Parser.Error ->
    raise_syntax lexbuf offset
  | Errors.LexingError (msg, pos) ->
    raise_lexer msg pos offset
  | BoundedInt.IntegerError msg ->
    raise_integer msg lexbuf offset
  | Errors.PDFError (msg, _) ->
    raise_pdf msg offset error_ctxt

let wrap_xrefparser f (offset : BoundedInt.t option) (lexbuf : Lexing.lexbuf) =
  try
    f Xreflexer.token lexbuf
  with
  | Xrefparser.Error ->
    raise_syntax lexbuf offset
  | Errors.LexingError (msg, pos) ->
    raise_lexer msg pos offset
  | BoundedInt.IntegerError msg ->
    raise_integer msg lexbuf offset

let wrap_strictparser f (offset : BoundedInt.t option) (lexbuf : Lexing.lexbuf) =
  try
    f Strictlexer.token lexbuf
  with
  | Strictparser.Error ->
    raise_syntax lexbuf offset
  | Errors.LexingError (msg, pos) ->
    raise_lexer msg pos offset
  | BoundedInt.IntegerError msg ->
    raise_integer msg lexbuf offset

