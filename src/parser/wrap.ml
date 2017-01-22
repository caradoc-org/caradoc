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
open Errors


let raise_syntax (off : BoundedInt.t) (error_ctxt : Errors.error_ctxt) =
  raise (Errors.PDFError ("Syntax error", Errors.ctxt_add_offset error_ctxt off))

let raise_syntax_content (off : BoundedInt.t) (error_ctxt : Errors.error_ctxt) =
  raise (Errors.PDFError ("Syntax error in content stream", Errors.ctxt_add_offset error_ctxt off))

let raise_lexer (msg : string) (off : BoundedInt.t) (error_ctxt : Errors.error_ctxt) =
  raise (Errors.PDFError (Printf.sprintf "Lexing error : %s" msg, Errors.ctxt_add_offset error_ctxt off))

let raise_integer (msg : string) (off : BoundedInt.t) (error_ctxt : Errors.error_ctxt) =
  raise (Errors.PDFError (Printf.sprintf "Lexing error : integer error : %s" msg, Errors.ctxt_add_offset error_ctxt off))

let raise_pdf (msg : string) (error_ctxt : Errors.error_ctxt) =
  raise (Errors.PDFError (msg, error_ctxt))


let wrap_parser f (lexbuf : Lexing.lexbuf) (error_ctxt : Errors.error_ctxt) =
  let getpos = fun () -> ~:(Lexing.lexeme_start lexbuf) in
  try
    f Lexer.token lexbuf
  with
  | Parser.Error ->
    raise_syntax (getpos ()) error_ctxt
  | Errors.LexingError (msg, off) ->
    raise_lexer msg off error_ctxt
  | BoundedInt.IntegerError msg ->
    raise_integer msg (getpos ()) error_ctxt
  | Errors.PDFError (msg, ctxt) when ctxt = Errors.ctxt_none ->
    raise_pdf msg error_ctxt

let wrap_xrefparser f (lexbuf : Lexing.lexbuf) (error_ctxt : Errors.error_ctxt) =
  let getpos = fun () -> ~:(Lexing.lexeme_start lexbuf) in
  try
    f Xreflexer.token lexbuf
  with
  | Xrefparser.Error ->
    raise_syntax (getpos ()) error_ctxt
  | Errors.LexingError (msg, pos) ->
    raise_lexer msg pos error_ctxt
  | BoundedInt.IntegerError msg ->
    raise_integer msg (getpos ()) error_ctxt
  | Errors.PDFError (msg, ctxt) when ctxt = Errors.ctxt_none ->
    raise_pdf msg error_ctxt

let wrap_strictparser f (lexbuf : Lexing.lexbuf) (error_ctxt : Errors.error_ctxt) =
  let getpos = fun () -> ~:(Lexing.lexeme_start lexbuf) in
  try
    f Strictlexer.token lexbuf
  with
  | Strictparser.Error ->
    raise_syntax (getpos ()) error_ctxt
  | Errors.LexingError (msg, pos) ->
    raise_lexer msg pos error_ctxt
  | BoundedInt.IntegerError msg ->
    raise_integer msg (getpos ()) error_ctxt
  | Errors.PDFError (msg, ctxt) when ctxt = Errors.ctxt_none ->
    raise_pdf msg error_ctxt

let wrap_contentparser f token (getpos : unit -> BoundedInt.t) (lexbuf : Lexing.lexbuf) (error_ctxt : Errors.error_ctxt) =
  try
    f token lexbuf
  with
  | Contentparser.Error ->
    raise_syntax_content (getpos ()) error_ctxt
  | Errors.LexingError (msg, pos) ->
    raise_lexer msg pos error_ctxt
  | BoundedInt.IntegerError msg ->
    raise_integer msg (getpos ()) error_ctxt
  | Errors.PDFError (msg, ctxt) when ctxt = Errors.ctxt_none ->
    raise_pdf msg error_ctxt

