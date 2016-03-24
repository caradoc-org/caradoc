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

(*   Calls a parse function on Lexer.token and intercept syntax errors to insert the correct offset
     Args    :
     - parse function
     - offset of the lexbuf from the beginning of the file
     - lexbuf to parse
     Returns :
     - result of the parse function
*)
val wrap_parser : ((Lexing.lexbuf -> Parser.token) -> Lexing.lexbuf -> 'a) -> (BoundedInt.t option) -> Lexing.lexbuf -> 'a

(*   Calls a parse function on Xreflexer.token and intercept syntax errors to insert the correct offset
     Args    :
     - parse function
     - offset of the lexbuf from the beginning of the file
     - lexbuf to parse
     Returns :
     - result of the parse function
*)
val wrap_xrefparser : ((Lexing.lexbuf -> Xrefparser.token) -> Lexing.lexbuf -> 'a) -> (BoundedInt.t option) -> Lexing.lexbuf -> 'a

(*   Calls a parse function on Strictlexer.token and intercept syntax errors to insert the correct offset
     Args    :
     - parse function
     - offset of the lexbuf from the beginning of the file
     - lexbuf to parse
     Returns :
     - result of the parse function
*)
val wrap_strictparser : ((Lexing.lexbuf -> Strictparser.token) -> Lexing.lexbuf -> 'a) -> (BoundedInt.t option) -> Lexing.lexbuf -> 'a

