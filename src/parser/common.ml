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

module Common = struct

  let print_lexbuf (name : string) (lexbuf : Lexing.lexbuf) : unit =
    print_string name;
    print_string " = { start_p = ";
    print_int lexbuf.Lexing.lex_start_p.Lexing.pos_cnum;
    print_string ", curr_p = ";
    print_int lexbuf.Lexing.lex_curr_p.Lexing.pos_cnum;
    print_string ", curr_pos = ";
    print_int lexbuf.Lexing.lex_curr_pos;
    print_string "}";
    print_newline ()


  let input_substr (input : in_channel) (pos : BoundedInt.t) (len : BoundedInt.t) : string =
    seek_in input (BoundedInt.to_int pos);
    let buf = Buffer.create (BoundedInt.to_int len) in
    Buffer.add_channel buf input (BoundedInt.to_int len);
    Buffer.contents buf

  (* Rewind lex buffer n chars back *)
  let rewind (lexbuf : Lexing.lexbuf) (n : int) : unit =
    if lexbuf.Lexing.lex_curr_pos < n then
      raise (Invalid_argument ("index out of bounds"));

    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_curr_pos - n;
    let curpos = lexbuf.Lexing.lex_curr_p in
    lexbuf.Lexing.lex_curr_p <- { curpos with Lexing.pos_cnum = curpos.Lexing.pos_cnum - n }

end

