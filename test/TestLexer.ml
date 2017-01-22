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


open OUnit
open Lexer
open Parser
open Errors
open Boundedint


let get_token f text =
  let lexbuf = Lexing.from_string text in
  f lexbuf

let check_token text f tok =
  assert_equal (get_token f text) tok


let rec next_tokens f lexbuf =
  let tok = f lexbuf in
  match tok with
  | EOF ->
    []
  | _ ->
    tok::(next_tokens f lexbuf)

let get_tokens f text =
  let lexbuf = Lexing.from_string text in
  next_tokens f lexbuf

let check_tokens text f toks =
  assert_equal (get_tokens f text) toks


let check_lexing_error text f msg pos =
  assert_raises (Errors.LexingError (msg, ~:pos)) (fun () -> f text)


let tests =
  "Lexer" >:::
  [
    "token" >:::
    [
      "null" >:::
      [
        "(1)" >:: (fun _ -> check_tokens "null" Lexer.token
                      [NULL]) ;

        "(2)" >:: (fun _ -> check_lexing_error "Null" (get_tokens Lexer.token)
                      "unexpected word" 0) ;
        "(3)" >:: (fun _ -> check_lexing_error "nUlL" (get_tokens Lexer.token)
                      "unexpected word" 0) ;
      ] ;

      "bool" >:::
      [
        "(1)" >:: (fun _ -> check_tokens "true false" Lexer.token
                      [BOOL true ; BOOL false]) ;

        "(2)" >:: (fun _ -> check_lexing_error "tRue" (get_tokens Lexer.token)
                      "unexpected word" 0) ;
        "(3)" >:: (fun _ -> check_lexing_error "FalsE" (get_tokens Lexer.token)
                      "unexpected word" 0) ;
      ] ;

      "int" >:::
      [
        "(1)" >:: (fun _ -> check_tokens "123 -45 +6789 -0 +0" Lexer.token
                      [INT ~:123 ; INT ~:(-45) ; INT ~:6789 ; INT ~:0 ; INT ~:0]) ;
        "(2)" >:: (fun _ -> check_tokens "2147483647 -2147483647" Lexer.token
                      (* +- (2^31 - 1) *)
                      [INT (BoundedInt.of_int64 2147483647L) ; INT (BoundedInt.of_int64 (-2147483647L))]) ;
        "(3)" >:: (fun _ -> check_tokens "123+456-789" Lexer.token
                      [INT ~:123 ; INT ~:456 ; INT ~:(-789)]) ;
      ] ;

      "real" >:::
      [
        "(1)" >:: (fun _ -> check_tokens "1. .23 45.67 800.00 000.009" Lexer.token
                      [REAL "1." ; REAL ".23" ; REAL "45.67" ; REAL "800.00" ; REAL "000.009"]) ;

        "(2)" >:: (fun _ -> check_lexing_error "12e78" (get_tokens Lexer.token)
                      "unexpected word" 0) ;
        "(3)" >:: (fun _ -> check_lexing_error "+67E-123" (get_tokens Lexer.token)
                      "unexpected word" 3) ;
      ] ;


      "array" >:::
      [
        "(1)" >:: (fun _ -> check_tokens "[123 (blabla) /test]" Lexer.token
                      [LSQUAREBRACKET ; INT ~:123 ; STRING "blabla" ; NAME "test" ; RSQUAREBRACKET]) ;
      ] ;

      "indirect" >:::
      [
        "(1)" >:: (fun _ -> check_tokens "1 0 R" Lexer.token
                      [INT ~:1 ; INT ~:0 ; R]) ;
      ] ;

      "obj" >:::
      [
        "(1)" >:: (fun _ -> check_tokens "obj\n123456\nendobj" Lexer.token
                      [OBJ ; INT ~:123456 ; ENDOBJ]) ;
      ] ;

      "stream" >:::
      [
        "(1)" >:: (fun _ -> check_tokens "<< /Length 7 >> stream\n" Lexer.token
                      [LDOUBLEANGLEBRACKET ; NAME "Length" ; INT ~:7 ; RDOUBLEANGLEBRACKET ; STREAM ~:23]) ;
        "(2)" >:: (fun _ -> check_tokens "\nendstream\nendobj" Lexer.token
                      [ENDSTREAM ; ENDOBJ]) ;

        "(3)" >:: (fun _ -> check_lexing_error "<< /Length 7 >> stream" (get_tokens Lexer.token)
                      "unexpected word" 16) ;
      ] ;

      "word" >:::
      [
        "(1)" >:: (fun _ -> check_lexing_error "endstreamendobj" (get_tokens Lexer.token)
                      "unexpected word" 0) ;
        "(2)" >:: (fun _ -> check_lexing_error "obj123" (get_tokens Lexer.token)
                      "unexpected word" 0) ;
        "(3)" >:: (fun _ -> check_lexing_error "123endobj" (get_tokens Lexer.token)
                      "unexpected word" 0) ;
      ] ;
    ] ;

    "name" >:::
    [
      "token" >:::
      [
        "(1)" >:: (fun _ -> check_tokens "/Hello/Hello-world" Lexer.token
                      [NAME "Hello" ; NAME "Hello-world"]) ;
        "(2)" >:: (fun _ -> check_tokens "/Specialchars&~-+@^_$" Lexer.token
                      [NAME "Specialchars&~-+@^_$"]) ;
        "(3)" >:: (fun _ -> check_tokens "/Escape#20#89#F5" Lexer.token
                      [NAME "Escape \x89\xF5"]) ;
        "(4)" >:: (fun _ -> check_tokens "/EscapeDelim#28#29#3C#3E#5B#5D#7B#7D#2F#25" Lexer.token
                      [NAME "EscapeDelim()<>[]{}/%"]) ;

        "(5)" >:: (fun _ -> check_lexing_error "/Null#00Char" (get_tokens Lexer.token)
                      "null character in escape sequence in name context" 5) ;
        "(6)" >:: (fun _ -> check_lexing_error "/Bad#7/Escape" (get_tokens Lexer.token)
                      "invalid escape sequence in name context" 4) ;
      ] ;

      "token_name" >:::
      [
        "(1)" >:: (fun _ -> check_token "hello world" (Lexer.token_name (Buffer.create 0))
                      (NAME "hello")) ;
        "(2)" >:: (fun _ -> check_token "just/testing" (Lexer.token_name (Buffer.create 0))
                      (NAME "just")) ;
        "(3)" >:: (fun _ -> check_token "value>>/key" (Lexer.token_name (Buffer.create 0))
                      (NAME "value")) ;
      ] ;
    ] ;

    "string" >:::
    [
      "token" >:::
      [
        "(1)" >:: (fun _ -> check_tokens "(hello) (hello world)" Lexer.token
                      [STRING "hello" ; STRING "hello world"]) ;
        "(2)" >:: (fun _ -> check_tokens "(special chars &~#{[|`^@]) (nested (parenthesis ()) ())" Lexer.token
                      [STRING "special chars &~#{[|`^@]" ; STRING "nested (parenthesis ()) ()"]) ;
        "(3)" >:: (fun _ -> check_tokens "(new\nline) (carriage\rreturn)" Lexer.token
                      [STRING "new\nline" ; STRING "carriage\nreturn"]) ;
        "(4)" >:: (fun _ -> check_tokens "(octal chars \\4 \\123\\56\\087\\0361) (escape \\\nsequences \\n\\r\\t\\b\\f\\)\\(\\\\)" Lexer.token
                      [STRING "octal chars \x04 \x53\x2E\x0087\x1E1" ; STRING "escape sequences \x0A\x0D\x09\x08\x0C)(\\"]) ;
      ] ;

      "token_string" >:::
      [
        "(1)" >:: (fun _ -> check_token "hello world)" (Lexer.token_string (Buffer.create 0) 1)
                      (STRING "hello world")) ;

        "(2)" >:: (fun _ -> check_lexing_error "not terminated () at end of file" (get_token (Lexer.token_string (Buffer.create 0) 1))
                      "string is not terminated at end of file" 32) ;
        "(3)" >:: (fun _ -> check_lexing_error "bad \\ escape)" (get_token (Lexer.token_string (Buffer.create 0) 1))
                      "invalid escape sequence in string context" 4) ;
        "(4)" >:: (fun _ -> check_lexing_error "other bad \\80 escape)" (get_token (Lexer.token_string (Buffer.create 0) 1))
                      "invalid escape sequence in string context" 10) ;
      ] ;
    ] ;

    "string_hex" >:::
    [
      "token" >:::
      [
        "(1)" >:: (fun _ -> check_tokens "<68656C6C6F> <68656C6C6F20776F726C64>" Lexer.token
                      [STRINGHEX "hello" ; STRINGHEX "hello world"]) ;
      ] ;

      "token_string_hex" >:::
      [
        "(1)" >:: (fun _ -> check_token "6 86\r56\tC6\nC6\x00F2\x0C1>" (Lexer.token_string_hex (Buffer.create 0))
                      (STRINGHEX "hello!")) ;
        "(2)" >:: (fun _ -> check_token "68656C2>" (Lexer.token_string_hex (Buffer.create 0))
                      (STRINGHEX "hel ")) ;

        "(3)" >:: (fun _ -> check_lexing_error "68656C6C6F20776F726C64" (get_token (Lexer.token_string_hex (Buffer.create 0)))
                      "hexadecimal string is not terminated at end of file" 22) ;
        "(4)" >:: (fun _ -> check_lexing_error "68656C\\6C6F20776F726C64>" (get_token (Lexer.token_string_hex (Buffer.create 0)))
                      "unexpected character in hexadecimal string context : 0x5c" 6) ;
      ] ;
    ] ;
  ]

