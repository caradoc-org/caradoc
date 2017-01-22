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
open Strictlexer
open Strictparser
open Errors
open Boundedint
open Xref


let get_token f text =
  let lexbuf = Lexing.from_string text in
  f lexbuf

let check_token text f tok =
  assert_equal (get_token f text) tok


let rec next_tokens f lexbuf =
  let tok = f lexbuf in
  match tok with
  | EOF_MARKER ->
    []
  | _ ->
    tok::(next_tokens f lexbuf)

let get_tokens f text =
  let lexbuf = Lexing.from_string text in
  next_tokens f lexbuf

let check_tokens text f toks =
  assert_equal (get_tokens f (text ^ "%%EOF")) toks


let check_lexing_error text f msg pos =
  assert_raises (Errors.LexingError (msg, ~:pos)) (fun () -> f text)


let tests =
  "StrictLexer" >:::
  [
    "version" >:::
    [
      "(1)" >:: (fun _ -> check_token "%PDF-1.5\n" Strictlexer.token
                    (VERSION (1, 5))) ;
      "(2)" >:: (fun _ -> check_token "%PDF-1.3\r" Strictlexer.token
                    (VERSION (1, 3))) ;

      "(3)" >:: (fun _ -> check_lexing_error "%PDF-2.5\n" (get_token Strictlexer.token)
                    "unexpected character : 0x25" 0) ;
      "(4)" >:: (fun _ -> check_lexing_error "%PDF-1.8\n" (get_token Strictlexer.token)
                    "unexpected character : 0x25" 0) ;
      "(5)" >:: (fun _ -> check_lexing_error "%PDF-1.7 \n" (get_token Strictlexer.token)
                    "unexpected character : 0x25" 0) ;
      "(6)" >:: (fun _ -> check_lexing_error "%PDF-1.7 blabla" (get_token Strictlexer.token)
                    "unexpected character : 0x25" 0) ;
    ] ;

    "notascii" >:::
    [
      "(1)" >:: (fun _ -> check_token "%\x80\x80\x80\x80\n" Strictlexer.token
                    NOTASCII) ;
      "(2)" >:: (fun _ -> check_token "%\x80\x80\x80\x80\r\n" Strictlexer.token
                    NOTASCII) ;

      "(3)" >:: (fun _ -> check_lexing_error "%1234\n" (get_token Strictlexer.token)
                    "unexpected character : 0x25" 0) ;
    ] ;

    "xref" >:::
    [
      "(1)" >:: (fun _ -> check_tokens "0000000012" Strictlexer.token
                    [UINT10 (~:12, ~:0)]) ;
      "(2)" >:: (fun _ -> check_tokens "00000" Strictlexer.token
                    [UINT5 (~:0, ~:0)]) ;
      "(3)" >:: (fun _ -> check_tokens "0000000012 00000" Strictlexer.token
                    [UINT10 (~:12, ~:0) ; SP ; UINT5 (~:0, ~:11)]) ;
      "(4)" >:: (fun _ -> check_tokens "2 0\n0000000000 65535 f \n0000000012 00000 n \n" Strictlexer.token
                    [UINT (~:2, ~:0) ; SP ; UINT (~:0, ~:2) ; LF ;
                     UINT10 (~:0, ~:4) ; SP ; UINT5 (~:65535, ~:15) ; SP ; INUSE false ; SP ; LF ;
                     UINT10 (~:12, ~:24) ; SP ; UINT5 (~:0, ~:35) ; SP ; INUSE true ; SP ; LF]) ;
      "(5)" >:: (fun _ -> check_tokens "xref\n" Strictlexer.token
                    [XREF ~:0]) ;
    ] ;

    "trailer" >:::
    [
      "(1)" >:: (fun _ -> check_token "trailer" Strictlexer.token
                    TRAILER) ;
      "(2)" >:: (fun _ -> check_token "startxref" Strictlexer.token
                    STARTXREF) ;
      "(3)" >:: (fun _ -> check_token "%%EOF" Strictlexer.token
                    EOF_MARKER) ;
      "(4)" >:: (fun _ -> check_token "%%EOF\n" Strictlexer.token
                    EOF_MARKER) ;

      "(5)" >:: (fun _ -> check_lexing_error "%EOF\n\r" (get_token Strictlexer.token)
                    "unexpected character : 0x25" 0) ;
      "(6)" >:: (fun _ -> check_lexing_error "%EOF " (get_token Strictlexer.token)
                    "unexpected character : 0x25" 0) ;
      "(7)" >:: (fun _ -> check_lexing_error "%EOF\nxref" (get_token Strictlexer.token)
                    "unexpected character : 0x25" 0) ;
    ] ;


    "null" >:::
    [
      "(1)" >:: (fun _ -> check_token "null" Strictlexer.token
                    NULL) ;

      "(2)" >:: (fun _ -> check_lexing_error "Null" (get_token Strictlexer.token)
                    "unexpected word" 0) ;
      "(3)" >:: (fun _ -> check_lexing_error "nUlL" (get_token Strictlexer.token)
                    "unexpected word" 0) ;
    ] ;

    "bool" >:::
    [
      "(1)" >:: (fun _ -> check_tokens "true false" Strictlexer.token
                    [BOOL true ; SP ; BOOL false]) ;

      "(2)" >:: (fun _ -> check_lexing_error "tRue" (get_token Strictlexer.token)
                    "unexpected word" 0) ;
      "(3)" >:: (fun _ -> check_lexing_error "FalsE" (get_token Strictlexer.token)
                    "unexpected word" 0) ;
    ] ;

    "int" >:::
    [
      "(1)" >:: (fun _ -> check_token "123" Strictlexer.token
                    (UINT (~:123, ~:0))) ;
      "(2)" >:: (fun _ -> check_token "-45" Strictlexer.token
                    (INT (~:(-45), ~:0))) ;
      "(3)" >:: (fun _ -> check_token "+6789" Strictlexer.token
                    (INT (~:6789, ~:0))) ;
      "(4)" >:: (fun _ -> check_token "-0" Strictlexer.token
                    (INT (~:0, ~:0))) ;
      "(5)" >:: (fun _ -> check_token "+0" Strictlexer.token
                    (INT (~:0, ~:0))) ;
      "(6)" >:: (fun _ -> check_token "12345" Strictlexer.token
                    (UINT5 (~:12345, ~:0))) ;
      (* +- (2^31 - 1) *)
      "(7)" >:: (fun _ -> check_token "2147483647" Strictlexer.token
                    (UINT10 ((BoundedInt.of_int64 2147483647L), ~:0))) ;
      "(8)" >:: (fun _ -> check_token "-2147483647" Strictlexer.token
                    (INT ((BoundedInt.of_int64 (-2147483647L)), ~:0))) ;
      "(9)" >:: (fun _ -> check_tokens "123+456-789" Strictlexer.token
                    [UINT (~:123, ~:0) ; INT (~:456, ~:3) ; INT (~:(-789), ~:7)]) ;

      (* +- 2^31 *)
      "(10)" >:: (fun _ -> assert_raises (BoundedInt.IntegerError "integer overflow")
                     (fun () -> get_token Strictlexer.token "2147483648")) ;
      "(11)" >:: (fun _ -> check_token "-2147483648" Strictlexer.token
                     (INT ((BoundedInt.of_int64 (-2147483648L)), ~:0))) ;

      (* +- (2^31 + 1) *)
      "(12)" >:: (fun _ -> assert_raises (BoundedInt.IntegerError "integer overflow")
                     (fun () -> get_token Strictlexer.token "2147483649")) ;
      "(13)" >:: (fun _ -> assert_raises (BoundedInt.IntegerError "integer overflow")
                     (fun () -> get_token Strictlexer.token "-2147483649")) ;
    ] ;

    "real" >:::
    [
      "(1)" >:: (fun _ -> check_tokens "1. .23 45.67 800.00 000.009" Strictlexer.token
                    [REAL "1." ; SP ; REAL ".23" ; SP ; REAL "45.67" ; SP ; REAL "800.00" ; SP ; REAL "000.009"]) ;

      "(2)" >:: (fun _ -> check_lexing_error "12e78" (get_tokens Strictlexer.token)
                    "unexpected word" 0) ;
      "(3)" >:: (fun _ -> check_lexing_error "+67E-123" (get_tokens Strictlexer.token)
                    "unexpected word" 3) ;
    ] ;


    "array" >:::
    [
      "(1)" >:: (fun _ -> check_tokens "[123 (blabla) /test]" Strictlexer.token
                    [LSQUAREBRACKET ; UINT (~:123, ~:1) ; SP ; STRING "blabla" ; SP ; NAME "test" ; RSQUAREBRACKET]) ;
      "(2)" >:: (fun _ -> check_tokens "[123/foo/bar(test)true false]" Strictlexer.token
                    [LSQUAREBRACKET ; UINT (~:123, ~:1) ; NAME "foo" ; NAME "bar" ; STRING "test" ; BOOL true ; SP ; BOOL false ; RSQUAREBRACKET]) ;
    ] ;

    "indirect" >:::
    [
      "(1)" >:: (fun _ -> check_tokens "1 0 R" Strictlexer.token
                    [UINT (~:1, ~:0) ; SP ; UINT (~:0, ~:2) ; SP ; R]) ;
    ] ;

    "obj" >:::
    [
      "(1)" >:: (fun _ -> check_tokens "obj\n(test)\rendobj" Strictlexer.token
                    [OBJ ; LF ; STRING "test" ; CR ; ENDOBJ]) ;
    ] ;

    "word" >:::
    [
      "(1)" >:: (fun _ -> check_lexing_error "endstreamendobj" (get_tokens Strictlexer.token)
                    "unexpected word" 0) ;
      "(2)" >:: (fun _ -> check_lexing_error "obj123" (get_tokens Strictlexer.token)
                    "unexpected word" 0) ;
      "(3)" >:: (fun _ -> check_lexing_error "123endobj" (get_tokens Strictlexer.token)
                    "unexpected word" 0) ;
      "(4)" >:: (fun _ -> check_lexing_error "truefalse" (get_tokens Strictlexer.token)
                    "unexpected word" 0) ;
      "(5)" >:: (fun _ -> check_lexing_error "1 0R" (get_tokens Strictlexer.token)
                    "unexpected word" 2) ;
    ] ;

    "stream" >:::
    [
      "token" >:::
      [
        "(1)" >:: (fun _ -> check_tokens "stream\nendstream" Strictlexer.token
                      [STREAM ""]) ;
        "(2)" >:: (fun _ -> check_tokens "stream\n\rfooendstream" Strictlexer.token
                      [STREAM "\rfoo"]) ;
        "(3)" >:: (fun _ -> check_tokens "stream\r\nbarendstream" Strictlexer.token
                      [STREAM "bar"]) ;
        "(4)" >:: (fun _ -> check_tokens "<< /Length 7 >> stream\n1234567\nendstream" Strictlexer.token
                      [LDOUBLEANGLEBRACKET ; SP ; NAME "Length" ; SP ; UINT (~:7, ~:11) ; SP ; RDOUBLEANGLEBRACKET ; SP ; STREAM "1234567\n"]) ;
        "(5)" >:: (fun _ -> check_tokens "stream\nfooendstream endobj" Strictlexer.token
                      [STREAM "foo" ; SP ; ENDOBJ]) ;
        "(6)" >:: (fun _ -> check_tokens "stream\nfooendstream/bar" Strictlexer.token
                      [STREAM "foo" ; NAME "bar"]) ;

        "(7)" >:: (fun _ -> check_lexing_error "streamfooendstream" (get_tokens Strictlexer.token)
                      "unexpected word" 0) ;
        "(8)" >:: (fun _ -> check_lexing_error "stream\nbarendstreamendobj" (get_tokens Strictlexer.token)
                      "unexpected word" 19) ;
      ] ;

      "token_stream" >:::
      [
        "(1)" >:: (fun _ -> check_token "endstream" (Strictlexer.token_stream (Buffer.create 0))
                      (STREAM "")) ;
        "(2)" >:: (fun _ -> check_token "endobjendstream" (Strictlexer.token_stream (Buffer.create 0))
                      (STREAM "endobj")) ;

        "(3)" >:: (fun _ -> check_lexing_error "not terminated at end of file" (get_token (Strictlexer.token_stream (Buffer.create 0)))
                      "stream is not terminated at end of file" 29) ;
        "(4)" >:: (fun _ -> check_lexing_error "123endstreamendobj" (get_token (Strictlexer.token_stream (Buffer.create 0)))
                      "unexpected word" 12) ;
      ] ;

    ] ;

    "name" >:::
    [
      "token" >:::
      [
        "(1)" >:: (fun _ -> check_tokens "/Hello/Hello-world" Strictlexer.token
                      [NAME "Hello" ; NAME "Hello-world"]) ;
        "(2)" >:: (fun _ -> check_tokens "/Specialchars&~-+@^_$" Strictlexer.token
                      [NAME "Specialchars&~-+@^_$"]) ;
        "(3)" >:: (fun _ -> check_tokens "/Escape#20#89#F5" Strictlexer.token
                      [NAME "Escape \x89\xF5"]) ;
        "(4)" >:: (fun _ -> check_tokens "/EscapeDelim#28#29#3C#3E#5B#5D#7B#7D#2F#25" Strictlexer.token
                      [NAME "EscapeDelim()<>[]{}/%"]) ;

        "(5)" >:: (fun _ -> check_lexing_error "/Null#00Char" (get_tokens Strictlexer.token)
                      "null character in escape sequence in name context" 5) ;
        "(6)" >:: (fun _ -> check_lexing_error "/Bad#7/Escape" (get_tokens Strictlexer.token)
                      "invalid escape sequence in name context" 4) ;
      ] ;

      "token_name" >:::
      [
        "(1)" >:: (fun _ -> check_token "hello world" (Strictlexer.token_name (Buffer.create 0))
                      (NAME "hello")) ;
        "(2)" >:: (fun _ -> check_token "just/testing" (Strictlexer.token_name (Buffer.create 0))
                      (NAME "just")) ;
        "(3)" >:: (fun _ -> check_token "value>>/key" (Strictlexer.token_name (Buffer.create 0))
                      (NAME "value")) ;
      ] ;
    ] ;

    "string" >:::
    [
      "token" >:::
      [
        "(1)" >:: (fun _ -> check_tokens "(hello) (hello world)" Strictlexer.token
                      [STRING "hello" ; SP ; STRING "hello world"]) ;
        "(2)" >:: (fun _ -> check_tokens "(special chars &~#{[|`^@]) (nested (parenthesis ()) ())" Strictlexer.token
                      [STRING "special chars &~#{[|`^@]" ; SP ; STRING "nested (parenthesis ()) ()"]) ;
        "(3)" >:: (fun _ -> check_tokens "(new\nline) (carriage\rreturn)" Strictlexer.token
                      [STRING "new\nline" ; SP ; STRING "carriage\nreturn"]) ;
        "(4)" >:: (fun _ -> check_tokens "(octal chars \\004 \\123\\056\\00087\\0361) (escape \\\nsequences \\n\\r\\t\\b\\f\\)\\(\\\\)" Strictlexer.token
                      [STRING "octal chars \x04 \x53\x2E\x0087\x1E1" ; SP ; STRING "escape sequences \x0A\x0D\x09\x08\x0C)(\\"]) ;
        "(5)" >:: (fun _ -> check_tokens "(\\377)" Strictlexer.token
                      [STRING "\xFF"]) ;

        "(6)" >:: (fun _ -> check_lexing_error "(\\4)" (get_tokens Strictlexer.token)
                      "invalid escape sequence in string context" 1) ;
        "(7)" >:: (fun _ -> check_lexing_error "(\\248)" (get_tokens Strictlexer.token)
                      "invalid escape sequence in string context" 1) ;
        "(8)" >:: (fun _ -> check_lexing_error "(\\400)" (get_tokens Strictlexer.token)
                      "invalid octal escape in string context (Octal character is out of bounds)" 1) ;
      ] ;

      "token_string" >:::
      [
        "(1)" >:: (fun _ -> check_token "hello world)" (Strictlexer.token_string (Buffer.create 0) 1)
                      (STRING "hello world")) ;

        "(2)" >:: (fun _ -> check_lexing_error "not terminated () at end of file" (get_token (Strictlexer.token_string (Buffer.create 0) 1))
                      "string is not terminated at end of file" 32) ;
        "(3)" >:: (fun _ -> check_lexing_error "bad \\ escape)" (get_token (Strictlexer.token_string (Buffer.create 0) 1))
                      "invalid escape sequence in string context" 4) ;
        "(4)" >:: (fun _ -> check_lexing_error "other bad \\80 escape)" (get_token (Strictlexer.token_string (Buffer.create 0) 1))
                      "invalid escape sequence in string context" 10) ;
      ] ;
    ] ;

    "string_hex" >:::
    [
      "token" >:::
      [
        "(1)" >:: (fun _ -> check_tokens "<68656C6C6F> <68656C6C6F20776F726C64>" Strictlexer.token
                      [STRING "hello" ; SP ; STRING "hello world"]) ;
      ] ;

      "token_string_hex" >:::
      [
        "(1)" >:: (fun _ -> check_token "6 86\r56\tC6\nC6F21>" (Strictlexer.token_string_hex (Buffer.create 0))
                      (STRING "hello!")) ;
        "(2)" >:: (fun _ -> check_token "68656C20>" (Strictlexer.token_string_hex (Buffer.create 0))
                      (STRING "hel ")) ;

        "(3)" >:: (fun _ -> check_lexing_error "2>" (get_token (Strictlexer.token_string_hex (Buffer.create 0)))
                      "unexpected character in hexadecimal string context : 0x32" 0) ;
        "(4)" >:: (fun _ -> check_lexing_error "68656C6C6F20776F726C64" (get_token (Strictlexer.token_string_hex (Buffer.create 0)))
                      "hexadecimal string is not terminated at end of file" 22) ;
        "(5)" >:: (fun _ -> check_lexing_error "68656C\\6C6F20776F726C64>" (get_token (Strictlexer.token_string_hex (Buffer.create 0)))
                      "unexpected character in hexadecimal string context : 0x5c" 6) ;
      ] ;
    ] ;
  ]

