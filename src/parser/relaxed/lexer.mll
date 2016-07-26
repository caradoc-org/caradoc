{
  open Parser
  open Errors
  open Common
  open Boundedint
  open Params
}


    (***********************)
    (* PDF reference 7.2.2 *)
    (***********************)
let eolstream = "\x0D\x0A" (* CRLF *) | "\x0A" (* LF *) 
let cr = '\x0D' (* CR *)
let eol = eolstream | cr
let noteol = [^ '\x0A' '\x0D']
let space = ['\x00' (* NUL *) '\x09' (* HT *) '\x0C' (* FF *) '\x20' (*SP *)]
let whitespace = space | '\x0A' (* LF *) | '\x0D' (* CR *)
let delimiter = ['(' ')' '<' '>' '[' ']' '{' '}' '/' '%']
let regular = [^ '\x0D' '\x0A' '\x00' '\x09' '\x0C' '\x20' '(' ')' '<' '>' '[' ']' '{' '}' '/' '%']

let digit = ['0'-'9']
let octal = ['0'-'7']
let hexa = ['0'-'9''A'-'F''a'-'f']

    (***********************)
    (* PDF reference 7.3.3 *)
    (***********************)
let sign = ['+''-']
let int = sign? digit+
let real = sign? ((digit+ '.' digit*) | ('.' digit+))


rule token = parse
  | eof       { EOF }
  | cr        { CR }
  | space | eolstream
    (***********************)
    (* PDF reference 7.2.3 *)
    (***********************)
  (* TODO : accept NUL and FF ? *)
  | '%' noteol* (* treat like whitespace *)
              { token lexbuf }
    (***********************)
    (* PDF reference 7.3.2 *)
    (***********************)
  | "true"    { BOOL true }
  | "false"   { BOOL false }
    (***********************)
    (* PDF reference 7.3.3 *)
    (***********************)
  | int as i  { INT (BoundedInt.int_of_string i) }
  (* TODO : convert and check bounds *)
  | real as r { REAL r }
    (*************************)
    (* PDF reference 7.3.4.2 *)
    (*************************)
  | '('       { (* arbitrary initial buffer length = 16 *)
                token_string (Buffer.create 16) 1 lexbuf }
    (*************************)
    (* PDF reference 7.3.4.3 *)
    (*************************)
  | '<'       { (* arbitrary initial buffer length = 16 *)
                token_string_hex (Buffer.create 16) lexbuf }
    (***********************)
    (* PDF reference 7.3.5 *)
    (***********************)
  | '/'       { (* arbitrary initial buffer length = 16 *)
                token_name (Buffer.create 16) lexbuf }
    (***********************)
    (* PDF reference 7.3.6 *)
    (***********************)
  | '['       { LSQUAREBRACKET }
  | ']'       { RSQUAREBRACKET }
    (***********************)
    (* PDF reference 7.3.7 *)
    (***********************)
  | "<<"      { LDOUBLEANGLEBRACKET }
  | ">>"      { RDOUBLEANGLEBRACKET }
    (***********************)
    (* PDF reference 7.3.8 *)
    (***********************)
  | "stream" eolstream
              { STREAM (~: (Lexing.lexeme_end lexbuf)) }
  (* TODO : move to other lexer (comment at EOL ??) *)
  | eol? "endstream"
              { ENDSTREAM }
    (***********************)
    (* PDF reference 7.3.9 *)
    (***********************)
  | "null"    { NULL }
    (************************)
    (* PDF reference 7.3.10 *)
    (************************)
  | "obj"     { OBJ }
  | "endobj"  { ENDOBJ }
  | "R"       { R }
    (* Lexical error *)
  | ['a'-'z''A'-'Z''0'-'9']+
              { raise (Errors.LexingError ("unexpected word", ~:(Lexing.lexeme_start lexbuf))) }
  | _ as c    { raise (Errors.LexingError (Printf.sprintf "unexpected character : 0x%x" (int_of_char c), ~:(Lexing.lexeme_start lexbuf))) }


    (*************************)
    (* PDF reference 7.3.4.3 *)
    (*************************)
and token_string_hex buf = parse
  | whitespace+
              { token_string_hex buf lexbuf }
  | '>'       { STRINGHEX (Buffer.contents buf) }
  | (hexa as n1) whitespace* (hexa as n2)
              { Buffer.add_char buf (Convert.char_of_hexa n1 n2);
                token_string_hex buf lexbuf }
  | (hexa as n1) whitespace* '>'
              { Buffer.add_char buf (Convert.char_of_hexa n1 '0');
                STRINGHEX (Buffer.contents buf) }
  | eof       { raise (Errors.LexingError ("hexadecimal string is not terminated at end of file", ~:(Lexing.lexeme_start lexbuf))) }
  | _ as c    { raise (Errors.LexingError (Printf.sprintf "unexpected character in hexadecimal string context : 0x%x" (int_of_char c), ~:(Lexing.lexeme_start lexbuf))) }


    (***********************)
    (* PDF reference 7.3.5 *)
    (***********************)
and token_name buf = parse
  | '#' (hexa as n1) (hexa as n2)
              { let c = Convert.char_of_hexa n1 n2 in
                if c = '\x00' then
                  raise (Errors.LexingError ("null character in escape sequence in name context", ~:(Lexing.lexeme_start lexbuf)));
                Buffer.add_char buf c;
                token_name buf lexbuf }
  | '#'       { raise (Errors.LexingError ("invalid escape sequence in name context", ~:(Lexing.lexeme_start lexbuf))) }
  | regular as c
              { 
                let i = int_of_char c in
                if i >= 0x21 && i <= 0x7E then (
                  Buffer.add_char buf c;
                  token_name buf lexbuf
                ) else (
                  let error_msg = Printf.sprintf "non-ASCII character in name context : 0x%x" (int_of_char c) in
                  if Params.global.Params.allow_nonascii_in_names then (
                    Printf.eprintf "Warning : %s\n" error_msg;
                    Buffer.add_char buf c;
                    token_name buf lexbuf
                  ) else
                    raise (Errors.LexingError (error_msg, ~:(Lexing.lexeme_start lexbuf)))
                ) }
  | eof
              { NAME (Buffer.contents buf) }
              (* TODO : check rewind function *)
  | _         { Common.rewind lexbuf 1;
                NAME (Buffer.contents buf) }


    (*************************)
    (* PDF reference 7.3.4.2 *)
    (*************************)
and token_string buf nesting = parse
  | ')'       { if nesting > 1 then (
                  Buffer.add_char buf ')';
                  token_string buf (nesting - 1) lexbuf
                ) else
                  STRING (Buffer.contents buf) }
  | '('       { Buffer.add_char buf '(';
                token_string buf (nesting + 1) lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\x0A';
                token_string buf nesting lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\x0D';
                token_string buf nesting lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\x09';
                token_string buf nesting lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\x08';
                token_string buf nesting lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\x0C';
                token_string buf nesting lexbuf }
  | '\\' '('  { Buffer.add_char buf '(';
                token_string buf nesting lexbuf }
  | '\\' ')'  { Buffer.add_char buf ')';
                token_string buf nesting lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\';
                token_string buf nesting lexbuf }
  | '\\' eol  { token_string buf nesting lexbuf }
  | '\\' (octal as n1) (octal as n2) (octal as n3)
              { begin
                  try
                    Buffer.add_char buf (Convert.char_of_octal3 n1 n2 n3)
                  with Convert.ConvertError msg ->
                    raise (Errors.LexingError (Printf.sprintf "invalid octal escape in string context (%s)" msg, ~:(Lexing.lexeme_start lexbuf)))
                end;
                token_string buf nesting lexbuf }
  | '\\' (octal as n1) (octal as n2)
              { Buffer.add_char buf (Convert.char_of_octal2 n1 n2);
                token_string buf nesting lexbuf }
  | '\\' (octal as n1)
              { Buffer.add_char buf (Convert.char_of_octal1 n1);
                token_string buf nesting lexbuf }
  | '\\'      { raise (Errors.LexingError (Printf.sprintf "invalid escape sequence in string context", ~:(Lexing.lexeme_start lexbuf))) }
  | eof       { raise (Errors.LexingError ("string is not terminated at end of file", ~:(Lexing.lexeme_start lexbuf))) }
  | eol       { Buffer.add_char buf '\x0A';
                token_string buf nesting lexbuf }
  | _ as c    { Buffer.add_char buf c;
                token_string buf nesting lexbuf }

