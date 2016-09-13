{
  open Strictparser
  open Boundedint
  open Errors
  open Common
  open Params
}


    (***********************)
    (* PDF reference 7.5.2 *)
    (***********************)
let version = ['0'-'7']
    (***********************)
    (* PDF reference 7.2.2 *)
    (***********************)
let cr = '\x0D'
let lf = '\x0A'
let sp = '\x20'
let tab = '\x09'
let whitespace = cr | lf | sp | tab
let eolstream = cr lf | lf
let eol = eolstream | cr
let nonascii = ['\x80'-'\xFF']

    (***********************)
    (* PDF reference 7.2.2 *)
    (***********************)
let regular = [^ '\x0D' '\x0A' '\x00' '\x09' '\x0C' '\x20' '(' ')' '<' '>' '[' ']' '{' '}' '/' '%']

let digit = ['0'-'9']
let digits5 = digit digit digit digit digit
let digits10 = digits5 digits5
let octal = ['0'-'7']
let hexa = ['0'-'9''A'-'F''a'-'f']

    (***********************)
    (* PDF reference 7.3.3 *)
    (***********************)
let sign = ['+''-']
let real = sign? ((digit+ '.' digit*) | ('.' digit+))


rule token = parse
    (***********************)
    (* PDF reference 7.5.2 *)
    (***********************)
  | "%PDF-1." (version as v) eol
              { VERSION (1, (Char.code v) - (Char.code '0')) }
  | '%' nonascii nonascii nonascii nonascii eol
              { NOTASCII }
    (***********************)
    (* PDF reference 7.5.4 *)
    (***********************)
  | "xref" eol
              { XREF ~:(Lexing.lexeme_start lexbuf) }
  | digits5 as g
              { UINT5 (BoundedInt.uint_of_string g, ~:(Lexing.lexeme_start lexbuf)) }
  | digits10 as o
              { UINT10 (BoundedInt.uint_of_string o, ~:(Lexing.lexeme_start lexbuf)) }
  | "n"       { INUSE true }
  | "f"       { INUSE false }
    (***********************)
    (* PDF reference 7.5.5 *)
    (***********************)
  | "trailer"
              { TRAILER }
  | "startxref"
              { STARTXREF }
  | "%%EOF" eol? eof
              { EOF_MARKER }
    (***********************)
    (* PDF reference 7.2.2 *)
    (***********************)
  | cr        { CR }
  | lf        { LF }
  | sp        { SP }
  (* TODO : remove ? *)
  | tab
              { WS }
    (***********************)
    (* PDF reference 7.3.2 *)
    (***********************)
  | "true"    { BOOL true }
  | "false"   { BOOL false }
    (***********************)
    (* PDF reference 7.3.3 *)
    (***********************)
  | digit+ as n
              { UINT (BoundedInt.uint_of_string n, ~:(Lexing.lexeme_start lexbuf)) }
  | sign? digit+ as n
              { INT (BoundedInt.int_of_string n, ~:(Lexing.lexeme_start lexbuf)) }
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
              { token_stream (Buffer.create 16) lexbuf }
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
  | _ as c    { raise (Errors.LexingError (Printf.sprintf "unexpected character : 0x%x" (Char.code c), ~:(Lexing.lexeme_start lexbuf))) }
  | eof       { raise (Errors.LexingError ("unexpected end-of-file", ~:(Lexing.lexeme_start lexbuf))) }


and token_test = parse
  | eof       { EOF_TEST }
  | _         { Common.rewind lexbuf 1;
                try
                  token lexbuf
                with Errors.LexingError (msg, _) when msg = "unexpected end-of-file" ->
                  EOF_TEST }



    (***********************)
    (* PDF reference 7.3.8 *)
    (***********************)
and token_stream buf = parse
  | "endstream" ['a'-'z''A'-'Z''0'-'9']
              { raise (Errors.LexingError ("unexpected word", ~:(Lexing.lexeme_start lexbuf) +: ~:(String.length "endstream"))) }
  | "endstream"
              { STREAM (Buffer.contents buf) }
  | eof       { raise (Errors.LexingError ("stream is not terminated at end of file", ~:(Lexing.lexeme_start lexbuf))) }
  | _ as c    { Buffer.add_char buf c;
                token_stream buf lexbuf }


    (*************************)
    (* PDF reference 7.3.4.3 *)
    (*************************)
and token_string_hex buf = parse
  | whitespace+
              { token_string_hex buf lexbuf }
  | '>'       { STRING (Buffer.contents buf) }
  | (hexa as n1) whitespace* (hexa as n2)
              { Buffer.add_char buf (Convert.char_of_hexa n1 n2);
                token_string_hex buf lexbuf }
  | eof       { raise (Errors.LexingError ("hexadecimal string is not terminated at end of file", ~:(Lexing.lexeme_start lexbuf))) }
  | _ as c    { raise (Errors.LexingError (Printf.sprintf "unexpected character in hexadecimal string context : 0x%x" (Char.code c), ~:(Lexing.lexeme_start lexbuf))) }


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
              { let i = Char.code c in
                if i < 0x21 || i > 0x7E then
                  Errors.warning_or_lexing_error Params.global.Params.allow_nonascii_in_names (Printf.sprintf "non-ASCII character in name context : 0x%x" i) ~:(Lexing.lexeme_start lexbuf);
                Buffer.add_char buf c;
                token_name buf lexbuf }
  | eof
              { NAME (Buffer.contents buf) }
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
  | '\\'      { raise (Errors.LexingError (Printf.sprintf "invalid escape sequence in string context", ~:(Lexing.lexeme_start lexbuf))) }
  | eof       { raise (Errors.LexingError ("string is not terminated at end of file", ~:(Lexing.lexeme_start lexbuf))) }
  | eol       { Buffer.add_char buf '\x0A';
                token_string buf nesting lexbuf }
  | _ as c    { Buffer.add_char buf c;
                token_string buf nesting lexbuf }

