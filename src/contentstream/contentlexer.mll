{
  open Contentparser
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
let space = ['\x00' (* NUL *) '\x09' (* HT *) '\x0C' (* FF *) '\x20' (*SP *)]
let whitespace = space | '\x0A' (* LF *) | '\x0D' (* CR *)
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
  | whitespace+
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
    (* PDF reference 7.3.9 *)
    (***********************)
  | "null"    { NULL }

  | "ID" whitespace
              { token_inline_image (Buffer.create 16) lexbuf }

    (*********************)
    (* PDF reference 8.2 *)
    (*********************)
  | 'w'       { OP_w }
  | 'J'       { OP_J }
  | 'j'       { OP_j }
  | 'M'       { OP_M }
  | 'd'       { OP_d }
  | "ri"      { OP_ri }
  | 'i'       { OP_i }
  | "gs"      { OP_gs }

  | 'q'       { OP_q }
  | 'Q'       { OP_Q }
  | "cm"      { OP_cm }

  | 'm'       { OP_m }
  | 'l'       { OP_l }
  | 'c'       { OP_c }
  | 'v'       { OP_v }
  | 'y'       { OP_y }
  | 'h'       { OP_h }
  | "re"      { OP_re }

  | 'S'       { OP_S }
  | 's'       { OP_s }
  | 'f'       { OP_f }
  | 'F'       { OP_F }
  | "f*"      { OP_f_star }
  | 'B'       { OP_B }
  | "B*"      { OP_B_star }
  | 'b'       { OP_b }
  | "b*"      { OP_b_star }
  | 'n'       { OP_n }

  | 'W'       { OP_W }
  | "W*"      { OP_W_star }

  | "BT"      { OP_BT }
  | "ET"      { OP_ET }

  | "Tc"      { OP_Tc }
  | "Tw"      { OP_Tw }
  | "Tz"      { OP_Tz }
  | "TL"      { OP_TL }
  | "Tf"      { OP_Tf }
  | "Tr"      { OP_Tr }
  | "Ts"      { OP_Ts }

  | "Td"      { OP_Td }
  | "TD"      { OP_TD }
  | "Tm"      { OP_Tm }
  | "T*"      { OP_T_star }

  | "Tj"      { OP_Tj }
  | "TJ"      { OP_TJ }
  | '\''      { OP_quote }
  | '"'       { OP_dblquote }

  | "d0"      { OP_d0 }
  | "d1"      { OP_d1 }

  | "CS"      { OP_CS }
  | "cs"      { OP_cs }
  | "SC"      { OP_SC }
  | "SCN"     { OP_SCN }
  | "sc"      { OP_sc }
  | "scn"     { OP_scn }
  | 'G'       { OP_G }
  | 'g'       { OP_g }
  | "RG"      { OP_RG }
  | "rg"      { OP_rg }
  | 'K'       { OP_K }
  | 'k'       { OP_k }

  | "sh"      { OP_sh }

  | "BI"      { OP_BI }

  | "Do"      { OP_Do }

  | "MP"      { OP_MP }
  | "DP"      { OP_DP }
  | "BMC"     { OP_BMC }
  | "BDC"     { OP_BDC }
  | "EMC"     { OP_EMC }

  | "BX"      { OP_BX }
  | "EX"      { OP_EX }

  | ['a'-'z''A'-'Z''0'-'9']+
              { raise (Errors.LexingError ("unexpected word", ~:(Lexing.lexeme_start lexbuf))) }
  | _ as c    { raise (Errors.LexingError (Printf.sprintf "unexpected character in content stream : 0x%x" (Char.code c), ~:(Lexing.lexeme_start lexbuf))) }


and token_inline_image buf = parse
  | "EI" ['a'-'z''A'-'Z''0'-'9']
              { raise (Errors.LexingError ("unexpected word", ~:(Lexing.lexeme_start lexbuf) +: ~:(String.length "endstream"))) }
  | "EI"
              { OP_INLINE_IMAGE (Buffer.contents buf) }
  | eof       { raise (Errors.LexingError ("inline image is not terminated at end of content stream", ~:(Lexing.lexeme_start lexbuf))) }
  | _ as c    { Buffer.add_char buf c;
                token_inline_image buf lexbuf }


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

