{
  open Xrefparser
  open Errors
  open Boundedint
  open Xref
}

    (***********************)
    (* PDF reference 7.5.4 *)
    (***********************)
let crlf = "\x0D\x0A" (* CRLF *)
let space_eol = "\x20\x0D" (* SP CR *) | "\x20\x0A" (* SP LF *)
let eol = ['\x0D' '\x0A']
let space = '\x20'
let digit = ['0'-'9']
let digits5 = digit digit digit digit digit
let digits10 = digits5 digits5
let version = ['0'-'7']


rule token = parse
  | eof       { EOF }
  | crlf      { CRLF }
  | space_eol { SPACE_EOL }
  | eol       { EOL }
  | space     { SPACE }
  | digits5 as g
              { GENERATION (BoundedInt.uint_of_string g) }
  | digits10 as o
              { OFFSET (BoundedInt.uint_of_string o) }
  | digit+ as n
              { OTHERNUM (BoundedInt.uint_of_string n) }
  | 'f'       { INUSE XRefTable.Free }
  | 'n'       { INUSE XRefTable.Inuse }
    (***********************)
    (* PDF reference 7.5.2 *)
    (***********************)
  | "%PDF-1." (version as v) eol
              { VERSION (1, (int_of_char v) - (int_of_char '0')) }
    (***********************)
    (* PDF reference 7.5.4 *)
    (***********************)
  | "xref"    { XREF }
    (***********************)
    (* PDF reference 7.5.5 *)
    (***********************)
  | "trailer" { TRAILER }
  | "startxref"
              { STARTXREF }
  | "%%EOF"   { EOF_MARKER }
    (* Lexical error *)
  | _ as c    { raise (Errors.LexingError (Printf.sprintf "unexpected character : 0x%x" (int_of_char c), ~:(Lexing.lexeme_start lexbuf))) }

