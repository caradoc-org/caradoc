%{
  open Xref
  open Boundedint
%}

%token <Boundedint.BoundedInt.t> OFFSET
%token <Boundedint.BoundedInt.t> GENERATION
%token <Boundedint.BoundedInt.t> OTHERNUM
%token <Xref.XRefTable.kind_t> INUSE
%token CRLF
%token SPACE_EOL
%token EOL
%token EOF
%token SPACE
%token <int * int> VERSION
%token XREF
%token TRAILER
%token STARTXREF
%token EOF_MARKER

%start <int * int> version

%start <unit> xref
%start <Boundedint.BoundedInt.t * Boundedint.BoundedInt.t> xrefsection
%start <Boundedint.BoundedInt.t * Xref.XRefTable.value_t> xrefentry

%start <unit> trailer
%start <Boundedint.BoundedInt.t> startxref
%start <Boundedint.BoundedInt.t> startxref2
%start <bool> eofmarker

%%

version:
  x = VERSION { x }


xref:
  XREF eol { }

xrefsection:
  n = number SPACE m = number maybe_space_eol
    { (n, m) }

xrefentry:
  o = OFFSET SPACE g = GENERATION SPACE i = INUSE twobyte_eol
    { g, (XRefTable.make_value o i) }


trailer:
  ignore_spaces TRAILER { }

startxref:
  STARTXREF eol o = startxref2
    { o }

startxref2:
  o = number eol eofmarker
    { o }

eofmarker:
  | EOF_MARKER eol_eof
    { true }
  | eol_eof
    { false }



number:
  | o = OFFSET
    { o }
  | g = GENERATION
    { g }
  | n = OTHERNUM
    { n }

ignore_spaces:
  |
  | space ignore_spaces
    { }

space:
  CRLF | SPACE_EOL | EOL | SPACE
    { }

twobyte_eol:
  CRLF | SPACE_EOL { }

eol_eof:
  EOF | eol eol_eof { }

(* TODO : ignore spaces ? *)
eol:
  CRLF | EOL { }

maybe_space_eol:
  CRLF | EOL | SPACE_EOL { }

