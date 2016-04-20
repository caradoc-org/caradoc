%{
  open Directobject
  open Indirectobject
  open Boundedint
  open Params
%}

%token EOF
%token CR
%token LSQUAREBRACKET
%token RSQUAREBRACKET
%token LDOUBLEANGLEBRACKET
%token RDOUBLEANGLEBRACKET
%token <Boundedint.BoundedInt.t> STREAM
%token ENDSTREAM
%token NULL
%token OBJ
%token ENDOBJ
%token R
%token <bool> BOOL
%token <Boundedint.BoundedInt.t> INT
%token <string> REAL
%token <string> STRING
%token <string> STRINGHEX
%token <string> NAME

%start <Directobject.DirectObject.t> one_object
%start <Directobject.DirectObject.dict_t> trailerdict
%start <Key.t * Indirectobject.IndirectObject.partial_t> indirectobj
%start <unit> endstream
%start <(Boundedint.BoundedInt.t list) * (Boundedint.BoundedInt.t list)> intpair_list
%start <unit> hole

%start <string> ascii_hex_decode

%%

(***********************)
(* PDF reference 7.5.5 *)
(***********************)
trailerdict:
  | ignore_spaces d = dict_without_space
    { d }

(***********************)
(* PDF reference 7.3.8 *)
(***********************)
endstream:
  | ENDSTREAM ignore_spaces ENDOBJ
    { }

intpair_list:
  | EOF
    { [], [] }
  | i = int_sp j = int_sp l = intpair_list
    { let m, n = l in
      (i::m, j::n) }

one_object:
  | o = directobj EOF
    { o }

hole:
  | ignore_spaces EOF
    { }


(***********************)
(* PDF reference 7.4.2 *)
(***********************)
ascii_hex_decode:
  | s = STRINGHEX ignore_spaces EOF
    { s }

indirectobj:
(************************)
(* PDF reference 7.3.10 *)
(************************)
  | k = indirectobj_header o = directobj ENDOBJ
    { (k, IndirectObject.Complete o) }
(***********************)
(* PDF reference 7.3.8 *)
(***********************)
  | k = indirectobj_header d = dictionary offset = STREAM
    { (k, IndirectObject.StreamOffset (d, offset)) }

(************************)
(* PDF reference 7.3.10 *)
(************************)
indirectobj_header:
  | id = int_sp gen = int_sp OBJ ignore_spaces
    { Key.make_gen id gen }

(***********************)
(* PDF reference 7.3.x *)
(***********************)
directobj:
  | o = directobj_not_int
    { o }
  | o = int_or_ref
    { o }

directobj_not_int:
  | null_sp
    { DirectObject.Null }
  | b = bool_sp
    { DirectObject.Bool b }
  | r = real_sp
    { DirectObject.Real r }
  | s = string_sp
    { DirectObject.String s }
  | n = name_sp
    { DirectObject.Name n }
  (***********************)
  (* PDF reference 7.3.6 *)
  (***********************)
  | LSQUAREBRACKET ignore_spaces a = array_content RSQUAREBRACKET ignore_spaces
    { DirectObject.Array a }
  | d = dictionary
    { DirectObject.Dictionary d }

int_or_ref:
  | i = int_sp
    { DirectObject.Int i }
  | r = ref
    { r }

(************************)
(* PDF reference 7.3.10 *)
(************************)
ref:
  | id = int_sp gen = int_sp R ignore_spaces
    { DirectObject.Reference (Key.make_gen id gen) }

(***********************)
(* PDF reference 7.3.6 *)
(***********************)
array_content:
  | { [] }
  | x = directobj_not_int l = array_content
    { x::l }
  | a = array_of_ints
    { a }
  | a = array_of_ints x = directobj_not_int l = array_content
    { a @ (x::l) }

array_of_ints:
  | i = int_sp
    { [DirectObject.Int i] }

  | r = ref
    { [r] }
  | i = int_sp j = int_sp
    { [DirectObject.Int i ; DirectObject.Int j] }
  | i = int_sp r = ref
    { [DirectObject.Int i ; r] }

  | r = ref a = array_of_ints
    { r::a }
  | i = int_sp j = int_sp a = array_of_ints
    { (DirectObject.Int i)::(DirectObject.Int j)::a }
  | i = int_sp r = ref a = array_of_ints
    { (DirectObject.Int i)::r::a }


(***********************)
(* PDF reference 7.3.7 *)
(***********************)
dictionary:
  | d = dict_without_space ignore_spaces
    { d }

dict_without_space:
  | LDOUBLEANGLEBRACKET ignore_spaces d = dict_content RDOUBLEANGLEBRACKET
    { d }

dict_content:
  | { DirectObject.dict_create () }
  | d = dict_content x = key_value
    { DirectObject.dict_add Params.global.Params.allow_dict_duplicates d x;
      d }

key_value:
  | n = name_sp x = directobj
    { (n, x) }


null_sp:
  x = NULL ignore_spaces { x }
bool_sp:
  x = BOOL ignore_spaces { x }
int_sp:
  x = INT ignore_spaces { x }
real_sp:
  x = REAL ignore_spaces { x }
string_sp:
  | x = STRING ignore_spaces { x }
  | x = STRINGHEX ignore_spaces { x }
name_sp:
  x = NAME ignore_spaces { x }


(***********************)
(* PDF reference 7.3.2 *)
(***********************)
ignore_spaces:
  |
  | ignore_spaces eol
    { }

eol:
  | CR
    { }

