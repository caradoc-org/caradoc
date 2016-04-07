%{
  open Xref
  open Boundedint
  open Pdfobject
%}

%token <int * int> VERSION
%token NOTASCII
%token <Boundedint.BoundedInt.t> XREF
%token <Boundedint.BoundedInt.t * Boundedint.BoundedInt.t> UINT5
%token <Boundedint.BoundedInt.t * Boundedint.BoundedInt.t> UINT10
%token <bool> INUSE
%token TRAILER
%token STARTXREF
%token EOF_MARKER
%token EOF_TEST
%token CR
%token LF
%token SP
%token WS
%token <bool> BOOL
%token <Boundedint.BoundedInt.t * Boundedint.BoundedInt.t> UINT
%token <Boundedint.BoundedInt.t * Boundedint.BoundedInt.t> INT
%token <string> REAL
%token LSQUAREBRACKET
%token RSQUAREBRACKET
%token LDOUBLEANGLEBRACKET
%token RDOUBLEANGLEBRACKET
%token NULL
%token OBJ
%token ENDOBJ
%token R

%token <string> STREAM
%token <string> STRING
%token <string> NAME

%start <(int * int) * Document.Document.t> file

(* Rules for testing purposes *)
%start <Xref.XRefTable.t * Boundedint.BoundedInt.t> xrefsection_test

%%


xrefsection_test:
  x = xrefsection EOF_TEST { x }

(***********************)
(* PDF reference 7.5.1 *)
(***********************)
file:
  v = VERSION not_ascii_marker b = body x = xref t = trailer EOF_MARKER
    { let doc, xbody = b in
      let xrefpos, xxref = x in
      let dict, pos = t in

      if xrefpos <> pos then
        raise (Errors.Errors.PDFError ("Startxref does not match real position", Errors.Errors.make_ctxt Key.Trailer xrefpos));
      Xref.XRefTable.compare xbody xxref;
      (*
      (* TODO : comparison seems to fail for some equal tables ?! *)
      if xbody <> xxref then (
        Printf.eprintf "Body :\n";
        Xref.XRefTable.debug stderr xbody;
        Printf.eprintf "Xref :\n";
        Xref.XRefTable.debug stderr xxref;
        raise (Errors.Errors.PDFError ("Xref table does not match positions of objects", Errors.ctxt_none));
      );
      *)

      Document.Document.add_trailer doc dict;
      v, doc }

(***********************)
(* PDF reference 7.5.2 *)
(***********************)
not_ascii_marker:
  NOTASCII? ignore_spaces
    { }


(***********************)
(* PDF reference 7.5.4 *)
(***********************)
xref:
  h = xrefhead s = xrefsection
    { let xrefpos, xstart, headcount = h in
      let x2, seccount = s in

      if headcount <> seccount then
        raise (Errors.Errors.PDFError ("Object count does not match in xref", Errors.Errors.make_ctxt Key.Trailer xrefpos));
      if xstart <> ~:0 then
        raise (Errors.Errors.PDFError ("Xref does not start at object 0", Errors.Errors.make_ctxt Key.Trailer xrefpos));
      (xrefpos, x2) }

xrefhead:
  pos = XREF start = anyuint SP count = anyuint eol
    { (pos, start, count) }

xrefsection:
  |
    { XRefTable.create (), ~:0 }
  | y = xrefsection o = UINT10 SP g = UINT5 SP inuse = INUSE eolxref
    { let x, count = y in
      let offset, _ = o in
      let gen, _ = g in
      XRefTable.add x (Key.make_gen count gen) (XRefTable.make_value offset (if inuse then XRefTable.Inuse else XRefTable.Free));
      (x, count +: ~:1) }


(***********************)
(* PDF reference 7.5.5 *)
(***********************)
trailer:
  TRAILER ignore_spaces d = dict_without_space eol STARTXREF eol pos = anyuint eol
    { (d, pos) }


(***********************)
(* PDF reference 7.5.3 *)
(***********************)
body:
  |
    { let x = XRefTable.create () in
      XRefTable.add x (Key.make_gen_i 0 65535) (XRefTable.make_value ~:0 XRefTable.Free);
      (Document.Document.create (), x) }
  | b = body i = indirectobj
    { let doc, x = b in
      let pos, key, obj = i in
      Document.Document.add doc key obj;
      XRefTable.add x key (XRefTable.make_value pos XRefTable.Inuse);
      (doc, x) }


indirectobj:
(************************)
(* PDF reference 7.3.10 *)
(************************)
  | i = indirectobj_header o = directobj endobj
    { let k, p = i in
      (p, k, o) }
(***********************)
(* PDF reference 7.3.8 *)
(***********************)
  | i = indirectobj_header d = dictionary raw = STREAM ignore_spaces endobj
    { let k, p = i in
      (p, k, PDFObject.Stream (d, raw, PDFObject.Raw)) }

(************************)
(* PDF reference 7.3.10 *)
(************************)
indirectobj_header:
  | i = uint_off_sp gen = uint_sp OBJ ignore_spaces
    { let id, pos = i in
      Key.make_gen id gen, pos }

endobj:
  ENDOBJ ignore_spaces
    { }

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
    { PDFObject.Null }
  | b = bool_sp
    { PDFObject.Bool b }
  | r = real_sp
    { PDFObject.Real r }
  | s = string_sp
    { PDFObject.String s }
  | n = name_sp
    { PDFObject.Name n }
  (***********************)
  (* PDF reference 7.3.6 *)
  (***********************)
  | LSQUAREBRACKET ignore_spaces a = array_content RSQUAREBRACKET ignore_spaces
    { PDFObject.Array a }
  | d = dictionary
    { PDFObject.Dictionary d }

int_or_ref:
  | i = int_sp
    { PDFObject.Int i }
  | r = ref
    { r }

(************************)
(* PDF reference 7.3.10 *)
(************************)
ref:
  | id = int_sp gen = int_sp R ignore_spaces
    { PDFObject.Reference (Key.make_gen id gen) }

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
    { [PDFObject.Int i] }

  | r = ref
    { [r] }
  | i = int_sp j = int_sp
    { [PDFObject.Int i ; PDFObject.Int j] }
  | i = int_sp r = ref
    { [PDFObject.Int i ; r] }

  | r = ref a = array_of_ints
    { r::a }
  | i = int_sp j = int_sp a = array_of_ints
    { (PDFObject.Int i)::(PDFObject.Int j)::a }
  | i = int_sp r = ref a = array_of_ints
    { (PDFObject.Int i)::r::a }


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
  | { PDFObject.dict_create () }
  | d = dict_content x = key_value
    { (* TODO : enable user to choose to allow dictionary duplicates even in strict mode? *)
      PDFObject.dict_add false d x;
      d }

key_value:
  | n = name_sp x = directobj
    { (n, x) }


null_sp:
  x = NULL ignore_spaces { x }
bool_sp:
  x = BOOL ignore_spaces { x }
int_sp:
  x = int_off_sp { let i, _ = x in i }
uint_sp:
  x = uint_off_sp { let i, _ = x in i }
real_sp:
  x = REAL ignore_spaces { x }
string_sp:
  x = STRING ignore_spaces { x }
name_sp:
  x = NAME ignore_spaces { x }


anyuint:
  x = anyuint_off
    { let i, _ = x in i }

int_off_sp:
  x = anyint_off ignore_spaces { x }

uint_off_sp:
  x = anyuint_off ignore_spaces { x }

anyint_off:
  | x = INT
  | x = anyuint_off
    { x }

anyuint_off:
  | x = UINT
  | x = UINT5
  | x = UINT10
    { x }


(***********************)
(* PDF reference 7.3.2 *)
(***********************)
ignore_spaces:
  |
  | ignore_spaces spacechar
    { }

spacechar:
  | CR
  | LF
  | SP
  | WS
    { }

eol:
  | CR LF
  | CR
  | LF
    { }

eolxref:
  | CR LF
  | SP LF
  | SP CR
    { }

