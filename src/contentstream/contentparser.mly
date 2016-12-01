%{
  open Directobject
  open Params
  open Operator.Operator
%}

%token EOF
%token LSQUAREBRACKET
%token RSQUAREBRACKET
%token LDOUBLEANGLEBRACKET
%token RDOUBLEANGLEBRACKET
%token NULL
%token <bool> BOOL
%token <Boundedint.BoundedInt.t> INT
%token <string> REAL
%token <string> STRING
%token <string> STRINGHEX
%token <string> NAME

(*********************)
(* PDF reference 8.2 *)
(*********************)
%token OP_w
%token OP_J
%token OP_j
%token OP_M
%token OP_d
%token OP_ri
%token OP_i
%token OP_gs

%token OP_q
%token OP_Q
%token OP_cm

%token OP_m
%token OP_l
%token OP_c
%token OP_v
%token OP_y
%token OP_h
%token OP_re

%token OP_S
%token OP_s
%token OP_f
%token OP_F
%token OP_f_star
%token OP_B
%token OP_B_star
%token OP_b
%token OP_b_star
%token OP_n

%token OP_W
%token OP_W_star

%token OP_BT
%token OP_ET

%token OP_Tc
%token OP_Tw
%token OP_Tz
%token OP_TL
%token OP_Tf
%token OP_Tr
%token OP_Ts

%token OP_Td
%token OP_TD
%token OP_Tm
%token OP_T_star

%token OP_Tj
%token OP_TJ
%token OP_quote
%token OP_dblquote

%token OP_d0
%token OP_d1

%token OP_CS
%token OP_cs
%token OP_SC
%token OP_SCN
%token OP_sc
%token OP_scn
%token OP_G
%token OP_g
%token OP_RG
%token OP_rg
%token OP_K
%token OP_k

%token OP_sh

%token OP_BI
%token <string> OP_INLINE_IMAGE

%token OP_Do

%token OP_MP
%token OP_DP
%token OP_BMC
%token OP_BDC
%token OP_EMC

%token OP_BX
%token OP_EX

%start <Operator.Operator.t list> content

%%

content:
  | c = ops EOF
    { c }

ops:
  | { [] }
  | c = op l = ops
    { c::l }

op:
  | c = op_57
    { c }
  | c = op_59
    { c }
  | c = op_60
    { c }
  | c = op_61
    { c }
  | c = op_107
    { c }
  | c = op_105
    { c }
  | c = op_108
    { c }
  | c = op_109
    { c }
  | c = op_113
    { c }
  | c = op_74
    { c }
  | c = op_77
    { c }
  | c = op_92
    { c }
  | c = op_87
    { c }
  | c = op_320
    { c }
  | c = op_32
    { c }

(***********************)
(* PDF reference 8.4.4 *)
(***********************)
op_57:
  | OP_q
    { Op_q }
  | OP_Q
    { Op_Q }
  | OP_cm m = matrix6
    { Op_cm m }
  | OP_w x = num
    { Op_w x }
  | OP_J x = num
    { Op_J x }
  | OP_j x = num
    { Op_j x }
  | OP_M x = num
    { Op_M x }
  | OP_d phase = num a = array_nums
    { Op_d (a, phase) }

(***********************)
(* PDF reference 8.5.2 *)
(***********************)
op_59:
  | OP_m c = coord
    { Op_m c }
  | OP_l c = coord
    { Op_l c }
  | OP_c c3 = coord c2 = coord c1 = coord
    { Op_c (c1, c2, c3) }
  | OP_v c3 = coord c2 = coord
    { Op_v (c2, c3) }
  | OP_y c3 = coord c1 = coord
    { Op_y (c1, c3) }
  | OP_h
    { Op_h }
  | OP_re size = coord c = coord
    { Op_re (c, size) }
  | OP_ri intent = NAME
    { Op_ri intent }
  | OP_i n = num
    { Op_i n }
  | OP_gs n = NAME
    { Op_gs n }

(***********************)
(* PDF reference 8.5.3 *)
(***********************)
op_60:
  | OP_S
    { Op_S }
  | OP_s
    { Op_s }
  | OP_f
    { Op_f }
  | OP_F
    { Op_F }
  | OP_f_star
    { Op_f_star }
  | OP_B
    { Op_B }
  | OP_B_star
    { Op_B_star }
  | OP_b
    { Op_b }
  | OP_b_star
    { Op_b_star }
  | OP_n
    { Op_n }

(***********************)
(* PDF reference 8.5.4 *)
(***********************)
op_61:
  | OP_W
    { Op_W }
  | OP_W_star
    { Op_W_star }

(***********************)
(* PDF reference 9.4.1 *)
(***********************)
op_107:
  | OP_BT
    { Op_BT }
  | OP_ET
    { Op_ET }

(***********************)
(* PDF reference 9.3.1 *)
(***********************)
op_105:
  | OP_Tc n = num
    { Op_Tc n }
  | OP_Tw n = num
    { Op_Tw n }
  | OP_Tz n = num
    { Op_Tz n }
  | OP_TL n = num
    { Op_TL n }
  | OP_Tf size = num font = NAME
    { Op_Tf (font, size) }
  | OP_Tr n = num
    { Op_Tr n }
  | OP_Ts n = num
    { Op_Ts n }

(***********************)
(* PDF reference 9.4.2 *)
(***********************)
op_108:
  | OP_Td c = coord
    { Op_Td c }
  | OP_TD c = coord
    { Op_TD c }
  | OP_Tm m = matrix6
    { Op_Tm m }
  | OP_T_star
    { Op_T_star }

(***********************)
(* PDF reference 9.4.3 *)
(***********************)
op_109:
  | OP_Tj s = str
    { Op_Tj s }
  | OP_quote s = str
    { Op_quote s }
  | OP_dblquote s = str ac = num aw = num
    { Op_dblquote (aw, ac, s) }
  | OP_TJ a = array_stringsnums
    { Op_TJ a }

(***********************)
(* PDF reference 9.6.5 *)
(***********************)
op_113:
  | OP_d0 w = coord
    { Op_d0 w }
  | OP_d1 ur = coord ll = coord w = coord
    { Op_d1 (w, ll, ur) }

(***********************)
(* PDF reference 8.6.8 *)
(***********************)
op_74:
  | OP_CS n = NAME
    { Op_CS n }
  | OP_cs n = NAME
    { Op_cs n }
(* TODO : check arguments for SC, SCN, sc, scn *)
  | OP_SC a = array_content
    { Op_SC a }
  | OP_SCN a = array_content
    { Op_SCN a }
  | OP_sc a = array_content
    { Op_sc a }
  | OP_scn a = array_content
    { Op_scn a }
  | OP_G grey = num
    { Op_G grey }
  | OP_g grey = num
    { Op_g grey }
  | OP_RG b = num g = num r = num
    { Op_RG {r = r; g = g; b = b} }
  | OP_rg b = num g = num r = num
    { Op_rg {r = r; g = g; b = b} }
  | OP_K k = num y = num m = num c = num
    { Op_K {c = c; m = m; y = y; k = k} }
  | OP_k k = num y = num m = num c = num
    { Op_k {c = c; m = m; y = y; k = k} }

(***********************)
(* PDF reference 8.7.4 *)
(***********************)
op_77:
  | OP_sh n = NAME
    { Op_sh n }

(***********************)
(* PDF reference 8.9.7 *)
(***********************)
op_92:
  | image = OP_INLINE_IMAGE d = dict_content OP_BI
    { Op_INLINE_IMAGE (d, image) }

(***********************)
(* PDF reference 8.8.1 *)
(***********************)
op_87:
  | OP_Do n = NAME
    { Op_Do n }

(************************)
(* PDF reference 14.6.1 *)
(************************)
op_320:
  | OP_MP tag = NAME
    { Op_MP tag }
  | OP_DP prop = dictionary tag = NAME
    { Op_DP1 (tag, prop) }
  | OP_DP prop = NAME tag = NAME
    { Op_DP2 (tag, prop) }
  | OP_BMC tag = NAME
    { Op_BMC tag }
  | OP_BDC prop = dictionary tag = NAME
    { Op_BDC1 (tag, prop) }
  | OP_BDC prop = NAME tag = NAME
    { Op_BDC2 (tag, prop) }
  | OP_EMC
    { Op_EMC }

(***********************)
(* PDF reference 7.8.2 *)
(***********************)
op_32:
  | OP_BX
    { Op_BX }
  | OP_EX
    { Op_EX }


(***********************)
(* PDF reference 7.3.x *)
(***********************)
directobj:
  | NULL
    { DirectObject.Null }
  | b = BOOL
    { DirectObject.Bool b }
  | n = num_obj
    { n }
  | s = str_obj
    { s }
  | n = NAME
    { DirectObject.Name n }
  | a = array
    { a }

(***********************)
(* PDF reference 7.3.6 *)
(***********************)
array:
  | RSQUAREBRACKET a = array_content LSQUAREBRACKET
    { DirectObject.Array a }

array_content:
  | a = array_content_impl
    { List.rev a }

array_content_impl:
  | { [] }
  | x = directobj l = array_content_impl
    { x::l }

array_nums:
  | RSQUAREBRACKET a = array_nums_content LSQUAREBRACKET
    { List.rev a }

array_nums_content:
  | { [] }
  | n = num l = array_nums_content
    { n::l }

array_stringsnums:
  | RSQUAREBRACKET a = array_stringsnums_content LSQUAREBRACKET
    { List.rev a }

array_stringsnums_content:
  | { [] }
  | n = num l = array_stringsnums_content
    { (Num n)::l }
  | s = str l = array_stringsnums_content
    { (Str s)::l }

(***********************)
(* PDF reference 7.3.7 *)
(***********************)
dictionary:
  | RDOUBLEANGLEBRACKET d = dict_content LDOUBLEANGLEBRACKET
    { d }

dict_content:
  | { DirectObject.dict_create () }
  | d = dict_content x = key_value
    { DirectObject.dict_add Params.global.Params.allow_dict_duplicates d x;
      d }

key_value:
  | x = directobj n = NAME
    { (n, x) }


matrix6:
  | f = num e = num d = num c = num b = num a = num
    { (a, b, c, d, e, f) }

coord:
  | y = num x = num
    { (x, y) }


str_obj:
  | s = str
    { DirectObject.String s }

num_obj:
  | i = INT
    { DirectObject.Int i }
  | r = REAL
    { DirectObject.Real r }

str:
  | s = STRING
    { s }
  | s = STRINGHEX
    { s }

num:
  | i = INT
    { Int i }
  | r = REAL
    { Real r }

