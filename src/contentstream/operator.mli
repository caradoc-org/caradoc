(*****************************************************************************)
(*  Caradoc: a PDF parser and validator                                      *)
(*  Copyright (C) 2016-2017 Guillaume Endignoux                              *)
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


open Boundedint
open Directobject
open Errors

module Operator : sig

  type num_t =
    | Int of BoundedInt.t
    | Real of string

  type coord_t = num_t * num_t

  type matrix6_t = num_t * num_t * num_t * num_t * num_t * num_t

  type rgb_t = {r : num_t; g : num_t; b : num_t}

  type cmyk_t = {c : num_t; m : num_t; y : num_t; k : num_t}

  type numstr_t =
    | Num of num_t
    | Str of string


  type t =
    | Op_q
    | Op_Q
    | Op_cm of matrix6_t
    | Op_w of num_t
    | Op_J of num_t
    | Op_j of num_t
    | Op_M of num_t
    | Op_d of num_t list * num_t

    | Op_m of coord_t
    | Op_l of coord_t
    | Op_c of coord_t * coord_t * coord_t
    | Op_v of coord_t * coord_t
    | Op_y of coord_t * coord_t
    | Op_h
    | Op_re of coord_t * coord_t
    | Op_ri of string
    | Op_i of num_t
    | Op_gs of string

    | Op_S
    | Op_s
    | Op_f
    | Op_F
    | Op_f_star
    | Op_B
    | Op_B_star
    | Op_b
    | Op_b_star
    | Op_n

    | Op_W
    | Op_W_star

    | Op_BT
    | Op_ET

    | Op_Tc of num_t
    | Op_Tw of num_t
    | Op_Tz of num_t
    | Op_TL of num_t
    | Op_Tf of string * num_t
    | Op_Tr of num_t
    | Op_Ts of num_t

    | Op_Td of coord_t
    | Op_TD of coord_t
    | Op_Tm of matrix6_t
    | Op_T_star

    | Op_Tj of string
    | Op_quote of string
    | Op_dblquote of num_t * num_t * string
    | Op_TJ of numstr_t list

    | Op_d0 of coord_t
    | Op_d1 of coord_t * coord_t * coord_t

    | Op_CS of string
    | Op_cs of string
    | Op_SC of DirectObject.t list
    | Op_SCN of DirectObject.t list
    | Op_sc of DirectObject.t list
    | Op_scn of DirectObject.t list
    | Op_G of num_t
    | Op_g of num_t
    | Op_RG of rgb_t
    | Op_rg of rgb_t
    | Op_K of cmyk_t
    | Op_k of cmyk_t

    | Op_sh of string

    | Op_INLINE_IMAGE of DirectObject.dict_t * string

    | Op_Do of string

    | Op_MP of string
    | Op_DP1 of string * DirectObject.dict_t
    | Op_DP2 of string * string
    | Op_BMC of string
    | Op_BDC1 of string * DirectObject.dict_t
    | Op_BDC2 of string * string
    | Op_EMC

    | Op_BX
    | Op_EX


  val check_commands : t list -> bool -> Errors.error_ctxt -> unit

end

