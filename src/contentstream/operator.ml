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
open Params

module Operator = struct

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


  (*********************)
  (* PDF reference 8.2 *)
  (*********************)
  type category_t =
    | Compatibility
    | GeneralGraphicsState
    | SpecialGraphicsState
    | PathConstruction
    | PathPainting
    | ClippingPath (* W W* *)
    | TextState
    | TextPositioning
    | TextShowing
    | Type3Font (* Specific to Type3 fonts *)
    | Color
    | MarkedContent
    | Other

  type state_t =
    | S_PageDescription
    | S_TextObject
    | S_PathObject
    | S_ClippingPath


  let command_to_string (command : t) : string =
    match command with
    | Op_q -> "q"
    | Op_Q -> "Q"
    | Op_cm _ -> "cm"
    | Op_w _ -> "w"
    | Op_J _ -> "J"
    | Op_j _ -> "j"
    | Op_M _ -> "M"
    | Op_d _ -> "d"
    | Op_m _ -> "m"
    | Op_l _ -> "l"
    | Op_c _ -> "c"
    | Op_v _ -> "v"
    | Op_y _ -> "y"
    | Op_h -> "h"
    | Op_re _ -> "re"
    | Op_ri _ -> "ri"
    | Op_i _ -> "i"
    | Op_gs _ -> "gs"
    | Op_S -> "S"
    | Op_s -> "s"
    | Op_f -> "f"
    | Op_F -> "F"
    | Op_f_star -> "f*"
    | Op_B -> "B"
    | Op_B_star -> "B*"
    | Op_b -> "b"
    | Op_b_star -> "B*"
    | Op_n -> "n"
    | Op_W -> "W"
    | Op_W_star -> "W*"
    | Op_BT -> "BT"
    | Op_ET -> "ET"
    | Op_Tc _ -> "Tc"
    | Op_Tw _ -> "Tw"
    | Op_Tz _ -> "Tz"
    | Op_TL _ -> "TL"
    | Op_Tf _ -> "Tf"
    | Op_Tr _ -> "Tr"
    | Op_Ts _ -> "Ts"
    | Op_Td _ -> "Td"
    | Op_TD _ -> "TD"
    | Op_Tm _ -> "Tm"
    | Op_T_star -> "T*"
    | Op_Tj _ -> "Tj"
    | Op_quote _ -> "'"
    | Op_dblquote _ -> "\""
    | Op_TJ _ -> "TJ"
    | Op_d0 _ -> "d0"
    | Op_d1 _ -> "d1"
    | Op_CS _ -> "CS"
    | Op_cs _ -> "cs"
    | Op_SC _ -> "SC"
    | Op_SCN _ -> "SCN"
    | Op_sc _ -> "sc"
    | Op_scn _ -> "scn"
    | Op_G _ -> "G"
    | Op_g _ -> "g"
    | Op_RG _ -> "RG"
    | Op_rg _ -> "rg"
    | Op_K _ -> "K"
    | Op_k _ -> "k"
    | Op_sh _ -> "sh"
    | Op_INLINE_IMAGE _ -> "BI"
    | Op_Do _ -> "Do"
    | Op_MP _ -> "MP"
    | Op_DP1 _
    | Op_DP2 _ -> "DP"
    | Op_BMC _ -> "BMC"
    | Op_BDC1 _
    | Op_BDC2 _ -> "BDC"
    | Op_EMC -> "EMC"
    | Op_BX -> "BX"
    | Op_EX -> "EX"

  let state_to_string (state : state_t) : string =
    match state with
    | S_PageDescription ->
      "page description"
    | S_TextObject ->
      "text object"
    | S_PathObject ->
      "path object"
    | S_ClippingPath ->
      "clipping path"


  (*********************)
  (* PDF reference 8.2 *)
  (*********************)
  let get_category (command : t) : category_t =
    match command with
    | Op_BX | Op_EX ->
      Compatibility
    | Op_w _ | Op_J _ | Op_j _ | Op_M _ | Op_d _ | Op_ri _ | Op_i _ | Op_gs _ ->
      GeneralGraphicsState
    | Op_q | Op_Q | Op_cm _ ->
      SpecialGraphicsState
    | Op_m _ | Op_l _ | Op_c _ | Op_v _ | Op_y _ | Op_h | Op_re _ ->
      PathConstruction
    | Op_S | Op_s | Op_f | Op_F | Op_f_star | Op_B | Op_B_star | Op_b | Op_b_star | Op_n ->
      PathPainting
    | Op_W | Op_W_star ->
      ClippingPath
    | Op_Tc _ | Op_Tw _ | Op_Tz _ | Op_TL _ | Op_Tf _ | Op_Tr _ | Op_Ts _ ->
      TextState
    | Op_Td _ | Op_TD _ | Op_Tm _ | Op_T_star ->
      TextPositioning
    | Op_Tj _ | Op_quote _ | Op_dblquote _ | Op_TJ _ ->
      TextShowing
    | Op_d0 _ | Op_d1 _ ->
      Type3Font
    | Op_CS _ | Op_cs _ | Op_SC _ | Op_SCN _ | Op_sc _ | Op_scn _ | Op_G _ | Op_g _ | Op_RG _ | Op_rg _ | Op_K _ | Op_k _ ->
      Color
    | Op_MP _ | Op_DP1 _ | Op_DP2 _ | Op_BMC _ | Op_BDC1 _ | Op_BDC2 _ | Op_EMC ->
      MarkedContent

    | Op_BT | Op_ET
    | Op_sh _
    | Op_INLINE_IMAGE _
    | Op_Do _ ->
      Other


  let check_command_page (command : t) : state_t option =
    match command with
    | Op_sh _
    | Op_INLINE_IMAGE _
    | Op_Do _ ->
      Some S_PageDescription
    | Op_BT ->
      Some S_TextObject
    | Op_m _
    | Op_re _ ->
      Some S_PathObject
    | _ ->
      begin
        match get_category command with
        | Compatibility
        | GeneralGraphicsState
        | SpecialGraphicsState
        | Color
        | TextState
        | MarkedContent ->
          Some S_PageDescription
        | PathConstruction
        | PathPainting
        | ClippingPath
        | TextPositioning
        | TextShowing
        | Type3Font
        | Other ->
          None
      end

  let check_command_text (command : t) : state_t option =
    match command with
    | Op_ET ->
      Some S_PageDescription
    | _ ->
      begin
        match get_category command with
        | Compatibility
        | GeneralGraphicsState
        | Color
        | TextState
        | TextPositioning
        | TextShowing
        | MarkedContent ->
          Some S_TextObject
        | SpecialGraphicsState
        | PathConstruction
        | PathPainting
        | ClippingPath
        | Type3Font
        | Other ->
          None
      end

  let check_command_path (command : t) : state_t option =
    match get_category command with
    | Compatibility
    | PathConstruction ->
      Some S_PathObject
    | PathPainting ->
      Some S_PageDescription
    | ClippingPath ->
      Some S_ClippingPath
    | GeneralGraphicsState
    | SpecialGraphicsState
    | TextState
    | TextPositioning
    | TextShowing
    | Type3Font
    | Color
    | MarkedContent
    | Other ->
      None

  let check_command_clip (command : t) : state_t option =
    match get_category command with
    | Compatibility ->
      Some S_ClippingPath
    | PathPainting ->
      Some S_PageDescription
    | GeneralGraphicsState
    | SpecialGraphicsState
    | PathConstruction
    | ClippingPath
    | TextState
    | TextPositioning
    | TextShowing
    | Type3Font
    | Color
    | MarkedContent
    | Other ->
      None

  let check_command (state : state_t) (command : t) : state_t option =
    match state with
    | S_PageDescription ->
      check_command_page command
    | S_TextObject ->
      check_command_text command
    | S_PathObject ->
      check_command_path command
    | S_ClippingPath ->
      check_command_clip command


  let check_font3 (commands : t list) (is_font3 : bool) (error_ctxt : Errors.error_ctxt) : t list =
    if not is_font3 then
      commands
    else
      match commands with
      | a::b when (get_category a) = Type3Font ->
        b
      | _ ->
        raise (Errors.PDFError ("Expected d0 or d1 as first command in content stream of Type 3 font", error_ctxt))


  let check_commands_statemachine (commands : t list) (is_font3 : bool) (error_ctxt : Errors.error_ctxt) : unit =
    let last_state = List.fold_left (fun state command ->
      (*
        if Params.global.Params.debug then
          Printf.eprintf "State = %s, command = %s\n" (state_to_string state) (command_to_string command);
          *)
        match check_command state command with
        | Some next_state ->
          next_state
        | None ->
          raise (Errors.PDFError (Printf.sprintf "Unexpected command %s in state \"%s\" of content stream" (command_to_string command) (state_to_string state), error_ctxt))
      ) S_PageDescription (check_font3 commands is_font3 error_ctxt)
    in

    if last_state != S_PageDescription then
      Errors.warning (Printf.sprintf "Content stream does not finish in state \"%s\" but in state \"%s\"" (state_to_string S_PageDescription) (state_to_string last_state)) error_ctxt


  let check_commands_statestack (commands : t list) (error_ctxt : Errors.error_ctxt) : unit =
    let balance = List.fold_left (fun count command ->
        match command with
        | Op_q ->
          count +: ~:1
        | Op_Q ->
          let result = count -: ~:1 in
          if result <: ~:0 then
            raise (Errors.PDFError ("Content stream contains unbalanced q and Q operators", error_ctxt));
          result
        | _ ->
          count
      ) ~:0 commands
    in

    if balance >: ~:0 then
      Errors.warning "Content stream contains useless q operator" error_ctxt


  let check_commands (commands : t list) (is_font3 : bool) (error_ctxt : Errors.error_ctxt) : unit =
    check_commands_statemachine commands is_font3 error_ctxt;
    check_commands_statestack commands error_ctxt
    (* TODO : check nesting of BT/ET and BMC/EMC operators (see 14.6.1) *)
    (* TODO : check SC/SCN operands *)
    (* TODO : check INLINE_IMAGE *)

end

