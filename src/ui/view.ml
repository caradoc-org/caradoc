(*****************************************************************************)
(*  Caradoc: a PDF parser and validator                                      *)
(*  Copyright (C) 2017 Guillaume Endignoux                                   *)
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


open Textview
open Selectview
open Hexview

module View = struct

  type t =
    | Text of TextView.t
    | Select of SelectView.t
    | Hex of HexView.t


  let make () : t =
    Text (TextView.make ())

  let make_help () : t =
    Text TextView.help

  let make_text (s : string) : t =
    Text (TextView.make_string s)

  let make_select (a : string array) (title : string) : t =
    Select (SelectView.make a title)

  let make_hex (s : string) : t =
    Hex (HexView.make_string s)


  let get_selection (v : t) : int option =
    match v with
    | Select sv ->
      Some (SelectView.get_selection sv)
    | Text _
    | Hex _ ->
      None


  let dispatch (ftext : TextView.t -> 'a) (fselect : SelectView.t -> 'a) (fhex : HexView.t -> 'a) (v : t) : 'a =
    match v with
    | Text tv ->
      ftext tv
    | Select sv ->
      fselect sv
    | Hex hv ->
      fhex hv

  let move_up : t -> int -> int -> unit =
    dispatch (fun v _ -> TextView.move_up v) (fun v _ -> SelectView.move_up v) HexView.move_up

  let move_down : t -> int -> int -> unit =
    dispatch (fun v _ -> TextView.move_down v) (fun v _ -> SelectView.move_down v) HexView.move_down

  let move_to : t -> int -> unit =
    dispatch TextView.move_to SelectView.move_to HexView.move_to

  let move_home : t -> unit =
    dispatch TextView.move_home SelectView.move_home HexView.move_home

  let move_end : t -> unit =
    dispatch TextView.move_end SelectView.move_end HexView.move_end

  let draw : t -> Curses.window -> unit =
    dispatch TextView.draw SelectView.draw HexView.draw

end

