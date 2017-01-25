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
open Hexview

module View = struct

  type t =
    | Text of TextView.t
    | Hex of HexView.t


  let make () : t =
    Text (TextView.make ())

  let make_help () : t =
    Text TextView.help

  let make_text (s : string) : t =
    Text (TextView.make_string s)

  let make_hex (s : string) : t =
    Hex (HexView.make_string s)


  let move_up (v : t) (i : int) (width : int) : t =
    match v with
    | Text tv ->
      Text (TextView.move_up tv i)
    | Hex hv ->
      Hex (HexView.move_up hv i width)

  let move_down (v : t) (i : int) (width : int) : t =
    match v with
    | Text tv ->
      Text (TextView.move_down tv i)
    | Hex hv ->
      Hex (HexView.move_down hv i width)

  let move_to (v : t) (i : int) : t =
    match v with
    | Text tv ->
      Text (TextView.move_to tv i)
    | Hex hv ->
      Hex (HexView.move_to hv i)

  let move_home (v : t) : t =
    match v with
    | Text tv ->
      Text (TextView.move_home tv)
    | Hex hv ->
      Hex (HexView.move_home hv)

  let move_end (v : t) : t =
    match v with
    | Text tv ->
      Text (TextView.move_end tv)
    | Hex hv ->
      Hex (HexView.move_end hv)

  let draw (w : Curses.window) (v : t) : unit =
    match v with
    | Text tv ->
      TextView.draw w tv
    | Hex hv ->
      HexView.draw w hv

end

