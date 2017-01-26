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


open Listview
open Uiutils

module SelectView = struct

  type t = {
    view : ListView.t;
    title : string;
    mutable offset : int;
  }


  let make (a : string array) (title : string) : t =
    {view = ListView.make a; title = title; offset = 0;}

  let get_selection (v : t) : int =
    v.view.ListView.offset

  let move_up (v : t) =
    ListView.move_up v.view

  let move_down (v : t) =
    ListView.move_down v.view

  let move_to (v : t) =
    ListView.move_to v.view

  let move_home (v : t) =
    ListView.move_home v.view

  let move_end (v : t) =
    ListView.move_end v.view


  let adjust (v : t) (height : int) : unit =
    (* Adjust w.r.t. selection *)
    if v.view.ListView.offset >= v.offset + height then
      v.offset <- v.view.ListView.offset - height + 1
    else if v.view.ListView.offset < v.offset then
      v.offset <- v.view.ListView.offset;
    (* Adjust w.r.t. window *)
    if v.view.ListView.len < v.offset + height then
      v.offset <- v.view.ListView.len - height;
    if v.offset < 0 then
      v.offset <- 0

  let draw (v : t) (w : Curses.window) : unit =
    let height, width = Curses.getmaxyx w in
    adjust v (height - 1);

    assert (Curses.waddstr w (inlinestr v.title width));

    for i = 1 to height - 1 do
      let j = v.offset + i - 1 in
      let s = trim_str (if j < v.view.ListView.len then v.view.ListView.buf.(j) else "~") width in

      if j == v.view.ListView.offset then
        reverse_wadd_inlinestr w i s width
      else
        assert (Curses.mvwaddstr w i 0 s);
    done

end

