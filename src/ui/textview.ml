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

module TextView = struct

  type t = ListView.t


  let make () : t =
    ListView.make [||]

  let make_string (s : string) : t =
    let rec split (s : string) (i : int) (l : int) (c : char) : string list =
      if i >= l then
        [""]
      else
        try
          let j = String.index_from s i c in
          (String.sub s i (j - i))::(split s (j+1) l c)
        with
        | Not_found ->
          [String.sub s i (l-i)]
    in

    let a = Array.of_list (split s 0 (String.length s) '\n') in
    ListView.make a


  let move_up = ListView.move_up
  let move_down = ListView.move_down
  let move_to = ListView.move_to
  let move_home = ListView.move_home
  let move_end = ListView.move_end


  let help_string =
    "\n"
    ^ "      HELP\n"
    ^ "\n"
    ^ "H          show/hide this notice\n"
    ^ "Q          quit\n"
    ^ "\n"
    ^ " # CONTENT\n"
    ^ "\n"
    ^ "F          show file content\n"
    ^ "I          show file info\n"
    ^ "T          show trailer\n"
    ^ "12345 O    show object number 12345\n"
    ^ "D          decode stream of current obj\n"
    ^ "123   R    find refs. to object 123\n"
    ^ "ENTER      show search result\n"
    ^ "\n"
    ^ " # VIEWS\n"
    ^ "\n"
    ^ "S          split active view\n"
    ^ "Q          quit active view\n"
    ^ "LEFT       activate next view\n"
    ^ "RIGHT      activate prev. view\n"
    ^ "\n"
    ^ " # MOVE\n"
    ^ "\n"
    ^ "(PG)UP     move in active view\n"
    ^ "(PG)DOWN   move in active view\n"
    ^ "HOME       begin. of active view\n"
    ^ "END        end of active view\n"
    ^ "12345 G    go to byte/line 12345\n"

  let help = make_string help_string


  let draw (v : t) (w : Curses.window) : unit =
    let height, width = Curses.getmaxyx w in
    for i = 0 to height - 1 do
      let j = v.ListView.offset + i in
      let s = trim_str (if j < v.ListView.len then v.ListView.buf.(j) else "~") width in
      assert (Curses.mvwaddstr w i 0 s)
    done

end

