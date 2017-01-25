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


module TextView = struct

  type t = {
    buf : string array;
    len : int;
    offset : int;
  }


  let make () : t =
    {
      buf = Array.make 0 "";
      len = 0;
      offset = 0;
    }

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
    {buf = a; len = Array.length a; offset = 0;}


  let help_string =
    "\n"
    ^ "      HELP\n"
    ^ "\n"
    ^ "H          show/hide this notice\n"
    ^ "Q          quit\n"
    ^ "\n"
    ^ " # CONTENT\n"
    ^ "\n"
    ^ "F          show file\n"
    ^ "T          show trailer\n"
    ^ "12345 O    show object number 12345\n"
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
    ^ "12345 G    got to offset 12345\n"

  let help = make_string help_string


  let move_up (view : t) (i : int) : t =
    let o = view.offset - i in
    let newoffset =
      if o >= 0 then
        o
      else
        0
    in
    {view with offset = newoffset}

  let move_down (view : t) (i : int) : t =
    let o = view.offset + i in
    let newoffset =
      if o < view.len - 1 then
        o
      else if view.len > 0 then
        view.len - 1
      else
        0
    in
    {view with offset = newoffset}

  let move_to (view : t) (o : int) : t =
    let newoffset =
      if o >= view.len then
        view.len - 1
      else if o < 0 then
        0
      else
        o
    in
    {view with offset = newoffset}

  let move_home (view : t) : t =
    {view with offset = 0}

  let move_end (view : t) : t =
    {view with offset = view.len - 1}


  let trim_str (s : string) (l : int) : string =
    if l < String.length s then
      String.sub s 0 l
    else
      s

  let draw (w : Curses.window) (v : t) : unit =
    let height, width = Curses.getmaxyx w in
    for i = 0 to height - 1 do
      let j = v.offset + i in
      let s = trim_str (if j < v.len then v.buf.(j) else "~") width in
      assert (Curses.mvwaddstr w i 0 s)
    done

end

