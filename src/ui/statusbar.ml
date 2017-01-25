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


open Uiutils

module StatusBar = struct

  type t = {
    mutable window : Curses.window;
    mutable text : string;
    mutable is_input : bool;
  }


  let make () : t =
    {
      window = newemptywin ();
      text = "";
      is_input = false;
    }

  let resize (s : t) (parent : Curses.window) (y : int) (x : int) (w : int) : unit =
    assert (Curses.delwin s.window);
    s.window <- Curses.derwin parent 1 w y x

  let draw (s : t) : unit =
    Curses.werase s.window;
    let txt = (if s.is_input then ":" else "") ^ s.text in
    let _, width = Curses.getmaxyx s.window in
    ignore (Curses.waddstr s.window (inlinestr txt width));
    assert (Curses.wrefresh s.window)


  let set_msg (s : t) (msg : string) : unit =
    s.text <- msg;
    s.is_input <- false;
    draw s

  let flush (s : t) : unit =
    s.text <- "";
    s.is_input <- false;
    draw s

  let set_input_mode (s : t) : unit =
    if not s.is_input then (
      s.text <- "";
      s.is_input <- true
    )

  let add_input (s : t) (c : char) : unit =
    set_input_mode s;
    s.text <- s.text ^ (String.make 1 c);
    draw s

  let backspace_input (s : t) : unit =
    set_input_mode s;
    let l = String.length s.text in
    if l > 1 then
      s.text <- String.sub s.text 0 (l-1)
    else (
      s.text <- "";
      s.is_input <- false
    );
    draw s

  let get_input (s : t) : string =
    set_input_mode s;
    s.text

end

