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


open Widget
open Statusbar
open Uiutils
open Algo

module MainWindow = struct

  type state_t = {
    window : Curses.window;
    mutable status : StatusBar.t;
    mutable widgets : Widget.t ref array;
    mutable active : int;
  }

  let init (filename : string) : state_t =
    Curses.ripoffline true;
    let window = Curses.initscr () in

    (* cbreak() : remove line buffering of characters *)
    (* raw() : same + capture control characters such as CTRL+C *)
    assert (Curses.cbreak ());
    (* Do not print characters when typed on keyboard *)
    assert (Curses.noecho ());
    (* Disable interruption flush *)
    assert (Curses.intrflush window false);
    (* Capture keypad characters such as arrows *)
    assert (Curses.keypad window true);
    (* Hide cursor *)
    assert (Curses.curs_set 0);

    {
      window = window;
      status = StatusBar.make ();
      widgets = [| ref (Widget.make filename "") |];
      active = 0;
    }

  let draw (s : state_t) : unit =
    let n = Array.length s.widgets in
    for i = 0 to n-1 do
      let w = !(s.widgets.(i)) in
      Widget.set_active w (i == s.active);
      Widget.draw w
    done;
    StatusBar.draw s.status

  let resized (s : state_t) : unit =
    Curses.werase s.window;

    let height, width = Curses.getmaxyx s.window in
    let n = Array.length s.widgets in
    let splitted = split_width width n in

    for i = 0 to n-1 do
      let x, w = splitted.(i) in
      Widget.resize !(s.widgets.(i)) s.window 0 x (height - 1) w;
    done;
    StatusBar.resize s.status s.window (height - 1) 0 width;

    let acs = Curses.get_acs_codes () in
    Curses.wattr_on s.window Curses.A.reverse;
    for i = 1 to n-1 do
      let x, _ = splitted.(i) in
      if x > 0 then (
        for j = 0 to height-1 do
          assert (Curses.mvwaddch s.window j (x-1) acs.Curses.Acs.vline)
        done;
      )
    done;
    Curses.wattr_off s.window Curses.A.reverse;

    assert (Curses.refresh ());
    draw s

  let get_active (s : state_t) : Widget.t =
    !(s.widgets.(s.active))

  let set_status (s : state_t) (msg : string) : unit =
    StatusBar.set_msg s.status msg;
    StatusBar.draw s.status

  let get_input (s : state_t) () : string =
    StatusBar.get_input s.status


  let loop (s : state_t) : unit =
    resized s;

    try
      while true do
        let i = Curses.getch () in
        (* Console window was resized *)
        if i == Curses.Key.resize then
          resized s
          (* Split active widget *)
        else if i == int_of_char 's' then (
          let w = ref (Widget.clone (get_active s)) in
          s.widgets <- Algo.array_insert s.widgets w s.active;
          s.active <- s.active + 1;
          resized s
        )
        (* Quit active widget *)
        else if i == int_of_char 'q' then (
          s.widgets <- Algo.array_erase s.widgets s.active;
          if Array.length s.widgets == 0 then
            raise Exit;
          let n = Array.length s.widgets in
          if s.active >= n then
            s.active <- n-1;
          resized s
        )
        (* Move active widget to right *)
        else if i == Curses.Key.right then (
          let n = Array.length s.widgets in
          s.active <- (s.active + 1) mod n;
          draw s
        )
        (* Move active widget to left *)
        else if i == Curses.Key.left then (
          let n = Array.length s.widgets in
          s.active <- (s.active + n - 1) mod n;
          draw s
        )
        (* Accumulate input *)
        else if i >= int_of_char '0' && i <= int_of_char '9' then
          StatusBar.add_input s.status (char_of_int i)
        else if i == Curses.Key.backspace then
          StatusBar.backspace_input s.status
          (* Events of active widget *)
        else
          Widget.on_event (get_active s) i (set_status s) (get_input s)
      done;
    with Exit ->
      ()

  let run (filename : string) : unit =
    let s = init filename in
    let error = ref None in
    begin
      try
        loop s
      with
      | Assert_failure (file, line, column) ->
        error := Some (Printf.sprintf "Assertion failed at: %s:%d:%d" file line column)
      | _ ->
        ()
    end;
    (* Always call endwin, even if an exception is raised *)
    Curses.endwin ();

    match !error with
    | Some s ->
      prerr_endline s
    | None ->
      ()

end

