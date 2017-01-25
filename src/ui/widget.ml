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


open View
open Uiutils
open Openfile
open File
open Directobject
open Indirectobject
open Boundedint
open Errors

module Widget = struct

  type t = {
    mutable window : Curses.window;
    mutable filenwin : Curses.window;
    mutable titlewin : Curses.window;
    mutable width : int;
    filename : string;
    mutable title : string;
    mutable view : View.t;
    mutable help : bool;
  }


  let make (filename : string) (title : string) : t =
    {
      window = newemptywin ();
      filenwin = newemptywin ();
      titlewin = newemptywin ();
      width = 0;
      filename = filename;
      title = title;
      view = View.make ();
      help = true;
    }

  let clone (x : t) : t =
    {
      window = newemptywin ();
      filenwin = newemptywin ();
      titlewin = newemptywin ();
      width = 0;
      filename = x.filename;
      title = x.title;
      view = x.view;
      help = x.help; (* TODO: set to false? *)
    }

  let resize (widget : t) (parent : Curses.window) (y : int) (x : int) (h : int) (w : int) : unit =
    assert (Curses.delwin widget.window);
    assert (Curses.delwin widget.filenwin);
    assert (Curses.delwin widget.titlewin);

    widget.window <- Curses.derwin parent (h - 2) w (y + 1) x;
    widget.filenwin <- Curses.derwin parent 1 w y x;
    widget.titlewin <- Curses.derwin parent 1 w (y + h - 1) x;
    widget.width <- w;
    Curses.wattr_on widget.filenwin Curses.A.reverse;
    Curses.wattr_on widget.titlewin Curses.A.reverse


  let set_active (widget : t) (is_active : bool) : unit =
    if is_active then (
      Curses.wattr_on widget.filenwin Curses.A.bold;
      Curses.wattr_on widget.titlewin Curses.A.bold;
    ) else (
      Curses.wattr_off widget.filenwin Curses.A.bold;
      Curses.wattr_off widget.titlewin Curses.A.bold;
    )

  let draw (widget : t) : unit =
    if widget.width > 0 then (
      Curses.werase widget.window;
      let view = (if widget.help then (View.make_help ()) else widget.view) in
      View.draw widget.window view;
      assert (Curses.wrefresh widget.window);

      Curses.werase widget.filenwin;
      ignore (Curses.waddstr widget.filenwin (inlinestr widget.filename widget.width));
      assert (Curses.wrefresh widget.filenwin);

      Curses.werase widget.titlewin;
      let title = widget.title ^ (if widget.help then "<help>" else "") in
      ignore (Curses.waddstr widget.titlewin (inlinestr title widget.width));
      assert (Curses.wrefresh widget.titlewin)
    )


  let fetch_file (widget : t) : unit =
    let input = OpenFile.in_bin widget.filename in
    let l = in_channel_length input in
    let buf = Buffer.create l in
    for i = 0 to l-1 do
      Buffer.add_char buf (input_char input)
    done;
    let content = Buffer.contents buf in
    widget.view <- View.make_hex content;
    widget.title <- "<file>";
    draw widget

  let fetch_trailer (widget : t) : unit =
    let input = OpenFile.in_bin widget.filename in
    let trailers = extract_trailers input in
    let buf = Buffer.create 16 in
    List.iter (fun obj ->
        Buffer.add_string buf (DirectObject.dict_to_string obj);
        Buffer.add_char buf '\n'
      ) trailers;
    widget.view <- View.make_text (Buffer.contents buf);
    widget.title <- "<trailer>";
    draw widget

  let fetch_object (widget : t) (k : Key.t) : unit =
    let input = OpenFile.in_bin widget.filename in
    let obj = extract_object input k in
    widget.view <- View.make_text (IndirectObject.to_string obj);
    widget.title <- Printf.sprintf "<object %s>" (Key.to_string k);
    draw widget


  let on_event (widget : t) (i : int) (set_status : string -> unit) (get_input : unit -> string) : unit =
    let success_or_status f =
      Errors.catch_msg ~fail_msg:set_status (fun () ->
          f ();
          set_status ""
        )
    in

    (* Toogle help *)
    if i == int_of_char 'h' then (
      widget.help <- not widget.help;
      draw widget
    )
    (* Update content *)
    (* File *)
    else if i == int_of_char 'f' then (
      widget.help <- false;
      set_status "Loading file...";
      success_or_status (fun () ->
          fetch_file widget
        )
    )
    (* Trailer *)
    else if i == int_of_char 't' then (
      widget.help <- false;
      set_status "Loading trailer...";
      success_or_status (fun () ->
          fetch_trailer widget
        )
    )
    (* Object *)
    else if i == int_of_char 'o' then (
      widget.help <- false;
      let input = get_input () in
      let num = BoundedInt.uint_of_string input in
      let k = Key.make_0 num in
      let ks = Key.to_string k in

      set_status (Printf.sprintf "Loading object %s ..." ks);
      success_or_status (fun () ->
          fetch_object widget k
        )
    )
    (* Move text view *)
    else if i == Curses.Key.down && not widget.help then (
      widget.view <- View.move_down widget.view 1 widget.width;
      draw widget
    )
    else if i == Curses.Key.up && not widget.help then (
      widget.view <- View.move_up widget.view 1 widget.width;
      draw widget
    )
    else if i == Curses.Key.npage && not widget.help then (
      let height, _ = Curses.getmaxyx widget.window in
      widget.view <- View.move_down widget.view (height - 1) widget.width;
      draw widget
    )
    else if i == Curses.Key.ppage && not widget.help then (
      let height, _ = Curses.getmaxyx widget.window in
      widget.view <- View.move_up widget.view (height - 1) widget.width;
      draw widget
    )
    else if i == Curses.Key.home && not widget.help then (
      widget.view <- View.move_home widget.view;
      draw widget
    )
    else if i == Curses.Key.end_ && not widget.help then (
      widget.view <- View.move_end widget.view;
      draw widget
    )
    (* Go to offset *)
    else if i == int_of_char 'g' && not widget.help then (
      let input = get_input () in
      let pos = BoundedInt.uint_of_string input in
      widget.view <- View.move_to widget.view (BoundedInt.to_int pos);
      draw widget;
      set_status ""
    )

end

