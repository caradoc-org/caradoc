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
open Cachedfile
open Stats
open Directobject
open Indirectobject
open Boundedint
open Errors

module Widget = struct

  type context_t = {
    key : Key.t option;
    search_results : Key.t array;
  }

  type t = {
    mutable window : Curses.window;
    mutable filenwin : Curses.window;
    mutable titlewin : Curses.window;
    mutable width : int;
    mutable file : CachedFile.t ref;
    mutable title : string;
    mutable view : View.t;
    mutable help : bool;
    mutable ctxt : context_t;
  }


  let empty_ctxt : context_t =
    {
      key = None;
      search_results = [||];
    }

  let make_ctxt_key (k : Key.t) : context_t =
    {
      key = Some k;
      search_results = [||];
    }

  let make_ctxt_search (a : Key.t array) : context_t =
    {
      key = None;
      search_results = a;
    }


  let make (title : string) : t =
    {
      window = newemptywin ();
      filenwin = newemptywin ();
      titlewin = newemptywin ();
      width = 0;
      file = ref (CachedFile.make ());
      title = title;
      view = View.make ();
      help = true;
      ctxt = empty_ctxt;
    }

  let clone (x : t) : t =
    {
      window = newemptywin ();
      filenwin = newemptywin ();
      titlewin = newemptywin ();
      width = 0;
      file = x.file;
      title = x.title;
      view = x.view;
      help = x.help; (* TODO: set to false? *)
      ctxt = x.ctxt;
    }

  let loadfile (widget : t) (filename : string) : unit =
    widget.file <- ref (CachedFile.make_file filename)

  let resize (widget : t) (parent : Curses.window) (y : int) (x : int) (h : int) (w : int) : unit =
    assert (Curses.delwin widget.window);
    assert (Curses.delwin widget.filenwin);
    assert (Curses.delwin widget.titlewin);

    widget.window <- Curses.derwin parent (h - 2) w (y + 1) x;
    widget.filenwin <- Curses.derwin parent 1 w y x;
    widget.titlewin <- Curses.derwin parent 1 w (y + h - 1) x;
    widget.width <- w


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
      View.draw view widget.window;
      assert (Curses.wrefresh widget.window);

      Curses.werase widget.filenwin;
      let filename = CachedFile.filename !(widget.file) in
      reverse_wadd_inlinestr widget.filenwin 0 filename widget.width;
      assert (Curses.wrefresh widget.filenwin);

      Curses.werase widget.titlewin;
      let title = widget.title ^ (if widget.help then "<help>" else "") in
      reverse_wadd_inlinestr widget.titlewin 0 title widget.width;
      assert (Curses.wrefresh widget.titlewin)
    )


  let fetch_file (widget : t) : unit =
    widget.view <- View.make_hex (CachedFile.contents !(widget.file));
    widget.title <- "<file>";
    widget.ctxt <- empty_ctxt;
    draw widget

  let fetch_stats (widget : t) : unit =
    let stats = CachedFile.fetch_stats !(widget.file) in
    widget.view <- View.make_text (Stats.to_string stats);
    widget.title <- "<statistics>";
    widget.ctxt <- empty_ctxt;
    draw widget

  let fetch_trailer (widget : t) : unit =
    let trailers = CachedFile.fetch_trailers !(widget.file) in
    let buf = Buffer.create 16 in
    List.iter (fun obj ->
        Buffer.add_string buf (DirectObject.dict_to_string obj);
        Buffer.add_char buf '\n'
      ) trailers;
    widget.view <- View.make_text (Buffer.contents buf);
    widget.title <- "<trailer>";
    widget.ctxt <- empty_ctxt;
    draw widget

  let fetch_object (widget : t) (k : Key.t) : unit =
    let obj = CachedFile.fetch_object !(widget.file) k in
    widget.view <- View.make_text (IndirectObject.to_string obj);
    widget.title <- Printf.sprintf "<object %s>" (Key.to_string k);
    widget.ctxt <- make_ctxt_key k;
    draw widget

  let decode_stream (widget : t) : unit =
    match widget.ctxt.key with
    | None ->
      raise (Errors.PDFError ("No object currently selected, cannot decode stream", Errors.ctxt_none))
    | Some k ->
      let decoded = CachedFile.decode_stream !(widget.file) k in
      widget.view <- View.make_hex decoded;
      widget.title <- Printf.sprintf "<stream of object %s>" (Key.to_string k);
      widget.ctxt <- empty_ctxt;
      draw widget

  let fetch_refs (widget : t) (k : Key.t) : unit =
    let entries = CachedFile.fetch_refs !(widget.file) k in
    if Array.length entries = 0 then
      raise (Errors.PDFError ("No matches found", Errors.make_ctxt_key k));

    widget.view <- View.make_select (Array.map Errors.ctxt_to_string entries) "Search results:";
    widget.title <- Printf.sprintf "<refs to object %s>" (Key.to_string k);

    let objects = Array.map (fun ctxt ->
        match Errors.key_of_ctxt ctxt with
        | Some k ->
          k
        | None ->
          Key.Trailer
      ) entries
    in
    widget.ctxt <- make_ctxt_search objects;
    draw widget

  let fetch_search (widget : t) : unit =
    match widget.ctxt.search_results, View.get_selection widget.view with
    | a, Some i when i < Array.length a ->
      let k = a.(i) in
      fetch_object widget k
    | _ ->
      raise (Errors.PDFError ("No search results, cannot show current result", Errors.ctxt_none))

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
    (* Info *)
    else if i == int_of_char 'i' then (
      widget.help <- false;
      set_status "Loading stats...";
      success_or_status (fun () ->
          fetch_stats widget
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
    (* Decode stream *)
    else if i == int_of_char 'd' then (
      widget.help <- false;
      set_status "Decoding stream...";
      success_or_status (fun () ->
          decode_stream widget
        )
    )
    (* Find reference *)
    else if i == int_of_char 'r' then (
      widget.help <- false;
      let input = get_input () in
      let num = BoundedInt.uint_of_string input in
      let k = Key.make_0 num in
      let ks = Key.to_string k in

      set_status (Printf.sprintf "Finding references to object %s ..." ks);
      success_or_status (fun () ->
          fetch_refs widget k
        )
    )
    (* Search result *)
    else if i == int_of_char '\n' && widget.ctxt.search_results <> [||] then (
      widget.help <- false;
      set_status "Loading search result...";
      success_or_status (fun () ->
          fetch_search widget
        )
    )
    (* Move text view *)
    else if i == Curses.Key.down && not widget.help then (
      View.move_down widget.view widget.width 1;
      draw widget
    )
    else if i == Curses.Key.up && not widget.help then (
      View.move_up widget.view widget.width 1;
      draw widget
    )
    else if i == Curses.Key.npage && not widget.help then (
      let height, _ = Curses.getmaxyx widget.window in
      View.move_down widget.view widget.width (height - 1);
      draw widget
    )
    else if i == Curses.Key.ppage && not widget.help then (
      let height, _ = Curses.getmaxyx widget.window in
      View.move_up widget.view widget.width (height - 1);
      draw widget
    )
    else if i == Curses.Key.home && not widget.help then (
      View.move_home widget.view;
      draw widget
    )
    else if i == Curses.Key.end_ && not widget.help then (
      View.move_end widget.view;
      draw widget
    )
    (* Go to offset *)
    else if i == int_of_char 'g' && not widget.help then (
      let input = get_input () in
      let pos = BoundedInt.uint_of_string input in
      View.move_to widget.view (BoundedInt.to_int pos);
      draw widget;
      set_status ""
    )

end

