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


module HexView = struct

  type t = {
    buf : string;
    len : int;
    mutable offset : int;
  }


  let make_string (s : string) : t =
    {buf = s; len = String.length s; offset = 0;}


  let pix_per_ch = 3
  let pix_per_blank = 2
  let pix_per_offset = 10
  let offsets_thresh = 32

  let adjust_params (width : int) : bool * int =
    let use_offsets = width >= offsets_thresh in
    let w = width - pix_per_blank - (if use_offsets then pix_per_offset else 0) in
    let ch_per_line = w / (pix_per_ch + 1) in

    use_offsets, ch_per_line


  let move_up (view : t) (width : int) (i : int) : unit =
    let _, ch_per_line = adjust_params width in
    let o = view.offset - i * ch_per_line in
    let newoffset =
      if o >= 0 then
        o
      else
        0
    in
    view.offset <- newoffset

  let move_down (view : t) (width : int) (i : int) : unit =
    let _, ch_per_line = adjust_params width in
    let o = view.offset + i * ch_per_line in
    let newoffset =
      if o < view.len - 1 then
        o
      else if view.len > 0 then
        view.len - 1
      else
        0
    in
    view.offset <- newoffset

  let move_to (view : t) (o : int) : unit =
    let newoffset =
      if o >= view.len then
        view.len - 1
      else if o < 0 then
        0
      else
        o
    in
    view.offset <- newoffset

  let move_home (view : t) : unit =
    view.offset <- 0

  let move_end (view : t) : unit =
    view.offset <- view.len - 1


  let display_char (c : char) : string =
    let i = int_of_char c in
    let cc =
      if i >= 0x20 && i < 0x7F then
        c
      else
        '.'
    in
    String.make 1 cc

  let display_char_hex (c : char) : string =
    (*
    let i = int_of_char c in
    if i >= 0x20 && i < 0x7F then
      "." ^  (String.make 1 c)
    else
      *)
    Convert.hexa_of_char c

  let draw (v : t) (w : Curses.window) : unit =
    let height, width = Curses.getmaxyx w in
    let use_offsets, ch_per_line = adjust_params width in
    let line_offset = v.offset / ch_per_line in

    let xhex = if use_offsets then pix_per_offset else 0 in
    let xascii = xhex + pix_per_ch * ch_per_line + pix_per_blank in

    for i = 0 to height - 1 do
      if use_offsets then (
        let pos = (line_offset + i) * ch_per_line in
        let s =
          if pos < v.len then
            Printf.sprintf "%08X" pos
          else
            "~"
        in
        assert (Curses.mvwaddstr w i 0 s)
      );

      for j = 0 to ch_per_line-1 do
        let pos = (line_offset + i) * ch_per_line + j in
        if pos < v.len then (
          let c = v.buf.[pos] in

          assert (Curses.mvwaddstr w i (j * pix_per_ch + xhex) (display_char_hex c));
          ignore (Curses.mvwaddstr w i (j + xascii) (display_char c));
        );
      done;
    done

end

