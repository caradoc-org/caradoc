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


let newemptywin () : Curses.window =
  Curses.newwin 0 0 0 0

let inlinestr (s : string) (width : int) : string =
  let l = String.length s in
  if l <= width then
    s
  else if width <= 3 then
    String.make width '.'
  else
    "..." ^ (String.sub s (l - width + 3) (width - 3))

let trim_str (s : string) (l : int) : string =
  if l < String.length s then
    String.sub s 0 l
  else
    s

let reverse_wadd_inlinestr (w : Curses.window) (y : int) (s : string) (width : int) : unit =
  Curses.wattr_on w Curses.A.reverse;
  ignore (Curses.mvwaddstr w y 0 (String.make width ' '));
  ignore (Curses.mvwaddstr w y 0 (inlinestr s width));
  Curses.wattr_off w Curses.A.reverse


let split_width (w : int) (n : int) : (int * int) array =
  let avail = w - n + 1 in
  let q = avail / n in
  let r = avail mod n in

  let result = Array.make n (0, 0) in
  let cursor = ref 0 in
  for i = 0 to n-1 do
    let ww = q + (if i < r then 1 else 0) in
    result.(i) <- (!cursor, ww);
    cursor := !cursor + ww + 1
  done;
  result

