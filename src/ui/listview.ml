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


module ListView = struct

  type t = {
    buf : string array;
    len : int;
    mutable offset : int;
  }


  let make (a : string array) : t =
    {buf = a; len = Array.length a; offset = 0;}


  let move_up (view : t) (i : int) : unit =
    let o = view.offset - i in
    let newoffset =
      if o >= 0 then
        o
      else
        0
    in
    view.offset <- newoffset

  let move_down (view : t) (i : int) : unit =
    let o = view.offset + i in
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

end

