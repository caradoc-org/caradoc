(*****************************************************************************)
(*  Caradoc: a PDF parser and validator                                      *)
(*  Copyright (C) 2015 ANSSI                                                 *)
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


module Entry = struct

  type t = string


  let empty : t = ""

  let make_index (i : int) : t =
    Printf.sprintf "[%d]" i

  let make_name (n : string) : t =
    "/" ^ n

  let append_entry (x : t) (y : t) : t =
    x ^ y

  let append_index (x : t) (i : int) : t =
    Printf.sprintf "%s[%d]" x i

  let append_name (x : t) (n : string) : t =
    Printf.sprintf "%s/%s" x n

  let is_empty (x : t) : bool =
    x = ""

  let to_string (x : t) : string =
    x

end

