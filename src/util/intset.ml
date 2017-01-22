(*****************************************************************************)
(*  Caradoc: a PDF parser and validator                                      *)
(*  Copyright (C) 2015 ANSSI                                                 *)
(*  Copyright (C) 2015-2017 Guillaume Endignoux                              *)
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


open Boundedint

module IntSet = struct

  type key_t = BoundedInt.t

  module O = struct
    type t = BoundedInt.t
    let compare x y = BoundedInt.compare x y
  end

  module S = Set.Make(O)
  type t = S.t ref


  let create () : t =
    ref S.empty

  let mem (x : t) (k : key_t) : bool =
    S.mem k !x

  let add (x : t) (k : key_t) : bool =
    if S.mem k !x then
      false
    else (
      x := S.add k !x;
      true
    )

end

