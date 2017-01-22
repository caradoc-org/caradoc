(*****************************************************************************)
(*  Caradoc: a PDF parser and validator                                      *)
(*  Copyright (C) 2016-2017 Guillaume Endignoux                              *)
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


module RC4 = struct

  type state_t = {
    s : int array;
    mutable i : int;
    mutable j : int;
  }

  let swap (s : int array) (i : int) (j : int) : unit =
    let x = s.(i) in
    s.(i) <- s.(j);
    s.(j) <- x

  let init (key : string) : state_t =
    let len = String.length key in
    let k = Array.init len (fun i -> Char.code key.[i]) in
    let s = Array.init 256 (fun i -> i) in

    let j = ref 0 in
    for i = 0 to 255 do
      j := (!j + s.(i) + k.(i mod len)) mod 256;
      swap s i !j
    done;
    {s = s; i = 0; j = 0;}

  let crypt (key : string) (src : string) : string =
    let dst = String.copy src in
    let x = init key in

    for a = 0 to (String.length src) - 1 do
      x.i <- (x.i + 1) mod 256;
      x.j <- (x.j + x.s.(x.i)) mod 256;
      swap x.s x.i x.j;
      let mask = x.s.((x.s.(x.i) + x.s.(x.j)) mod 256) in
      dst.[a] <- Char.chr ((Char.code dst.[a]) lxor mask);
    done;
    dst

end

