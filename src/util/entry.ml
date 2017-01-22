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


module Entry = struct

  type elem =
    | Index of int
    | Name of string
    | NameKey of string

  type t = elem list


  let empty : t = []

  let make_index (i : int) : t =
    [Index i]

  let make_name (n : string) : t =
    [Name n]

  let make_name_key (n : string) : t =
    [NameKey n]

  let append_entry (x : t) (y : t) : t =
    y @ x

  let append_index (x : t) (i : int) : t =
    (Index i)::x

  let append_name (x : t) (n : string) : t =
    (Name n)::x

  let append_name_key (x : t) (n : string) : t =
    (NameKey n)::x

  let is_empty (x : t) : bool =
    x = []

  let elem_to_string (x : elem) : string =
    match x with
    | Index i ->
      Printf.sprintf "[%d]" i
    | Name n ->
      "/" ^ n
    | NameKey n ->
      "\\" ^ n

  let to_string (x : t) : string =
    let buf = Buffer.create 16 in
    List.iter (fun e -> Buffer.add_string buf (elem_to_string e)) (List.rev x);
    Buffer.contents buf


  type select_t = elem list list


  let no_selector : select_t =
    []

  let validate (s : select_t) : select_t =
    if List.mem [] s then
      [[]]
    else
      s

  let make_selector (l : t list) : select_t =
    validate (List.map List.rev l)

  let move_to_index (s : select_t) (i : int) : select_t =
    validate (List.fold_right (fun entry l ->
        match entry with
        | (Index j)::x when i = j ->
          x::l
        | _ ->
          l
      ) s [])

  let move_to_name (s : select_t) (n : string) : select_t =
    validate (List.fold_right (fun entry l ->
        match entry with
        | (Name m)::x when n = m ->
          x::l
        | _ ->
          l
      ) s [])

  let move_to_name_key (s : select_t) (n : string) : select_t =
    validate (List.fold_right (fun entry l ->
        match entry with
        | (NameKey m)::x when n = m ->
          x::l
        | _ ->
          l
      ) s [])

  let is_selected (s : select_t) : bool =
    s = [[]]

end

