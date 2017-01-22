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


open Mapkey

module Algo = struct

  (* TODO : List.iteri is available from OCaml 4.00.0 *)
  let iteri fold_left f iterable =
    let (_:int) = fold_left (fun i x -> f i x; i+1) 0 iterable in
    ()

  let iter_start fold_left f iterable =
    let (_:bool) = fold_left (fun started x -> f started x; true) false iterable in
    ()

  let fold_lefti fold_left f init iterable =
    let (_:int), result = fold_left (fun (i, y) x -> i+1, f i y x) (0, init) iterable in
    result

  let fold_left_start fold_left f init iterable =
    let (_:bool), result = fold_left (fun (started, y) x -> true, f started y x) (false, init) iterable in
    result


  let array_contains (x : 'a array) (y : 'a) : bool =
    Array.fold_left
      (fun result z ->
         result || y = z
      ) false x

  let sort_hash (x : (string, 'a) Hashtbl.t) : (string * 'a) list =
    let l = Hashtbl.fold
        (fun key value l ->
           (key, value)::l
        ) x []
    in

    List.sort
      (fun (key1, _) (key2, _) ->
         String.compare key1 key2
      ) l

  let rec merge_unique (x : 'a list) (y : 'a list) (cmp : ('a -> 'a -> int)) : 'a list =
    match (x, y) with
    | _, [] -> x
    | [], _ -> y
    | a::q, b::r ->
      let tmp = cmp a b in
      if tmp < 0 then
        a::(merge_unique q y cmp)
      else if tmp > 0 then
        b::(merge_unique x r cmp)
      else
        a::(merge_unique q r cmp)

  let mapkey_union (x : 'a MapKey.t) (y : 'a MapKey.t) : 'a MapKey.t =
    MapKey.fold (fun key value z ->
        if not (MapKey.mem key z) then
          MapKey.add key value z
        else
          z
      ) y x


  let string_contains (x : string) (y : string) : bool =
    let result = ref false in
    try
      for i = 0 to (String.length x) - (String.length y) do
        result := true;
        for j = 0 to (String.length y) - 1 do
          if x.[i + j] <> y.[j] then
            result := false;
        done;
        if !result then
          raise Exit;
      done;
      !result
    with Exit ->
      !result


  let string_starts_with (x : string) (y : string) : bool =
    String.length x >= String.length y && (String.sub x 0 (String.length y)) = y


  let join_buffer (buf : Buffer.t) fold_left to_buffer (separator : string) iterable : unit =
    iter_start fold_left (fun started x ->
        if started then
          Buffer.add_string buf separator;
        to_buffer buf x
      ) iterable

  let join_bufferi (buf : Buffer.t) fold_left to_buffer (separator : string) iterable : unit =
    iteri fold_left (fun i x ->
        if i <> 0 then
          Buffer.add_string buf separator;
        to_buffer i buf x
      ) iterable

  let join_string fold_left to_string (separator : string) iterable : string =
    let to_buffer buf x =
      Buffer.add_string buf (to_string x)
    in

    let buf = Buffer.create 16 in
    join_buffer buf fold_left to_buffer separator iterable;
    Buffer.contents buf


  let remove_if (predicate : char -> bool) (x : string) : string =
    let l = String.length x in
    let buf = Buffer.create l in
    for i = 0 to l - 1 do
      let c = x.[i] in
      if not (predicate c) then
        Buffer.add_char buf c
    done;
    Buffer.contents buf

end

