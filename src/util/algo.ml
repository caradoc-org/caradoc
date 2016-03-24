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


module Algo = struct

  let array_contains (x : 'a array) (y : 'a) : bool =
    Array.fold_left
      (fun result z -> result || y = z
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


  exception Break

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
          raise Break;
      done;
      !result
    with
    | Break ->
      !result


  let join_string fold_left to_string (separator : string) iterable : string =
    fold_left (fun s x ->
        s ^ (if String.length s > 0 then separator else "") ^ (to_string x)
      ) "" iterable

end

