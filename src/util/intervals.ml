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

module Intervals = struct

  module OrderBInt = struct
    type t = BoundedInt.t
    let compare x y = BoundedInt.to_int (x -: y)
  end
  module MapBInt = Map.Make(OrderBInt)

  type interval = BoundedInt.t * BoundedInt.t
  type 'a overlap_t = interval * 'a * interval * 'a

  type 'a t = {
    mutable intervals : (BoundedInt.t * 'a) MapBInt.t;
    mutable overlap : ('a overlap_t) option;
  }


  let create () : 'a t = {
    intervals = MapBInt.empty;
    overlap = None;
  }

  let add (x : 'a t) ((low, high) : interval) (value : 'a) : unit =
    match x.overlap with
    | None ->
      begin
        try
          let high2, value2 = MapBInt.find low x.intervals in
          x.intervals <- MapBInt.empty; (* TODO : necessary ? *)
          x.overlap <- Some ((low, high2), value2, (low, high), value)
        with Not_found ->
          x.intervals <- MapBInt.add low (high, value) x.intervals
      end
    | _ -> ()

  let check_overlaps (x : 'a t) : (('a overlap_t) option) =
    match x.overlap with
    | None ->
      (* Iterate in increasing order of low *)
      let _, _, _, overlap = MapBInt.fold
          (fun low (high, value) (last_low, last_high, last_value, overlap) ->
             match (overlap, last_value) with
             | None, Some y ->
               let over =
                 if last_high >=: low then
                   Some ((last_low, last_high), y, (low, high), value)
                 else
                   None
               in
               (low, high, Some value, over)
             | _ ->
               (low, high, Some value, overlap)
          ) x.intervals (~:(-1), ~:(-1), None, None)
      in overlap
    | _ -> x.overlap


  let iter f (x : 'a t) : unit =
    match x.overlap with
    | None ->
      MapBInt.iter (
        fun low (high, value) ->
          f (low, high) value
      ) x.intervals
    | _ -> ()

  let fold f (x : 'a t) (b : 'b) : 'b =
    match x.overlap with
    | None ->
      MapBInt.fold (
        fun low (high, value) bb ->
          f (low, high) value bb
      ) x.intervals b
    | _ ->
      b

end

