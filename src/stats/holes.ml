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


open Common
open Wrap
open Intervals
open Boundedint
open Key
open Errors


let dump_hole (input : in_channel) (start : BoundedInt.t) (length : BoundedInt.t) : string =
  let hole = Common.input_substr input start length in
  let lexbuf = Lexing.from_string hole in

  let blank =
    try
      wrap_parser Parser.hole lexbuf (Errors.make_ctxt_pos (Errors.make_pos_file start));
      true
    with _ ->
      false
  in

  if not blank then
    hole
  else
    ""


let dump_intervals (intervals : Key.t Intervals.t) (filename : string) (input : in_channel) (length : BoundedInt.t) : unit =
  let out = open_out_bin filename in

  let lastend = Intervals.fold (
      fun (low, high) key lastend ->
        let hole = dump_hole input lastend (low -: lastend) in
        if (String.length hole) > 0 then
          Printf.fprintf out "*Hole of %d (0x%x) bytes at %d [0x%x] :\n" (String.length hole) (String.length hole) (BoundedInt.to_int lastend) (BoundedInt.to_int lastend);

        Printf.fprintf out "[0x%x -> 0x%x = 0x%x bytes] = %s\n" (BoundedInt.to_int low) (BoundedInt.to_int high) (BoundedInt.to_int (high -: low +: ~:1)) (Key.to_string key);

        high +: ~:1
    ) intervals ~:0 in

  let hole = dump_hole input lastend (length -: lastend) in
  if (String.length hole) > 0 then
    Printf.fprintf out "*Hole of %d (0x%x) bytes at %d [0x%x] :\n" (String.length hole) (String.length hole) (BoundedInt.to_int lastend) (BoundedInt.to_int lastend);

  close_out out


let dump_holes (intervals : Key.t Intervals.t) (filename : string) (input : in_channel) (length : BoundedInt.t) : unit =
  let out = open_out_bin filename in

  let lastend = Intervals.fold (
      fun (low, high) _ lastend ->
        let hole = dump_hole input lastend (low -: lastend) in
        if (String.length hole) > 0 then
          Printf.fprintf out "*Hole of %d (0x%x) bytes at %d [0x%x] :\n%s\n" (String.length hole) (String.length hole) (BoundedInt.to_int lastend) (BoundedInt.to_int lastend) hole;

        high +: ~:1
    ) intervals ~:0 in

  let hole = dump_hole input lastend (length -: lastend) in
  if (String.length hole) > 0 then
    Printf.fprintf out "*Hole of %d (0x%x) bytes at %d [0x%x] :\n%s\n" (String.length hole) (String.length hole) (BoundedInt.to_int lastend) (BoundedInt.to_int lastend) hole;

  close_out out

