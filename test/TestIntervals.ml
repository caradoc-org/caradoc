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


open OUnit
open Intervals
open Boundedint


let make_intervals id =
  let x = Intervals.create () in
  begin
    match id with
    | 0 ->
      Intervals.add x (~:0, ~:5) 1;
      Intervals.add x (~:6, ~:8) 2;
      Intervals.add x (~:9, ~:10) 3;
    | 1 ->
      Intervals.add x (~:0, ~:5) 1;
      Intervals.add x (~:5, ~:10) 2;
    | 2 ->
      Intervals.add x (~:0, ~:5) 1;
      Intervals.add x (~:3, ~:10) 2;
    | 3 ->
      Intervals.add x (~:4, ~:10) 1;
      Intervals.add x (~:2, ~:7) 2;
      Intervals.add x (~:1, ~:8) 3;
      Intervals.add x (~:3, ~:20) 4;
      Intervals.add x (~:0, ~:5) 5;
      Intervals.add x (~:5, ~:13) 6;
    | _ ->
      ()
  end;
  x


let tests =
  "Intervals" >:::
  [
    "check_overlaps" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (Intervals.check_overlaps (make_intervals 0))
                    None) ;
      "(2)" >:: (fun _ -> assert_equal
                    (Intervals.check_overlaps (make_intervals (-1)))
                    None) ;
      "(3)" >:: (fun _ -> assert_equal
                    (Intervals.check_overlaps (make_intervals 1))
                    (Some ((~:0, ~:5), 1, (~:5, ~:10), 2))) ;
      "(4)" >:: (fun _ -> assert_equal
                    (Intervals.check_overlaps (make_intervals 2))
                    (Some ((~:0, ~:5), 1, (~:3, ~:10), 2))) ;
      "(5)" >:: (fun _ -> assert_equal
                    (Intervals.check_overlaps (make_intervals 3))
                    (Some ((~:0, ~:5), 5, (~:1, ~:8), 3))) ;
    ]
  ]

