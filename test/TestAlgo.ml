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
open Algo
open Boundedint


let create_hash id =
  let x = Hashtbl.create 16 in
  begin
    match id with
    | 0 ->
      Hashtbl.add x "apple" 15;
      Hashtbl.add x "pear" 8;
      Hashtbl.add x "banana" 31;
      Hashtbl.add x "tomato" 21;
      Hashtbl.add x "potato" 10;
    | 1 ->
      Hashtbl.add x "apple" 0;
      Hashtbl.add x "pear" 0;
      Hashtbl.add x "banana" 0;
      Hashtbl.add x "tomato" 0;
      Hashtbl.add x "potato" 0;
    | 2 ->
      Hashtbl.add x "one" 1;
      Hashtbl.add x "two" 2;
      Hashtbl.add x "three" 3;
      Hashtbl.add x "four" 4;
      Hashtbl.add x "five" 5;
      Hashtbl.add x "six" 6;
      Hashtbl.add x "seven" 7;
      Hashtbl.add x "eight" 8;
    | _ ->
      ()
  end;
  x

let create_list id =
  match id with
  | 0 ->
    ["apple", 15 ; "banana", 31 ; "pear", 8 ; "potato", 10 ; "tomato", 21]
  | 1 ->
    ["apple", 0 ; "banana", 0 ; "pear", 0 ; "potato", 0 ; "tomato", 0]
  | 2 ->
    ["eight", 8 ; "five", 5 ; "four", 4 ; "one", 1 ; "seven", 7 ; "six", 6 ; "three", 3 ; "two", 2]
  | _ ->
    []


let tests =
  "Algo" >:::
  [
    "array_contains" >:::
    [
      "(1)" >:: (fun _ -> assert_equal (Algo.array_contains [| 1 ; 3 ; 5 ; 7 |] 7) true) ;
      "(2)" >:: (fun _ -> assert_equal (Algo.array_contains [| 1 ; 3 ; 5 ; 7 |] 2) false) ;
      "(3)" >:: (fun _ -> assert_equal (Algo.array_contains [| "foo" ; "bar" |] "bar") true) ;
      "(4)" >:: (fun _ -> assert_equal (Algo.array_contains [| "alpha" ; "beta" ; "gamma" |] "delta") false) ;
    ] ;

    "sort_hash" >:::
    [
      "(1)" >:: (fun _ -> assert_equal (Algo.sort_hash (create_hash 0)) (create_list 0)) ;
      "(2)" >:: (fun _ -> assert_equal (Algo.sort_hash (create_hash 1)) (create_list 1)) ;
      "(3)" >:: (fun _ -> assert_equal (Algo.sort_hash (create_hash 2)) (create_list 2)) ;
      "(4)" >:: (fun _ -> assert_equal (Algo.sort_hash (create_hash (-1))) (create_list (-1))) ;
    ] ;

    "merge_unique" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (Algo.merge_unique [1 ; 2 ; 3] [1 ; 3 ; 4] compare)
                    [1 ; 2 ; 3 ; 4]) ;
      "(2)" >:: (fun _ -> assert_equal
                    (Algo.merge_unique [1 ; 4 ; 7] [2 ; 3 ; 9] compare)
                    [1 ; 2 ; 3 ; 4 ; 7 ; 9]) ;
      "(3)" >:: (fun _ -> assert_equal
                    (Algo.merge_unique [1 ; 2] [] compare)
                    [1 ; 2]) ;
    ] ;

    "mapkey_union" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (Algo.mapkey_union (TestMapkey.add_all [Key.make_0 ~:1, "foo" ; Key.make_0 ~:2, "hello"]) (TestMapkey.add_all [Key.make_0 ~:1, "bar" ; Key.make_0 ~:3, "world"]))
                    (TestMapkey.add_all [Key.make_0 ~:1, "foo" ; Key.make_0 ~:2, "hello" ; Key.make_0 ~:3, "world"])) ;
    ] ;
  ]

