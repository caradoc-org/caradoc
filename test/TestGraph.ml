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
open Graph
open Key
open Boundedint


let make_graph id =
  match id with
  | 1 ->
    let x = Graph.create () in

    Graph.add_vertex x Key.Trailer;
    Graph.add_vertex x (Key.make_0 ~:1);
    Graph.add_vertex x (Key.make_0 ~:2);
    Graph.add_vertex x (Key.make_gen ~:3 ~:1);
    Graph.add_vertex x (Key.make_0 ~:4);

    Graph.add_edge x (Key.make_0 ~:1, Key.make_0 ~:2);
    Graph.add_edge x (Key.make_0 ~:1, Key.make_0 ~:4);
    Graph.add_edge x (Key.make_gen ~:3 ~:1, Key.make_0 ~:4);
    Graph.add_edge x (Key.make_0 ~:4, Key.make_0 ~:2);

    x
  | 2 ->
    let x = Graph.create () in

    Graph.add_vertex x Key.Trailer;
    Graph.add_vertex x (Key.make_0 ~:1);
    Graph.add_vertex x (Key.make_0 ~:2);
    Graph.add_vertex x (Key.make_0 ~:3);
    Graph.add_vertex x (Key.make_0 ~:4);

    Graph.add_edge x (Key.make_0 ~:1, Key.make_0 ~:2);
    Graph.add_edge x (Key.make_0 ~:1, Key.make_0 ~:3);
    Graph.add_edge x (Key.make_0 ~:2, Key.make_0 ~:4);
    Graph.add_edge x (Key.make_0 ~:3, Key.make_0 ~:2);

    x
  | 3 ->
    let x = Graph.create () in

    Graph.add_vertex x (Key.make_0 ~:2);
    Graph.add_vertex x (Key.make_0 ~:1);
    Graph.add_vertex x (Key.make_0 ~:4);
    Graph.add_vertex x Key.Trailer;
    Graph.add_vertex x (Key.make_0 ~:3);

    Graph.add_edge x (Key.make_0 ~:2, Key.make_0 ~:4);
    Graph.add_edge x (Key.make_0 ~:1, Key.make_0 ~:2);
    Graph.add_edge x (Key.make_0 ~:3, Key.make_0 ~:2);
    Graph.add_edge x (Key.make_0 ~:1, Key.make_0 ~:3);

    x
  | _ ->
    Graph.create ()


let tests =
  "Graph" >:::
  [
    "equals" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    true
                    (Graph.equals (make_graph 2) (make_graph 3))) ;
      "(2)" >:: (fun _ -> assert_equal
                    true
                    (Graph.equals (make_graph 3) (make_graph 2))) ;
      "(3)" >:: (fun _ -> assert_equal
                    true
                    (Graph.equals (make_graph 1) (make_graph 1))) ;
      "(4)" >:: (fun _ -> assert_equal
                    false
                    (Graph.equals (make_graph 1) (make_graph 2))) ;
      "(5)" >:: (fun _ -> assert_equal
                    false
                    (Graph.equals (make_graph 3) (make_graph 1))) ;
    ] ;

    "neighbors" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (Graph.neighbors (make_graph 1) (Key.make_gen ~:3 ~:1))
                    [Key.make_0 ~:4]) ;
      "(2)" >:: (fun _ -> assert_equal
                    (Graph.neighbors (make_graph 1) (Key.make_0 ~:1))
                    [Key.make_0 ~:4 ; Key.make_0 ~:2]) ;
      "(3)" >:: (fun _ -> assert_equal
                    (Graph.neighbors (make_graph 1) (Key.make_0 ~:2))
                    []) ;
      "(4)" >:: (fun _ -> assert_equal
                    (Graph.neighbors (make_graph 1) (Key.make_0 ~:5))
                    []) ;
    ] ;
  ]

