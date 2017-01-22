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
open Key
open Setkey
open Boundedint
open Uniqueid
open Type
open Algo

module Graph = struct

  type node_t = Key.t

  type t = {
    mutable vertices : SetKey.t;
    mutable edges : (node_t list) MapKey.t;
  }


  let create () : t = {
    vertices = SetKey.empty;
    edges = MapKey.empty;
  }

  let neighbors (x : t) (v : node_t) : node_t list =
    try
      MapKey.find v x.edges
    with Not_found ->
      []

  let add_vertex (x : t) (v : node_t) : unit =
    x.vertices <- SetKey.add v x.vertices

  let add_edge (x : t) (e : node_t * node_t) : unit =
    let (src, dst) = e in
    let l = neighbors x src in
    x.edges <- MapKey.add src (dst::l) x.edges


  let iter_vertices f (x : t) : unit =
    SetKey.iter f x.vertices

  let iter_edges f (x : t) : unit =
    MapKey.iter
      (fun src l ->
         List.iter (f src) l
      ) x.edges

  let iter_neighbors f (x : t) (v : node_t) : unit =
    List.iter f (neighbors x v)


  let normalize (x : t) : ((Key.t list) * ((Key.t * Key.t) list)) =
    let list_v = ref [] in
    let list_e = ref [] in
    iter_vertices (fun v -> list_v := v::(!list_v)) x;
    iter_edges (fun u v -> list_e := (u, v)::(!list_e)) x;

    let compare_edges (x0, y0) (x1, y1) =
      match Key.compare x0 x1 with
      | 0 -> Key.compare y0 y1
      | x -> x
    in
    (!list_v, List.sort compare_edges !list_e)

  let equals (x : t) (y : t) : bool =
    (normalize x) = (normalize y)


  let print_dot_colors (graph : t) (f : Key.t -> string option) (filename : string) : unit =
    let out = open_out_bin filename in
    Printf.fprintf out "digraph objects {\n";

    iter_vertices
      (fun key ->
         let color =
           match (f key) with
           | None -> ""
           | Some c ->
             ", color=\"" ^ c ^ "\""
         in
         Printf.fprintf out "    R_%s [label=\"%s\"%s];\n" (Key.to_string_nosp key) (Key.to_string key) color
      ) graph;

    iter_edges
      (fun key0 key1 ->
         Printf.fprintf out "    R_%s -> R_%s;\n" (Key.to_string_nosp key0) (Key.to_string_nosp key1)
      ) graph;

    Printf.fprintf out "}\n";
    close_out out

  let print_dot (graph : t) (filename : string) : unit =
    print_dot_colors graph (fun _ -> None) filename

  let print_dot_types (graph : t) (types : Type.kind_t MapKey.t) (filename : string) : unit =
    let colors key =
      try
        let typ = MapKey.find key types in
        match typ with
        | Type.Class "resources" ->
          Some "green"
        | Type.Stream "content_stream" ->
          Some "blue"
        | Type.Class "destination" ->
          Some "orange"
        | Type.Class c when Algo.string_starts_with c "page" ->
          Some "red"
        | Type.Class c when Algo.string_starts_with c "annot_" ->
          Some "yellow"
        | Type.Class c when Algo.string_starts_with c "outline" ->
          Some "turquoise"
        | Type.Class c when Algo.string_starts_with c "nametree" ->
          Some "brown"
        | Type.Class c when Algo.string_starts_with c "action_" ->
          Some "magenta"
        | Type.Class c
        | Type.Stream c when Algo.string_starts_with c "font_" ->
          Some "purple"
        | _ ->
          None
      with Not_found ->
        None
    in
    print_dot_colors graph colors filename


  let print_visjs (graph : t) (filename : string) : unit =
    let ids = UniqueId.create () in
    let out = open_out_bin filename in

    Printf.fprintf out "var nodes = [\n";
    iter_vertices
      (fun v ->
         let lbl = Key.to_string v in
         let id = UniqueId.id ids lbl in
         Printf.fprintf out "    {id: %d, label: '%s'},\n" id lbl
      ) graph;
    Printf.fprintf out "];\n\n";

    Printf.fprintf out "var edges = [\n";
    iter_edges
      (fun v0 v1 ->
         let id0 = UniqueId.id ids (Key.to_string v0) in
         let id1 = UniqueId.id ids (Key.to_string v1) in
         Printf.fprintf out "    {from: %d, to: %d},\n" id0 id1
      ) graph;
    Printf.fprintf out "];\n\n";

    Printf.fprintf out "var container = document.getElementById('graph');\n";
    Printf.fprintf out "var data = {nodes: nodes, edges: edges};\n";
    Printf.fprintf out "var options = {edges: {style: 'arrow'}};\n";
    Printf.fprintf out "var graph = new vis.Network(container, data, options);\n";

    close_out out

end
