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
open Boundedint
open Errors
open Algo

module XRefTable = struct

  type kind_t = Free | Inuse | Compressed of BoundedInt.t

  (* TODO : check positions of object *)
  type value_t = {
    off : BoundedInt.t;
    kind : kind_t;
  }

  type t = ((value_t list) MapKey.t) ref


  let create () : t =
    ref MapKey.empty

  let make_value (o : BoundedInt.t) (k : kind_t) : value_t = {
    off = o;
    kind = k;
  }

  let mem (x : t) (k : Key.t) : bool =
    MapKey.mem k !x

  let find_list (x : t) (k : Key.t) : value_t list =
    try
      MapKey.find k !x
    with Not_found ->
      []

  let find (x : t) (k : Key.t) (msg : string) : value_t =
    match find_list x k with
    | [] ->
      raise (Errors.PDFError (msg, Errors.make_ctxt_key k))
    | v::_ -> v

  let iter f (x : t) : unit =
    MapKey.iter f !x

  let iter_all f (x : t) : unit =
    MapKey.iter
      (fun key values ->
         List.iter (f key) values
      ) !x

  let fold f (x : t) a =
    MapKey.fold f !x a

  let fold_all f (x : t) a =
    MapKey.fold
      (fun key values b ->
         Algo.fold_left_start List.fold_left
           (fun started c value ->
              f key value c started
           ) b values
      ) !x a


  let compare (body : t) (xref : t) : unit =
    (* Iterate over objects in the body *)
    iter_all (fun key value ->
        let error_ctxt = Errors.make_ctxt key (Errors.make_pos_file value.off) in

        if not (MapKey.mem key !xref) then
          raise (Errors.PDFError ("Xref table does not contain object", error_ctxt));

        match key, MapKey.find key !xref with
        | Key.Object (0, _), [v] when value.kind = v.kind ->
          ()
        | _, [v] when value = v ->
          ()
        | _, [v] ->
          raise (Errors.PDFError (Printf.sprintf "Xref table states position %s, which does not match" (BoundedInt.to_string v.off), error_ctxt))
        | _, _ ->
          raise (Errors.PDFError ("Xref table does not match position of object", error_ctxt))
      ) body;

    (* Iterate over objects in the xref table *)
    iter_all (fun key v ->
        if not (MapKey.mem key !body) then
          raise (Errors.PDFError ("Xref table contains object that does not exist", Errors.make_ctxt key (Errors.make_pos_file v.off)));
      ) xref


  let print (out : out_channel) (x : t) : unit =
    let count, free, compressed, shadowed = fold_all
        (fun key v (count, free, compressed, shadowed) started ->
           let id, gen = Key.get_obj_ref key in
           if started then
             Printf.fprintf out "*";

           Printf.fprintf out "(%d, %d) " id gen;

           begin
             match v.kind with
             | Free ->
               Printf.fprintf out "[not used, next is %d]" (BoundedInt.to_int v.off)
             | Inuse ->
               Printf.fprintf out "at %d [0x%x]" (BoundedInt.to_int v.off) (BoundedInt.to_int v.off)
             | Compressed index ->
               Printf.fprintf out "in (%d, 0) [at %d]" (BoundedInt.to_int v.off) (BoundedInt.to_int index)
           end;
           Printf.fprintf out "\n";

           (count + 1, free + (if v.kind = Free then 1 else 0), compressed + (match v.kind with Compressed _ -> 1 | _ -> 0), shadowed + (if started then 1 else 0))
        ) x (0, 0, 0, 0)
    in

    Printf.fprintf out "\n%d object(s) in total\n%d free object(s)\n%d compressed object(s)\n%d shadowed object(s)\n" count free compressed shadowed

  let add (x : t) (k : Key.t) (v : value_t) : unit =
    x := MapKey.add k (
        try
          match MapKey.find k !x with
          | [] -> [v]
          | t::q -> t::v::q
        with Not_found ->
          [v]
      ) !x
  (*
  if MapKey.mem k !x then
    raise (Errors.PDFError "Several declarations of object in xref table")
  else
    x := MapKey.add k v !x
  *)

  let cleanup_zero_offsets (x : t) : unit =
    x := MapKey.mapi (fun k l ->
        List.map (fun v ->
            match v.kind with
            | Inuse when v.off = ~:0 ->
              Printf.eprintf "Warning : in-use object %s is at offset 0, it should be marked as free instead\n" (Key.to_string k);
              make_value ~:0 Free
            | _ -> v
          ) l
      ) !x

end
