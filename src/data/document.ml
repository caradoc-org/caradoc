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


open Mapkey
open Key
open Setkey
open Errors
open Pdfobject
open Boundedint
open Common
open Graph
open Params

module Document = struct

  type kind_t = Objstm | Xrefstm

  type t = {
    mutable objects : PDFObject.t MapKey.t;
    mutable trailers : PDFObject.dict_t list;
    mutable special_streams : kind_t MapKey.t;
  }


  let create () : t = {
    objects = MapKey.empty;
    trailers = [];
    special_streams = MapKey.empty;
  }

  let mem (x : t) (k : Key.t) : bool =
    MapKey.mem k x.objects

  let find (x : t) (k : Key.t) : PDFObject.t =
    MapKey.find k x.objects

  let remove_ref (x : t) (o : PDFObject.t) : PDFObject.t =
    match o with
    | PDFObject.Reference k ->
      begin
        try
          find x k
        with Not_found ->
          PDFObject.Null
      end
    | _ -> o


  let finalize_trailers (x : t) : unit =
    x.trailers <- List.rev x.trailers

  let trailers (x : t) : PDFObject.dict_t list =
    x.trailers

  let main_trailer (x : t) : PDFObject.dict_t =
    match x.trailers with
    | [] -> raise (Errors.UnexpectedError "No trailer found in document")
    | t::_ -> t

  let add (x : t) (k : Key.t) (v : PDFObject.t) : unit =
    (* TODO : check unicity *)
    if MapKey.mem k x.objects then
      Printf.eprintf "Several declarations of object %s in xref table\n" (Key.to_string k)
    else
      x.objects <- MapKey.add k v x.objects
  (*
    raise (Common.PDFError "Several declarations of object in xref table")
  *)

  let set (x : t) (k : Key.t) (v : PDFObject.t) : unit =
    x.objects <- MapKey.add k v x.objects

  let add_trailer (x : t) (v : PDFObject.dict_t) : unit =
    x.trailers <- v::x.trailers

  let add_objstm (x : t) (k : Key.t) : unit =
    if not (MapKey.mem k x.special_streams) then (* TODO : necessary to check this? *)
      x.special_streams <- MapKey.add k Objstm x.special_streams

  let add_xrefstm (x : t) (k : Key.t) : unit =
    if not (MapKey.mem k x.special_streams) then (* TODO : necessary to check this? *)
      x.special_streams <- MapKey.add k Xrefstm x.special_streams


  let iter_objects f (x : t) : unit =
    MapKey.iter f x.objects

  let fold_objects f (x : t) a =
    MapKey.fold f x.objects a

  let map_objects f (x : t) : unit =
    x.objects <- MapKey.mapi f x.objects

  let iter_stms f (x : t) : unit =
    MapKey.iter f x.special_streams


  let check_refs (x : PDFObject.t MapKey.t) (s : SetKey.t) (indobj : Key.t) : SetKey.t =
    if Params.global.Params.undefined_ref_as_null then (
      let unknown_refs = SetKey.fold (fun key unknown_refs ->
          if not (MapKey.mem key x) then (
            Printf.eprintf "Warning : Reference to unknown object %s in object %s\n" (Key.to_string key) (Key.to_string indobj);
            SetKey.add key unknown_refs
          ) else
            unknown_refs
        ) s SetKey.empty
      in
      SetKey.diff s unknown_refs
    ) else (
      SetKey.iter (fun key ->
          if not (MapKey.mem key x) then
            raise (Errors.PDFError (Printf.sprintf "Reference to unknown object : %s" (Key.to_string key), Errors.make_ctxt_key indobj))
        ) s;
      s
    )

  let ref_closure (x : t) (o : PDFObject.t) (k : Key.t) : SetKey.t =
    let result = ref (SetKey.empty) in
    let queue = ref (check_refs x.objects (PDFObject.refs o) k) in

    while not (SetKey.is_empty !queue) do
      let key = SetKey.min_elt !queue in
      result := SetKey.add key !result;
      queue := SetKey.remove key !queue;

      let obj = MapKey.find key x.objects in
      let refs = SetKey.diff (check_refs x.objects (PDFObject.refs obj) key) !result in
      queue := SetKey.union refs !queue
    done;
    !result


  let graph (x : t) : Graph.t =
    let g = Graph.create () in

    let f key obj =
      let refs = PDFObject.refs obj in
      SetKey.iter
        (fun k ->
           Graph.add_edge g (key, k)
        ) refs;

      Graph.add_vertex g key;
    in

    let trailer = main_trailer x in
    iter_objects f x;
    f Key.Trailer (PDFObject.Dictionary trailer);
    g


  let print (out : out_channel) (x : t) : unit =
    List.iter
      (fun obj ->
         Printf.fprintf out "trailer\n";
         Printf.fprintf out "%s\n\n" (PDFObject.dict_to_string obj)
      ) x.trailers;
    MapKey.iter
      (fun key obj ->
         let id, gen = Key.get_obj_ref key in
         Printf.fprintf out "obj(%d, %d)\n" id gen;
         Printf.fprintf out "%s\n\n" (PDFObject.to_string obj)
      ) x.objects


  let print_pdf (out : out_channel) (x : t) : unit =
    let trailer = main_trailer x in
    let positions = ref MapKey.empty in

    let header = "%PDF-1.7\n%\x80\x80\x80\x80\n" in
    Printf.fprintf out "%s" header;
    let offset = ref (String.length header) in
    let count = ref 0 in

    MapKey.iter
      (fun key obj ->
         let id, gen = Key.get_obj_ref key in
         let content = Printf.sprintf "%d %d obj%s%s%sendobj\n" id gen (if PDFObject.need_space_before obj then " " else "") (PDFObject.to_pdf obj) (if PDFObject.need_space_after obj then " " else "") in
         Printf.fprintf out "%s" content;

         positions := MapKey.add key !offset !positions;
         offset := (String.length content) + !offset;
         incr count
      ) x.objects;

    Printf.fprintf out "xref\n0 %d\n" (1 + !count);
    Printf.fprintf out "0000000000 65535 f \n";
    MapKey.iter
      (fun _ pos ->
         Printf.fprintf out "%010d 00000 n \n" pos
      ) !positions;

    Printf.fprintf out "trailer\n%s\n" (PDFObject.dict_to_pdf trailer);
    Printf.fprintf out "startxref\n%d\n%%%%EOF" !offset


  let sanitize_trailer (newkeys : Key.t MapKey.t) (trailer : PDFObject.dict_t) : PDFObject.dict_t =
    let tmp = PDFObject.relink_dict newkeys Key.Trailer trailer in
    let size = 1 + (MapKey.cardinal newkeys) in
    let root = PDFObject.dict_find tmp "Root" in
    let info = PDFObject.dict_find tmp "Info" in
    let id = PDFObject.dict_find tmp "ID" in

    let result = PDFObject.dict_create () in
    if root <> PDFObject.Null then
      PDFObject.dict_set result ("Root", root);
    if info <> PDFObject.Null then
      PDFObject.dict_set result ("Info", info);
    if id <> PDFObject.Null then
      PDFObject.dict_set result ("ID", id);
    PDFObject.dict_set result ("Size", PDFObject.Int ~:size);
    result


  let simplify_refs (x : t) : t =
    let trailer = main_trailer x in
    let xx = ref MapKey.empty in
    MapKey.iter (fun key obj ->
        xx := MapKey.add key (PDFObject.simplify_refs x.objects key obj) !xx
      ) x.objects;

    {
      objects = !xx;
      trailers = [PDFObject.dict_simplify_refs x.objects Key.Trailer trailer];
      special_streams = MapKey.empty;
    }


  let sanitize_nums (x : t) : t =
    let trailer = main_trailer x in
    let used = ref_closure x (PDFObject.Dictionary trailer) Key.Trailer in

    let newkeys = ref MapKey.empty in
    let next = ref 1 in
    SetKey.iter (fun key ->
        newkeys := MapKey.add key (Key.make_0 ~:(!next)) !newkeys;
        incr next
      ) used;

    let xx = ref MapKey.empty in
    SetKey.iter (fun key ->
        let o = MapKey.find key x.objects in
        xx := MapKey.add (MapKey.find key !newkeys) (PDFObject.relink !newkeys key o) !xx
      ) used;

    {
      objects = !xx;
      trailers = [sanitize_trailer !newkeys trailer];
      special_streams = MapKey.empty;
    }

end

