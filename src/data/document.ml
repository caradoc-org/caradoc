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
open Errors
open Indirectobject
open Directobject
open Boundedint
open Common
open Graph
open Params
open Errors
open Entry
open Crypto

module Document = struct

  type kind_t = Objstm | Xrefstm

  type t = {
    mutable objects : IndirectObject.t MapKey.t;
    mutable trailers : DirectObject.dict_t list;
    mutable special_streams : kind_t MapKey.t;
    mutable crypto : Crypto.t option;
  }


  let create () : t = {
    objects = MapKey.empty;
    trailers = [];
    special_streams = MapKey.empty;
    crypto = None;
  }

  let mem_obj (x : t) (k : Key.t) : bool =
    MapKey.mem k x.objects

  let find_obj (x : t) (k : Key.t) : IndirectObject.t =
    MapKey.find k x.objects

  let remove_ref (x : t) (o : DirectObject.t) (ctxt : Errors.error_ctxt) : (IndirectObject.t * Errors.error_ctxt) =
    match o with
    | DirectObject.Reference k ->
      begin
        try
          find_obj x k, Errors.make_ctxt_key k
        with Not_found ->
          IndirectObject.Direct DirectObject.Null, ctxt
      end
    | _ -> IndirectObject.Direct o, ctxt


  let finalize_trailers (x : t) : unit =
    x.trailers <- List.rev x.trailers

  let trailers (x : t) : DirectObject.dict_t list =
    x.trailers

  let main_trailer (x : t) : DirectObject.dict_t =
    match x.trailers with
    | [] -> raise (Errors.UnexpectedError "No trailer found in document")
    | t::_ -> t

  let add (x : t) (k : Key.t) (v : IndirectObject.t) : unit =
    (* TODO : check unicity *)
    if MapKey.mem k x.objects then
      Printf.eprintf "Several declarations of object %s in xref table\n" (Key.to_string k)
    else
      x.objects <- MapKey.add k v x.objects

  let set (x : t) (k : Key.t) (v : IndirectObject.t) : unit =
    x.objects <- MapKey.add k v x.objects

  let add_trailer (x : t) (v : DirectObject.dict_t) : unit =
    x.trailers <- v::x.trailers

  let add_objstm (x : t) (k : Key.t) : unit =
    if not (MapKey.mem k x.special_streams) then (* TODO : necessary to check this? *)
      x.special_streams <- MapKey.add k Objstm x.special_streams

  let add_xrefstm (x : t) (k : Key.t) : unit =
    if not (MapKey.mem k x.special_streams) then (* TODO : necessary to check this? *)
      x.special_streams <- MapKey.add k Xrefstm x.special_streams

  let add_crypto (x : t) (crypto : Crypto.t) : unit =
    x.crypto <- Some crypto

  let crypto (x : t) : Crypto.t option =
    x.crypto


  let iter_objects f (x : t) : unit =
    MapKey.iter f x.objects

  let fold_objects f (x : t) a =
    MapKey.fold f x.objects a

  let map_objects f (x : t) : unit =
    x.objects <- MapKey.mapi f x.objects

  let iter_stms f (x : t) : unit =
    MapKey.iter f x.special_streams


  let max_objnum (x : t) : BoundedInt.t =
    fold_objects (fun k _ m ->
        let i, _ = Key.get_obj_ref k in
        let j = ~:i in
        if j >: m then
          j
        else
          m
      ) x ~:0


  let find (indirect_find : 'a -> IndirectObject.t -> Entry.t list) (dict_find : 'a -> DirectObject.dict_t -> Entry.t list) (what : 'a) (x : t) : Entry.t list MapKey.t =
    let tmp = fold_objects (fun k o m ->
        let l = indirect_find what o in
        if l = [] then
          m
        else
          MapKey.add k l m
      ) x MapKey.empty
    in

    let l = dict_find what (main_trailer x) in
    if l = [] then
      tmp
    else
      MapKey.add Key.Trailer l tmp

  let find_ref : Key.t -> t -> Entry.t list MapKey.t =
    find IndirectObject.find_ref DirectObject.find_ref_dict

  let find_name : string -> t -> Entry.t list MapKey.t =
    find IndirectObject.find_name DirectObject.find_name_dict


  let undef_refs_to_null (x : t) : unit =
    let warnings = ref [] in
    x.trailers <- List.map (DirectObject.undef_refs_to_null_dict x.objects warnings (Errors.make_ctxt_key Key.Trailer)) x.trailers;
    x.objects <- MapKey.mapi (fun key obj ->
        IndirectObject.undef_refs_to_null x.objects warnings (Errors.make_ctxt_key key) obj
      ) x.objects;
    List.iter (fun (key, ctxt) ->
        Errors.warning (Printf.sprintf "Reference to unknown object : %s" (Key.to_string key)) ctxt
      ) (List.rev !warnings)

  let check_refs (objects : IndirectObject.t MapKey.t) (refs : Entry.t MapKey.t) (ctxt : Errors.error_ctxt) : SetKey.t =
    MapKey.iter (fun key entry ->
        if not (MapKey.mem key objects) then
          raise (Errors.PDFError (Printf.sprintf "Reference to unknown object : %s" (Key.to_string key), Errors.ctxt_append_entry ctxt entry))
      ) refs;

    MapKey.fold (fun key _ result ->
        SetKey.add key result
      ) refs SetKey.empty

  let ref_closure (x : t) (o : IndirectObject.t) (k : Key.t) : SetKey.t =
    let result = ref (SetKey.empty) in
    let queue = ref (check_refs x.objects (IndirectObject.refs o) (Errors.make_ctxt_key k)) in

    while not (SetKey.is_empty !queue) do
      let key = SetKey.min_elt !queue in
      result := SetKey.add key !result;
      queue := SetKey.remove key !queue;

      let obj = MapKey.find key x.objects in
      let refs = SetKey.diff (check_refs x.objects (IndirectObject.refs obj) (Errors.make_ctxt_key key)) !result in
      queue := SetKey.union refs !queue
    done;
    !result


  let graph (x : t) : Graph.t =
    let g = Graph.create () in

    let f key obj =
      MapKey.iter
        (fun k _ ->
           Graph.add_edge g (key, k)
        ) (IndirectObject.refs obj);

      Graph.add_vertex g key;
    in

    let trailer = main_trailer x in
    iter_objects f x;
    f Key.Trailer (IndirectObject.Direct (DirectObject.Dictionary trailer));
    g


  let print (out : out_channel) (x : t) : unit =
    List.iter
      (fun obj ->
         Printf.fprintf out "trailer\n";
         Printf.fprintf out "%s\n\n" (DirectObject.dict_to_string obj)
      ) x.trailers;
    MapKey.iter
      (fun key obj ->
         let id, gen = Key.get_obj_ref key in
         Printf.fprintf out "obj(%d, %d)\n" id gen;
         Printf.fprintf out "%s\n\n" (IndirectObject.to_string obj)
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
         let content = Printf.sprintf "%d %d obj%s%s%sendobj\n" id gen (if IndirectObject.need_space_before obj then " " else "") (IndirectObject.to_pdf obj) (if IndirectObject.need_space_after obj then " " else "") in
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

    Printf.fprintf out "trailer\n%s\n" (DirectObject.dict_to_pdf trailer);
    Printf.fprintf out "startxref\n%d\n%%%%EOF" !offset


  let simplify_info_dict (d : DirectObject.dict_t) : DirectObject.t =
    (* TODO : check this list of keys *)
    DirectObject.Dictionary (DirectObject.dict_simplify ["Title" ; "Author" ; "Subject" ; "Keywords" ; "Creator" ; "Producer" ; "CreationDate" ; "ModDate" ; "Trapped"] d)

  let simplify_trailer (x : t) (simplify_info : bool) : DirectObject.dict_t =
    let trailer = main_trailer x in
    let tmp = IndirectObject.simplify_refs_dict x.objects (Errors.make_ctxt_key Key.Trailer) trailer in
    let result = DirectObject.dict_simplify ["Size" ; "Root" ; "Info" ; "ID"] tmp in

    if simplify_info then (
      let info = DirectObject.dict_find result "Info" in
      if info <> DirectObject.Null then (
        match info with
        | DirectObject.Dictionary d ->
          DirectObject.dict_set result ("Info", simplify_info_dict d)
        | DirectObject.Reference k ->
          let d = IndirectObject.get_direct_of
              "Expected a dictionary for object /Info" (Errors.make_ctxt_key k)
              ~transform:(DirectObject.get_dict ())
              (find_obj x k) in
          set x k (IndirectObject.Direct (simplify_info_dict d))
        | _ ->
          raise (Errors.PDFError ("Expected dictionary or reference", Errors.make_ctxt_name Key.Trailer "Info"))
      )
    );

    result

  let simplify_refs (x : t) (simplify_info : bool) : t =
    let trailer = simplify_trailer x simplify_info in

    let xx = ref MapKey.empty in
    MapKey.iter (fun key obj ->
        xx := MapKey.add key (IndirectObject.simplify_refs x.objects (Errors.make_ctxt_key key) obj) !xx
      ) x.objects;

    {
      objects = !xx;
      trailers = [trailer];
      special_streams = MapKey.empty;
      crypto = x.crypto;
    }


  let sanitize_trailer (newkeys : Key.t MapKey.t) (trailer : DirectObject.dict_t) : DirectObject.dict_t =
    let result = DirectObject.relink_dict newkeys (Errors.make_ctxt_key Key.Trailer) trailer in
    let size = 1 + (MapKey.cardinal newkeys) in
    DirectObject.dict_set result ("Size", DirectObject.Int ~:size);
    result


  let sanitize_nums (x : t) : t =
    let trailer = main_trailer x in
    let used = ref_closure x (IndirectObject.Direct (DirectObject.Dictionary trailer)) Key.Trailer in

    let newkeys = ref MapKey.empty in
    let next = ref 1 in
    SetKey.iter (fun key ->
        newkeys := MapKey.add key (Key.make_0 ~:(!next)) !newkeys;
        incr next
      ) used;

    let xx = ref MapKey.empty in
    SetKey.iter (fun key ->
        let o = MapKey.find key x.objects in
        xx := MapKey.add (MapKey.find key !newkeys) (IndirectObject.relink !newkeys (Errors.make_ctxt_key key) o) !xx
      ) used;

    {
      objects = !xx;
      trailers = [sanitize_trailer !newkeys trailer];
      special_streams = MapKey.empty;
      crypto = x.crypto;
    }

end

