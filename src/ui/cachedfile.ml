(*****************************************************************************)
(*  Caradoc: a PDF parser and validator                                      *)
(*  Copyright (C) 2017 Guillaume Endignoux                                   *)
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


open Document
open Stats
open Directobject
open Indirectobject
open Pdfstream
open Openfile
open Errors
open Mapkey

module CachedFile = struct

  type t = {
    filename : string;
    contents : string;
    doc : Document.t option;
    stats : Stats.t;
  }


  let make () : t =
    {
      filename = "";
      contents = "";
      doc = None;
      stats = Stats.create ();
    }

  let read_file (filename : string) : string =
    let input = OpenFile.in_bin filename in
    let l = in_channel_length input in
    let buf = Buffer.create l in
    for i = 0 to l-1 do
      Buffer.add_char buf (input_char input)
    done;
    close_in input;
    Buffer.contents buf

  let make_file (filename : string) : t =
    let contents = read_file filename in
    let stats = Stats.create () in
    let doc = Errors.catch_msg
        ~fail_msg:(fun _ -> None)
        (fun () ->
           let d = File.parse_file filename stats in
           Errors.catch_msg ~fail_msg:ignore (fun () -> File.extract_info d stats);
           Some d
        )
    in

    {
      filename = filename;
      contents = contents;
      doc = doc;
      stats = stats;
    }

  let filename (x : t) : string =
    x.filename

  let contents (x : t) : string =
    x.contents

  let fetch_stats (x : t) : Stats.t =
    x.stats

  let fetch_trailers (x : t) : DirectObject.dict_t list =
    match x.doc with
    | Some d ->
      Document.trailers d
    | None ->
      let input = OpenFile.in_bin x.filename in
      let trailers = File.extract_trailers input in
      close_in input;
      trailers

  let fetch_object (x : t) (k : Key.t) : IndirectObject.t =
    match x.doc with
    | Some d ->
      begin
        try
          Document.find_obj d k
        with Not_found ->
          raise (Errors.PDFError ("Object not found in xref table", Errors.make_ctxt_key k))
      end
    | None ->
      let input = OpenFile.in_bin x.filename in
      let obj = File.extract_object input k in
      close_in input;
      obj

  let decode_stream (x : t) (k : Key.t) : string =
    let obj = fetch_object x k in
    match obj with
    | IndirectObject.Direct _ ->
      raise (Errors.PDFError ("Object is not a stream, cannot decode it", Errors.make_ctxt_key k))
    | IndirectObject.Stream s ->
      PDFStream.get_decoded s (Errors.make_ctxt_key k)

  let fetch_refs (x : t) (k : Key.t) : Errors.error_ctxt array =
    match x.doc with
    | Some d ->
      let occurrences = Document.find_ref k d in
      let l = MapKey.fold (fun k l result ->
          List.fold_left (fun res entry ->
              (Errors.make_ctxt_entry k entry)::res
            ) result l
        ) occurrences []
      in
      Array.of_list l
    | None ->
      raise (Errors.PDFError ("Could not load document", Errors.ctxt_none))

end

