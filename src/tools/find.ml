(*****************************************************************************)
(*  Caradoc: a PDF parser and validator                                      *)
(*  Copyright (C) 2016-2017 Guillaume Endignoux                              *)
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


open Key
open File
open Document
open Mapkey
open Errors
open Entry
open Directobject
open Indirectobject
open Stats


module Find = struct

  let print_occurrences (occurrences : Entry.t list MapKey.t) (doc : Document.t) (show_ctxt : bool) (highlight : bool) : unit =
    if occurrences = MapKey.empty then (
      print_string "Not found\n";
      exit 255
    ) else (
      let count = ref 0 in
      let count_obj = ref 0 in

      MapKey.iter (fun k l ->
          count_obj := !count_obj + 1;
          List.iter (fun entry ->
              count := !count + 1;
              Printf.printf "Found%s\n" (Errors.ctxt_to_string (Errors.make_ctxt_entry k entry))
            ) l;

          if show_ctxt then (
            let selector =
              if highlight then
                Entry.make_selector l
              else
                Entry.no_selector
            in

            let tmp =
              if k = Key.Trailer then
                DirectObject.dict_to_string_hl (Document.main_trailer doc) selector
              else
                IndirectObject.to_string_hl (Document.find_obj doc k) selector
            in
            Printf.printf "%s\n\n" tmp
          )
        ) occurrences;

      Printf.printf "Found %d occurrence(s) in %d object(s).\n" !count !count_obj
    )


  let find_ref (key : Key.t) (filename : string) (show_ctxt : bool) (highlight : bool) : unit =
    let doc = File.parse_file filename (Stats.create ()) in
    let occurrences = Document.find_ref key doc in
    print_occurrences occurrences doc show_ctxt highlight

  let find_name (name : string) (filename : string) (show_ctxt : bool) (highlight : bool) : unit =
    let doc = File.parse_file filename (Stats.create ()) in
    let occurrences = Document.find_name name doc in
    print_occurrences occurrences doc show_ctxt highlight

end

