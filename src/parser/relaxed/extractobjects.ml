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


open Document
open Intervals
open Xref
open Fetchcommon
open Fetchimpl
open Key
open Indirectobject
open Params
open Boundedint


let parseobjects (input : in_channel) (length : BoundedInt.t) (xref : XRefTable.t) (intervals : Key.t Intervals.t) (doc : Document.t) : unit =
  let ctxt = FetchCommon.make_context input length xref intervals doc in

  XRefTable.iter_all
    (fun key entry ->
       begin
         match entry.XRefTable.kind with
         | XRefTable.Inuse ->
           if Params.global.Params.debug then
             Printf.eprintf "Extracting in-use object %s\n" (Key.to_string key);
           let (_:IndirectObject.t) = FetchImpl.fetchobject key entry.XRefTable.off ctxt in ()
         | XRefTable.Compressed index ->
           if Params.global.Params.debug then
             Printf.eprintf "Extracting compressed object %s\n" (Key.to_string key);
           let (_:IndirectObject.t) = FetchCompImpl.fetchcompressed key entry.XRefTable.off index ctxt in ()
         | XRefTable.Free ->
           if Params.global.Params.debug then
             Printf.eprintf "Free object %s\n" (Key.to_string key);
       end;
       if Params.global.Params.debug then
         Printf.eprintf "Object %s done\n" (Key.to_string key);
    ) xref

