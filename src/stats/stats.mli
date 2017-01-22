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


module Stats : sig

  type version_t =
    | NotPDF
    | Unknown
    | Version of int

  type t = {
    (* PDF minor version (0-7) *)
    mutable version : version_t;
    (* Encrypted document *)
    mutable encrypted : bool;
    (* Number of updates *)
    mutable updatecount : int;
    (* Contains object streams *)
    mutable objstm : bool;
    (* Contains free objects *)
    mutable free : bool;
    (* Number of objects *)
    mutable objcount : int;
    (* Stream filters used *)
    mutable filters : (string, int) Hashtbl.t;
    (* Number of objects of known type *)
    mutable knowntypes : int;
    (* Not all types were fully checked *)
    mutable incompletetypes : bool;
    (* No error found on graph *)
    mutable nographerror : bool;
    (* No error found in content streams *)
    mutable nocontentstreamerror : bool;

    (**** /Info dictionary ****)
    (* ∕Producer field in /Info dictionary *)
    mutable producer : string option;
    (* ∕Creator field in /Info dictionary *)
    mutable creator : string option;
    (* ∕CreationDate field in /Info dictionary *)
    mutable creation_date : string option;
    (* ∕ModDate field in /Info dictionary *)
    mutable mod_date : string option;

    (**** /Encrypt dictionary ****)
    (* ∕V field in /Encrypt dictionary *)
    mutable encrypt_v : int option;
    (* ∕R field in /Encrypt dictionary *)
    mutable encrypt_r : int option;
    (* ∕U field in /Encrypt dictionary *)
    mutable encrypt_u : string option;
    (* ∕O field in /Encrypt dictionary *)
    mutable encrypt_o : string option;
    (* First element of ∕ID field *)
    mutable encrypt_id : string option;
    (* Validity of user password *)
    mutable verify_user : bool;
    (* Validity of owner password *)
    mutable verify_owner : bool;
  }

  val create : unit -> t
  val print : t -> unit

end

