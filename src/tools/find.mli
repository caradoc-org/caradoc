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


module Find : sig

  (*   Find all references to an object
       Args    :
       - reference to find
       - PDF file name to search into
       - whether to show context of occurrences
       - whether to highlight occurrences in console
  *)
  val find_ref : Key.t -> string -> bool -> bool -> unit

  (*   Find all occurrences of a PDF name
       Args    :
       - reference to find
       - PDF file name to search into
       - whether to show context of occurrences
       - whether to highlight occurrences in console
  *)
  val find_name : string -> string -> bool -> bool -> unit

end

