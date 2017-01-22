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


open Operator

module Resources = struct

  type dict_t = (string, unit) Hashtbl.t

  type t = {
    extgstate : dict_t;
    font : dict_t;
    colorspace : dict_t;
    shading : dict_t;
    xobject : dict_t;
    properties : dict_t;
  }


  let make () : t = {
    extgstate = Hashtbl.create 16;
    font = Hashtbl.create 16;
    colorspace = Hashtbl.create 16;
    shading = Hashtbl.create 16;
    xobject = Hashtbl.create 16;
    properties = Hashtbl.create 16;
  }

  let add_command (resources : t) (command : Operator.t) : unit =
    match command with
    | Operator.Op_gs x ->
      Hashtbl.replace resources.extgstate x ()
    | Operator.Op_Tf (x, _) ->
      Hashtbl.replace resources.font x ()
    | Operator.Op_CS x
    | Operator.Op_cs x ->
      Hashtbl.replace resources.colorspace x ()
    | Operator.Op_sh x ->
      Hashtbl.replace resources.shading x ()
    | Operator.Op_Do x ->
      Hashtbl.replace resources.xobject x ()
    | Operator.Op_DP2 (_, x)
    | Operator.Op_BDC2 (_, x) ->
      Hashtbl.replace resources.properties x ()
    | _ -> ()

  let from_commands (commands : Operator.t list) : t =
    let resources = make () in
    List.iter (add_command resources) commands;
    resources

  let dict_to_buf (buf : Buffer.t) (title : string) (d : dict_t) : unit =
    Buffer.add_string buf title;
    Buffer.add_string buf " :";
    Hashtbl.iter (fun s _ ->
        Buffer.add_char buf ' ';
        Buffer.add_string buf s
      ) d;
    Buffer.add_char buf '\n'

  let to_string (x : t) : string =
    let buf = Buffer.create 16 in
    dict_to_buf buf "/ExtGState" x.extgstate;
    dict_to_buf buf "/Font" x.font;
    dict_to_buf buf "/ColorSpace" x.colorspace;
    dict_to_buf buf "/Shading" x.shading;
    dict_to_buf buf "/XObject" x.xobject;
    dict_to_buf buf "/Properties" x.properties;
    Buffer.contents buf

end

