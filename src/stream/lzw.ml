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


open Print

(*************************)
(* PDF reference 7.4.4.2 *)
(*************************)
module LZW = struct

  type code =
    | Char of char
    | Clear
    | EOD
    | Null
    | Code of int * int

  type dec_state_t = {
    mutable table : code array;
    (* Current table size *)
    mutable size : int;
    mutable bitsize : int;
    (* Bits left from previous value *)
    mutable tmp : int;
    mutable tmpsize : int;
    (* Position inside input *)
    mutable pos : int;
    (* EOD reached *)
    mutable eod : bool;
  }

  let make_table () : code array =
    Array.init 4096 (fun i ->
        if i < 256 then
          Char (char_of_int i)
        else if i == 256 then
          Clear
        else if i == 257 then
          EOD
        else
          Null
      )

  let make_state () : dec_state_t = {
    table = make_table ();
    size = 258;
    bitsize = 9;
    tmp = 0;
    tmpsize = 0;
    pos = 0;
    eod = false;
  }

  let next_index (s : dec_state_t) (content : string) (len : int) : int =
    (* Extract enough bits for the next index *)
    while s.tmpsize < s.bitsize do
      if s.pos = len then (
        Print.debug "[LZW] missing input bytes";
        raise Exit
      );
      s.tmp <- s.tmp * 256 + (int_of_char content.[s.pos]);
      s.tmpsize <- s.tmpsize + 8;
      s.pos <- s.pos + 1
    done;

    (* Trim index to bitsize bits *)
    s.tmpsize <- s.tmpsize - s.bitsize;
    let shift = s.tmpsize in
    let index = s.tmp lsr shift in
    s.tmp <- s.tmp - (index lsl shift);

    index

  let grow_table (s : dec_state_t) : unit =
    (* Increase table size *)
    s.size <- s.size + 1;
    if s.size = 512 || s.size = 1024 || s.size = 2048 then
      s.bitsize <- s.bitsize + 1
    else if s.size = 4095 then (
      Print.debug "[LZW] max size reached";
      raise Exit
    )

  let clear_table (s : dec_state_t) : unit =
    s.table <- make_table ();
    s.size <- 258;
    s.bitsize <- 9


  let decode (content : string) : string option =
    let s = make_state () in
    let buf = Buffer.create 16 in
    let len = String.length content in

    begin
      try
        while s.pos < len do
          let index = next_index s content len in

          (* Execute opcode *)
          begin
            match s.table.(index) with
            | Char c ->
              s.table.(s.size) <- Code (Buffer.length buf, 2);
              Buffer.add_char buf c;
              grow_table s
            | Code (pos, size) ->
              s.table.(s.size) <- Code (Buffer.length buf, size + 1);
              for i = 1 to size do
                Buffer.add_char buf (Buffer.nth buf (pos + i - 1));
              done;
              grow_table s
            | Clear ->
              clear_table s
            | EOD ->
              if s.pos <> len then
                Print.debug "[LZW] data after EOD"
              else if s.tmp <> 0 then
                Print.debug "[LZW] expected padding with zeros"
              else
                s.eod <- true;
              raise Exit
            | Null ->
              Print.debug "[LZW] invalid code";
              raise Exit
          end;
        done;

        Print.debug "[LZW] EOD missing";
      with Exit ->
        ()
    end;

    if s.eod then
      Some (Buffer.contents buf)
    else
      None


  type enc_state_t = {
    mutable tmp : int;
    mutable tmpsize : int;
  }

  let write_9bit (buf : Buffer.t) (s : enc_state_t) (x : int) : unit =
    s.tmp <- (s.tmp lsl 9) + x;
    s.tmpsize <- s.tmpsize + 9;
    while s.tmpsize >= 8 do
      s.tmpsize <- s.tmpsize - 8;
      let shift = s.tmpsize in
      let y = s.tmp lsr shift in
      Buffer.add_char buf (char_of_int y);
      s.tmp <- s.tmp - (y lsl shift);
    done

  let flush (buf : Buffer.t) (s : enc_state_t) : unit =
    if s.tmpsize > 0 then (
      let shift = 8 - s.tmpsize in
      let y = s.tmp lsl shift in
      Buffer.add_char buf (char_of_int y)
    )

  let encode (content : string) : string =
    (* Dumb encoder char by char *)
    let buf = Buffer.create 16 in
    let s = {
      tmp = 0;
      tmpsize = 0;
    } in

    for i = 0 to (String.length content) - 1 do
      (* Clear table before it reaches 512 entries *)
      if i mod 253 = 0 then
        write_9bit buf s 256;

      let c = content.[i] in
      write_9bit buf s (int_of_char c)
    done;

    (* EOD *)
    write_9bit buf s 257;
    flush buf s;
    Buffer.contents buf

end

