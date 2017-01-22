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


open OUnit
open Directobject
open Indirectobject.IndirectObject
open Mapkey
open Errors
open Boundedint
open Params
open Pdfstream
open Entry


let init_params () =
  Params.clear_global ();
  Params.global.Params.sort_dicts <- true

let tests =
  "IndirectObject" >:::
  [
    "to_string" >:::
    [
      "direct" >:::
      [
        "(1)" >:: (fun _ -> assert_equal
                      (init_params (); to_string (Direct DirectObject.Null))
                      "null") ;
        "(2)" >:: (fun _ -> assert_equal
                      (init_params (); to_string (Direct (DirectObject.Bool true)))
                      "true") ;
        "(3)" >:: (fun _ -> assert_equal
                      (init_params (); to_string (Direct (DirectObject.Int ~:531)))
                      "531") ;
        "(4)" >:: (fun _ -> assert_equal
                      (init_params (); to_string (Direct (DirectObject.Real "1.0")))
                      "1.0") ;
        "(5)" >:: (fun _ -> assert_equal
                      (init_params (); to_string (Direct (DirectObject.String "hello\nworld")))
                      "(hello\\nworld)") ;
        "(6)" >:: (fun _ -> assert_equal
                      (init_params (); to_string (Direct (DirectObject.Name "hello")))
                      "/hello") ;
        "(7)" >:: (fun _ -> assert_equal
                      (init_params (); to_string (Direct (DirectObject.Array [DirectObject.Bool true ; DirectObject.String "blabla" ; DirectObject.Name "key"])))
                      "[true (blabla) /key]") ;
        "(8)" >:: (fun _ -> assert_equal
                      (init_params (); to_string (Direct (DirectObject.Dictionary (TestDict.add_all ["Key", DirectObject.String "value" ; "Foo", DirectObject.Name "bar"]))))
                      "<<\n    /Foo /bar\n    /Key (value)\n>>") ;
        "(9)" >:: (fun _ -> assert_equal
                      (init_params (); to_string (Direct (DirectObject.Reference (Key.make_gen ~:3 ~:2))))
                      "3 2 R") ;
      ] ;

      "stream" >:::
      [
        "(1)" >:: (fun _ -> assert_equal
                      (init_params (); to_string (Stream (TestStream.make_decoded "stream content")))
                      "<<\n    /Length 14\n>>\nstream <decoded stream of length 14>") ;
        "(2)" >:: (fun _ -> assert_equal
                      (init_params ();
                       Params.global.Params.expand_streams <- true;
                       to_string (Stream (TestStream.make_decoded "stream content")))
                      "<<\n    /Length 14\n>>\nstream <decoded stream of length 14>\nstream content\nendstream\n") ;
        "(3)" >:: (fun _ -> assert_equal
                      (init_params (); to_string (Stream (TestStream.make_raw_dict ["foo", DirectObject.String "bar"] "encoded content")))
                      "<<\n    /Length 15\n    /foo (bar)\n>>\nstream <encoded stream of length 15>") ;
        "(4)" >:: (fun _ -> assert_equal
                      (init_params ();
                       Params.global.Params.expand_streams <- true;
                       to_string (Stream (TestStream.make_raw "encoded content")))
                      "<<\n    /Length 15\n>>\nstream <encoded stream of length 15>\nencoded content\nendstream\n") ;
      ] ;
    ] ;

    "to_pdf" >:::
    [
      "direct" >:::
      [
        "(1)" >:: (fun _ -> assert_equal
                      (to_pdf (Direct DirectObject.Null))
                      "null") ;
        "(2)" >:: (fun _ -> assert_equal
                      (to_pdf (Direct (DirectObject.Bool false)))
                      "false") ;
        "(3)" >:: (fun _ -> assert_equal
                      (to_pdf (Direct (DirectObject.Int ~:(-10))))
                      "-10") ;
        "(4)" >:: (fun _ -> assert_equal
                      (to_pdf (Direct (DirectObject.Real "1.0")))
                      "1.0") ;
        "(5)" >:: (fun _ -> assert_equal
                      (to_pdf (Direct (DirectObject.String "hello\n\r\t\b\x0C\\world")))
                      "(hello\\n\\r\\t\\b\\f\\\\world)") ;
        "(6)" >:: (fun _ -> assert_equal
                      (to_pdf (Direct (DirectObject.Name "hello\n#world")))
                      "/hello#0A#23world") ;
        "(7)" >:: (fun _ -> assert_equal
                      (to_pdf (Direct (DirectObject.Array [DirectObject.String "test" ; DirectObject.Bool true ; DirectObject.Int ~:123 ; DirectObject.String "blabla" ; DirectObject.Name "key" ; DirectObject.Null ; DirectObject.Name "name" ; DirectObject.Name "other" ; DirectObject.String "value" ; DirectObject.Int ~:456])))
                      "[(test)true 123(blabla)/key null/name/other(value)456]") ;
        "(8)" >:: (fun _ -> assert_equal
                      (to_pdf (Direct (DirectObject.Dictionary (TestDict.add_all ["Key", DirectObject.String "value" ; "Foo", DirectObject.Bool false ; "G", DirectObject.Dictionary (TestDict.add_all ["Nested", DirectObject.Int ~:456])]))))
                      "<</Foo false/G<</Nested 456>>/Key(value)>>") ;
        "(9)" >:: (fun _ -> assert_equal
                      (to_pdf (Direct (DirectObject.Reference (Key.make_gen ~:3 ~:2))))
                      "3 2 R") ;
      ] ;

      "stream" >:::
      [
        "(1)" >:: (fun _ -> assert_equal
                      (to_pdf (Stream (TestStream.make_raw "raw content")))
                      "<</Length 11>>stream\nraw content\nendstream") ;
      ] ;
    ] ;

    "find_ref" >:::
    [
      "direct" >:::
      [
        "(1)" >:: (fun _ -> assert_equal
                      (find_ref (Key.make_0 ~:1) (Direct (DirectObject.Array [DirectObject.Reference (Key.make_0 ~:1) ; DirectObject.Bool false ; DirectObject.Reference (Key.make_0 ~:1)])))
                      [Entry.make_index 0 ; Entry.make_index 2]) ;
      ] ;

      "stream" >:::
      [
        "(1)" >:: (fun _ -> assert_equal
                      (find_ref (Key.make_0 ~:2) (Stream (TestStream.make_raw_dict ["Key", DirectObject.Reference (Key.make_0 ~:2)] "raw content")))
                      [Entry.make_name "Key"]) ;
      ] ;
    ] ;

    "find_name" >:::
    [
      "direct" >:::
      [
        "(1)" >:: (fun _ -> assert_equal
                      (find_name "Foo" (Direct (DirectObject.Array [DirectObject.Name "Foo" ; DirectObject.Int ~:123 ; DirectObject.Name "Foo" ; DirectObject.String "Foo"])))
                      [Entry.make_index 0 ; Entry.make_index 2]) ;
      ] ;

      "stream" >:::
      [
        "(1)" >:: (fun _ -> assert_equal
                      (find_name "Foo" (Stream (TestStream.make_raw_dict ["Foo", DirectObject.Reference (Key.make_0 ~:2) ; "Bar", DirectObject.Array [DirectObject.Name "Foo"]] "raw content")))
                      [Entry.append_index (Entry.make_name "Bar") 0 ; Entry.make_name_key "Foo"]) ;
      ] ;
    ] ;

    "refs" >:::
    [
      "direct" >:::
      [
        "(1)" >:: (fun _ -> assert_equal
                      (refs (Direct (DirectObject.Array [DirectObject.Reference (Key.make_gen ~:2 ~:1) ; DirectObject.Reference (Key.make_0 ~:5) ; DirectObject.Reference (Key.make_gen ~:2 ~:1) ; DirectObject.Reference (Key.make_0 ~:3) ; DirectObject.Reference (Key.make_0 ~:123456)])))
                      (TestMapkey.add_all [Key.make_gen ~:2 ~:1, Entry.make_index 0 ; Key.make_0 ~:5, Entry.make_index 1 ; Key.make_0 ~:3, Entry.make_index 3 ; Key.make_0 ~:123456, Entry.make_index 4])) ;
      ] ;

      "stream" >:::
      [
        "(1)" >:: (fun _ -> assert_equal
                      (refs (Stream (TestStream.make_raw_dict ["Key", DirectObject.Reference (Key.make_0 ~:2)] "raw content")))
                      (TestMapkey.add_all [Key.make_0 ~:2, Entry.make_name "Key"])) ;
      ] ;
    ] ;

    "undef_refs_to_null" >:::
    [
      "direct" >:::
      [
        "(1)" >:: (fun _ -> assert_equal
                      (let w = ref [] in let x = undef_refs_to_null (TestMapkey.add_all [Key.make_0 ~:1, ()]) w (Errors.make_ctxt_key Key.Trailer) (Direct (DirectObject.Array [DirectObject.Reference (Key.make_0 ~:1) ; DirectObject.Array [DirectObject.Reference (Key.make_0 ~:2)]])) in x, !w)
                      (Direct (DirectObject.Array [DirectObject.Reference (Key.make_0 ~:1) ; DirectObject.Array [DirectObject.Null]]),
                       [Key.make_0 ~:2, Errors.make_ctxt_entry Key.Trailer (Entry.append_index (Entry.make_index 1) 0)])) ;
        "(2)" >:: (fun _ -> assert_equal
                      (let w = ref [] in let x = undef_refs_to_null (TestMapkey.add_all [Key.make_0 ~:1, ()]) w (Errors.make_ctxt_key Key.Trailer) (Direct (DirectObject.Dictionary (TestDict.add_all ["Key", DirectObject.Reference (Key.make_0 ~:1) ; "Other", DirectObject.Reference (Key.make_0 ~:2)]))) in x, !w)
                      (Direct (DirectObject.Dictionary (TestDict.add_all ["Key", DirectObject.Reference (Key.make_0 ~:1)])),
                       [Key.make_0 ~:2, Errors.make_ctxt_entry Key.Trailer (Entry.make_name "Other")])) ;
      ] ;

      "stream" >:::
      [
        "(1)" >:: (fun _ -> assert_equal
                      (let w = ref [] in let x = undef_refs_to_null (TestMapkey.add_all [Key.make_0 ~:1, ()]) w (Errors.make_ctxt_key Key.Trailer) (Stream (TestStream.make_raw_dict ["Foo", DirectObject.Reference (Key.make_0 ~:1) ; "Bar", DirectObject.Reference (Key.make_0 ~:2)] "raw content")) in x, !w)
                      (Stream (TestStream.make_raw_dict ["Foo", DirectObject.Reference (Key.make_0 ~:1)] "raw content"),
                       [Key.make_0 ~:2, Errors.make_ctxt_entry Key.Trailer (Entry.make_name "Bar")])) ;
      ] ;
    ] ;

    "relink" >:::
    [
      "direct" >:::
      [
        "(1)" >:: (fun _ -> assert_equal
                      (relink (TestMapkey.add_all [Key.make_0 ~:4, Key.make_0 ~:1 ; Key.make_0 ~:3, Key.make_0 ~:2]) (Errors.make_ctxt_key Key.Trailer)
                         (Direct (DirectObject.Array [DirectObject.Reference (Key.make_0 ~:3) ; DirectObject.Array [DirectObject.Reference (Key.make_0 ~:4)]])))
                      (Direct (DirectObject.Array [DirectObject.Reference (Key.make_0 ~:2) ; DirectObject.Array [DirectObject.Reference (Key.make_0 ~:1)]]))) ;
        "(2)" >:: (fun _ -> assert_equal
                      (relink (TestMapkey.add_all [Key.make_0 ~:4, Key.make_0 ~:1 ; Key.make_0 ~:3, Key.make_0 ~:2]) (Errors.make_ctxt_key Key.Trailer)
                         (Direct (DirectObject.Dictionary (TestDict.add_all ["Key", DirectObject.Reference (Key.make_0 ~:3) ; "Other", DirectObject.Reference (Key.make_0 ~:4)]))))
                      (Direct (DirectObject.Dictionary (TestDict.add_all ["Other", DirectObject.Reference (Key.make_0 ~:1) ; "Key", DirectObject.Reference (Key.make_0 ~:2)])))) ;

        "(3)" >:: (fun _ -> assert_raises
                      (Errors.PDFError ("Reference to unknown object : 2", Errors.make_ctxt_name Key.Trailer "foo"))
                      (fun () -> relink MapKey.empty (Errors.make_ctxt_key Key.Trailer)
                          (Direct (DirectObject.Dictionary (TestDict.add_all ["foo", DirectObject.Reference (Key.make_0 ~:2)])))
                      )) ;
      ] ;

      "stream" >:::
      [
        "(1)" >:: (fun _ -> assert_equal
                      (relink (TestMapkey.add_all [Key.make_0 ~:2, Key.make_0 ~:1]) (Errors.make_ctxt_key Key.Trailer)
                         (Stream (TestStream.make_raw_dict ["Key", DirectObject.Reference (Key.make_0 ~:2)] "raw content")))
                      (Stream (TestStream.make_raw_dict ["Key", DirectObject.Reference (Key.make_0 ~:1)] "raw content"))) ;
      ] ;
    ] ;

    "get_direct" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (get_direct "msg" Errors.ctxt_none (Direct (DirectObject.String "blabla")))
                    (DirectObject.String "blabla")) ;
      "(2)" >:: (fun _ -> assert_equal
                    (get_direct "msg" Errors.ctxt_none (Direct DirectObject.Null))
                    DirectObject.Null) ;

      "(3)" >:: (fun _ -> assert_raises
                    (Errors.PDFError ("msg", Errors.ctxt_none))
                    (fun () -> get_direct "msg" Errors.ctxt_none (Stream (TestStream.make_raw "")))) ;
    ] ;

    "get_direct_of" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (get_direct_of "msg" Errors.ctxt_none ~transform:(DirectObject.get_name ()) (Direct (DirectObject.Name "foo")))
                    "foo") ;
      "(2)" >:: (fun _ -> assert_equal
                    (get_direct_of "msg" Errors.ctxt_none ~transform:(DirectObject.get_positive_int ()) (Direct (DirectObject.Int ~:123)))
                    ~:123) ;

      "(3)" >:: (fun _ -> assert_raises
                    (Errors.PDFError ("msg", Errors.ctxt_none))
                    (fun () -> get_direct_of "msg" Errors.ctxt_none ~transform:(DirectObject.get_name ()) (Direct (DirectObject.String "foo")))) ;
      "(4)" >:: (fun _ -> assert_raises
                    (Errors.PDFError ("msg", Errors.ctxt_none))
                    (fun () -> get_direct_of "msg" Errors.ctxt_none ~transform:(DirectObject.get_name ()) (Stream (TestStream.make_raw "content")))) ;
    ] ;

    "get_stream" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (get_stream "msg" Errors.ctxt_none (Stream (TestStream.make_raw "content")))
                    (TestStream.make_raw "content")) ;

      "(2)" >:: (fun _ -> assert_raises
                    (Errors.PDFError ("msg", Errors.ctxt_none))
                    (fun () -> get_stream "msg" Errors.ctxt_none (Direct (DirectObject.String "blabla")))) ;
    ] ;
  ]

