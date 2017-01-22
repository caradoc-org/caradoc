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
open Directobject.DirectObject
open Mapkey
open Errors
open Boundedint
open Params
open Entry
open Console


let init_params () =
  Params.clear_global ();
  Params.global.Params.sort_dicts <- true

let tests =
  "DirectObject" >:::
  [
    "to_string" >:::
    [
      "null" >:::
      [
        "(1)" >:: (fun _ -> assert_equal
                      (init_params (); to_string Null)
                      "null") ;
      ] ;

      "bool" >:::
      [
        "(1)" >:: (fun _ -> assert_equal
                      (init_params (); to_string (Bool true))
                      "true") ;
        "(2)" >:: (fun _ -> assert_equal
                      (init_params (); to_string (Bool false))
                      "false") ;
      ] ;

      "int" >:::
      [
        "(1)" >:: (fun _ -> assert_equal
                      (init_params (); to_string (Int ~:531))
                      "531") ;
        "(2)" >:: (fun _ -> assert_equal
                      (init_params (); to_string (Int ~:(-10)))
                      "-10") ;
      ] ;

      "real" >:::
      [
        "(1)" >:: (fun _ -> assert_equal
                      (init_params (); to_string (Real "1.0"))
                      "1.0") ;
      ] ;

      "string" >:::
      [
        "(1)" >:: (fun _ -> assert_equal
                      (init_params (); to_string (String "hello\nworld"))
                      "(hello\\nworld)") ;
        "(2)" >:: (fun _ -> assert_equal
                      (init_params (); to_string (String "hello (world))"))
                      "(hello \\(world\\)\\))") ;
        "(3)" >:: (fun _ -> assert_equal
                      (init_params (); to_string (String "hello\n\r\t\b\x0C\\world"))
                      "(hello\\n\\r\\t\\b\\f\\\\world)") ;
      ] ;

      "name" >:::
      [
        "(1)" >:: (fun _ -> assert_equal
                      (init_params (); to_string (Name "hello"))
                      "/hello") ;
        "(2)" >:: (fun _ -> assert_equal
                      (init_params (); to_string (Name "hello world"))
                      "/hello#20world") ;
        "(3)" >:: (fun _ -> assert_equal
                      (init_params (); to_string (Name "hello\n#world"))
                      "/hello#0A#23world") ;
      ] ;

      "array" >:::
      [
        "(1)" >:: (fun _ -> assert_equal
                      (init_params (); to_string (Array [Bool true ; String "blabla" ; Name "key"]))
                      "[true (blabla) /key]") ;
        "(2)" >:: (fun _ -> assert_equal
                      (init_params (); to_string (Array []))
                      "[]") ;
      ] ;

      "dictionary" >:::
      [
        "(1)" >:: (fun _ -> assert_equal
                      (init_params (); dict_to_string (TestDict.add_all ["Key", String "value" ; "Foo", Name "bar"]))
                      "<<\n    /Foo /bar\n    /Key (value)\n>>") ;
        "(2)" >:: (fun _ -> assert_equal
                      (init_params (); dict_to_string (TestDict.add_all ["Key", String "value" ; "Foo", Name "bar" ; "G", Dictionary (TestDict.add_all ["Nested", Int ~:456])]))
                      "<<\n    /Foo /bar\n    /G <<\n        /Nested 456\n    >>\n    /Key (value)\n>>") ;
      ] ;

      "reference" >:::
      [
        "(1)" >:: (fun _ -> assert_equal
                      (init_params (); to_string (Reference (Key.make_gen ~:3 ~:2)))
                      "3 2 R") ;
      ] ;
    ] ;

    "to_string_hl" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (to_string_hl Null Entry.no_selector)
                    "null") ;
      "(2)" >:: (fun _ -> assert_equal
                    (to_string_hl Null (Entry.make_selector [Entry.empty]))
                    (Console.highlight ^ "null" ^ Console.reset)) ;
      "(3)" >:: (fun _ -> assert_equal
                    (to_string_hl (Array [Null ; Bool true ; Int ~:123 ; String "foo"]) (Entry.make_selector [Entry.make_index 1 ; Entry.make_index 2]))
                    ("[null " ^ Console.highlight ^ "true" ^ Console.reset ^ " " ^ Console.highlight ^ "123" ^ Console.reset ^ " (foo)]")) ;
      "(4)" >:: (fun _ -> assert_equal
                    (to_string_hl (Array [Null ; Array [Bool true ; Name "bar"] ; Int ~:123 ; String "foo"]) (Entry.make_selector [Entry.make_index 1 ; Entry.append_index (Entry.make_index 1) 0]))
                    ("[null " ^ Console.highlight ^ "[true /bar]" ^ Console.reset ^ " 123 (foo)]")) ;
      "(5)" >:: (fun _ -> assert_equal
                    (init_params (); dict_to_string_hl (TestDict.add_all ["Key", String "value" ; "Foo", Name "bar"]) (Entry.make_selector [Entry.make_name "Key" ; Entry.make_name_key "Foo"]))
                    ("<<\n    " ^ Console.highlight ^ "/Foo" ^ Console.reset ^ " /bar\n    /Key " ^ Console.highlight ^ "(value)" ^ Console.reset ^ "\n>>")) ;
    ] ;

    "to_pdf" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (to_pdf Null)
                    "null") ;
      "(2)" >:: (fun _ -> assert_equal
                    (to_pdf (Bool true))
                    "true") ;
      "(3)" >:: (fun _ -> assert_equal
                    (to_pdf (Bool false))
                    "false") ;
      "(4)" >:: (fun _ -> assert_equal
                    (to_pdf (Int ~:531))
                    "531") ;
      "(5)" >:: (fun _ -> assert_equal
                    (to_pdf (Int ~:(-10)))
                    "-10") ;
      "(6)" >:: (fun _ -> assert_equal
                    (to_pdf (Real "1.0"))
                    "1.0") ;
      "(7)" >:: (fun _ -> assert_equal
                    (to_pdf (String "hello\nworld"))
                    "(hello\\nworld)") ;
      "(8)" >:: (fun _ -> assert_equal
                    (to_pdf (String "hello (world))"))
                    "(hello \\(world\\)\\))") ;
      "(9)" >:: (fun _ -> assert_equal
                    (to_pdf (String "hello\n\r\t\b\x0C\\world"))
                    "(hello\\n\\r\\t\\b\\f\\\\world)") ;
      "(10)" >:: (fun _ -> assert_equal
                     (to_pdf (Name "hello"))
                     "/hello") ;
      "(11)" >:: (fun _ -> assert_equal
                     (to_pdf (Name "hello world"))
                     "/hello#20world") ;
      "(12)" >:: (fun _ -> assert_equal
                     (to_pdf (Name "hello\n#world"))
                     "/hello#0A#23world") ;
      "(13)" >:: (fun _ -> assert_equal
                     (to_pdf (Array [String "test" ; Bool true ; Int ~:123 ; String "blabla" ; Name "key" ; Null ; Name "name" ; Name "other" ; String "value" ; Int ~:456]))
                     "[(test)true 123(blabla)/key null/name/other(value)456]") ;
      "(14)" >:: (fun _ -> assert_equal
                     (to_pdf (Array []))
                     "[]") ;
      "(15)" >:: (fun _ -> assert_equal
                     (dict_to_pdf (TestDict.add_all ["Key", String "value" ; "Foo", Name "bar"]))
                     "<</Foo/bar/Key(value)>>") ;
      "(16)" >:: (fun _ -> assert_equal
                     (dict_to_pdf (TestDict.add_all ["Key", String "value" ; "Foo", Bool false ; "G", Dictionary (TestDict.add_all ["Nested", Int ~:456])]))
                     "<</Foo false/G<</Nested 456>>/Key(value)>>") ;
      "(17)" >:: (fun _ -> assert_equal
                     (to_pdf (Reference (Key.make_gen ~:3 ~:2)))
                     "3 2 R") ;
    ] ;

    "find_ref" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (find_ref (Key.make_0 ~:1) (String "foo"))
                    []) ;
      "(2)" >:: (fun _ -> assert_equal
                    (find_ref (Key.make_0 ~:1) (Reference (Key.make_0 ~:1)))
                    [Entry.empty]) ;
      "(3)" >:: (fun _ -> assert_equal
                    (find_ref (Key.make_0 ~:1) (Array [Reference (Key.make_0 ~:1) ; Bool false ; Reference (Key.make_0 ~:1)]))
                    [Entry.make_index 0 ; Entry.make_index 2]) ;
      "(4)" >:: (fun _ -> assert_equal
                    (find_ref_dict (Key.make_0 ~:1) (TestDict.add_all ["Foo", Reference (Key.make_0 ~:1) ; "Bar", Bool false ; "Hello", Reference (Key.make_0 ~:1) ; "World", Reference (Key.make_0 ~:2) ; "Alpha", Reference (Key.make_0 ~:1)]))
                    [Entry.make_name "Alpha" ; Entry.make_name "Foo" ; Entry.make_name "Hello"]) ;
    ] ;

    "find_name" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (find_name "Foo" (String "Foo"))
                    []) ;
      "(2)" >:: (fun _ -> assert_equal
                    (find_name "Foo" (Reference (Key.make_0 ~:1)))
                    []) ;
      "(3)" >:: (fun _ -> assert_equal
                    (find_name "Foo" (Name "Foo"))
                    [Entry.empty]) ;
      "(4)" >:: (fun _ -> assert_equal
                    (find_name "Foo" (Array [Name "Foo" ; Name "Bar" ; Bool false ; Name "Foo"]))
                    [Entry.make_index 0 ; Entry.make_index 3]) ;
      "(5)" >:: (fun _ -> assert_equal
                    (find_name_dict "Foo" (TestDict.add_all ["Foo", Reference (Key.make_0 ~:1) ; "Bar", Bool false ; "Hello", Name "Foo"]))
                    [Entry.make_name_key "Foo" ; Entry.make_name "Hello"]) ;
    ] ;

    "refs" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (refs (Array [Reference (Key.make_gen ~:2 ~:1)]))
                    (TestMapkey.add_all [Key.make_gen ~:2 ~:1, Entry.make_index 0])) ;
      "(2)" >:: (fun _ -> assert_equal
                    (refs (Array [Reference (Key.make_gen ~:2 ~:1) ; Reference (Key.make_0 ~:5) ; Reference (Key.make_gen ~:2 ~:1) ; Reference (Key.make_0 ~:3) ; Reference (Key.make_0 ~:123456)]))
                    (TestMapkey.add_all [Key.make_gen ~:2 ~:1, Entry.make_index 0 ; Key.make_0 ~:5, Entry.make_index 1 ; Key.make_0 ~:3, Entry.make_index 3 ; Key.make_0 ~:123456, Entry.make_index 4])) ;
      "(3)" >:: (fun _ -> assert_equal
                    (refs_dict (TestDict.add_all ["Foo", Reference (Key.make_gen ~:2 ~:1) ; "Bar", Reference (Key.make_0 ~:5)]))
                    (TestMapkey.add_all [Key.make_gen ~:2 ~:1, Entry.make_name "Foo" ; Key.make_0 ~:5, Entry.make_name "Bar"])) ;
    ] ;

    "undef_refs_to_null" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (let w = ref [] in let x = undef_refs_to_null MapKey.empty w (Errors.make_ctxt_key Key.Trailer) Null in x, !w)
                    (Null, [])) ;
      "(2)" >:: (fun _ -> assert_equal
                    (let w = ref [] in let x = undef_refs_to_null MapKey.empty w (Errors.make_ctxt_key Key.Trailer) (Bool false) in x, !w)
                    (Bool false, [])) ;
      "(3)" >:: (fun _ -> assert_equal
                    (let w = ref [] in let x = undef_refs_to_null MapKey.empty w (Errors.make_ctxt_key Key.Trailer) (Int ~:123) in x, !w)
                    (Int ~:123, [])) ;
      "(4)" >:: (fun _ -> assert_equal
                    (let w = ref [] in let x = undef_refs_to_null MapKey.empty w (Errors.make_ctxt_key Key.Trailer) (Real "1.02") in x, !w)
                    (Real "1.02", [])) ;
      "(5)" >:: (fun _ -> assert_equal
                    (let w = ref [] in let x = undef_refs_to_null MapKey.empty w (Errors.make_ctxt_key Key.Trailer) (String "test") in x, !w)
                    (String "test", [])) ;
      "(6)" >:: (fun _ -> assert_equal
                    (let w = ref [] in let x = undef_refs_to_null MapKey.empty w (Errors.make_ctxt_key Key.Trailer) (Name "key") in x, !w)
                    (Name "key", [])) ;
      "(7)" >:: (fun _ -> assert_equal
                    (let w = ref [] in let x = undef_refs_to_null (TestMapkey.add_all [Key.make_0 ~:1, ()]) w (Errors.make_ctxt_key Key.Trailer) (Array [Reference (Key.make_0 ~:1) ; Reference (Key.make_0 ~:2) ; Array [Reference (Key.make_0 ~:3)]]) in x, !w)
                    (Array [Reference (Key.make_0 ~:1) ; Null ; Array [Null]],
                     [Key.make_0 ~:3, Errors.make_ctxt_entry Key.Trailer (Entry.append_index (Entry.make_index 2) 0) ; Key.make_0 ~:2, Errors.make_ctxt_entry Key.Trailer (Entry.make_index 1)])) ;
      "(8)" >:: (fun _ -> assert_equal
                    (let w = ref [] in let x = undef_refs_to_null_dict (TestMapkey.add_all [Key.make_0 ~:1, ()]) w (Errors.make_ctxt_key Key.Trailer) (TestDict.add_all ["Key", Reference (Key.make_0 ~:1) ; "Other", Reference (Key.make_0 ~:2)]) in x, !w)
                    (TestDict.add_all ["Key", Reference (Key.make_0 ~:1)],
                     [Key.make_0 ~:2, Errors.make_ctxt_entry Key.Trailer (Entry.make_name "Other")])) ;
      "(9)" >:: (fun _ -> assert_equal
                    (let w = ref [] in let x = undef_refs_to_null (TestMapkey.add_all [Key.make_0 ~:1, ()]) w (Errors.make_ctxt_key Key.Trailer) (Reference (Key.make_0 ~:1)) in x, !w)
                    (Reference (Key.make_0 ~:1), [])) ;
      "(10)" >:: (fun _ -> assert_equal
                     (let w = ref [] in let x = undef_refs_to_null (TestMapkey.add_all [Key.make_0 ~:1, ()]) w (Errors.make_ctxt_key Key.Trailer) (Reference (Key.make_0 ~:2)) in x, !w)
                     (Null,
                      [Key.make_0 ~:2, Errors.make_ctxt_key Key.Trailer])) ;
    ] ;

    "relink" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (relink MapKey.empty (Errors.make_ctxt_key Key.Trailer) Null)
                    Null) ;
      "(2)" >:: (fun _ -> assert_equal
                    (relink MapKey.empty (Errors.make_ctxt_key Key.Trailer) (Bool false))
                    (Bool false)) ;
      "(3)" >:: (fun _ -> assert_equal
                    (relink MapKey.empty (Errors.make_ctxt_key Key.Trailer) (Int ~:123))
                    (Int ~:123)) ;
      "(4)" >:: (fun _ -> assert_equal
                    (relink MapKey.empty (Errors.make_ctxt_key Key.Trailer) (Real "1.02"))
                    (Real "1.02")) ;
      "(5)" >:: (fun _ -> assert_equal
                    (relink MapKey.empty (Errors.make_ctxt_key Key.Trailer) (String "test"))
                    (String "test")) ;
      "(6)" >:: (fun _ -> assert_equal
                    (relink MapKey.empty (Errors.make_ctxt_key Key.Trailer) (Name "key"))
                    (Name "key")) ;
      "(7)" >:: (fun _ -> assert_equal
                    (relink (TestMapkey.add_all [Key.make_0 ~:4, Key.make_0 ~:1 ; Key.make_0 ~:3, Key.make_0 ~:2]) (Errors.make_ctxt_key Key.Trailer)
                       (Array [Reference (Key.make_0 ~:3) ; Array [Reference (Key.make_0 ~:4)]]))
                    (Array [Reference (Key.make_0 ~:2) ; Array [Reference (Key.make_0 ~:1)]])) ;
      "(8)" >:: (fun _ -> assert_equal
                    (relink_dict (TestMapkey.add_all [Key.make_0 ~:4, Key.make_0 ~:1 ; Key.make_0 ~:3, Key.make_0 ~:2]) (Errors.make_ctxt_key Key.Trailer)
                       (TestDict.add_all ["Key", Reference (Key.make_0 ~:3) ; "Other", Reference (Key.make_0 ~:4)]))
                    (TestDict.add_all ["Other", Reference (Key.make_0 ~:1) ; "Key", Reference (Key.make_0 ~:2)])) ;
      "(9)" >:: (fun _ -> assert_equal
                    (relink (TestMapkey.add_all [Key.make_0 ~:2, Key.make_0 ~:1]) (Errors.make_ctxt_key Key.Trailer)
                       (Reference (Key.make_0 ~:2)))
                    (Reference (Key.make_0 ~:1))) ;

      "(10)" >:: (fun _ -> assert_raises
                     (Errors.PDFError ("Reference to unknown object : 2", Errors.make_ctxt_index Key.Trailer 0))
                     (fun () -> relink MapKey.empty (Errors.make_ctxt_key Key.Trailer) (Array [Reference (Key.make_0 ~:2)]))) ;
    ] ;

    "get_positive_int" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (get_positive_int () "msg" Errors.ctxt_none (Int ~:142857))
                    ~:142857) ;
      "(2)" >:: (fun _ -> assert_equal
                    (get_positive_int ~default:(~:123456) () "msg" Errors.ctxt_none (Int ~:142857))
                    ~:142857) ;
      "(3)" >:: (fun _ -> assert_equal
                    (get_positive_int ~default:(~:123456) () "msg" Errors.ctxt_none Null)
                    ~:123456) ;

      "(4)" >:: (fun _ -> assert_raises
                    (Errors.PDFError ("msg", Errors.ctxt_none))
                    (fun () -> get_positive_int () "msg" Errors.ctxt_none (Int ~:0))) ;
      "(5)" >:: (fun _ -> assert_raises
                    (Errors.PDFError ("msg", Errors.ctxt_none))
                    (fun () -> get_positive_int () "msg" Errors.ctxt_none (Int ~:(-1)))) ;
      "(6)" >:: (fun _ -> assert_raises
                    (Errors.PDFError ("msg", Errors.ctxt_none))
                    (fun () -> get_positive_int () "msg" Errors.ctxt_none (String "blabla"))) ;
      "(7)" >:: (fun _ -> assert_raises
                    (Errors.PDFError ("msg", Errors.ctxt_none))
                    (fun () -> get_positive_int ~default:(~:123456) () "msg" Errors.ctxt_none (Name "blabla"))) ;
    ] ;

    "get_nonnegative_int" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (get_nonnegative_int () "msg" Errors.ctxt_none (Int ~:142857))
                    ~:142857) ;
      "(2)" >:: (fun _ -> assert_equal
                    (get_nonnegative_int ~default:(~:123456) () "msg" Errors.ctxt_none (Int ~:142857))
                    ~:142857) ;
      "(3)" >:: (fun _ -> assert_equal
                    (get_nonnegative_int ~default:(~:123456) () "msg" Errors.ctxt_none Null)
                    ~:123456) ;
      "(4)" >:: (fun _ -> assert_equal
                    (get_nonnegative_int () "msg" Errors.ctxt_none (Int ~:0))
                    ~:0) ;

      "(5)" >:: (fun _ -> assert_raises
                    (Errors.PDFError ("msg", Errors.ctxt_none))
                    (fun () -> get_nonnegative_int () "msg" Errors.ctxt_none (Int ~:(-1)))) ;
      "(6)" >:: (fun _ -> assert_raises
                    (Errors.PDFError ("msg", Errors.ctxt_none))
                    (fun () -> get_nonnegative_int () "msg" Errors.ctxt_none (String "blabla"))) ;
      "(7)" >:: (fun _ -> assert_raises
                    (Errors.PDFError ("msg", Errors.ctxt_none))
                    (fun () -> get_nonnegative_int ~default:(~:123456) () "msg" Errors.ctxt_none (Name "blabla"))) ;
    ] ;

    "get_string" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (get_string () "msg" Errors.ctxt_none (String "blabla"))
                    "blabla") ;
      "(2)" >:: (fun _ -> assert_equal
                    (get_string ~default:("test") () "msg" Errors.ctxt_none (String "blabla"))
                    "blabla") ;
      "(3)" >:: (fun _ -> assert_equal
                    (get_string ~default:("test") () "msg" Errors.ctxt_none Null)
                    "test") ;

      "(4)" >:: (fun _ -> assert_raises
                    (Errors.PDFError ("msg", Errors.ctxt_none))
                    (fun () -> get_string () "msg" Errors.ctxt_none (Name "blabla"))) ;
      "(5)" >:: (fun _ -> assert_raises
                    (Errors.PDFError ("msg", Errors.ctxt_none))
                    (fun () -> get_string ~default:("test") () "msg" Errors.ctxt_none (Int ~:123))) ;
    ] ;

    "get_name" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (get_name () "msg" Errors.ctxt_none (Name "blabla"))
                    "blabla") ;
      "(2)" >:: (fun _ -> assert_equal
                    (get_name ~default:("test") () "msg" Errors.ctxt_none (Name "blabla"))
                    "blabla") ;
      "(3)" >:: (fun _ -> assert_equal
                    (get_name ~default:("test") () "msg" Errors.ctxt_none Null)
                    "test") ;

      "(4)" >:: (fun _ -> assert_raises
                    (Errors.PDFError ("msg", Errors.ctxt_none))
                    (fun () -> get_name () "msg" Errors.ctxt_none (String "blabla"))) ;
      "(5)" >:: (fun _ -> assert_raises
                    (Errors.PDFError ("msg", Errors.ctxt_none))
                    (fun () -> get_name ~default:("test") () "msg" Errors.ctxt_none (Int ~:123))) ;
    ] ;

    "get_dict" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (get_dict () "msg" Errors.ctxt_none (Dictionary (TestDict.add_all ["Length", Int ~:123])))
                    (TestDict.add_all ["Length", Int ~:123])) ;
      "(2)" >:: (fun _ -> assert_equal
                    (get_dict ~default:(TestDict.add_all ["Param", Name "value"]) () "msg" Errors.ctxt_none (Dictionary (TestDict.add_all ["Length", Int ~:123])))
                    (TestDict.add_all ["Length", Int ~:123])) ;
      "(3)" >:: (fun _ -> assert_equal
                    (get_dict ~default:(TestDict.add_all ["Param", Name "value"]) () "msg" Errors.ctxt_none Null)
                    (TestDict.add_all ["Param", Name "value"])) ;

      "(4)" >:: (fun _ -> assert_raises
                    (Errors.PDFError ("msg", Errors.ctxt_none))
                    (fun () -> get_dict () "msg" Errors.ctxt_none (String "blabla"))) ;
      "(5)" >:: (fun _ -> assert_raises
                    (Errors.PDFError ("msg", Errors.ctxt_none))
                    (fun () -> get_dict ~default:(TestDict.add_all ["Param", Name "value"]) () "msg" Errors.ctxt_none (Int ~:123))) ;
    ] ;

    "get_array" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (get_array () "msg" Errors.ctxt_none (Array [Int ~:14285 ; String "blabla"]))
                    [|Int ~:14285 ; String "blabla"|]) ;

      "(2)" >:: (fun _ -> assert_equal
                    (get_array ~length:2 () "msg" Errors.ctxt_none (Array [Int ~:14285 ; String "blabla"]))
                    [|Int ~:14285 ; String "blabla"|]) ;
      "(3)" >:: (fun _ -> assert_equal
                    (get_array ~accept_one:true () "msg" Errors.ctxt_none (String "blabla"))
                    [|String "blabla"|]) ;
      "(4)" >:: (fun _ -> assert_equal
                    (get_array ~accept_one:true () "msg" Errors.ctxt_none Null)
                    [|Null|]) ;
      "(5)" >:: (fun _ -> assert_equal
                    (get_array ~default:[Name "key" ; Int ~:(-987)] () "msg" Errors.ctxt_none Null)
                    [|Name "key" ; Int ~:(-987)|]) ;
      "(6)" >:: (fun _ -> assert_equal
                    (get_array ~default:[Name "key" ; Int ~:(-987)] () "msg" Errors.ctxt_none (Array [Int ~:14285 ; String "blabla"]))
                    [|Int ~:14285 ; String "blabla"|]) ;

      "(7)" >:: (fun _ -> assert_equal
                    (get_array ~length:1 ~accept_one:true () "msg" Errors.ctxt_none (String "blabla"))
                    [|String "blabla"|]) ;
      "(8)" >:: (fun _ -> assert_equal
                    (get_array ~length:2 ~accept_one:true () "msg" Errors.ctxt_none (Array [Int ~:14285 ; String "blabla"]))
                    [|Int ~:14285 ; String "blabla"|]) ;
      "(9)" >:: (fun _ -> assert_equal
                    (get_array ~accept_one:true ~default:[Name "key" ; Int ~:(-987)] () "msg" Errors.ctxt_none (String "blabla"))
                    [|String "blabla"|]) ;
      "(10)" >:: (fun _ -> assert_equal
                     (get_array ~accept_one:true ~default:[Name "key" ; Int ~:(-987)] () "msg" Errors.ctxt_none Null)
                     [|Name "key" ; Int ~:(-987)|]) ;
      "(11)" >:: (fun _ -> assert_equal
                     (get_array ~length:2 ~default:[Name "key" ; Int ~:(-987)] () "msg" Errors.ctxt_none (Array [Int ~:14285 ; String "blabla"]))
                     [|Int ~:14285 ; String "blabla"|]) ;
      "(12)" >:: (fun _ -> assert_equal
                     (get_array ~length:2 ~default:[Name "key"] () "msg" Errors.ctxt_none (Array [Int ~:14285 ; String "blabla"]))
                     [|Int ~:14285 ; String "blabla"|]) ;

      "(13)" >:: (fun _ -> assert_raises
                     (Errors.PDFError ("msg", Errors.ctxt_none))
                     (fun () -> get_array () "msg" Errors.ctxt_none (String "blabla"))) ;
      "(14)" >:: (fun _ -> assert_raises
                     (Errors.PDFError ("msg", Errors.ctxt_none))
                     (fun () -> (get_array ~length:3 () "msg" Errors.ctxt_none (Array [Int ~:14285 ; String "blabla"])))) ;
      "(15)" >:: (fun _ -> assert_raises
                     (Errors.PDFError ("msg", Errors.ctxt_none))
                     (fun () -> get_array ~default:[Name "key" ; Int ~:(-987)] () "msg" Errors.ctxt_none (String "blabla"))) ;
      "(16)" >:: (fun _ -> assert_raises
                     (Errors.PDFError ("msg", Errors.ctxt_none))
                     (fun () -> get_array ~length:2 ~accept_one:true () "msg" Errors.ctxt_none (String "blabla"))) ;
      "(17)" >:: (fun _ -> assert_raises
                     (Errors.PDFError ("msg", Errors.ctxt_none))
                     (fun () -> get_array ~length:2 ~default:[Name "key"] () "msg" Errors.ctxt_none Null)) ;
    ] ;

    "get_array_of" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (get_array_of () "msg" Errors.ctxt_none ~transform:(get_positive_int ()) (Array [Int ~:14285 ; Int ~:42857]))
                    [|~:14285 ; ~:42857|]) ;
      "(2)" >:: (fun _ -> assert_equal
                    (get_array_of ~length:2 () "msg" Errors.ctxt_none ~transform:(get_positive_int ()) (Array [Int ~:14285 ; Int ~:42857]))
                    [|~:14285 ; ~:42857|]) ;
      "(3)" >:: (fun _ -> assert_equal
                    (get_array_of ~accept_one:true () "msg" Errors.ctxt_none ~transform:(get_positive_int ()) (Int ~:14285))
                    [|~:14285|]) ;

      "(4)" >:: (fun _ -> assert_raises
                    (Errors.PDFError ("msg", Errors.ctxt_none))
                    (fun () -> get_array_of () "msg" Errors.ctxt_none ~transform:(get_positive_int ()) (Array [Int ~:14285 ; String "blabla"]))) ;
      "(5)" >:: (fun _ -> assert_raises
                    (Errors.PDFError ("msg", Errors.ctxt_none))
                    (fun () -> get_array_of ~length:3 () "msg" Errors.ctxt_none ~transform:(get_positive_int ()) (Array [Int ~:14285 ; Int ~:42857]))) ;
      "(6)" >:: (fun _ -> assert_raises
                    (Errors.PDFError ("msg", Errors.ctxt_none))
                    (fun () -> get_array_of ~accept_one:true () "msg" Errors.ctxt_none ~transform:(get_positive_int ()) (String "blabla"))) ;
    ] ;
  ]

