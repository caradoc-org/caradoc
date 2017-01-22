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
open Checkobjecttype
open Directobject
open Indirectobject.IndirectObject
open Boundedint
open Key
open Mapkey
open Type
open Errors


let make_context_types id l =
  let ctxt = Type.create_context () in
  ctxt.Type.pool <- TestType.make_pool id;
  List.iter (fun (key, kind) ->
      ctxt.Type.types <- MapKey.add key kind ctxt.Type.types
    ) l;
  ctxt


let tests =
  "CheckType" >:::
  [
    "check_object" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (CheckObjectType.check_object_direct (Type.create_context ()) (DirectObject.Null) (Util.make_type Type.Null) (Errors.make_ctxt_key Key.Trailer))
                    (Util.make_type Type.Null)) ;
      "(2)" >:: (fun _ -> assert_equal
                    (CheckObjectType.check_object_direct (Type.create_context ()) (DirectObject.Bool true) (Util.make_type Type.Bool) (Errors.make_ctxt_key Key.Trailer))
                    (Util.make_type Type.Bool)) ;
      "(3)" >:: (fun _ -> assert_equal
                    (CheckObjectType.check_object_direct (Type.create_context ()) (DirectObject.Int ~:123) (Util.make_type Type.Int) (Errors.make_ctxt_key Key.Trailer))
                    (Util.make_type Type.Int)) ;
      "(4)" >:: (fun _ -> assert_equal
                    (CheckObjectType.check_object_direct (Type.create_context ()) (DirectObject.Int ~:123) (Util.make_type ~allow_ind:false Type.Int) (Errors.make_ctxt_key Key.Trailer))
                    (Util.make_type ~allow_ind:false Type.Int)) ;
      "(5)" >:: (fun _ -> assert_equal
                    (CheckObjectType.check_object_direct (Type.create_context ()) (DirectObject.Real "123.456") (Util.make_type Type.Real) (Errors.make_ctxt_key Key.Trailer))
                    (Util.make_type Type.Real)) ;
      "(6)" >:: (fun _ -> assert_equal
                    (CheckObjectType.check_object_direct (Type.create_context ()) (DirectObject.String "foo") (Util.make_type Type.String) (Errors.make_ctxt_key Key.Trailer))
                    (Util.make_type Type.String)) ;
      "(7)" >:: (fun _ -> assert_equal
                    (CheckObjectType.check_object_direct (Type.create_context ()) (DirectObject.Name "key") (Util.make_type Type.Name) (Errors.make_ctxt_key Key.Trailer))
                    (Util.make_type Type.Name)) ;
      "(8)" >:: (fun _ -> assert_equal
                    (CheckObjectType.check_object_direct (Type.create_context ()) (DirectObject.String "text") (Util.make_type Type.Text) (Errors.make_ctxt_key Key.Trailer))
                    (Util.make_type Type.Text)) ;
      "(9)" >:: (fun _ -> assert_equal
                    (CheckObjectType.check_object_direct (Type.create_context ()) (DirectObject.String "D:20001231235959+01'00") (Util.make_type Type.Date) (Errors.make_ctxt_key Key.Trailer))
                    (Util.make_type Type.Date)) ;

      "(10)" >:: (fun _ -> assert_equal
                     (CheckObjectType.check_object_direct (Type.create_context ()) (DirectObject.Bool true) (Util.make_type (Type.BoolExact true)) (Errors.make_ctxt_key Key.Trailer))
                     (Util.make_type (Type.BoolExact true))) ;
      "(11)" >:: (fun _ -> assert_equal
                     (CheckObjectType.check_object_direct (Type.create_context ()) (DirectObject.Bool false) (Util.make_type (Type.BoolExact false)) (Errors.make_ctxt_key Key.Trailer))
                     (Util.make_type (Type.BoolExact false))) ;
      "(12)" >:: (fun _ -> assert_equal
                     (CheckObjectType.check_object_direct (Type.create_context ()) (DirectObject.Int ~:1) (Util.make_type (Type.IntRange (Some ~:1, Some ~:10))) (Errors.make_ctxt_key Key.Trailer))
                     (Util.make_type (Type.IntRange (Some ~:1, Some ~:10)))) ;
      "(13)" >:: (fun _ -> assert_equal
                     (CheckObjectType.check_object_direct (Type.create_context ()) (DirectObject.Int ~:10) (Util.make_type (Type.IntRange (Some ~:1, Some ~:10))) (Errors.make_ctxt_key Key.Trailer))
                     (Util.make_type (Type.IntRange (Some ~:1, Some ~:10)))) ;
      "(14)" >:: (fun _ -> assert_equal
                     (CheckObjectType.check_object_direct (Type.create_context ()) (DirectObject.Int ~:3) (Util.make_type (Type.IntRange (Some ~:1, Some ~:10))) (Errors.make_ctxt_key Key.Trailer))
                     (Util.make_type (Type.IntRange (Some ~:1, Some ~:10)))) ;
      "(15)" >:: (fun _ -> assert_equal
                     (CheckObjectType.check_object_direct (Type.create_context ()) (DirectObject.Int ~:123456789) (Util.make_type (Type.IntRange (Some ~:0, None))) (Errors.make_ctxt_key Key.Trailer))
                     (Util.make_type (Type.IntRange (Some ~:0, None)))) ;
      "(16)" >:: (fun _ -> assert_equal
                     (CheckObjectType.check_object_direct (Type.create_context ()) (DirectObject.Int ~:(-123456789)) (Util.make_type (Type.IntRange (None, Some ~:0))) (Errors.make_ctxt_key Key.Trailer))
                     (Util.make_type (Type.IntRange (None, Some ~:0)))) ;
      "(17)" >:: (fun _ -> assert_equal
                     (CheckObjectType.check_object_direct (Type.create_context ()) (DirectObject.Int ~:111) (Util.make_type (Type.IntExact ~:111)) (Errors.make_ctxt_key Key.Trailer))
                     (Util.make_type (Type.IntExact ~:111))) ;
      "(18)" >:: (fun _ -> assert_equal
                     (CheckObjectType.check_object_direct (Type.create_context ()) (DirectObject.Int ~:111) (Util.make_type (Type.IntIn [| ~:1 ; ~:11 ; ~:111 ; ~:1111 |])) (Errors.make_ctxt_key Key.Trailer))
                     (Util.make_type (Type.IntIn [| ~:1 ; ~:11 ; ~:111 ; ~:1111 |]))) ;
      "(19)" >:: (fun _ -> assert_equal
                     (CheckObjectType.check_object_direct (Type.create_context ()) (DirectObject.Name "bar") (Util.make_type (Type.NameExact "bar")) (Errors.make_ctxt_key Key.Trailer))
                     (Util.make_type (Type.NameExact "bar"))) ;
      "(20)" >:: (fun _ -> assert_equal
                     (CheckObjectType.check_object_direct (Type.create_context ()) (DirectObject.Name "bar") (Util.make_type (Type.NameIn (TestType.add_all ["foo" ; "bar"]))) (Errors.make_ctxt_key Key.Trailer))
                     (Util.make_type (Type.NameIn (TestType.add_all ["foo" ; "bar"])))) ;

      "(21)" >:: (fun _ -> assert_raises
                     (Errors.TypeError ("Invalid type : expected null", Errors.make_ctxt_name Key.Trailer "dummy"))
                     (fun () -> CheckObjectType.check_object_direct (Type.create_context ()) (DirectObject.Int ~:123) (Util.make_type Type.Null) (Errors.make_ctxt_name Key.Trailer "dummy"))) ;
      "(22)" >:: (fun _ -> assert_raises
                     (Errors.TypeError ("Invalid type : expected bool", Errors.make_ctxt_name Key.Trailer "dummy"))
                     (fun () -> CheckObjectType.check_object_direct (Type.create_context ()) (DirectObject.String "foo") (Util.make_type Type.Bool) (Errors.make_ctxt_name Key.Trailer "dummy"))) ;
      "(23)" >:: (fun _ -> assert_raises
                     (Errors.TypeError ("Invalid type : expected int", Errors.make_ctxt_name Key.Trailer "dummy"))
                     (fun () -> CheckObjectType.check_object_direct (Type.create_context ()) (DirectObject.Name "key") (Util.make_type Type.Int) (Errors.make_ctxt_name Key.Trailer "dummy"))) ;
      "(24)" >:: (fun _ -> assert_raises
                     (Errors.TypeError ("Invalid type : expected real", Errors.make_ctxt_name Key.Trailer "dummy"))
                     (fun () -> CheckObjectType.check_object_direct (Type.create_context ()) (DirectObject.Bool true) (Util.make_type Type.Real) (Errors.make_ctxt_name Key.Trailer "dummy"))) ;
      "(25)" >:: (fun _ -> assert_raises
                     (Errors.TypeError ("Invalid type : expected string", Errors.make_ctxt_name Key.Trailer "dummy"))
                     (fun () -> CheckObjectType.check_object_direct (Type.create_context ()) (DirectObject.Array [DirectObject.String "foo"]) (Util.make_type Type.String) (Errors.make_ctxt_name Key.Trailer "dummy"))) ;
      "(26)" >:: (fun _ -> assert_raises
                     (Errors.TypeError ("Invalid type : expected name", Errors.make_ctxt_name Key.Trailer "dummy"))
                     (fun () -> CheckObjectType.check_object_direct (Type.create_context ()) (DirectObject.Null) (Util.make_type Type.Name) (Errors.make_ctxt_name Key.Trailer "dummy"))) ;
          (*
          (* TODO : check Text *)
      "(27)" >:: (fun _ -> assert_raises
          (Errors.TypeError ("Invalid type : expected text", Errors.make_ctxt_name Key.Trailer "dummy"))
          (fun () -> CheckObjectType.check_object_direct (Type.create_context ()) (DirectObject.String "\x00") (Util.make_type Type.Text) (Errors.make_ctxt_name Key.Trailer "dummy"))) ;
          (* TODO : check Date *)
      "(28)" >:: (fun _ -> assert_raises
          (Errors.TypeError ("Invalid type : expected date", Errors.make_ctxt_name Key.Trailer "dummy"))
          (fun () -> CheckObjectType.check_object_direct (Type.create_context ()) (DirectObject.String "foo") (Util.make_type Type.Date) (Errors.make_ctxt_name Key.Trailer "dummy"))) ;
          *)
      "(29)" >:: (fun _ -> assert_raises
                     (Errors.TypeError ("Boolean value false is not the expected one (true)", Errors.make_ctxt_name Key.Trailer "dummy"))
                     (fun () -> CheckObjectType.check_object_direct (Type.create_context ()) (DirectObject.Bool false) (Util.make_type (Type.BoolExact true)) (Errors.make_ctxt_name Key.Trailer "dummy"))) ;
      "(30)" >:: (fun _ -> assert_raises
                     (Errors.TypeError ("Boolean value true is not the expected one (false)", Errors.make_ctxt_name Key.Trailer "dummy"))
                     (fun () -> CheckObjectType.check_object_direct (Type.create_context ()) (DirectObject.Bool true) (Util.make_type (Type.BoolExact false)) (Errors.make_ctxt_name Key.Trailer "dummy"))) ;
      "(31)" >:: (fun _ -> assert_raises
                     (Errors.TypeError ("Integer value 0 is below the minimum (5)", Errors.make_ctxt_name Key.Trailer "dummy"))
                     (fun () -> CheckObjectType.check_object_direct (Type.create_context ()) (DirectObject.Int ~:0) (Util.make_type (Type.IntRange (Some ~:5, Some ~:10))) (Errors.make_ctxt_name Key.Trailer "dummy"))) ;
      "(32)" >:: (fun _ -> assert_raises
                     (Errors.TypeError ("Integer value 100 is above the maximum (10)", Errors.make_ctxt_name Key.Trailer "dummy"))
                     (fun () -> CheckObjectType.check_object_direct (Type.create_context ()) (DirectObject.Int ~:100) (Util.make_type (Type.IntRange (Some ~:5, Some ~:10))) (Errors.make_ctxt_name Key.Trailer "dummy"))) ;
      "(33)" >:: (fun _ -> assert_raises
                     (Errors.TypeError ("Integer value 123 is not the expected one (456)", Errors.make_ctxt_name Key.Trailer "dummy"))
                     (fun () -> CheckObjectType.check_object_direct (Type.create_context ()) (DirectObject.Int ~:123) (Util.make_type (Type.IntExact ~:456)) (Errors.make_ctxt_name Key.Trailer "dummy"))) ;
      "(34)" >:: (fun _ -> assert_raises
                     (Errors.TypeError ("Integer value 3 is not among the expected ones (1, 2, 4)", Errors.make_ctxt_name Key.Trailer "dummy"))
                     (fun () -> CheckObjectType.check_object_direct (Type.create_context ()) (DirectObject.Int ~:3) (Util.make_type (Type.IntIn [| ~:1 ; ~:2 ; ~:4 |])) (Errors.make_ctxt_name Key.Trailer "dummy"))) ;
      "(35)" >:: (fun _ -> assert_raises
                     (Errors.TypeError ("Name value /bar is not the expected one (/foo)", Errors.make_ctxt_name Key.Trailer "dummy"))
                     (fun () -> CheckObjectType.check_object_direct (Type.create_context ()) (DirectObject.Name "bar") (Util.make_type (Type.NameExact "foo")) (Errors.make_ctxt_name Key.Trailer "dummy"))) ;
      "(36)" >:: (fun _ -> assert_raises
                     (Errors.TypeError ("Name value /d is not among the expected ones (/a, /b, /c, /e, /f)", Errors.make_ctxt_name Key.Trailer "dummy"))
                     (fun () -> CheckObjectType.check_object_direct (Type.create_context ()) (DirectObject.Name "d") (Util.make_type (Type.NameIn (TestType.add_all ["a" ; "b" ; "c" ; "e" ; "f"]))) (Errors.make_ctxt_name Key.Trailer "dummy"))) ;
    ] ;

    "check_alias" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (CheckObjectType.check_alias (TestTypeUtil.make_context 10) (Direct (DirectObject.Int ~:123)) "X" false (Errors.make_ctxt_key Key.Trailer))
                    (Util.make_type ~allow_ind:false Type.Int)) ;
      "(2)" >:: (fun _ -> assert_equal
                    (CheckObjectType.check_alias (TestTypeUtil.make_context 10) (Direct (DirectObject.Int ~:123)) "X" true (Errors.make_ctxt_key Key.Trailer))
                    (Util.make_type Type.Int)) ;
      "(3)" >:: (fun _ -> assert_equal
                    (let ctxt = TestTypeUtil.make_context 10 in
                     let (_:Type.t) = CheckObjectType.check_alias ctxt (Direct (DirectObject.Reference (Key.make_0 ~:1))) "X" true (Errors.make_ctxt_key Key.Trailer) in
                     MapKey.find (Key.make_0 ~:1) ctxt.Type.types)
                    Type.Int) ;
      "(4)" >:: (fun _ -> assert_equal
                    (CheckObjectType.check_alias (TestTypeUtil.make_context 10) (Direct (DirectObject.Reference (Key.make_0 ~:1))) "X" true (Errors.make_ctxt_key Key.Trailer))
                    (Util.make_type Type.Int)) ;

      "(5)" >:: (fun _ -> assert_raises
                    (Errors.TypeError ("Invalid type : expected int", Errors.make_ctxt_name Key.Trailer "dummy"))
                    (fun () -> CheckObjectType.check_alias (TestTypeUtil.make_context 10) (Direct (DirectObject.Bool false)) "X" false (Errors.make_ctxt_name Key.Trailer "dummy"))) ;
      "(6)" >:: (fun _ -> assert_raises
                    (Errors.TypeError ("Invalid type : expected int", Errors.make_ctxt_name Key.Trailer "dummy"))
                    (fun () -> CheckObjectType.check_alias (TestTypeUtil.make_context 10) (Direct (DirectObject.Reference (Key.make_0 ~:1))) "X" false (Errors.make_ctxt_name Key.Trailer "dummy"))) ;
      (* TODO *)
    ] ;

    "check_dict" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (CheckObjectType.check_dict (Type.create_context ()) (TestDict.add_all ["foo", DirectObject.Int ~:123 ; "bar", DirectObject.Int ~:456]) (Util.make_type Type.Int) (Errors.make_ctxt_key Key.Trailer))
                    ()) ;
      "(2)" >:: (fun _ -> assert_equal
                    (CheckObjectType.check_dict (Type.create_context ()) (TestDict.add_all ["a", DirectObject.Name "foo" ; "b", DirectObject.Name "bar"]) (Util.make_type Type.Name) (Errors.make_ctxt_key Key.Trailer))
                    ()) ;
      "(3)" >:: (fun _ -> assert_equal
                    (CheckObjectType.check_dict (Type.create_context ()) (TestDict.add_all []) (Util.make_type Type.String) (Errors.make_ctxt_key Key.Trailer))
                    ()) ;
      "(4)" >:: (fun _ -> assert_equal
                    (CheckObjectType.check_object_direct (Type.create_context ()) (DirectObject.Dictionary (TestDict.add_all ["abc", DirectObject.String "def"])) (Util.type_dict (Util.make_type Type.String)) (Errors.make_ctxt_key Key.Trailer))
                    (Util.type_dict (Util.make_type Type.String))) ;

      "(5)" >:: (fun _ -> assert_raises
                    (Errors.TypeError ("Invalid type : expected string", Errors.make_ctxt_name Key.Trailer "foo"))
                    (fun () -> CheckObjectType.check_dict (Type.create_context ()) (TestDict.add_all ["foo", DirectObject.Int ~:123]) (Util.make_type Type.String) (Errors.make_ctxt_key Key.Trailer))) ;
      "(6)" >:: (fun _ -> assert_raises
                    (Errors.TypeError ("Invalid type : expected string{}", Errors.make_ctxt_name Key.Trailer "dummy"))
                    (fun () -> CheckObjectType.check_object_direct (Type.create_context ()) (DirectObject.Array [DirectObject.String "def"]) (Util.type_dict (Util.make_type Type.String)) (Errors.make_ctxt_name Key.Trailer "dummy"))) ;
    ] ;

    "check_array" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (CheckObjectType.check_array (Type.create_context ()) [DirectObject.Real "1.23" ; DirectObject.Real "45.6"] (Util.make_type Type.Real) (Errors.make_ctxt_key Key.Trailer))
                    ()) ;
      "(2)" >:: (fun _ -> assert_equal
                    (CheckObjectType.check_array (Type.create_context ()) [DirectObject.Null] (Util.make_type Type.Null) (Errors.make_ctxt_key Key.Trailer))
                    ()) ;
      "(3)" >:: (fun _ -> assert_equal
                    (CheckObjectType.check_array (Type.create_context ()) [] (Util.make_type Type.Bool) (Errors.make_ctxt_key Key.Trailer))
                    ()) ;
      "(4)" >:: (fun _ -> assert_equal
                    (CheckObjectType.check_object_direct (Type.create_context ()) (DirectObject.Array [DirectObject.String "def"]) (Util.type_array (Util.make_type Type.String)) (Errors.make_ctxt_key Key.Trailer))
                    (Util.type_array (Util.make_type Type.String))) ;

      "(5)" >:: (fun _ -> assert_raises
                    (Errors.TypeError ("Invalid type : expected int", Errors.make_ctxt_index Key.Trailer 0))
                    (fun () -> CheckObjectType.check_array (Type.create_context ()) [DirectObject.Real "1.23" ; DirectObject.Real "45.6"] (Util.make_type Type.Int) (Errors.make_ctxt_key Key.Trailer))) ;
      "(6)" >:: (fun _ -> assert_raises
                    (Errors.TypeError ("Invalid type : expected int[]", Errors.make_ctxt_name Key.Trailer "dummy"))
                    (fun () -> CheckObjectType.check_object_direct (Type.create_context ()) DirectObject.Null (Util.type_array (Util.make_type Type.Int)) (Errors.make_ctxt_name Key.Trailer "dummy"))) ;
    ] ;

    "check_array_sized" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (CheckObjectType.check_array_sized (Type.create_context ()) [DirectObject.Bool true ; DirectObject.Bool false] (Util.make_type Type.Bool) 2 (Errors.make_ctxt_key Key.Trailer))
                    ()) ;
      "(2)" >:: (fun _ -> assert_equal
                    (CheckObjectType.check_array_sized (Type.create_context ()) [] (Util.make_type Type.String) 0 (Errors.make_ctxt_key Key.Trailer))
                    ()) ;
      "(3)" >:: (fun _ -> assert_equal
                    (CheckObjectType.check_object_direct (Type.create_context ()) (DirectObject.Array [DirectObject.String "def"]) (Util.type_sized_array 1 (Util.make_type Type.String)) (Errors.make_ctxt_key Key.Trailer))
                    (Util.type_sized_array 1 (Util.make_type Type.String))) ;

      "(4)" >:: (fun _ -> assert_raises
                    (Errors.TypeError ("Invalid type : expected int", Errors.make_ctxt_index Key.Trailer 0))
                    (fun () -> CheckObjectType.check_array_sized (Type.create_context ()) [DirectObject.Real "1.23" ; DirectObject.Real "45.6"] (Util.make_type Type.Int) 2 (Errors.make_ctxt_key Key.Trailer))) ;
      "(5)" >:: (fun _ -> assert_raises
                    (Errors.TypeError ("Array size (2) is not the expected one (3)", Errors.make_ctxt_name Key.Trailer "dummy"))
                    (fun () -> CheckObjectType.check_array_sized (Type.create_context ()) [DirectObject.Int ~:123 ; DirectObject.Int ~:456] (Util.make_type Type.Int) 3 (Errors.make_ctxt_name Key.Trailer "dummy"))) ;
      "(6)" >:: (fun _ -> assert_raises
                    (Errors.TypeError ("Invalid type : expected int[1]", Errors.make_ctxt_name Key.Trailer "dummy"))
                    (fun () -> CheckObjectType.check_object_direct (Type.create_context ()) (DirectObject.Int ~:123) (Util.type_sized_array 1 (Util.make_type Type.Int)) (Errors.make_ctxt_name Key.Trailer "dummy"))) ;
    ] ;

    "check_array_variant_sized" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (CheckObjectType.check_array_variant_sized (Type.create_context ()) [DirectObject.Bool true ; DirectObject.Bool false] (Util.make_type Type.Bool) [| 2 ; 3 |] (Errors.make_ctxt_key Key.Trailer))
                    ()) ;
      "(2)" >:: (fun _ -> assert_equal
                    (CheckObjectType.check_array_variant_sized (Type.create_context ()) [DirectObject.Int ~:123 ; DirectObject.Int ~:456 ; DirectObject.Int ~:789] (Util.make_type Type.Int) [| 2 ; 3 |] (Errors.make_ctxt_key Key.Trailer))
                    ()) ;
      "(3)" >:: (fun _ -> assert_equal
                    (CheckObjectType.check_array_variant_sized (Type.create_context ()) [] (Util.make_type Type.String) [| 0 ; 2 |] (Errors.make_ctxt_key Key.Trailer))
                    ()) ;
      "(4)" >:: (fun _ -> assert_equal
                    (CheckObjectType.check_object_direct (Type.create_context ()) (DirectObject.Array [DirectObject.String "def"]) (Util.make_type (Type.ArrayVariantSized (Util.make_type Type.String, [| 0 ; 1 |]))) (Errors.make_ctxt_key Key.Trailer))
                    (Util.make_type (Type.ArrayVariantSized (Util.make_type Type.String, [| 0 ; 1 |])))) ;

      "(5)" >:: (fun _ -> assert_raises
                    (Errors.TypeError ("Array size (2) is not among the expected ones (1, 3)", Errors.make_ctxt_name Key.Trailer "dummy"))
                    (fun () -> CheckObjectType.check_array_variant_sized (Type.create_context ()) [DirectObject.Int ~:123 ; DirectObject.Int ~:456] (Util.make_type Type.Int) [| 1 ; 3 |] (Errors.make_ctxt_name Key.Trailer "dummy"))) ;
      "(6)" >:: (fun _ -> assert_raises
                    (Errors.TypeError ("Invalid type : expected int[0, 1]", Errors.make_ctxt_name Key.Trailer "dummy"))
                    (fun () -> CheckObjectType.check_object_direct (Type.create_context ()) (DirectObject.Real "1.02") (Util.make_type (Type.ArrayVariantSized (Util.make_type Type.Int, [| 0 ; 1 |]))) (Errors.make_ctxt_name Key.Trailer "dummy"))) ;
    ] ;

    "check_array_tuples" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (CheckObjectType.check_array_tuples (Type.create_context ()) [DirectObject.Int ~:123 ; DirectObject.String "foo" ; DirectObject.Int ~:456 ; DirectObject.String "bar"] [| Util.make_type Type.Int ; Util.make_type Type.String |] (Errors.make_ctxt_key Key.Trailer))
                    ()) ;
      "(2)" >:: (fun _ -> assert_equal
                    (CheckObjectType.check_array_tuples (Type.create_context ()) [] [| Util.make_type Type.Int ; Util.make_type Type.String |] (Errors.make_ctxt_key Key.Trailer))
                    ()) ;
      "(3)" >:: (fun _ -> assert_equal
                    (CheckObjectType.check_object_direct (Type.create_context ()) (DirectObject.Array [DirectObject.String "def" ; DirectObject.Int ~:123 ; DirectObject.String "ghi" ; DirectObject.Int ~:789]) (Util.type_array_tuples [| Util.make_type Type.String ; Util.make_type Type.Int |]) (Errors.make_ctxt_key Key.Trailer))
                    (Util.type_array_tuples [| Util.make_type Type.String ; Util.make_type Type.Int |])) ;

      "(4)" >:: (fun _ -> assert_raises
                    (Errors.TypeError ("Array size (3) is not among the expected ones (multiples of 2) for this array of tuples", Errors.make_ctxt_name Key.Trailer "dummy"))
                    (fun () -> CheckObjectType.check_array_tuples (Type.create_context ()) [DirectObject.Int ~:123 ; DirectObject.String "foo" ; DirectObject.Int ~:456] [| Util.make_type Type.Int ; Util.make_type Type.String |] (Errors.make_ctxt_name Key.Trailer "dummy"))) ;
      "(5)" >:: (fun _ -> assert_raises
                    (Errors.TypeError ("Invalid type : expected string", Errors.make_ctxt_index Key.Trailer 3))
                    (fun () -> CheckObjectType.check_array_tuples (Type.create_context ()) [DirectObject.Int ~:123 ; DirectObject.String "foo" ; DirectObject.Int ~:456 ; DirectObject.Name "bar"] [| Util.make_type Type.Int ; Util.make_type Type.String |] (Errors.make_ctxt_key Key.Trailer))) ;
      "(6)" >:: (fun _ -> assert_raises
                    (Errors.TypeError ("Invalid type : expected {string, int}[]", Errors.make_ctxt_name Key.Trailer "dummy"))
                    (fun () -> CheckObjectType.check_object_direct (Type.create_context ()) (DirectObject.Bool true) (Util.type_array_tuples [| Util.make_type Type.String ; Util.make_type Type.Int |]) (Errors.make_ctxt_name Key.Trailer "dummy"))) ;
    ] ;

    "check_tuple" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (CheckObjectType.check_tuple (Type.create_context ()) [DirectObject.Int ~:123 ; DirectObject.String "foo" ; DirectObject.Name "bar"] [| Util.make_type Type.Int ; Util.make_type Type.String ; Util.make_type Type.Name |] (Errors.make_ctxt_key Key.Trailer))
                    ()) ;
      "(2)" >:: (fun _ -> assert_equal
                    (CheckObjectType.check_object_direct (Type.create_context ()) (DirectObject.Array [DirectObject.String "def" ; DirectObject.Int ~:123]) (Util.type_tuple [| Util.make_type Type.String ; Util.make_type Type.Int |]) (Errors.make_ctxt_key Key.Trailer))
                    (Util.type_tuple [| Util.make_type Type.String ; Util.make_type Type.Int |])) ;

      "(3)" >:: (fun _ -> assert_raises
                    (Errors.TypeError ("Invalid type : expected string", Errors.make_ctxt_index Key.Trailer 1))
                    (fun () -> CheckObjectType.check_tuple (Type.create_context ()) [DirectObject.Int ~:123 ; DirectObject.Name "bar"] [| Util.make_type Type.Int ; Util.make_type Type.String |] (Errors.make_ctxt_key Key.Trailer))) ;
      "(4)" >:: (fun _ -> assert_raises
                    (Errors.TypeError ("Array size (1) is not the expected one (2) for this tuple", Errors.make_ctxt_name Key.Trailer "dummy"))
                    (fun () -> CheckObjectType.check_tuple (Type.create_context ()) [DirectObject.Int ~:123] [| Util.make_type Type.Int ; Util.make_type Type.String |] (Errors.make_ctxt_name Key.Trailer "dummy"))) ;
      "(5)" >:: (fun _ -> assert_raises
                    (Errors.TypeError ("Invalid type : expected {{string, int}}", Errors.make_ctxt_name Key.Trailer "dummy"))
                    (fun () -> CheckObjectType.check_object_direct (Type.create_context ()) (DirectObject.Int ~:456) (Util.type_tuple [| Util.make_type Type.String ; Util.make_type Type.Int |]) (Errors.make_ctxt_name Key.Trailer "dummy"))) ;
    ] ;

    "check_variant" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (CheckObjectType.check_variant (Type.create_context ()) (Direct (DirectObject.Int ~:123)) [Type.Int ; Type.String ; Type.Name] (Errors.make_ctxt_key Key.Trailer))
                    (Util.make_type Type.Int)) ;
      "(2)" >:: (fun _ -> assert_equal
                    (CheckObjectType.check_variant (Type.create_context ()) (Direct (DirectObject.String "foo")) [Type.Int ; Type.String ; Type.Name] (Errors.make_ctxt_key Key.Trailer))
                    (Util.make_type Type.String)) ;
      "(3)" >:: (fun _ -> assert_equal
                    (CheckObjectType.check_object_direct (Type.create_context ()) (DirectObject.Name "bar") (Util.type_variant [Type.Int ; Type.String ; Type.Name]) (Errors.make_ctxt_key Key.Trailer))
                    (Util.make_type Type.Name)) ;

      "(4)" >:: (fun _ -> assert_raises
                    (Errors.TypeError ("Invalid variant type : expected (int | string | name)", Errors.make_ctxt_name Key.Trailer "dummy"))
                    (fun () -> CheckObjectType.check_variant (Type.create_context ()) (Direct (DirectObject.Bool true)) [Type.Int ; Type.String ; Type.Name] (Errors.make_ctxt_name Key.Trailer "dummy"))) ;
      "(5)" >:: (fun _ -> assert_raises
                    (Errors.TypeError ("Invalid variant type : expected (string | int | name)", Errors.make_ctxt_name Key.Trailer "dummy"))
                    (fun () -> CheckObjectType.check_object_direct (Type.create_context ()) (DirectObject.Array []) (Util.type_variant [Type.String ; Type.Int ; Type.Name]) (Errors.make_ctxt_name Key.Trailer "dummy"))) ;
      (* TODO *)
    ] ;

    "check_indirect" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (CheckObjectType.check_indirect (Type.create_context ()) (Key.make_0 ~:1) (Util.make_type Type.Int) (Errors.make_ctxt_key Key.Trailer))
                    (Util.make_type Type.Int)) ;
      "(2)" >:: (fun _ -> assert_equal
                    (CheckObjectType.check_indirect (make_context_types 0 [Key.make_0 ~:1, Type.Int]) (Key.make_0 ~:1) (Util.make_type Type.Int) (Errors.make_ctxt_key Key.Trailer))
                    (Util.make_type Type.Int)) ;
      "(3)" >:: (fun _ -> assert_equal
                    (CheckObjectType.check_indirect (make_context_types 0 [Key.make_0 ~:1, Type.Variant [Type.Int ; Type.String]]) (Key.make_0 ~:1) (Util.make_type (Type.Variant [Type.Name ; Type.Int])) (Errors.make_ctxt_key Key.Trailer))
                    (Util.make_type Type.Int)) ;

      "(4)" >:: (fun _ -> assert_raises
                    (Errors.TypeError ("Inconsistent type inference between name and string", Errors.make_ctxt_key (Key.make_0 ~:1)))
                    (fun () -> CheckObjectType.check_indirect (make_context_types 0 [Key.make_0 ~:1, Type.String]) (Key.make_0 ~:1) (Util.make_type Type.Name) (Errors.make_ctxt_key Key.Trailer))) ;
      "(5)" >:: (fun _ -> assert_raises
                    (Errors.TypeError ("Inconsistent type inference between (name | int) and (real | string)", Errors.make_ctxt_key (Key.make_0 ~:1)))
                    (fun () -> CheckObjectType.check_indirect (make_context_types 0 [Key.make_0 ~:1, Type.Variant [Type.Real ; Type.String]]) (Key.make_0 ~:1) (Util.make_type (Type.Variant [Type.Name ; Type.Int])) (Errors.make_ctxt_key Key.Trailer))) ;
      (* TODO *)
    ] ;

    "check_class" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (CheckObjectType.check_class (TestTypeUtil.make_context 20) (TestDict.add_all ["foo", DirectObject.Int ~:123 ; "bar", DirectObject.String "test"]) "A" (Errors.make_ctxt_key Key.Trailer))
                    ()) ;
      "(2)" >:: (fun _ -> assert_equal
                    (CheckObjectType.check_class (TestTypeUtil.make_context 20) (TestDict.add_all ["foo", DirectObject.Int ~:456]) "A" (Errors.make_ctxt_key Key.Trailer))
                    ()) ;

      "(3)" >:: (fun _ -> assert_raises
                    (Errors.TypeError ("Mandatory entry /foo was not found in instance of class A", Errors.make_ctxt_name Key.Trailer "dummy"))
                    (fun () -> CheckObjectType.check_class (TestTypeUtil.make_context 20) (TestDict.add_all ["bar", DirectObject.String "test"]) "A" (Errors.make_ctxt_name Key.Trailer "dummy"))) ;
      "(4)" >:: (fun _ -> assert_raises
                    (Errors.TypeError ("Invalid type : expected int", Errors.make_ctxt_name Key.Trailer "foo"))
                    (fun () -> CheckObjectType.check_class (TestTypeUtil.make_context 20) (TestDict.add_all ["foo", DirectObject.Bool true]) "A" (Errors.make_ctxt_key Key.Trailer))) ;
      "(5)" >:: (fun _ -> assert_raises
                    (Errors.TypeError ("Unexpected entry /key in instance of class A", Errors.make_ctxt_name Key.Trailer "dummy"))
                    (fun () -> CheckObjectType.check_class (TestTypeUtil.make_context 20) (TestDict.add_all ["foo", DirectObject.Int ~:123 ; "key", DirectObject.String "text"]) "A" (Errors.make_ctxt_name Key.Trailer "dummy"))) ;
      (* TODO *)
    ] ;

    "check_subclass" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (CheckObjectType.check_subclass (TestTypeUtil.make_context 20) (TestDict.add_all ["foo", DirectObject.Int ~:123 ; "bar", DirectObject.String "text" ; "key", DirectObject.Name "test"]) "A" (Errors.make_ctxt_key Key.Trailer) (Hashtbl.create 1))
                    true) ;
      "(2)" >:: (fun _ -> assert_equal
                    (CheckObjectType.check_subclass (TestTypeUtil.make_context 20) (TestDict.add_all ["foo", DirectObject.Int ~:123 ; "key", DirectObject.Name "test"]) "A" (Errors.make_ctxt_key Key.Trailer) (Hashtbl.create 1))
                    true) ;
      "(3)" >:: (fun _ -> assert_equal
                    (CheckObjectType.check_subclass (TestTypeUtil.make_context 20) (TestDict.add_all ["foo", DirectObject.Int ~:123 ; "key", DirectObject.Name "test"]) "B" (Errors.make_ctxt_key Key.Trailer) (Hashtbl.create 1))
                    true) ;

      "(4)" >:: (fun _ -> assert_raises
                    (Errors.TypeError ("Mandatory entry /foo was not found in instance of class A", Errors.make_ctxt_name Key.Trailer "dummy"))
                    (fun () -> CheckObjectType.check_subclass (TestTypeUtil.make_context 20) (TestDict.add_all ["bar", DirectObject.String "test"]) "A" (Errors.make_ctxt_name Key.Trailer "dummy") (Hashtbl.create 1))) ;
      "(5)" >:: (fun _ -> assert_raises
                    (Errors.TypeError ("Mandatory entry /foo was not found in instance of class A", Errors.make_ctxt_name Key.Trailer "dummy"))
                    (fun () -> CheckObjectType.check_subclass (TestTypeUtil.make_context 20) (TestDict.add_all ["key", DirectObject.Name "test"]) "B" (Errors.make_ctxt_name Key.Trailer "dummy") (Hashtbl.create 1))) ;
      "(6)" >:: (fun _ -> assert_raises
                    (Errors.TypeError ("Invalid type : expected name", Errors.make_ctxt_name Key.Trailer "key"))
                    (fun () -> CheckObjectType.check_subclass (TestTypeUtil.make_context 20) (TestDict.add_all ["foo", DirectObject.Int ~:123 ; "key", DirectObject.String "text"]) "B" (Errors.make_ctxt_key Key.Trailer) (Hashtbl.create 1))) ;
      (* TODO *)
    ] ;
  ]

