(*****************************************************************************)
(*  Caradoc: a PDF parser and validator                                      *)
(*  Copyright (C) 2015 ANSSI                                                 *)
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
open Pdfobject
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
                    (CheckObjectType.check_object (Type.create_context ()) (PDFObject.Null) (Util.make_type Type.Null) Key.Trailer "")
                    (Util.make_type Type.Null)) ;
      "(2)" >:: (fun _ -> assert_equal
                    (CheckObjectType.check_object (Type.create_context ()) (PDFObject.Bool true) (Util.make_type Type.Bool) Key.Trailer "")
                    (Util.make_type Type.Bool)) ;
      "(3)" >:: (fun _ -> assert_equal
                    (CheckObjectType.check_object (Type.create_context ()) (PDFObject.Int ~:123) (Util.make_type Type.Int) Key.Trailer "")
                    (Util.make_type Type.Int)) ;
      "(4)" >:: (fun _ -> assert_equal
                    (CheckObjectType.check_object (Type.create_context ()) (PDFObject.Int ~:123) (Util.make_type ~allow_ind:false Type.Int) Key.Trailer "")
                    (Util.make_type ~allow_ind:false Type.Int)) ;
      "(5)" >:: (fun _ -> assert_equal
                    (CheckObjectType.check_object (Type.create_context ()) (PDFObject.Real "123.456") (Util.make_type Type.Real) Key.Trailer "")
                    (Util.make_type Type.Real)) ;
      "(6)" >:: (fun _ -> assert_equal
                    (CheckObjectType.check_object (Type.create_context ()) (PDFObject.String "foo") (Util.make_type Type.String) Key.Trailer "")
                    (Util.make_type Type.String)) ;
      "(7)" >:: (fun _ -> assert_equal
                    (CheckObjectType.check_object (Type.create_context ()) (PDFObject.Name "key") (Util.make_type Type.Name) Key.Trailer "")
                    (Util.make_type Type.Name)) ;
      "(8)" >:: (fun _ -> assert_equal
                    (CheckObjectType.check_object (Type.create_context ()) (PDFObject.String "text") (Util.make_type Type.Text) Key.Trailer "")
                    (Util.make_type Type.Text)) ;
      "(9)" >:: (fun _ -> assert_equal
                    (CheckObjectType.check_object (Type.create_context ()) (PDFObject.String "D:20001231235959+01'00") (Util.make_type Type.Date) Key.Trailer "")
                    (Util.make_type Type.Date)) ;

      "(10)" >:: (fun _ -> assert_equal
                     (CheckObjectType.check_object (Type.create_context ()) (PDFObject.Bool true) (Util.make_type (Type.BoolExact true)) Key.Trailer "")
                     (Util.make_type (Type.BoolExact true))) ;
      "(11)" >:: (fun _ -> assert_equal
                     (CheckObjectType.check_object (Type.create_context ()) (PDFObject.Bool false) (Util.make_type (Type.BoolExact false)) Key.Trailer "")
                     (Util.make_type (Type.BoolExact false))) ;
      "(12)" >:: (fun _ -> assert_equal
                     (CheckObjectType.check_object (Type.create_context ()) (PDFObject.Int ~:1) (Util.make_type (Type.IntRange (Some ~:1, Some ~:10))) Key.Trailer "")
                     (Util.make_type (Type.IntRange (Some ~:1, Some ~:10)))) ;
      "(13)" >:: (fun _ -> assert_equal
                     (CheckObjectType.check_object (Type.create_context ()) (PDFObject.Int ~:10) (Util.make_type (Type.IntRange (Some ~:1, Some ~:10))) Key.Trailer "")
                     (Util.make_type (Type.IntRange (Some ~:1, Some ~:10)))) ;
      "(14)" >:: (fun _ -> assert_equal
                     (CheckObjectType.check_object (Type.create_context ()) (PDFObject.Int ~:3) (Util.make_type (Type.IntRange (Some ~:1, Some ~:10))) Key.Trailer "")
                     (Util.make_type (Type.IntRange (Some ~:1, Some ~:10)))) ;
      "(15)" >:: (fun _ -> assert_equal
                     (CheckObjectType.check_object (Type.create_context ()) (PDFObject.Int ~:123456789) (Util.make_type (Type.IntRange (Some ~:0, None))) Key.Trailer "")
                     (Util.make_type (Type.IntRange (Some ~:0, None)))) ;
      "(16)" >:: (fun _ -> assert_equal
                     (CheckObjectType.check_object (Type.create_context ()) (PDFObject.Int ~:(-123456789)) (Util.make_type (Type.IntRange (None, Some ~:0))) Key.Trailer "")
                     (Util.make_type (Type.IntRange (None, Some ~:0)))) ;
      "(17)" >:: (fun _ -> assert_equal
                     (CheckObjectType.check_object (Type.create_context ()) (PDFObject.Int ~:111) (Util.make_type (Type.IntExact ~:111)) Key.Trailer "")
                     (Util.make_type (Type.IntExact ~:111))) ;
      "(18)" >:: (fun _ -> assert_equal
                     (CheckObjectType.check_object (Type.create_context ()) (PDFObject.Int ~:111) (Util.make_type (Type.IntIn [| ~:1 ; ~:11 ; ~:111 ; ~:1111 |])) Key.Trailer "")
                     (Util.make_type (Type.IntIn [| ~:1 ; ~:11 ; ~:111 ; ~:1111 |]))) ;
      "(19)" >:: (fun _ -> assert_equal
                     (CheckObjectType.check_object (Type.create_context ()) (PDFObject.Name "bar") (Util.make_type (Type.NameExact "bar")) Key.Trailer "")
                     (Util.make_type (Type.NameExact "bar"))) ;
      "(20)" >:: (fun _ -> assert_equal
                     (CheckObjectType.check_object (Type.create_context ()) (PDFObject.Name "bar") (Util.make_type (Type.NameIn (TestType.add_all ["foo" ; "bar"]))) Key.Trailer "")
                     (Util.make_type (Type.NameIn (TestType.add_all ["foo" ; "bar"])))) ;

      "(21)" >:: (fun _ -> assert_raises
                     (Errors.TypeError ("Invalid type : expected null", Key.Trailer, "dummy"))
                     (fun () -> CheckObjectType.check_object (Type.create_context ()) (PDFObject.Int ~:123) (Util.make_type Type.Null) Key.Trailer "dummy")) ;
      "(22)" >:: (fun _ -> assert_raises
                     (Errors.TypeError ("Invalid type : expected bool", Key.Trailer, "dummy"))
                     (fun () -> CheckObjectType.check_object (Type.create_context ()) (PDFObject.String "foo") (Util.make_type Type.Bool) Key.Trailer "dummy")) ;
      "(23)" >:: (fun _ -> assert_raises
                     (Errors.TypeError ("Invalid type : expected int", Key.Trailer, "dummy"))
                     (fun () -> CheckObjectType.check_object (Type.create_context ()) (PDFObject.Name "key") (Util.make_type Type.Int) Key.Trailer "dummy")) ;
      "(24)" >:: (fun _ -> assert_raises
                     (Errors.TypeError ("Invalid type : expected real", Key.Trailer, "dummy"))
                     (fun () -> CheckObjectType.check_object (Type.create_context ()) (PDFObject.Bool true) (Util.make_type Type.Real) Key.Trailer "dummy")) ;
      "(25)" >:: (fun _ -> assert_raises
                     (Errors.TypeError ("Invalid type : expected string", Key.Trailer, "dummy"))
                     (fun () -> CheckObjectType.check_object (Type.create_context ()) (PDFObject.Array [PDFObject.String "foo"]) (Util.make_type Type.String) Key.Trailer "dummy")) ;
      "(26)" >:: (fun _ -> assert_raises
                     (Errors.TypeError ("Invalid type : expected name", Key.Trailer, "dummy"))
                     (fun () -> CheckObjectType.check_object (Type.create_context ()) (PDFObject.Null) (Util.make_type Type.Name) Key.Trailer "dummy")) ;
          (*
          (* TODO : check Text *)
      "(27)" >:: (fun _ -> assert_raises
          (Errors.TypeError ("Invalid type : expected text", Key.Trailer, "dummy"))
          (fun () -> CheckObjectType.check_object (Type.create_context ()) (PDFObject.String "\x00") (Util.make_type Type.Text) Key.Trailer "dummy")) ;
          (* TODO : check Date *)
      "(28)" >:: (fun _ -> assert_raises
          (Errors.TypeError ("Invalid type : expected date", Key.Trailer, "dummy"))
          (fun () -> CheckObjectType.check_object (Type.create_context ()) (PDFObject.String "foo") (Util.make_type Type.Date) Key.Trailer "dummy")) ;
          *)
      "(29)" >:: (fun _ -> assert_raises
                     (Errors.TypeError ("Boolean value false is not the expected one (true)", Key.Trailer, "dummy"))
                     (fun () -> CheckObjectType.check_object (Type.create_context ()) (PDFObject.Bool false) (Util.make_type (Type.BoolExact true)) Key.Trailer "dummy")) ;
      "(30)" >:: (fun _ -> assert_raises
                     (Errors.TypeError ("Boolean value true is not the expected one (false)", Key.Trailer, "dummy"))
                     (fun () -> CheckObjectType.check_object (Type.create_context ()) (PDFObject.Bool true) (Util.make_type (Type.BoolExact false)) Key.Trailer "dummy")) ;
      "(31)" >:: (fun _ -> assert_raises
                     (Errors.TypeError ("Integer value 0 is below the minimum (5)", Key.Trailer, "dummy"))
                     (fun () -> CheckObjectType.check_object (Type.create_context ()) (PDFObject.Int ~:0) (Util.make_type (Type.IntRange (Some ~:5, Some ~:10))) Key.Trailer "dummy")) ;
      "(32)" >:: (fun _ -> assert_raises
                     (Errors.TypeError ("Integer value 100 is above the maximum (10)", Key.Trailer, "dummy"))
                     (fun () -> CheckObjectType.check_object (Type.create_context ()) (PDFObject.Int ~:100) (Util.make_type (Type.IntRange (Some ~:5, Some ~:10))) Key.Trailer "dummy")) ;
      "(33)" >:: (fun _ -> assert_raises
                     (Errors.TypeError ("Integer value 123 is not the expected one (456)", Key.Trailer, "dummy"))
                     (fun () -> CheckObjectType.check_object (Type.create_context ()) (PDFObject.Int ~:123) (Util.make_type (Type.IntExact ~:456)) Key.Trailer "dummy")) ;
      "(34)" >:: (fun _ -> assert_raises
                     (Errors.TypeError ("Integer value 3 is not among the expected ones (1, 2, 4)", Key.Trailer, "dummy"))
                     (fun () -> CheckObjectType.check_object (Type.create_context ()) (PDFObject.Int ~:3) (Util.make_type (Type.IntIn [| ~:1 ; ~:2 ; ~:4 |])) Key.Trailer "dummy")) ;
      "(35)" >:: (fun _ -> assert_raises
                     (Errors.TypeError ("Name value /bar is not the expected one (/foo)", Key.Trailer, "dummy"))
                     (fun () -> CheckObjectType.check_object (Type.create_context ()) (PDFObject.Name "bar") (Util.make_type (Type.NameExact "foo")) Key.Trailer "dummy")) ;
      "(36)" >:: (fun _ -> assert_raises
                     (Errors.TypeError ("Name value /d is not among the expected ones (/a, /b, /c, /e, /f)", Key.Trailer, "dummy"))
                     (fun () -> CheckObjectType.check_object (Type.create_context ()) (PDFObject.Name "d") (Util.make_type (Type.NameIn (TestType.add_all ["a" ; "b" ; "c" ; "e" ; "f"]))) Key.Trailer "dummy")) ;
    ] ;

    "check_alias" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (CheckObjectType.check_alias (TestTypeUtil.make_context 10) (PDFObject.Int ~:123) "X" false Key.Trailer "")
                    (Util.make_type ~allow_ind:false Type.Int)) ;
      "(2)" >:: (fun _ -> assert_equal
                    (CheckObjectType.check_alias (TestTypeUtil.make_context 10) (PDFObject.Int ~:123) "X" true Key.Trailer "")
                    (Util.make_type Type.Int)) ;
      "(3)" >:: (fun _ -> assert_equal
                    (let ctxt = TestTypeUtil.make_context 10 in
                     let (_:Type.t) = CheckObjectType.check_alias ctxt (PDFObject.Reference (Key.make_0 ~:1)) "X" true Key.Trailer "" in
                     MapKey.find (Key.make_0 ~:1) ctxt.Type.types)
                    Type.Int) ;
      "(4)" >:: (fun _ -> assert_equal
                    (CheckObjectType.check_alias (TestTypeUtil.make_context 10) (PDFObject.Reference (Key.make_0 ~:1)) "X" true Key.Trailer "")
                    (Util.make_type Type.Int)) ;

      "(5)" >:: (fun _ -> assert_raises
                    (Errors.TypeError ("Invalid type : expected int", Key.Trailer, "dummy"))
                    (fun () -> CheckObjectType.check_alias (TestTypeUtil.make_context 10) (PDFObject.Bool false) "X" false Key.Trailer "dummy")) ;
      "(6)" >:: (fun _ -> assert_raises
                    (Errors.TypeError ("Invalid type : expected int", Key.Trailer, "dummy"))
                    (fun () -> CheckObjectType.check_alias (TestTypeUtil.make_context 10) (PDFObject.Reference (Key.make_0 ~:1)) "X" false Key.Trailer "dummy")) ;
      (* TODO *)
    ] ;

    "check_dict" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (CheckObjectType.check_dict (Type.create_context ()) (TestDict.add_all ["foo", PDFObject.Int ~:123 ; "bar", PDFObject.Int ~:456]) (Util.make_type Type.Int) Key.Trailer "")
                    ()) ;
      "(2)" >:: (fun _ -> assert_equal
                    (CheckObjectType.check_dict (Type.create_context ()) (TestDict.add_all ["a", PDFObject.Name "foo" ; "b", PDFObject.Name "bar"]) (Util.make_type Type.Name) Key.Trailer "")
                    ()) ;
      "(3)" >:: (fun _ -> assert_equal
                    (CheckObjectType.check_dict (Type.create_context ()) (TestDict.add_all []) (Util.make_type Type.String) Key.Trailer "")
                    ()) ;
      "(4)" >:: (fun _ -> assert_equal
                    (CheckObjectType.check_object (Type.create_context ()) (PDFObject.Dictionary (TestDict.add_all ["abc", PDFObject.String "def"])) (Util.type_dict (Util.make_type Type.String)) Key.Trailer "")
                    (Util.type_dict (Util.make_type Type.String))) ;

      "(5)" >:: (fun _ -> assert_raises
                    (Errors.TypeError ("Invalid type : expected string", Key.Trailer, "dummy/foo"))
                    (fun () -> CheckObjectType.check_dict (Type.create_context ()) (TestDict.add_all ["foo", PDFObject.Int ~:123]) (Util.make_type Type.String) Key.Trailer "dummy")) ;
      "(6)" >:: (fun _ -> assert_raises
                    (Errors.TypeError ("Invalid type : expected string{}", Key.Trailer, "dummy"))
                    (fun () -> CheckObjectType.check_object (Type.create_context ()) (PDFObject.Array [PDFObject.String "def"]) (Util.type_dict (Util.make_type Type.String)) Key.Trailer "dummy")) ;
    ] ;

    "check_array" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (CheckObjectType.check_array (Type.create_context ()) [PDFObject.Real "1.23" ; PDFObject.Real "45.6"] (Util.make_type Type.Real) Key.Trailer "")
                    ()) ;
      "(2)" >:: (fun _ -> assert_equal
                    (CheckObjectType.check_array (Type.create_context ()) [PDFObject.Null] (Util.make_type Type.Null) Key.Trailer "")
                    ()) ;
      "(3)" >:: (fun _ -> assert_equal
                    (CheckObjectType.check_array (Type.create_context ()) [] (Util.make_type Type.Bool) Key.Trailer "")
                    ()) ;
      "(4)" >:: (fun _ -> assert_equal
                    (CheckObjectType.check_object (Type.create_context ()) (PDFObject.Array [PDFObject.String "def"]) (Util.type_array (Util.make_type Type.String)) Key.Trailer "")
                    (Util.type_array (Util.make_type Type.String))) ;

      "(5)" >:: (fun _ -> assert_raises
                    (Errors.TypeError ("Invalid type : expected int", Key.Trailer, "[0]"))
                    (fun () -> CheckObjectType.check_array (Type.create_context ()) [PDFObject.Real "1.23" ; PDFObject.Real "45.6"] (Util.make_type Type.Int) Key.Trailer "")) ;
      "(6)" >:: (fun _ -> assert_raises
                    (Errors.TypeError ("Invalid type : expected int[]", Key.Trailer, "dummy"))
                    (fun () -> CheckObjectType.check_object (Type.create_context ()) PDFObject.Null (Util.type_array (Util.make_type Type.Int)) Key.Trailer "dummy")) ;
    ] ;

    "check_array_sized" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (CheckObjectType.check_array_sized (Type.create_context ()) [PDFObject.Bool true ; PDFObject.Bool false] (Util.make_type Type.Bool) 2 Key.Trailer "")
                    ()) ;
      "(2)" >:: (fun _ -> assert_equal
                    (CheckObjectType.check_array_sized (Type.create_context ()) [] (Util.make_type Type.String) 0 Key.Trailer "")
                    ()) ;
      "(3)" >:: (fun _ -> assert_equal
                    (CheckObjectType.check_object (Type.create_context ()) (PDFObject.Array [PDFObject.String "def"]) (Util.type_sized_array 1 (Util.make_type Type.String)) Key.Trailer "")
                    (Util.type_sized_array 1 (Util.make_type Type.String))) ;

      "(4)" >:: (fun _ -> assert_raises
                    (Errors.TypeError ("Invalid type : expected int", Key.Trailer, "dummy[0]"))
                    (fun () -> CheckObjectType.check_array_sized (Type.create_context ()) [PDFObject.Real "1.23" ; PDFObject.Real "45.6"] (Util.make_type Type.Int) 2 Key.Trailer "dummy")) ;
      "(5)" >:: (fun _ -> assert_raises
                    (Errors.TypeError ("Array size (2) is not the expected one (3)", Key.Trailer, "dummy"))
                    (fun () -> CheckObjectType.check_array_sized (Type.create_context ()) [PDFObject.Int ~:123 ; PDFObject.Int ~:456] (Util.make_type Type.Int) 3 Key.Trailer "dummy")) ;
      "(6)" >:: (fun _ -> assert_raises
                    (Errors.TypeError ("Invalid type : expected int[1]", Key.Trailer, "dummy"))
                    (fun () -> CheckObjectType.check_object (Type.create_context ()) (PDFObject.Int ~:123) (Util.type_sized_array 1 (Util.make_type Type.Int)) Key.Trailer "dummy")) ;
    ] ;

    "check_array_variant_sized" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (CheckObjectType.check_array_variant_sized (Type.create_context ()) [PDFObject.Bool true ; PDFObject.Bool false] (Util.make_type Type.Bool) [| 2 ; 3 |] Key.Trailer "")
                    ()) ;
      "(2)" >:: (fun _ -> assert_equal
                    (CheckObjectType.check_array_variant_sized (Type.create_context ()) [PDFObject.Int ~:123 ; PDFObject.Int ~:456 ; PDFObject.Int ~:789] (Util.make_type Type.Int) [| 2 ; 3 |] Key.Trailer "")
                    ()) ;
      "(3)" >:: (fun _ -> assert_equal
                    (CheckObjectType.check_array_variant_sized (Type.create_context ()) [] (Util.make_type Type.String) [| 0 ; 2 |] Key.Trailer "")
                    ()) ;
      "(4)" >:: (fun _ -> assert_equal
                    (CheckObjectType.check_object (Type.create_context ()) (PDFObject.Array [PDFObject.String "def"]) (Util.make_type (Type.ArrayVariantSized (Util.make_type Type.String, [| 0 ; 1 |]))) Key.Trailer "")
                    (Util.make_type (Type.ArrayVariantSized (Util.make_type Type.String, [| 0 ; 1 |])))) ;

      "(5)" >:: (fun _ -> assert_raises
                    (Errors.TypeError ("Array size (2) is not among the expected ones (1, 3)", Key.Trailer, "dummy"))
                    (fun () -> CheckObjectType.check_array_variant_sized (Type.create_context ()) [PDFObject.Int ~:123 ; PDFObject.Int ~:456] (Util.make_type Type.Int) [| 1 ; 3 |] Key.Trailer "dummy")) ;
      "(6)" >:: (fun _ -> assert_raises
                    (Errors.TypeError ("Invalid type : expected int[0, 1]", Key.Trailer, "dummy"))
                    (fun () -> CheckObjectType.check_object (Type.create_context ()) (PDFObject.Real "1.02") (Util.make_type (Type.ArrayVariantSized (Util.make_type Type.Int, [| 0 ; 1 |]))) Key.Trailer "dummy")) ;
    ] ;

    "check_array_tuples" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (CheckObjectType.check_array_tuples (Type.create_context ()) [PDFObject.Int ~:123 ; PDFObject.String "foo" ; PDFObject.Int ~:456 ; PDFObject.String "bar"] [| Util.make_type Type.Int ; Util.make_type Type.String |] Key.Trailer "")
                    ()) ;
      "(2)" >:: (fun _ -> assert_equal
                    (CheckObjectType.check_array_tuples (Type.create_context ()) [] [| Util.make_type Type.Int ; Util.make_type Type.String |] Key.Trailer "")
                    ()) ;
      "(3)" >:: (fun _ -> assert_equal
                    (CheckObjectType.check_object (Type.create_context ()) (PDFObject.Array [PDFObject.String "def" ; PDFObject.Int ~:123 ; PDFObject.String "ghi" ; PDFObject.Int ~:789]) (Util.type_array_tuples [| Util.make_type Type.String ; Util.make_type Type.Int |]) Key.Trailer "")
                    (Util.type_array_tuples [| Util.make_type Type.String ; Util.make_type Type.Int |])) ;

      "(4)" >:: (fun _ -> assert_raises
                    (Errors.TypeError ("Array size (3) is not among the expected ones (multiples of 2) for this array of tuples", Key.Trailer, "dummy"))
                    (fun () -> CheckObjectType.check_array_tuples (Type.create_context ()) [PDFObject.Int ~:123 ; PDFObject.String "foo" ; PDFObject.Int ~:456] [| Util.make_type Type.Int ; Util.make_type Type.String |] Key.Trailer "dummy")) ;
      "(5)" >:: (fun _ -> assert_raises
                    (Errors.TypeError ("Invalid type : expected string", Key.Trailer, "dummy[3]"))
                    (fun () -> CheckObjectType.check_array_tuples (Type.create_context ()) [PDFObject.Int ~:123 ; PDFObject.String "foo" ; PDFObject.Int ~:456 ; PDFObject.Name "bar"] [| Util.make_type Type.Int ; Util.make_type Type.String |] Key.Trailer "dummy")) ;
      "(6)" >:: (fun _ -> assert_raises
                    (Errors.TypeError ("Invalid type : expected {string, int}[]", Key.Trailer, "dummy"))
                    (fun () -> CheckObjectType.check_object (Type.create_context ()) (PDFObject.Bool true) (Util.type_array_tuples [| Util.make_type Type.String ; Util.make_type Type.Int |]) Key.Trailer "dummy")) ;
    ] ;

    "check_tuple" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (CheckObjectType.check_tuple (Type.create_context ()) [PDFObject.Int ~:123 ; PDFObject.String "foo" ; PDFObject.Name "bar"] [| Util.make_type Type.Int ; Util.make_type Type.String ; Util.make_type Type.Name |] Key.Trailer "")
                    ()) ;
      "(2)" >:: (fun _ -> assert_equal
                    (CheckObjectType.check_object (Type.create_context ()) (PDFObject.Array [PDFObject.String "def" ; PDFObject.Int ~:123]) (Util.type_tuple [| Util.make_type Type.String ; Util.make_type Type.Int |]) Key.Trailer "")
                    (Util.type_tuple [| Util.make_type Type.String ; Util.make_type Type.Int |])) ;

      "(3)" >:: (fun _ -> assert_raises
                    (Errors.TypeError ("Invalid type : expected string", Key.Trailer, "dummy[1]"))
                    (fun () -> CheckObjectType.check_tuple (Type.create_context ()) [PDFObject.Int ~:123 ; PDFObject.Name "bar"] [| Util.make_type Type.Int ; Util.make_type Type.String |] Key.Trailer "dummy")) ;
      "(4)" >:: (fun _ -> assert_raises
                    (Errors.TypeError ("Array size (1) is not the expected one (2) for this tuple", Key.Trailer, "dummy"))
                    (fun () -> CheckObjectType.check_tuple (Type.create_context ()) [PDFObject.Int ~:123] [| Util.make_type Type.Int ; Util.make_type Type.String |] Key.Trailer "dummy")) ;
      "(5)" >:: (fun _ -> assert_raises
                    (Errors.TypeError ("Invalid type : expected {{string, int}}", Key.Trailer, "dummy"))
                    (fun () -> CheckObjectType.check_object (Type.create_context ()) (PDFObject.Int ~:456) (Util.type_tuple [| Util.make_type Type.String ; Util.make_type Type.Int |]) Key.Trailer "dummy")) ;
    ] ;

    "check_variant" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (CheckObjectType.check_variant (Type.create_context ()) (PDFObject.Int ~:123) [Type.Int ; Type.String ; Type.Name] Key.Trailer "")
                    (Util.make_type Type.Int)) ;
      "(2)" >:: (fun _ -> assert_equal
                    (CheckObjectType.check_variant (Type.create_context ()) (PDFObject.String "foo") [Type.Int ; Type.String ; Type.Name] Key.Trailer "")
                    (Util.make_type Type.String)) ;
      "(3)" >:: (fun _ -> assert_equal
                    (CheckObjectType.check_object (Type.create_context ()) (PDFObject.Name "bar") (Util.type_variant [Type.Int ; Type.String ; Type.Name]) Key.Trailer "")
                    (Util.make_type Type.Name)) ;

      "(4)" >:: (fun _ -> assert_raises
                    (Errors.TypeError ("Invalid variant type : expected (int | string | name)", Key.Trailer, "dummy"))
                    (fun () -> CheckObjectType.check_variant (Type.create_context ()) (PDFObject.Bool true) [Type.Int ; Type.String ; Type.Name] Key.Trailer "dummy")) ;
      "(5)" >:: (fun _ -> assert_raises
                    (Errors.TypeError ("Invalid variant type : expected (string | int | name)", Key.Trailer, "dummy"))
                    (fun () -> CheckObjectType.check_object (Type.create_context ()) (PDFObject.Array []) (Util.type_variant [Type.String ; Type.Int ; Type.Name]) Key.Trailer "dummy")) ;
      (* TODO *)
    ] ;

    "check_indirect" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (CheckObjectType.check_indirect (Type.create_context ()) (Key.make_0 ~:1) (Util.make_type Type.Int) (Key.make_0 ~:1) "")
                    (Util.make_type Type.Int)) ;
      "(2)" >:: (fun _ -> assert_equal
                    (CheckObjectType.check_indirect (make_context_types 0 [Key.make_0 ~:1, Type.Int]) (Key.make_0 ~:1) (Util.make_type Type.Int) (Key.make_0 ~:1) "")
                    (Util.make_type Type.Int)) ;
      "(3)" >:: (fun _ -> assert_equal
                    (CheckObjectType.check_indirect (make_context_types 0 [Key.make_0 ~:1, Type.Variant [Type.Int ; Type.String]]) (Key.make_0 ~:1) (Util.make_type (Type.Variant [Type.Name ; Type.Int])) (Key.make_0 ~:1) "")
                    (Util.make_type Type.Int)) ;

      "(4)" >:: (fun _ -> assert_raises
                    (Errors.TypeError ("Inconsistent type inference between name and string", Key.make_0 ~:1, "dummy"))
                    (fun () -> CheckObjectType.check_indirect (make_context_types 0 [Key.make_0 ~:1, Type.String]) (Key.make_0 ~:1) (Util.make_type Type.Name) (Key.make_0 ~:1) "dummy")) ;
      "(5)" >:: (fun _ -> assert_raises
                    (Errors.TypeError ("Inconsistent type inference between (name | int) and (real | string)", Key.make_0 ~:1, "dummy"))
                    (fun () -> CheckObjectType.check_indirect (make_context_types 0 [Key.make_0 ~:1, Type.Variant [Type.Real ; Type.String]]) (Key.make_0 ~:1) (Util.make_type (Type.Variant [Type.Name ; Type.Int])) (Key.make_0 ~:1) "dummy")) ;
      (* TODO *)
    ] ;

    "check_class" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (CheckObjectType.check_class (TestTypeUtil.make_context 20) (TestDict.add_all ["foo", PDFObject.Int ~:123 ; "bar", PDFObject.String "test"]) "A" Key.Trailer "")
                    ()) ;
      "(2)" >:: (fun _ -> assert_equal
                    (CheckObjectType.check_class (TestTypeUtil.make_context 20) (TestDict.add_all ["foo", PDFObject.Int ~:456]) "A" Key.Trailer "")
                    ()) ;

      "(3)" >:: (fun _ -> assert_raises
                    (Errors.TypeError ("Mandatory entry /foo was not found in instance of class A", Key.Trailer, "dummy"))
                    (fun () -> CheckObjectType.check_class (TestTypeUtil.make_context 20) (TestDict.add_all ["bar", PDFObject.String "test"]) "A" Key.Trailer "dummy")) ;
      "(4)" >:: (fun _ -> assert_raises
                    (Errors.TypeError ("Invalid type : expected int", Key.Trailer, "dummy/foo"))
                    (fun () -> CheckObjectType.check_class (TestTypeUtil.make_context 20) (TestDict.add_all ["foo", PDFObject.Bool true]) "A" Key.Trailer "dummy")) ;
      "(5)" >:: (fun _ -> assert_raises
                    (Errors.TypeError ("Unexpected entry /key in instance of class A", Key.Trailer, "dummy"))
                    (fun () -> CheckObjectType.check_class (TestTypeUtil.make_context 20) (TestDict.add_all ["foo", PDFObject.Int ~:123 ; "key", PDFObject.String "text"]) "A" Key.Trailer "dummy")) ;
      (* TODO *)
    ] ;

    "check_subclass" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (CheckObjectType.check_subclass (TestTypeUtil.make_context 20) (TestDict.add_all ["foo", PDFObject.Int ~:123 ; "bar", PDFObject.String "text" ; "key", PDFObject.Name "test"]) "A" Key.Trailer "" (Hashtbl.create 1))
                    true) ;
      "(2)" >:: (fun _ -> assert_equal
                    (CheckObjectType.check_subclass (TestTypeUtil.make_context 20) (TestDict.add_all ["foo", PDFObject.Int ~:123 ; "key", PDFObject.Name "test"]) "A" Key.Trailer "" (Hashtbl.create 1))
                    true) ;
      "(3)" >:: (fun _ -> assert_equal
                    (CheckObjectType.check_subclass (TestTypeUtil.make_context 20) (TestDict.add_all ["foo", PDFObject.Int ~:123 ; "key", PDFObject.Name "test"]) "B" Key.Trailer "" (Hashtbl.create 1))
                    true) ;

      "(4)" >:: (fun _ -> assert_raises
                    (Errors.TypeError ("Mandatory entry /foo was not found in instance of class A", Key.Trailer, "dummy"))
                    (fun () -> CheckObjectType.check_subclass (TestTypeUtil.make_context 20) (TestDict.add_all ["bar", PDFObject.String "test"]) "A" Key.Trailer "dummy" (Hashtbl.create 1))) ;
      "(5)" >:: (fun _ -> assert_raises
                    (Errors.TypeError ("Mandatory entry /foo was not found in instance of class A", Key.Trailer, "dummy"))
                    (fun () -> CheckObjectType.check_subclass (TestTypeUtil.make_context 20) (TestDict.add_all ["key", PDFObject.Name "test"]) "B" Key.Trailer "dummy" (Hashtbl.create 1))) ;
      "(6)" >:: (fun _ -> assert_raises
                    (Errors.TypeError ("Invalid type : expected name", Key.Trailer, "dummy/key"))
                    (fun () -> CheckObjectType.check_subclass (TestTypeUtil.make_context 20) (TestDict.add_all ["foo", PDFObject.Int ~:123 ; "key", PDFObject.String "text"]) "B" Key.Trailer "dummy" (Hashtbl.create 1))) ;
      (* TODO *)
    ] ;
  ]

