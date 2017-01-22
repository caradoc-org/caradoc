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
open Type.Type
open Key
open Errors
open Boundedint
open Typechecker


let add_all l =
  let x = Hashtbl.create 16 in
  List.iter (fun y -> Hashtbl.add x y ()) l;
  x

let make_pool id =
  let pool = (Hashtbl.create 16, Hashtbl.create 16) in
  begin
    match id with
    (* Valid *)
    | 1 ->
      register_class pool "A" [] ;
    | 2 ->
      register_alias pool "X" Null ;
    | 3 ->
      register_class pool "A" [
        "b", Util.make_entry_type ~optional:false Int ;
      ] ;
    | 4 ->
      register_class pool "A" [
        "a", Util.entry_class ~optional:false "A" ;
      ] ;
    | 5 ->
      register_class pool "A" [
        "b", Util.entry_class ~optional:false "B" ;
      ] ;
      register_class pool "B" [
        "a", Util.entry_class ~optional:false "A" ;
      ] ;
    | 6 ->
      register_class pool "A" ~includes:["B"] [] ;
      register_class pool "B" [] ;
    | 10 ->
      register_alias pool "X" Int ;
    | 20 ->
      register_class pool "A" [
        "foo", Util.make_entry_type ~optional:false Int ;
        "bar", Util.make_entry_type ~optional:true String ;
      ] ;
      register_class pool "B" ~includes:[
        "A"
      ] [
        "key", Util.make_entry_type ~optional:true Name ;
      ] ;
      (* Invalid *)
    | -1 ->
      register_class pool "A" [
        "b", Util.entry_class ~optional:false "B" ;
      ] ;
    | -2 ->
      register_class pool "A" ~includes:["B"] [] ;
    | -3 ->
      register_class pool "A" [
        "b", Util.entry_stream ~optional:false "B" ;
      ] ;
      (* Empty *)
    | _ ->
      ()
  end;
  pool


let make_pool_aliases l =
  let pool = (Hashtbl.create 16, Hashtbl.create 16) in
  List.iter (fun (name, kind) ->
      register_alias pool name kind
    ) l;
  pool


let tests =
  "Type" >:::
  [
    "init" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (let (_:context) = TypeChecker.init () in ())
                    ()) ;
    ] ;

    "kind_to_string" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (kind_to_string (Alias "name"))
                    "a\"name\"") ;
      "(2)" >:: (fun _ -> assert_equal
                    (kind_to_string (Class "name"))
                    "c\"name\"") ;
      "(3)" >:: (fun _ -> assert_equal
                    (kind_to_string Null)
                    "null") ;
      "(4)" >:: (fun _ -> assert_equal
                    (kind_to_string Bool)
                    "bool") ;
      "(5)" >:: (fun _ -> assert_equal
                    (kind_to_string Int)
                    "int") ;
      "(6)" >:: (fun _ -> assert_equal
                    (kind_to_string Real)
                    "real") ;
      "(7)" >:: (fun _ -> assert_equal
                    (kind_to_string String)
                    "string") ;
      "(8)" >:: (fun _ -> assert_equal
                    (kind_to_string Name)
                    "name") ;
      "(9)" >:: (fun _ -> assert_equal
                    (kind_to_string (Stream "name"))
                    "s\"name\"") ;
      "(10)" >:: (fun _ -> assert_equal
                     (kind_to_string Text)
                     "text") ;
      "(11)" >:: (fun _ -> assert_equal
                     (kind_to_string Date)
                     "date") ;
      "(12)" >:: (fun _ -> assert_equal
                     (kind_to_string (BoolExact false))
                     "false") ;
      "(13)" >:: (fun _ -> assert_equal
                     (kind_to_string (BoolExact true))
                     "true") ;
      "(14)" >:: (fun _ -> assert_equal
                     (kind_to_string (IntRange (Some ~:0, Some ~:1)))
                     "(int >= 0 <= 1)") ;
      "(15)" >:: (fun _ -> assert_equal
                     (kind_to_string (IntRange (Some ~:0, None)))
                     "(int >= 0)") ;
      "(16)" >:: (fun _ -> assert_equal
                     (kind_to_string (IntRange (None, Some ~:1)))
                     "(int <= 1)") ;
      "(17)" >:: (fun _ -> assert_equal
                     (kind_to_string (IntRange (None, None)))
                     "(int)") ;
      "(18)" >:: (fun _ -> assert_equal
                     (kind_to_string (IntExact ~:567))
                     "(int = 567)") ;
      "(19)" >:: (fun _ -> assert_equal
                     (kind_to_string (IntIn [| ~:1 ; ~:2 ; ~:4 |]))
                     "(int = 1 | 2 | 4)") ;
      "(20)" >:: (fun _ -> assert_equal
                     (kind_to_string (NameExact "value"))
                     "(/value)") ;
      "(21)" >:: (fun _ -> assert_equal
                     (kind_to_string (NameIn (add_all ["a" ; "b" ; "c"])))
                     "(/a, /b, /c)") ;
      "(22)" >:: (fun _ -> assert_equal
                     (kind_to_string (Array (Util.make_type Int)))
                     "int[]") ;
      "(23)" >:: (fun _ -> assert_equal
                     (kind_to_string (ArrayOrOne (Util.make_type Int)))
                     "int[?]") ;
      "(24)" >:: (fun _ -> assert_equal
                     (kind_to_string (ArraySized (Util.make_type Int, 5)))
                     "int[5]") ;
      "(25)" >:: (fun _ -> assert_equal
                     (kind_to_string (ArrayVariantSized (Util.make_type Int, [| 1 ; 5 ; 7 |])))
                     "int[1, 5, 7]") ;
      "(26)" >:: (fun _ -> assert_equal
                     (kind_to_string (ArrayTuples [| Util.make_type Int ; Util.make_type Bool |]))
                     "{int, bool}[]") ;
      "(27)" >:: (fun _ -> assert_equal
                     (kind_to_string (Tuple [| Util.make_type String ; Util.make_type Name |]))
                     "{{string, name}}") ;
      "(28)" >:: (fun _ -> assert_equal
                     (kind_to_string (Variant [Int ; (Stream "name") ; Variant [Bool ; Null]]))
                     "(int | s\"name\" | (bool | null))") ;
      "(29)" >:: (fun _ -> assert_equal
                     (kind_to_string (Dictionary (Util.make_type Int)))
                     "int{}") ;
    ] ;

    "type_to_string" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (type_to_string (Util.make_type Int))
                    "int") ;
      "(2)" >:: (fun _ -> assert_equal
                    (type_to_string (Util.make_type ~allow_ind:false String))
                    "string*") ;
    ] ;

    "check_pool_type" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (check_pool_type (make_pool 0) "dummy" Null)
                    ()) ;
      "(2)" >:: (fun _ -> assert_equal
                    (check_pool_type (make_pool 0) "dummy" Bool)
                    ()) ;
      "(3)" >:: (fun _ -> assert_equal
                    (check_pool_type (make_pool 0) "dummy" Int)
                    ()) ;
      "(4)" >:: (fun _ -> assert_equal
                    (check_pool_type (make_pool 0) "dummy" Real)
                    ()) ;
      "(5)" >:: (fun _ -> assert_equal
                    (check_pool_type (make_pool 0) "dummy" String)
                    ()) ;
      "(6)" >:: (fun _ -> assert_equal
                    (check_pool_type (make_pool 0) "dummy" Name)
                    ()) ;
      "(7)" >:: (fun _ -> assert_equal
                    (check_pool_type (make_pool 0) "dummy" Text)
                    ()) ;
      "(8)" >:: (fun _ -> assert_equal
                    (check_pool_type (make_pool 0) "dummy" Date)
                    ()) ;
      "(9)" >:: (fun _ -> assert_equal
                    (check_pool_type (make_pool 0) "dummy" (BoolExact false))
                    ()) ;
      "(10)" >:: (fun _ -> assert_equal
                     (check_pool_type (make_pool 0) "dummy" (IntRange (Some ~:2, None)))
                     ()) ;
      "(11)" >:: (fun _ -> assert_equal
                     (check_pool_type (make_pool 0) "dummy" (IntExact ~:123))
                     ()) ;
      "(12)" >:: (fun _ -> assert_equal
                     (check_pool_type (make_pool 0) "dummy" (IntIn [| ~:2 ; ~:3 ; ~:5 |]))
                     ()) ;
      "(13)" >:: (fun _ -> assert_equal
                     (check_pool_type (make_pool 0) "dummy" (NameExact "key"))
                     ()) ;
      "(14)" >:: (fun _ -> assert_equal
                     (check_pool_type (make_pool 0) "dummy" (NameIn (add_all ["a" ; "b" ; "c"])))
                     ()) ;

      "(15)" >:: (fun _ -> assert_equal
                     (check_pool_type (make_pool 1) "dummy" (Class "A"))
                     ()) ;
      "(16)" >:: (fun _ -> assert_equal
                     (check_pool_type (make_pool 1) "dummy" (Stream "A"))
                     ()) ;
      "(17)" >:: (fun _ -> assert_equal
                     (check_pool_type (make_pool 2) "dummy" (Alias "X"))
                     ()) ;
      "(18)" >:: (fun _ -> assert_equal
                     (check_pool_type (make_pool 0) "dummy" (Array (Util.make_type Int)))
                     ()) ;
      "(19)" >:: (fun _ -> assert_equal
                     (check_pool_type (make_pool 1) "dummy" (Array (Util.type_class "A")))
                     ()) ;
      "(20)" >:: (fun _ -> assert_equal
                     (check_pool_type (make_pool 0) "dummy" (ArrayOrOne (Util.make_type String)))
                     ()) ;
      "(21)" >:: (fun _ -> assert_equal
                     (check_pool_type (make_pool 2) "dummy" (ArrayOrOne (Util.type_alias "X")))
                     ()) ;
      "(22)" >:: (fun _ -> assert_equal
                     (check_pool_type (make_pool 0) "dummy" (ArraySized (Util.make_type Name, 7)))
                     ()) ;
      "(23)" >:: (fun _ -> assert_equal
                     (check_pool_type (make_pool 1) "dummy" (ArraySized (Util.type_stream "A", 5)))
                     ()) ;
      "(24)" >:: (fun _ -> assert_equal
                     (check_pool_type (make_pool 0) "dummy" (ArrayVariantSized (Util.make_type Date, [| 2 ; 3 ; 5 |])))
                     ()) ;
      "(25)" >:: (fun _ -> assert_equal
                     (check_pool_type (make_pool 2) "dummy" (ArrayVariantSized (Util.type_alias "X", [| 4 ; 7 |])))
                     ()) ;
      "(26)" >:: (fun _ -> assert_equal
                     (check_pool_type (make_pool 0) "dummy" (ArrayTuples [| Util.make_type Date ; Util.make_type Text ; Util.make_type Real |]))
                     ()) ;
      "(27)" >:: (fun _ -> assert_equal
                     (check_pool_type (make_pool 1) "dummy" (ArrayTuples [| Util.make_type Date ; Util.type_class "A" |]))
                     ()) ;
      "(28)" >:: (fun _ -> assert_equal
                     (check_pool_type (make_pool 0) "dummy" (Tuple [| Util.make_type Date ; Util.make_type Text ; Util.make_type Real |]))
                     ()) ;
      "(29)" >:: (fun _ -> assert_equal
                     (check_pool_type (make_pool 2) "dummy" (Tuple [| Util.type_alias "X" ; Util.make_type Date |]))
                     ()) ;
      "(30)" >:: (fun _ -> assert_equal
                     (check_pool_type (make_pool 0) "dummy" (Variant [Date ; Text ; Real]))
                     ()) ;
      "(31)" >:: (fun _ -> assert_equal
                     (check_pool_type (make_pool 1) "dummy" (Variant [Class "A" ; Date]))
                     ()) ;

      "(32)" >:: (fun _ -> assert_raises
                     (Errors.UnexpectedError "Undeclared class type \"A\", used in \"dummy\"")
                     (fun () -> check_pool_type (make_pool 0) "dummy" (Class "A"))) ;
      "(33)" >:: (fun _ -> assert_raises
                     (Errors.UnexpectedError "Undeclared class type \"A\", used in \"dummy\"")
                     (fun () -> check_pool_type (make_pool 0) "dummy" (Stream "A"))) ;
      "(34)" >:: (fun _ -> assert_raises
                     (Errors.UnexpectedError "Undeclared alias type \"X\", used in \"dummy\"")
                     (fun () -> check_pool_type (make_pool 0) "dummy" (Alias "X"))) ;
      "(35)" >:: (fun _ -> assert_raises
                     (Errors.UnexpectedError "Undeclared class type \"A\", used in \"dummy[]\"")
                     (fun () -> check_pool_type (make_pool 0) "dummy" (Array (Util.type_class "A")))) ;
      "(36)" >:: (fun _ -> assert_raises
                     (Errors.UnexpectedError "Undeclared alias type \"X\", used in \"dummy[?]\"")
                     (fun () -> check_pool_type (make_pool 0) "dummy" (ArrayOrOne (Util.type_alias "X")))) ;
      "(37)" >:: (fun _ -> assert_raises
                     (Errors.UnexpectedError "Undeclared class type \"A\", used in \"dummy[]\"")
                     (fun () -> check_pool_type (make_pool 0) "dummy" (ArraySized (Util.type_class "A", 8)))) ;
      "(38)" >:: (fun _ -> assert_raises
                     (Errors.UnexpectedError "Undeclared class type \"A\", used in \"dummy[]\"")
                     (fun () -> check_pool_type (make_pool 0) "dummy" (ArrayVariantSized (Util.type_class "A", [| 6 ; 9 |])))) ;
      "(39)" >:: (fun _ -> assert_raises
                     (Errors.UnexpectedError "Undeclared alias type \"X\", used in \"dummy[{1}]\"")
                     (fun () -> check_pool_type (make_pool 0) "dummy" (ArrayTuples [| Util.make_type Date ; Util.type_alias "X" |]))) ;
      "(40)" >:: (fun _ -> assert_raises
                     (Errors.UnexpectedError "Undeclared class type \"B\", used in \"dummy[{0}]\"")
                     (fun () -> check_pool_type (make_pool 1) "dummy" (ArrayTuples [| Util.type_stream "B" ; Util.type_class "A" |]))) ;
      "(41)" >:: (fun _ -> assert_raises
                     (Errors.UnexpectedError "Undeclared alias type \"Y\", used in \"dummy[{1}]\"")
                     (fun () -> check_pool_type (make_pool 2) "dummy" (ArrayTuples [| Util.type_alias "X" ; Util.type_alias "Y" |]))) ;
      "(42)" >:: (fun _ -> assert_raises
                     (Errors.UnexpectedError "Undeclared class type \"A\", used in \"dummy{0}\"")
                     (fun () -> check_pool_type (make_pool 0) "dummy" (Tuple [| Util.type_stream "A" ; Util.make_type Date |]))) ;
      "(43)" >:: (fun _ -> assert_raises
                     (Errors.UnexpectedError "Undeclared class type \"B\", used in \"dummy{1}\"")
                     (fun () -> check_pool_type (make_pool 1) "dummy" (Tuple [| Util.type_class "A" ; Util.type_class "B" |]))) ;
      "(44)" >:: (fun _ -> assert_raises
                     (Errors.UnexpectedError "Undeclared alias type \"Y\", used in \"dummy{0}\"")
                     (fun () -> check_pool_type (make_pool 2) "dummy" (Tuple [| Util.type_alias "Y" ; Util.type_alias "X" |]))) ;
      "(45)" >:: (fun _ -> assert_raises
                     (Errors.UnexpectedError "Undeclared alias type \"X\", used in \"dummy<0>\"")
                     (fun () -> check_pool_type (make_pool 0) "dummy" (Variant [Alias "X" ; Date]))) ;
      "(46)" >:: (fun _ -> assert_raises
                     (Errors.UnexpectedError "Undeclared alias type \"X\", used in \"dummy<1>\"")
                     (fun () -> check_pool_type (make_pool 1) "dummy" (Variant [Class "A" ; Alias "X"]))) ;

      "(47)" >:: (fun _ -> assert_raises
                     (Errors.UnexpectedError "Undeclared class type \"A\", used in \"dummy[]{}\"")
                     (fun () -> check_pool_type (make_pool 0) "dummy" (Array (Util.make_type (Dictionary (Util.type_class "A")))))) ;
      "(48)" >:: (fun _ -> assert_raises
                     (Errors.UnexpectedError "Undeclared class type \"A\", used in \"dummy[]{}\"")
                     (fun () -> check_pool_type (make_pool 0) "dummy" (Array (Util.make_type (Dictionary (Util.type_stream "A")))))) ;
    ] ;

    "check_pool" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (check_pool (make_pool 1))
                    ()) ;
      "(2)" >:: (fun _ -> assert_equal
                    (check_pool (make_pool 2))
                    ()) ;
      "(3)" >:: (fun _ -> assert_equal
                    (check_pool (make_pool 3))
                    ()) ;
      "(4)" >:: (fun _ -> assert_equal
                    (check_pool (make_pool 4))
                    ()) ;
      "(5)" >:: (fun _ -> assert_equal
                    (check_pool (make_pool 5))
                    ()) ;
      "(6)" >:: (fun _ -> assert_equal
                    (check_pool (make_pool 6))
                    ()) ;
      "(7)" >:: (fun _ -> assert_equal
                    (check_pool (make_pool 10))
                    ()) ;
      "(8)" >:: (fun _ -> assert_equal
                    (check_pool (make_pool 20))
                    ()) ;

      "(8)" >:: (fun _ -> assert_raises
                    (Errors.UnexpectedError "Undeclared class type \"B\", used in \"class A/b\"")
                    (fun () -> check_pool (make_pool (-1)))) ;
      "(9)" >:: (fun _ -> assert_raises
                    (Errors.UnexpectedError "Undeclared class type \"B\", included by class \"A\"")
                    (fun () -> check_pool (make_pool (-2)))) ;
      "(10)" >:: (fun _ -> assert_raises
                     (Errors.UnexpectedError "Undeclared class type \"B\", used in \"class A/b\"")
                     (fun () -> check_pool (make_pool (-3)))) ;
      (* TODO *)
    ] ;

    "remove_alias" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (remove_alias (make_pool_aliases ["A", Int]) (Alias "A"))
                    Int) ;
      "(2)" >:: (fun _ -> assert_equal
                    (remove_alias (make_pool_aliases ["A", Int ; "B", Alias "A"]) (Alias "B"))
                    Int) ;

      (* TODO : behavior ? *)
      "(3)" >:: (fun _ -> assert_equal
                    (remove_alias (make_pool_aliases ["A", Int ; "B", Tuple [| Util.make_type String ; Util.type_alias "A" |]]) (Alias "B"))
                    (Tuple [| Util.make_type String ; Util.type_alias "A" |])) ;
      (* TODO *)
    ] ;

    "remove_variant" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (remove_variant (make_pool 0) (Variant [String ; Int]))
                    [Int ; String]) ;
      "(2)" >:: (fun _ -> assert_equal
                    (remove_variant (make_pool 0) (Variant [Int ; Text ; Date ; Real ; Null ; String]))
                    [Null ; Int ; Real ; String ; Text ; Date]) ;

      "(3)" >:: (fun _ -> assert_equal
                    (remove_variant (make_pool 0) (Variant [Int]))
                    [Int]) ;
      "(4)" >:: (fun _ -> assert_equal
                    (remove_variant (make_pool 0) (Variant [Int ; String]))
                    (remove_variant (make_pool 0) (Variant [String ; Int]))) ;
      "(5)" >:: (fun _ -> assert_equal
                    (remove_variant (make_pool 0) (Variant [Int ; Text ; Date ; Real ; Null ; String]))
                    (remove_variant (make_pool 0) (Variant [Text ; Null ; Date ; String ; Int ; Real]))) ;
      "(6)" >:: (fun _ -> assert_equal
                    (remove_variant (make_pool 0) (Variant [Int ; Variant [String ; Text]]))
                    (remove_variant (make_pool 0) (Variant [Int ; String ; Text]))) ;
      "(7)" >:: (fun _ -> assert_equal
                    (remove_variant (make_pool 0) (Variant [Variant [Variant [Null ; Array (Util.make_type Int)]] ; Variant [String ; Text]]))
                    (remove_variant (make_pool 0) (Variant [Array (Util.make_type Int) ; Null ; String ; Text]))) ;
      "(8)" >:: (fun _ -> assert_equal
                    (remove_variant (make_pool 0) (Tuple [| Util.make_type Int ; Util.make_type (Variant [String ; Int]) |]))
                    [Tuple [| Util.make_type Int ; Util.make_type (Variant [String ; Int]) |] ]) ;
      "(9)" >:: (fun _ -> assert_equal
                    (remove_variant (make_pool_aliases ["A", Variant [Int ; String]]) (Variant [Alias "A" ; Text]))
                    (remove_variant (make_pool 0) (Variant [Int ; String ; Text]))) ;
      "(10)" >:: (fun _ -> assert_equal
                     (remove_variant (make_pool_aliases ["A", Variant [Int ; Alias "B"] ; "B", Variant [String ; Date]]) (Variant [Alias "A" ; Text]))
                     (remove_variant (make_pool 0) (Variant [Int ; String ; Text ; Date]))) ;
    ] ;

    "type_intersection" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
                    (type_intersection (make_pool 0) Bool Bool (Errors.make_ctxt_key Key.Trailer))
                    Bool) ;
      "(2)" >:: (fun _ -> assert_equal
                    (type_intersection (make_pool 0) (Variant [Int ; String]) (Variant [Name ; Int]) (Errors.make_ctxt_key Key.Trailer))
                    Int) ;
      "(3)" >:: (fun _ -> assert_equal
                    (type_intersection (make_pool 0) (Variant [Int ; Name ; String]) Name (Errors.make_ctxt_key Key.Trailer))
                    Name) ;
      "(4)" >:: (fun _ -> assert_equal
                    (type_intersection (make_pool 0) (Variant [Int ; Variant [Name ; String]]) (Variant [Variant [Null ; Name] ; Bool]) (Errors.make_ctxt_key Key.Trailer))
                    Name) ;
      "(5)" >:: (fun _ -> assert_equal
                    (remove_variant (make_pool 0)
                       (type_intersection (make_pool_aliases ["A", Variant [Name ; String] ; "B", Variant [Null ; Name] ; "C", Variant [String ; Real]])
                          (Variant [Int ; Alias "A" ; Real]) (Variant [Alias "B" ; Alias "C" ; Bool]) (Errors.make_ctxt_key Key.Trailer)))
                    (remove_variant (make_pool 0) (Variant [String ; Real ; Name]))) ;
      "(6)" >:: (fun _ -> assert_equal
                    (type_intersection (make_pool_aliases ["A", Bool ; "B", Bool]) (Alias "A") (Alias "B") (Errors.make_ctxt_key Key.Trailer))
                    Bool) ;

      "(7)" >:: (fun _ -> assert_raises
                    (Errors.TypeError ("Inconsistent type inference between bool and int", Errors.make_ctxt_name Key.Trailer "dummy"))
                    (fun () -> type_intersection (make_pool 0) Bool Int (Errors.make_ctxt_name Key.Trailer "dummy"))) ;
      "(8)" >:: (fun _ -> assert_raises
                    (Errors.TypeError ("Inconsistent type inference between a\"A\" and a\"B\"", Errors.make_ctxt_name Key.Trailer "dummy"))
                    (fun () -> type_intersection (make_pool_aliases ["A", Bool ; "B", Int]) (Alias "A") (Alias "B") (Errors.make_ctxt_name Key.Trailer "dummy"))) ;
      (* TODO : behavior ? *)
      "(9)" >:: (fun _ -> assert_raises
                    (Errors.TypeError ("Inconsistent type inference between {{a\"A\", int}} and {{bool, a\"B\"}}", Errors.make_ctxt_name Key.Trailer "dummy"))
                    (fun () -> type_intersection (make_pool_aliases ["A", Bool ; "B", Int])
                        (Tuple [| Util.type_alias "A" ; Util.make_type Int |]) (Tuple [| Util.make_type Bool ; Util.type_alias "B" |]) (Errors.make_ctxt_name Key.Trailer "dummy"))) ;
      (* TODO *)
    ] ;
  ]

