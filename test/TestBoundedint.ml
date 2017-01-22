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
open Boundedint


let tests =
  "BoundedInt" >:::
  [
    "to_int" >:::
    [
      "(1)" >:: (fun _ -> assert_equal (BoundedInt.to_int ~:4) 4) ;
    ] ;

    "to_string" >:::
    [
      "(1)" >:: (fun _ -> assert_equal (BoundedInt.to_string ~:0) "0") ;
      "(2)" >:: (fun _ -> assert_equal (BoundedInt.to_string ~:123456789) "123456789") ;
      "(3)" >:: (fun _ -> assert_equal (BoundedInt.to_string ~:(-987)) "-987") ;

      "(4)" >:: (fun _ -> assert_equal (BoundedInt.to_string (BoundedInt.of_int64 2147483647L)) "2147483647") ;
    ] ;

    "int_of_string" >:::
    [
      "(1)" >:: (fun _ -> assert_equal (BoundedInt.int_of_string "-7") ~:(-7)) ;
      "(2)" >:: (fun _ -> assert_equal (BoundedInt.int_of_string "2147483647") (BoundedInt.of_int64 2147483647L)) ;
      "(3)" >:: (fun _ -> assert_equal (BoundedInt.int_of_string "-2147483647") (BoundedInt.of_int64 (-2147483647L))) ;
      "(4)" >:: (fun _ -> assert_equal (BoundedInt.int_of_string "-2147483648") (BoundedInt.of_int64 (-2147483648L))) ;

      "(5)" >:: (fun _ -> assert_raises (BoundedInt.IntegerError "integer overflow")
                    (fun () -> BoundedInt.int_of_string "-2147483649")) ;
      "(6)" >:: (fun _ -> assert_raises (BoundedInt.IntegerError "integer overflow")
                    (fun () -> BoundedInt.int_of_string "2147483648")) ;
      "(7)" >:: (fun _ -> assert_raises (BoundedInt.IntegerError "integer overflow")
                    (fun () -> BoundedInt.int_of_string "18446744073709551616")) ;
      "(8)" >:: (fun _ -> assert_raises (BoundedInt.IntegerError "string contains a non-digit character")
                    (fun () -> BoundedInt.int_of_string "blabla")) ;
    ] ;

    "uint_of_string" >:::
    [
      "(1)" >:: (fun _ -> assert_equal (BoundedInt.uint_of_string "2147483647") (BoundedInt.of_int64 2147483647L)) ;

      "(2)" >:: (fun _ -> assert_raises (BoundedInt.IntegerError "string contains a non-digit character")
                    (fun () -> BoundedInt.uint_of_string "-7")) ;
      "(3)" >:: (fun _ -> assert_raises (BoundedInt.IntegerError "string contains a non-digit character")
                    (fun () -> BoundedInt.uint_of_string "-2147483647")) ;
      "(4)" >:: (fun _ -> assert_raises (BoundedInt.IntegerError "string contains a non-digit character")
                    (fun () -> BoundedInt.uint_of_string "-2147483648")) ;
      "(5)" >:: (fun _ -> assert_raises (BoundedInt.IntegerError "integer overflow")
                    (fun () -> BoundedInt.uint_of_string "2147483648")) ;
      "(6)" >:: (fun _ -> assert_raises (BoundedInt.IntegerError "integer overflow")
                    (fun () -> BoundedInt.uint_of_string "18446744073709551616")) ;
      "(7)" >:: (fun _ -> assert_raises (BoundedInt.IntegerError "string contains a non-digit character")
                    (fun () -> BoundedInt.uint_of_string "blabla")) ;
    ] ;

    "compare" >:::
    [
      "(1)" >:: (fun _ -> assert_equal (BoundedInt.compare (BoundedInt.of_int64 (-2147483647L)) (BoundedInt.of_int64 2147483647L)) ~cmp:(<) 0) ;
      "(2)" >:: (fun _ -> assert_equal (BoundedInt.compare (BoundedInt.of_int64 2147483647L) (BoundedInt.of_int64 2147483647L)) 0) ;
      "(3)" >:: (fun _ -> assert_equal (BoundedInt.compare ~:3 ~:5) ~cmp:(<) 0) ;
      "(4)" >:: (fun _ -> assert_equal (BoundedInt.compare ~:7 ~:4) ~cmp:(>) 0) ;
    ] ;

    "rem" >:::
    [
      "(1)" >:: (fun _ -> assert_equal (BoundedInt.rem ~:7 ~:5) ~:2) ;
      "(2)" >:: (fun _ -> assert_equal (BoundedInt.rem ~:7 ~:(-5)) ~:2) ;
      "(3)" >:: (fun _ -> assert_equal (BoundedInt.rem ~:(-7) ~:5) ~:(-2)) ;

      "(4)" >:: (fun _ -> assert_raises Division_by_zero
                    (fun () -> BoundedInt.rem ~:7 ~:0)) ;
    ] ;

    "add" >:::
    [
      "(1)" >:: (fun _ -> assert_equal (~:123 +: ~:456) ~:579) ;
      "(2)" >:: (fun _ -> assert_equal (~:1073741824 +: ~:1073741823) (BoundedInt.of_int64 2147483647L)) ;

      "(3)" >:: (fun _ -> assert_raises (BoundedInt.IntegerError "integer overflow")
                    (fun () -> (BoundedInt.of_int64 2147483647L) +: (BoundedInt.of_int64 2147483647L))) ;
    ] ;

    "sub" >:::
    [
      "(1)" >:: (fun _ -> assert_equal (~:123 -: ~:456) ~:(-333)) ;
      "(2)" >:: (fun _ -> assert_equal (~:(-1073741824) -: ~:1073741823) (BoundedInt.of_int64 (-2147483647L))) ;
      "(3)" >:: (fun _ -> assert_equal (~:(-1073741824) -: ~:1073741824) (BoundedInt.of_int64 (-2147483648L))) ;

      "(4)" >:: (fun _ -> assert_raises (BoundedInt.IntegerError "integer overflow")
                    (fun () -> (BoundedInt.of_int64 (-2147483648L)) -: ~:1)) ;
    ] ;

    "mul" >:::
    [
      "(1)" >:: (fun _ -> assert_equal (~:123 *: ~:456) ~:(123 * 456)) ;
      "(2)" >:: (fun _ -> assert_equal (~:65536 *: ~:(-32768)) (BoundedInt.of_int64 (-2147483648L))) ;

      "(3)" >:: (fun _ -> assert_raises (BoundedInt.IntegerError "integer overflow")
                    (fun () -> ~:65536 *: ~:65536)) ;
    ] ;

    "div" >:::
    [
      "(1)" >:: (fun _ -> assert_equal (~:7 /: ~:5) ~:1) ;
      "(2)" >:: (fun _ -> assert_equal (~:7 /: ~:(-5)) ~:(-1)) ;
      "(3)" >:: (fun _ -> assert_equal (~:(-7) /: ~:5) ~:(-1)) ;

      "(4)" >:: (fun _ -> assert_raises Division_by_zero
                    (fun () -> ~:7 /: ~:0)) ;
    ] ;

    "shift" >:::
    [
      "(1)" >:: (fun _ -> assert_equal (~:1 <<: 5) ~:32) ;
      "(2)" >:: (fun _ -> assert_equal (~:1 <<: 30) ~:1073741824) ;
      "(3)" >:: (fun _ -> assert_equal (~:(-1) <<: 3) ~:(-8)) ;

      "(4)" >:: (fun _ -> assert_raises (BoundedInt.IntegerError "integer overflow")
                    (fun () -> ~:1 <<: 31)) ;
      "(5)" >:: (fun _ -> assert_raises (BoundedInt.IntegerError "integer overflow")
                    (fun () -> ~:1 <<: (-1))) ;
    ] ;
  ]

