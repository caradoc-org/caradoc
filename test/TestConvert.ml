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
open Convert
open Boundedint


let tests =
  "Convert" >:::
  [
    "int_of_bytes" >:::
    [
      "(1)" >:: (fun _ -> assert_equal (int_of_bytes "\x42" ~:0 ~:1 ()) ~:66) ;
      "(2)" >:: (fun _ -> assert_equal (int_of_bytes "\x01\x00" ~:0 ~:2 ()) ~:256) ;
      "(3)" >:: (fun _ -> assert_equal (int_of_bytes "\x00\x01" ~:0 ~:2 ()) ~:1) ;
      "(4)" >:: (fun _ -> assert_equal (int_of_bytes "\x01\x00" ~:0 ~:2 ~default:(~:123) ()) ~:256) ;
      "(5)" >:: (fun _ -> assert_equal (int_of_bytes "" ~:0 ~:0 ~default:(~:123) ()) ~:123) ;

      "(6)" >:: (fun _ -> assert_raises (ConvertError "Binary integer has length zero") (fun () -> int_of_bytes "" ~:0 ~:0 ())) ;
    ] ;

    "int_of_octal_digit" >:::
    [
      "(1)" >:: (fun _ -> assert_equal (int_of_octal_digit '0') 0) ;
      "(2)" >:: (fun _ -> assert_equal (int_of_octal_digit '5') 5) ;
      "(3)" >:: (fun _ -> assert_equal (int_of_octal_digit '7') 7) ;

      "(4)" >:: (fun _ -> assert_raises (ConvertError "Not an octal digit") (fun () -> int_of_octal_digit '8')) ;
      "(5)" >:: (fun _ -> assert_raises (ConvertError "Not an octal digit") (fun () -> int_of_octal_digit '%')) ;
      "(6)" >:: (fun _ -> assert_raises (ConvertError "Not an octal digit") (fun () -> int_of_octal_digit '\x00')) ;
    ] ;

    "int_of_octal" >:::
    [
      "(1)" >:: (fun _ -> assert_equal (int_of_octal '1' '2' '3') 83) ;
      "(2)" >:: (fun _ -> assert_equal (int_of_octal '6' '2' '3') 403) ;

      "(3)" >:: (fun _ -> assert_raises (ConvertError "Not an octal digit") (fun () -> int_of_octal '1' '8' '3')) ;
    ] ;

    "char_of_octal3" >:::
    [
      "(1)" >:: (fun _ -> assert_equal (char_of_octal3 '1' '2' '3') '\x53') ;

      "(2)" >:: (fun _ -> assert_raises (ConvertError "Octal character is out of bounds") (fun () -> char_of_octal3 '6' '2' '3')) ;
    ] ;

    "char_of_octal2" >:::
    [
      "(1)" >:: (fun _ -> assert_equal (char_of_octal2 '1' '2') '\x0A') ;
    ] ;

    "char_of_octal1" >:::
    [
      "(1)" >:: (fun _ -> assert_equal (char_of_octal1 '7') '\x07') ;
    ] ;

    "int_of_hexa_digit" >:::
    [
      "(1)" >:: (fun _ -> assert_equal (int_of_hexa_digit '0') 0) ;
      "(2)" >:: (fun _ -> assert_equal (int_of_hexa_digit '5') 5) ;
      "(3)" >:: (fun _ -> assert_equal (int_of_hexa_digit '8') 8) ;
      "(4)" >:: (fun _ -> assert_equal (int_of_hexa_digit 'b') 11) ;
      "(5)" >:: (fun _ -> assert_equal (int_of_hexa_digit 'F') 15) ;

      "(6)" >:: (fun _ -> assert_raises (ConvertError "Not a hexadecimal digit") (fun () -> int_of_hexa_digit 't')) ;
      "(7)" >:: (fun _ -> assert_raises (ConvertError "Not a hexadecimal digit") (fun () -> int_of_hexa_digit '%')) ;
      "(8)" >:: (fun _ -> assert_raises (ConvertError "Not a hexadecimal digit") (fun () -> int_of_hexa_digit '\x00')) ;
    ] ;

    "int_of_hexa" >:::
    [
      "(1)" >:: (fun _ -> assert_equal (int_of_hexa '1' '2') 18) ;
      "(2)" >:: (fun _ -> assert_equal (int_of_hexa 'a' 'F') 175) ;

      "(3)" >:: (fun _ -> assert_raises (ConvertError "Not a hexadecimal digit") (fun () -> int_of_hexa '1' 'g')) ;
    ] ;

    "char_of_hexa" >:::
    [
      "(1)" >:: (fun _ -> assert_equal (char_of_hexa '4' '5') '\x45') ;
      "(2)" >:: (fun _ -> assert_equal (char_of_hexa 'A' 'c') '\xAC') ;

      "(3)" >:: (fun _ -> assert_raises (ConvertError "Not a hexadecimal digit") (fun () -> char_of_hexa '1' 'g')) ;
    ] ;

    "hexa_digit_of_int" >:::
    [
      "(1)" >:: (fun _ -> assert_equal (hexa_digit_of_int 0) '0') ;
      "(2)" >:: (fun _ -> assert_equal (hexa_digit_of_int 5) '5') ;
      "(3)" >:: (fun _ -> assert_equal (hexa_digit_of_int 12) 'C') ;
      "(4)" >:: (fun _ -> assert_equal (hexa_digit_of_int 15) 'F') ;

      "(5)" >:: (fun _ -> assert_raises (ConvertError "Hexadecimal digit is out of bounds") (fun () -> hexa_digit_of_int (-1))) ;
      "(6)" >:: (fun _ -> assert_raises (ConvertError "Hexadecimal digit is out of bounds") (fun () -> hexa_digit_of_int 16)) ;
    ] ;

    "hexa_of_char" >:::
    [
      "(1)" >:: (fun _ -> assert_equal (hexa_of_char '\x00') "00") ;
      "(2)" >:: (fun _ -> assert_equal (hexa_of_char '\x13') "13") ;
      "(3)" >:: (fun _ -> assert_equal (hexa_of_char '\xFD') "FD") ;
    ] ;
  ]

