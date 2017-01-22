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


let tests =
  "PDF" >:::
  [
    (* crypto/ *)
    TestAES.tests;
    TestMD5.tests;
    TestRC4.tests;

    (* data/ *)
    TestDocument.tests;
    TestDict.tests;
    TestDirectObject.tests;
    TestIndirectObject.tests;

    (* parse/ *)
    TestCommon.tests;
    TestConvert.tests;
    TestLexer.tests;
    TestParser.tests;
    TestStrictLexer.tests;
    TestXrefLexer.tests;
    TestXrefParser.tests;

    (* util/ *)
    TestAlgo.tests;
    TestBoundedint.tests;
    TestErrors.tests;
    TestKey.tests;

    (* TODO : finish *)
    (* crypto/ *)
    TestCryptoParse.tests;

    (* data/ *)
    TestGraph.tests;
    TestIntervals.tests;
    TestXref.tests;

    (* parse/ *)
    TestStrictParser.tests;

    (* stream/ *)
    TestStream.tests;

    (* type/ *)
    TestType.tests;

    (* util/ *)
    TestEntry.tests;

    (* TODO *)
    (* graph/ *)
    TestTree.tests;

    (* type/ *)
    TestCheckType.tests;
    TestTypeUtil.tests;
  ]

let (_:OUnit.test_results) =
  run_test_tt_main tests

