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


open Type.Type
open Util


let register_font ctxt =

  (*************************)
  (* PDF reference 9.5-9.7 *)
  (*************************)
  register_alias ctxt.pool "font" (Variant [
      Alias "font_type1" ;
      Class "font_mmtype1" ;
      Class "font_truetype" ;
      Class "font_type3" ;

      (* TODO : remove *)
      Class "font_base" ;
    ]);
  register_alias ctxt.pool "font_type1" (Variant [
      Class "font_type1_standard" ;
      Class "font_type1_other" ;
    ]);

  register_class ~strict:false ctxt.pool "font_base" [
    "Type", entry_name_exact ~allow_ind:false ~optional:false "Font" ;
  ];

  register_class ctxt.pool "simple_font" [
    "BaseFont", make_entry_type ~optional:false Name ;

    "FirstChar", entry_alias ~optional:false "intnonnegative" ;
    "LastChar", entry_alias ~optional:false "intnonnegative" ;
    "Widths", entry_array ~optional:false (type_alias "numnonnegative") ;
    "FontDescriptor", entry_alias ~optional:false "font_descriptor" ;
    "Encoding", entry_alias ~optional:true "char_encoding" ;
    "ToUnicode", entry_alias ~optional:true "cmap_unicode" ;
  ];

  (*************************)
  (* PDF reference 9.6.2.2 *)
  (*************************)
  register_class ctxt.pool "font_type1_base" ~includes:[
    "font_base" ;
  ] [
    "Subtype", entry_name_exact ~allow_ind:false ~optional:false "Type1" ;

    (* TODO : required for PDF 1.0 *)
    "Name", make_entry_type ~optional:true Name ;
  ];

  register_class ctxt.pool "font_type1_standard" ~includes:[
    "font_type1_base" ;
  ] [
    "BaseFont", entry_name_in ~optional:false ["Times-Roman" ; "Helvetica" ; "Courier" ; "Symbol" ; "Times-Bold" ; "Helvetica-Bold" ; "Courier-Bold" ; "ZapfDingbats" ; "Times-Italic" ; "Helvetica-Oblique" ; "Courier-Oblique" ; "Times-BoldItalic" ; "Helvetica-BoldOblique" ; "Courier-BoldOblique"] ;

    "Encoding", entry_alias ~optional:true "char_encoding" ;
    "ToUnicode", entry_alias ~optional:true "cmap_unicode" ;
  ];

  register_class ctxt.pool "font_type1_other" ~includes:[
    "font_type1_base" ;
    "simple_font" ;
  ] [
  ];

  (*************************)
  (* PDF reference 9.6.2.3 *)
  (*************************)
  register_class ctxt.pool "font_mmtype1" ~includes:[
    "font_base" ;
    "simple_font" ;
  ] [
    "Subtype", entry_name_exact ~allow_ind:false ~optional:false "MMType1" ;

    (* TODO : required for PDF 1.0 *)
    "Name", make_entry_type ~optional:true Name ;
  ];

  (***********************)
  (* PDF reference 9.6.3 *)
  (***********************)
  register_class ctxt.pool "font_truetype" ~includes:[
    "font_base" ;
    "simple_font" ;
  ] [
    "Subtype", entry_name_exact ~allow_ind:false ~optional:false "TrueType" ;

    (* TODO : required for PDF 1.0 *)
    "Name", make_entry_type ~optional:true Name ;
  ];

  (***********************)
  (* PDF reference 9.6.5 *)
  (***********************)
  register_class ctxt.pool "font_type3" ~includes:[
    "font_base" ;
  ] [
    "Subtype", entry_name_exact ~allow_ind:false ~optional:false "Type3" ;

    (* TODO : required for PDF 1.0 *)
    "Name", make_entry_type ~optional:true Name ;

    "FontBBox", entry_alias ~optional:false "rectangle" ;
    "FontMatrix", entry_alias ~optional:false "matrix6" ;
    "CharProcs", entry_alias ~optional:false "char_procs" ;
    "Encoding", entry_alias ~optional:false "char_encoding" ;
    "FirstChar", entry_alias ~optional:false "intnonnegative" ;
    "LastChar", entry_alias ~optional:false "intnonnegative" ;
    "Widths", entry_array ~optional:false (type_alias "numnonnegative") ;

    "FontDescriptor", entry_alias ~optional:true "font_descriptor" ;
    "Resources", entry_class ~optional:true "resources" ;
    "ToUnicode", entry_alias ~optional:true "cmap_unicode" ;
  ];

  (* TODO *)
  register_class ctxt.pool "content_stream_font3" ~includes:[
    "content_stream"
  ] [
  ];

  register_alias ctxt.pool "char_procs" (Dictionary (type_stream "content_stream_font3"));

  (***********************)
  (* PDF reference 9.6.6 *)
  (***********************)
  register_alias ctxt.pool "char_encoding" (Variant [
      Alias "encoding_base" ;
      Class "encoding_dict" ;
    ]);

  (*
  register_alias ctxt.pool "encoding_base" (kind_name_in ["StandardEncoding" ; "MacRomanEncoding" ; "WinAnsiEncoding" ; "PDFDocEncoding"]);
  *)
  register_alias ctxt.pool "encoding_base" (kind_name_in ["MacRomanEncoding" ; "MacExpertEncoding" ; "WinAnsiEncoding"]);

  register_class ctxt.pool "encoding_dict" [
    "Type", entry_name_exact ~allow_ind:false ~optional:true "Encoding" ;

    "BaseEncoding", entry_alias ~optional:true "encoding_base" ;
    "Differences", entry_alias ~optional:true "differences" ;
  ];

  (* TODO *)
  register_alias ctxt.pool "differences" ArrayDifferences;

  (*********************)
  (* PDF reference 9.8 *)
  (*********************)
  register_alias ctxt.pool "font_descriptor" (Variant [
      Class "font_descriptor_base" ;
      Class "font_descriptor_file1" ;
      Class "font_descriptor_file2" ;
      Class "font_descriptor_file3" ;
    ]);

  register_class ctxt.pool "font_descriptor_base" [
    "Type", entry_name_exact ~allow_ind:false ~optional:false "FontDescriptor" ;

    "FontName", make_entry_type ~optional:false Name ;

    "FontFamily", make_entry_type ~optional:true String ;
    "FontStretch", entry_name_in ~optional:true ["UltraCondensed" ; "ExtraCondensed" ; "Condensed" ; "SemiCondensed" ; "Normal" ; "SemiExpanded" ; "Expanded" ; "ExtraExpanded" ; "UltraExpanded"] ;
    (* TODO : set of values *)
    "FontWeight", entry_alias ~optional:true "number" ;

    (* TODO : set of bits *)
    "Flags", entry_flags ~optional:false 19 ;
    "FontBBox", entry_alias ~optional:false "rectangle" ;
    "ItalicAngle", entry_alias ~optional:false "number" ;
    "Ascent", entry_alias ~optional:false "number" ;
    "Descent", entry_alias ~optional:false "number" ;

    "Leading", entry_alias ~optional:true "number" ;
    "CapHeight", entry_alias ~optional:true "number" ;
    "XHeight", entry_alias ~optional:true "number" ;
    "StemV", entry_alias ~optional:true "number" ;
    "StemH", entry_alias ~optional:true "number" ;
    "AvgWidth", entry_alias ~optional:true "number" ;
    "MaxWidth", entry_alias ~optional:true "number" ;
    "MissingWidth", entry_alias ~optional:true "number" ;
    "CharSet", make_entry_type ~optional:true String ;
  ];

  register_class ctxt.pool "font_descriptor_file1" ~includes:[
    "font_descriptor_base" ;
  ] [
    "FontFile", entry_stream ~optional:false "font_file1" ;
  ];
  register_class ctxt.pool "font_descriptor_file2" ~includes:[
    "font_descriptor_base" ;
  ] [
    "FontFile2", entry_stream ~optional:false "font_file2" ;
  ];
  register_class ctxt.pool "font_descriptor_file3" ~includes:[
    "font_descriptor_base" ;
  ] [
    "FontFile3", entry_stream ~optional:false "font_file3" ;
  ];

  register_class ctxt.pool "font_file_base" ~includes:[
    "stream_base" ;
  ] [
    "Metadata", entry_stream ~optional:true "metadata_stream" ;
  ];
  register_class ctxt.pool "font_file1" ~includes:[
    "font_file_base" ;
  ] [
    "Length1", entry_alias ~optional:false "intnonnegative" ;
    "Length2", entry_alias ~optional:false "intnonnegative" ;
    "Length3", entry_alias ~optional:false "intnonnegative" ;
  ];
  register_class ctxt.pool "font_file2" ~includes:[
    "font_file_base" ;
  ] [
    "Length1", entry_alias ~optional:false "intnonnegative" ;
  ];
  register_class ctxt.pool "font_file3" ~includes:[
    "font_file_base" ;
  ] [
    "Subtype", entry_name_in ~optional:false ["Type1C" ; "CIDFontType0C" ; "OpenType"] ;
  ];

  (* TODO *)
  register_alias ctxt.pool "cmap_unicode" (Stream "stream_base");

