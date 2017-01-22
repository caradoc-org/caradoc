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
open Boundedint

open Typesaction
open Typesannot
open Typescolorspace
open Typesfont
open Typesfunction
open Typesgraphic
open Typesoutline
open Typespage
open Typesstream
open Typestransition
open Typestree
open Typesxobject


let register_util ctxt =
  register_alias ctxt.pool "intpositive" (IntRange (Some ~:1, None));
  register_alias ctxt.pool "intnonnegative" (IntRange (Some ~:0, None));
  register_alias ctxt.pool "number" (Variant [Int ; Real]);
  (* TODO : check real numbers *)
  register_alias ctxt.pool "numpositive" (Variant [IntRange (Some ~:1, None) ; Real]);
  (* TODO : check real numbers *)
  register_alias ctxt.pool "numnonnegative" (Variant [IntRange (Some ~:0, None) ; Real]);
  (* TODO : check real numbers *)
  register_alias ctxt.pool "numnonzero" (Variant [IntRange (None, Some ~:(-1)) ; IntRange (Some ~:1, None) ; Real]);
  (* TODO : check real numbers *)
  register_alias ctxt.pool "numone" (Variant [IntExact ~:1 ; Real]);
  register_alias ctxt.pool "numornull" (Variant [Null ; Alias "number"]);
  (* TODO : check real numbers *)
  register_alias ctxt.pool "num01" (Variant [IntRange (Some ~:0, Some ~:1) ; Real]);
  register_alias ctxt.pool "num02" (Variant [IntRange (Some ~:0, Some ~:2) ; Real]);
  (* TODO : check *)
  register_alias ctxt.pool "angle" Int;
  (***********************)
  (* PDF reference 7.9.5 *)
  (***********************)
  let number = type_alias ~allow_ind:false "number" in
  register_alias ctxt.pool "rectangle" (ArraySized (number, 4));
  register_alias ctxt.pool "matrix6" (ArraySized (number, 6));
  register_alias ctxt.pool "matrix9" (ArraySized (number, 9));

  register_alias ctxt.pool "quad_points" (ArrayTuples [| number ; number ; number ; number ; number ; number ; number ; number |]);

  register_alias ctxt.pool "color" (ArrayVariantSized (type_alias "num01", [| 0 ; 1 ; 3 ; 4 |]));
  register_alias ctxt.pool "color_rgb" (ArraySized (type_alias "num01", 3));

  register_alias ctxt.pool "stream_or_bytes" (Variant [Stream "stream_base" ; String]);
  ()


let load_types ctxt =
  register_util ctxt;

  register_action ctxt;
  register_annot ctxt;
  register_colorspace ctxt;
  register_font ctxt;
  register_function ctxt;
  register_graphic ctxt;
  register_outline ctxt;
  register_page ctxt;
  register_stream ctxt;
  register_transition ctxt;
  register_xobject ctxt;



  register_alias ctxt.pool "stream_objstm" (Stream "object_stream");
  register_alias ctxt.pool "stream_xrefstm" (Stream "xref_stream");

  (***********************)
  (* PDF reference 7.8.2 *)
  (***********************)
  register_class ctxt.pool "content_stream" ~includes:[
    "stream_base"
  ] [
  ];

  (************************)
  (* PDF reference 14.3.2 *)
  (************************)
  register_class ctxt.pool "metadata_stream" ~includes:[
    "stream_base"
  ] [
    "Type", entry_name_exact ~optional:false "Metadata" ;
    "Subtype", entry_name_exact ~optional:false "XML" ;
  ];

  (***********************)
  (* PDF reference 7.5.5 *)
  (***********************)
  register_class ctxt.pool "xref_base" [
    "Size", entry_alias ~allow_ind:false ~optional:false "intpositive" ;

    "Prev", entry_alias ~optional:true "intpositive" ;
    "Encrypt", entry_class ~optional:true "encrypt" ;
    "Info", entry_class ~optional:true "info" ;
    "ID", entry_sized_array ~optional:true 2 (make_type ~allow_ind:false String) ;
  ];

  register_class ctxt.pool "trailer_base" ~includes:[
    "xref_base" ;
  ] [
    "Root", entry_class ~optional:false "catalog" ;

    (* TODO : Non-standard, OpenOffice/LibreOffice *)
    "DocChecksum", make_entry_type ~optional:true Name ;
  ];

  (*************************)
  (* PDF reference 7.5.8.2 *)
  (*************************)
  register_class ctxt.pool "xrefstm_base" [
    "Type", entry_name_exact ~allow_ind:false ~optional:true "XRef" ;
    "Index", entry_array_tuples ~allow_ind:false ~optional:true [| type_alias ~allow_ind:false "intnonnegative" ; type_alias ~allow_ind:false "intnonnegative" |] ;
    "W", entry_sized_array ~allow_ind:false ~optional:true 3 (type_alias ~allow_ind:false "intnonnegative") ;
  ];

  register_class ctxt.pool "trailer_xref" ~includes:[
    "trailer_base" ;
    "stream_base" ;
    "xrefstm_base" ;
  ] [
  ];
  register_class ctxt.pool "xref_stream" ~includes:[
    "xref_base" ;
    "stream_base" ;
    "xrefstm_base" ;
  ] [
    "Root", entry_class ~optional:true "catalog" ;
  ];

  (*************************)
  (* PDF reference 7.5.8.4 *)
  (*************************)
  register_class ctxt.pool "trailer_hybrid" ~includes:[
    "trailer_base" ;
  ] [
    "XRefStm", entry_alias ~optional:false "intpositive" ;
  ];

  register_class ctxt.pool "encrypt" [
    (* TODO *)
  ];

  register_alias ctxt.pool "trailer" (Variant [
      Class "trailer_base" ;
      Class "trailer_xref" ;
      Class "trailer_hybrid" ;
    ]);

  (***********************)
  (* PDF reference 7.7.2 *)
  (***********************)
  let type_pagelabels = register_numtree ctxt "pagelabels" (type_class "pagelabel") in

  register_class ~strict:false ctxt.pool "catalog" [
    "Type", entry_name_exact ~allow_ind:false ~optional:false "Catalog" ;

    "Pages", entry_class ~optional:false "pageroot" ;
    "Names", entry_class ~optional:true "names" ;
    "Outlines", entry_class ~optional:true "outlines" ;
    "PageMode", entry_name_in ~optional:true ["UseNone" ; "UseOutlines" ; "UseThumbs" ; "FullScreen" ; "UseOC" ; "UseAttachments"] ;
    "PageLayout", entry_name_in ~optional:true ["SinglePage" ; "OneColumn" ; "TwoColumnLeft" ; "TwoColumnRight" ; "TwoPageLeft" ; "TwoPageRight"] ;

    "OpenAction", entry_variant ~optional:true [Alias "alias_dest" ; Alias "action"] ;
    "PageLabels", make_entry ~optional:true type_pagelabels ;
    "ViewerPreferences", entry_class ~optional:true "viewer_prefs" ;
    "Metadata", entry_stream ~optional:true "metadata_stream" ;

    (* TODO *)
  ];

  (**********************)
  (* PDF reference 12.2 *)
  (**********************)
  register_class ctxt.pool "viewer_prefs" [
    "HideToolbar", make_entry_type ~optional:true Bool ;
    "HideMenubar", make_entry_type ~optional:true Bool ;
    "HideWindowUI", make_entry_type ~optional:true Bool ;
    "FitWindow", make_entry_type ~optional:true Bool ;
    "CenterWindow", make_entry_type ~optional:true Bool ;
    "DisplayDocTitle", make_entry_type ~optional:true Bool ;
    "NonFullScreenPageMode", entry_name_in ~optional:true ["UseNone" ; "UseOutlines" ; "UseThumbs" ; "UseOC"] ;
    "Direction", entry_name_in ~optional:true ["L2R" ; "R2L"] ;
    "ViewArea", entry_name_in ~optional:true ["MediaBox" ; "CropBox" ; "BleedBox" ; "TrimBox" ; "ArtBox"] ;
    "ViewClip", entry_name_in ~optional:true ["MediaBox" ; "CropBox" ; "BleedBox" ; "TrimBox" ; "ArtBox"] ;
    "PrintArea", entry_name_in ~optional:true ["MediaBox" ; "CropBox" ; "BleedBox" ; "TrimBox" ; "ArtBox"] ;
    "PrintClip", entry_name_in ~optional:true ["MediaBox" ; "CropBox" ; "BleedBox" ; "TrimBox" ; "ArtBox"] ;
    "PrintScaling", entry_name_in ~optional:true ["None" ; "AppDefault"] ;
    "Duplex", entry_name_in ~optional:true ["Simplex" ; "DuplexFlipShortEdge" ; "DuplexFlipLongEdge"] ;
    "PickTrayByPDFSize", make_entry_type ~optional:true Bool ;
    "PrintPageRange", entry_array_tuples ~optional:true [|type_alias "intpositive" ; type_alias "intpositive"|] ;
    "NumCopies", entry_alias ~optional:true "intpositive" ;
  ];


  (************************)
  (* PDF reference 12.4.2 *)
  (************************)
  register_class ~strict:false ctxt.pool "pagelabel" [
    "Type", entry_name_exact ~allow_ind:false ~optional:true "PageLabel" ;

    "S", entry_name_in ~optional:true ["D" ; "R" ; "r" ; "A" ; "a"] ;
    "P", make_entry_type ~optional:true Text ;
    "St", entry_alias ~optional:true "intpositive" ;
  ];

  (************************)
  (* PDF reference 14.3.3 *)
  (************************)
  register_class ~strict:false ctxt.pool "info" [
    "Title", make_entry_type ~optional:true Text ;
    "Author", make_entry_type ~optional:true Text ;
    "Subject", make_entry_type ~optional:true Text ;
    "Keywords", make_entry_type ~optional:true Text ;
    "Creator", make_entry_type ~optional:true Text ;
    "Producer", make_entry_type ~optional:true Text ;
    "CreationDate", make_entry_type ~optional:true Date ;
    "ModDate", make_entry_type ~optional:true Date ;
    "Trapped", entry_name_in ~optional:true ["True" ; "False" ; "Unknown"] ;
    (* TODO *)
  ];

  (**************************)
  (* PDF reference 12.3.2.2 *)
  (**************************)
  register_alias ctxt.pool "alias_dest" (Variant [
      Tuple [| type_class "page" ; type_name_in ["Fit" ; "FitB"] |] ;
      Tuple [| type_class "page" ; type_name_in ["FitH" ; "FitV" ; "FitBH" ; "FitBV"] ; type_alias "numornull" |] ;
      Tuple [| type_class "page" ; type_name_exact "XYZ" ; type_alias "numornull" ; type_alias "numornull" ; type_alias "numornull" |] ;
      Tuple [| type_class "page" ; type_name_exact "FitR" ; type_alias "number" ; type_alias "number" ; type_alias "number" ; type_alias "number" |] ;
    ]);

  (**************************)
  (* PDF reference 12.3.2.3 *)
  (**************************)
  register_class ~strict:false ctxt.pool "destination" [
    "D", entry_alias ~optional:true "alias_dest" ;
  ];
  let type_dest = register_nametree ctxt "dest" (type_variant [Class "destination" ; Alias "alias_dest"]) in

  register_alias ctxt.pool "named_dest" (Variant [
      Name ;
      String ;
      Alias "alias_dest" ;
    ]);

  (***********************)
  (* PDF reference 7.7.4 *)
  (***********************)
  register_class ~strict:false ctxt.pool "names" [
    "Dests", make_entry ~optional:true type_dest ;

    "Type", entry_name_exact ~allow_ind:false ~optional:true "Names" ;
    (* TODO : clean up *)
  ];

  (************************)
  (* PDF reference 14.7.2 *)
  (************************)
  register_class ~strict:false ctxt.pool "struct_element" [
    (* TODO *)
  ];

  (***********************)
  (* PDF reference 7.8.3 *)
  (***********************)
  register_class ~strict:false ctxt.pool "resources" [
    "ExtGState", entry_dict ~optional:true (type_class "graphic_state") ;
    "ColorSpace", entry_dict ~optional:true (type_alias "color_space") ;
    "Pattern", entry_dict ~optional:true (type_alias "pattern") ;
    "Shading", entry_dict ~optional:true (type_alias "shading") ;
    "XObject", entry_dict ~optional:true (type_alias "xobject") ;
    "Font", entry_dict ~optional:true (type_alias "font") ;
    "ProcSet", entry_alias ~optional:true "procedure_set" ;
    (*
    "Properties", entry_dict ~optional:true (type_alias "properties") ;
    *)
  ];

  (**********************)
  (* PDF reference 14.2 *)
  (**********************)
  register_alias ctxt.pool "procedure_set" (Array (type_name_in ["PDF" ; "Text" ; "ImageB" ; "ImageC" ; "ImageI"]));

  (************************)
  (* PDF reference 14.6.2 *)
  (************************)
  register_alias ctxt.pool "properties" Any;
  (* TODO *)

