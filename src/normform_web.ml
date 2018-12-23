(* 
 * The web interface for POINT, built using Js_of_ocaml, and using 
 * one of the examples "wysiswyg" as a template (though most of the
 * specific code for it has been removed or adapted )
 *
 * This is based on:
 *   
 * Js_of_ocaml example
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2010 Dmitry Kosarev
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)
open Normform_base
open Js_of_ocaml
module Html = Dom_html
module T = Tyxml_js.Html5

let getElementsByClassName name =
  Js.Unsafe.meth_call
    Dom_html.document "getElementsByClassName" [|Js.Unsafe.inject name|]

let onload _ =
  let d = Html.document in
  let body =
    Js.Opt.get (d##getElementById (Js.string "point"))
      (fun () -> assert false) in
  let tab_function (event : Js_of_ocaml.Dom_html.mouseEvent Js_of_ocaml.Js.t) name = 
    Dom.list_of_nodeList $ getElementsByClassName (Js.string "tabcontent") 
    |> List.iter (fun e -> e##.style##.display := Js.string "none" );     
    Dom.list_of_nodeList $ getElementsByClassName (Js.string "tablink active") 
    |> List.iter (fun e -> e##.className := Js.string ("tablink") ); 
    (Html.getElementById name)##.style##.display := Js.string "block";  
    let active_tablink = ((Js.Unsafe.coerce event)##.currentTarget) in 
    active_tablink##.className := Js.string ( (Js.to_string active_tablink##.className) ^ " active") in

  let html_div = Tyxml_js.To_dom.of_body T.( 
     body [ 
      h1 [pcdata "Presentation of interactive Normalform transformations"];
      p [pcdata "Choose which tool you want to use (click the tab)"];
      div ~a:[a_class ["tab"];] [

        button ~a:[a_class ["tablink"]; a_onclick (fun e -> tab_function e "FDtool";true)] [
          pcdata "Functional Dependency tools "
        ];
        button ~a:[a_class ["tablink"]; a_onclick (fun e -> tab_function e "synth";true)] [
          pcdata "Synthesis algorithm"
        ];          
        button ~a:[a_class ["tablink"]; a_onclick (fun e -> tab_function e "decomp";true)] [
          pcdata "Decomposition algorithm"
        ];          
        button ~a:[a_class ["tablink"]; a_onclick (fun e -> tab_function e "fd_gen";true)] [
          pcdata "Functional Dependency generation"
        ]

     ];

      div ~a:[a_class ["tabcontent"]; a_id "FDtool"] [
        p [pcdata "Generate keys, canonical forms, and LaTeX export for given FD"];

        strong [pcdata "Schema  "]; input ~a:[a_id "input_schema"] (); br ();
        h4 ~a:[a_style "display: inline-block;"] [pcdata "Functional Dependencies"; br ();
        textarea  ~a:[a_style "font-size: 13pt"; a_id "input_fdep";a_rows 20;a_cols 60]  (pcdata "" ); ]; 
        span ~a:[a_style "display: inline-block; width: 5ch;"] [pcdata "  "] ;
        h4 ~a:[a_style "display: inline-block;"] [pcdata "Output"; br ();
        textarea ~a:[ a_style "font-size: 13pt"; a_id "output_fdep";a_rows 20;a_cols 80;a_readonly ()] (pcdata "" )]; 

      ];
      
      div ~a:[a_class ["tabcontent"]; a_id "synth"] [
        h1 [pcdata "TODO"];
      ];

      div ~a:[a_class ["tabcontent"]; a_id "decomp" ] [
        h1 [pcdata "TODO"];
      ];

      div ~a:[a_class ["tabcontent"]; a_id "fd_gen"] [
        h1 [pcdata "TODO"];
      ]

     ]  ) in
  Dom.appendChild body html_div;
  (* tab_function "FDtool"; *)

  let input_schema = (Js.Unsafe.coerce  (Html.getElementById "input_schema")) in 
  let input_fdep   = (Js.Unsafe.coerce  (Html.getElementById "input_fdep")) in 
  let output_fdep  = (Js.Unsafe.coerce  (Html.getElementById "output_fdep")) in 

  input_fdep##.oninput := input_schema##.oninput := Html.handler (fun _ -> 
      (
        output_fdep##.value := (Js.string "");
        try
         (  Firebug.console##log  input_fdep##.value;
            let fdep   = Normform.get_fdep $ Js.to_string input_fdep##.value in 

            Firebug.console##log  input_schema##.value ;
            let schema = Normform.get_schema $ Js.to_string input_schema##.value in 
            
            Firebug.console##log  (Js.string $ "Schema: " ^ (Normform_base.sch_to_string schema));
            Firebug.console##log  (Js.string  $ "Fdep: " ^(Normform_base.fdep_to_string fdep));
            
            output_fdep##.value := (Js.string $ Normform.latex_transformer (schema,fdep) () ))
        with _ -> output_fdep##.value := (Js.string "Couldn't parse your input. 🙁"); (); 
      );
      Js._true );
  Js._false

let _ = Html.window##.onload := Html.handler onload