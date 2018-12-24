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
open Point.Lib
open Point.Base
open Js_of_ocaml

let coerce a = (* JS magic *)  
  (Js.Unsafe.coerce a)

let getElementbyId string = 
  coerce (Dom_html.getElementById string ) 

let getElementsByClassName name =
  Js.Unsafe.meth_call
    Dom_html.document "getElementsByClassName" [|Js.Unsafe.inject name|]

let onload _ =
  let body = getElementbyId "point" in

  (** Used to show the tabs on click, and hide the unselected ones  *)
  let tab_function (event : Js_of_ocaml.Dom_html.mouseEvent Js_of_ocaml.Js.t) name = 
    Dom.list_of_nodeList $ getElementsByClassName (Js.string "tabcontent") 
    |> List.iter (fun e -> e##.style##.display := Js.string "none" );     
    Dom.list_of_nodeList $ getElementsByClassName (Js.string "tablink active") 
    |> List.iter (fun e -> e##.className := Js.string ("tablink") ); 
    (Dom_html.getElementById name)##.style##.display := Js.string "block";  
    let active_tablink = (coerce event)##.currentTarget in 
    active_tablink##.className := Js.string ( (Js.to_string active_tablink##.className) ^ " active") in

  (** The main body of the point web page, using TyXML to easily represent the structure  *)
  let html_div = Tyxml_js.To_dom.of_body Tyxml_js.Html.( 
     body [ 
      h1 [pcdata "Presentation Of Interactive Normalform Transformations"];
      p [pcdata "Choose which tool you want to use (click the tab)"];
      div ~a:[a_class ["tab"];] [

        button ~a:[a_class ["tablink"]; a_onclick (fun e -> tab_function e "FDtool";true)] [
          pcdata "Functional Dependency tools "
        ];    
        button ~a:[a_class ["tablink"]; a_onclick (fun e -> tab_function e "fd_gen";true)] [
          pcdata "Functional Dependency generation"
        ];
        button ~a:[a_class ["tablink"]; a_onclick (fun e -> tab_function e "synth";true)] [
          pcdata "Synthesis algorithm"
        ];          
        button ~a:[a_class ["tablink"]; a_onclick (fun e -> tab_function e "decomp";true)] [
          pcdata "Decomposition algorithm"
        ]      

     ];

      div ~a:[a_class ["tabcontent"]; a_id "FDtool"] [
        p [pcdata "Generate keys, canonical forms, and LaTeX export for given FD"];

        strong [pcdata "Schema  "]; input ~a:[a_id "input_schema"] (); 
        span ~a:[a_style "display: inline-block; width: 5ch;"] [pcdata "  "] ;
        span ~a:[a_id "error_fdep"; a_style "color: red;"] [pcdata ""] ;  br ();
        h4 ~a:[a_style "display: inline-block;"] [pcdata "Functional Dependencies"; br ();
        textarea  ~a:[a_style "font-size: 13pt"; a_id "input_fdep";a_rows 20;a_cols 60]  (pcdata "" ); ]; 
        span ~a:[a_style "display: inline-block; width: 5ch;"] [pcdata "  "] ;
        h4 ~a:[a_style "display: inline-block;"] [pcdata "Output"; br ();
        textarea ~a:[ a_style "font-size: 13pt"; a_id "output_fdep";a_rows 20;a_cols 80;a_readonly ()] (pcdata "" )]; 

        br(); h2 ~a:[a_style "display: inline-block;"]  [pcdata "Testing equivalence"];     
        span ~a:[a_style "display: inline-block; width: 5ch;"] [pcdata "  "] ;
        span ~a:[a_id "error_fdep2"; a_style "color: red;"] [pcdata ""] ;  br ();
        b [pcdata "Don't forget to enter a schema above! Don't expect correct results without one"];
        p [pcdata "Note: This is computed using the _entire_ transitive closure as of now, so fairly inefficient"];
        h4 ~a:[a_style "display: inline-block;"] [pcdata "First Dependency"; br ();
        textarea  ~a:[a_style "font-size: 13pt"; a_id "input_fdep2";a_rows 20;a_cols 70]  (pcdata "" ); ]; 
        span ~a:[a_style "display: inline-block; width: 5ch;"] [pcdata "  "] ;
        h4 ~a:[a_style "display: inline-block;"] [pcdata "Second Dependency"; br ();
        textarea ~a:[ a_style "font-size: 13pt"; a_id "input_fdep3";a_rows 20;a_cols 70] (pcdata "" )]; 
        br ();        
        button ~a:[a_id "equiv_button"] [pcdata "Check equivalence"] ;
        span ~a:[a_style "display: inline-block; width: 5ch;"] [pcdata "  "] ;
        strong [pcdata "Result:  "]; input ~a:[a_id "output_fdep2"; a_size 50; a_readonly()] (); 

      ];
      
      div ~a:[a_class ["tabcontent"]; a_id "synth"] [
        h1 [pcdata "TODO"];
      ];

      div ~a:[a_class ["tabcontent"]; a_id "decomp" ] [
        h1 [pcdata "TODO"];
      ];

      div ~a:[a_class ["tabcontent"]; a_id "fd_gen"] [ 
        br ();
        strong [pcdata "Schema  "]; input ~a:[a_size 6; a_id "input_schema_gen"] (); 
        span ~a:[a_style "display: inline-block; width: 5ch;"] [pcdata "  "] ;
        strong [pcdata "Normalform  "]; 
        select ~a:[a_id "choice_nf"] [
          option ~a:[a_value "neither"] (pcdata "Not in 3NF");          
          option ~a:[a_value "3NF only"] (pcdata "In 3NF, but not BCNF");
          option ~a:[a_value "BCNF"] (pcdata "In BCNF")
        ]; 
        span ~a:[a_style "display: inline-block; width: 5ch;"] [pcdata "  "] ;
        strong [pcdata "# of keys (upper bound)  "]; input ~a:[a_value "3"; a_id "choice_keys"; a_size 1;] (); 
        span ~a:[a_style "display: inline-block; width: 5ch;"] [pcdata "  "] ;
        strong [pcdata "Cardinality of keys (upper bound) "]; input ~a:[a_value "4"; a_id "choice_card"; a_size 1;] (); 
        span ~a:[a_style "display: inline-block; width: 5ch;"] [pcdata "  "] ;
        span ~a:[a_id "error_gen"; a_style "color: red;"] [pcdata ""] ;  br ();   br (); 
        button ~a:[a_id "gen_button"] [pcdata "Generate dependency"] ;
        h4 (* ~a:[a_style "display: inline-block;"] *) [pcdata "Output"; br ();
        textarea ~a:[ a_style "font-size: 13pt"; a_id "output_gen";a_rows 20;a_cols 80;a_readonly ()] (pcdata "" )];
  
      ]

     ]  ) in
  Dom.appendChild body html_div;

  (* Install the needed handlers *)

  let input_schema = getElementbyId "input_schema"  in 
  let input_fdep   = getElementbyId "input_fdep"    in 
  let input_fdep2  = getElementbyId "input_fdep2"   in 
  let input_fdep3  = getElementbyId "input_fdep3"   in 
  let output_fdep  = getElementbyId "output_fdep"   in 
  let output_fdep2 = getElementbyId "output_fdep2"  in 
  let error_fdep   = getElementbyId "error_fdep"    in 
  let error_fdep2  = getElementbyId "error_fdep2"   in 
  let equiv_button = getElementbyId "equiv_button"  in 
  let gen_button   = getElementbyId "gen_button"    in 
  let error_gen    = getElementbyId "error_gen"     in 
  let choice_nf    = getElementbyId "choice_nf"     in 
  let choice_keys  = getElementbyId "choice_keys"   in 
  let choice_card  = getElementbyId "choice_card"   in 
  let output_gen   = getElementbyId "output_gen"    in 
  let input_schema_gen = getElementbyId "input_schema_gen"  in 

  input_fdep##.oninput := input_schema##.oninput := Dom_html.handler (fun _ -> 
      (
        error_fdep##.innerHTML := (Js.string "");
        try
         (  Firebug.console##log  input_fdep##.value;
            let fdep   = get_fdep $ Js.to_string input_fdep##.value in 

            Firebug.console##log  input_schema##.value ;
            let schema = get_schema $ Js.to_string input_schema##.value in 
            
            Firebug.console##log  (Js.string $ "Schema: " ^ (sch_to_string schema));
            Firebug.console##log  (Js.string  $ "Fdep: " ^(fdep_to_string fdep));
            
            output_fdep##.value := (Js.string $ latex_transformer (schema,fdep) () ))
        with _ -> error_fdep##.innerHTML := (Js.string "Couldn't parse your input. 🙁"); (); 
      );
      Js._true );

   equiv_button##.onclick := Dom_html.handler (fun _ -> 
      (
        error_fdep2##.innerHTML := (Js.string "");
        try
         (  Firebug.console##log  input_fdep2##.value;
            let fdep2   = get_fdep $ Js.to_string input_fdep2##.value in 
            Firebug.console##log  input_fdep3##.value;
            let fdep3   = get_fdep $ Js.to_string input_fdep3##.value in 

            Firebug.console##log  input_schema##.value ;
            let schema = get_schema $ Js.to_string input_schema##.value in 
            
            Firebug.console##log  (Js.string $ "Schema: " ^ (sch_to_string schema));
            Firebug.console##log  (Js.string  $ "Fdep: " ^ (fdep_to_string fdep2));
            Firebug.console##log  (Js.string  $ "Fdep: " ^ (fdep_to_string fdep3));
            
            output_fdep2##.value := (Js.string $ equiv_test schema fdep2 fdep3 ))
        with _ -> error_fdep2##.innerHTML := (Js.string "Couldn't parse your input. 🙁"); (); 
      );
      Js._true );

     gen_button##.onclick :=  Dom_html.handler (fun _ -> 
     (
          error_gen##.innerHTML := (Js.string "");
          try
           (  Firebug.console##log  choice_nf##.value;
              let choice_nf  = 
                match Js.to_string choice_nf##.value with    
                | "neither"  -> neither  
                | "3NF only" -> third_only
                | "BCNF"     -> is_in_bcnf
                | _          -> raise  $ failwith "Wrong predictate chosen"
              in 
              Firebug.console##log  choice_keys##.value;
              let choice_keys  = int_of_string $ Js.to_string choice_keys##.value in 
              Firebug.console##log  choice_card##.value;
              let choice_card  = int_of_string $ Js.to_string choice_card##.value in 

              Firebug.console##log  input_schema_gen##.value ;
              let schema = get_schema $ Js.to_string input_schema_gen##.value in 

              output_gen##.value := (Js.string $ key_exercises choice_nf choice_keys choice_card schema ()  )
            )
          with _ -> error_gen##.innerHTML := (Js.string "Couldn't parse your input. 🙁"); (); 
      );
     Js._true );
   Js._false

let _ = Dom_html.window##.onload := Dom_html.handler onload