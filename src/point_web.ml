(* 
 * The web interface for POINT, built using Js_of_ocaml, and using 
 * one of the examples "wysiwyg" as a template (though most of the
 * specific code for it has been removed or adapted )
 *
 * This was originally based on:
 *   
 * Js_of_ocaml example
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2010 Dmitry Kosarev
 *
 * https://github.com/ocsigen/js_of_ocaml/tree/master/examples/wysiwyg
 *)
open Point.Lib
open Point.Base
open Js_of_ocaml

let syn_count = ref 0 
let syn_schem = ref (get_schema "")
let syn_fdep  = ref (get_fdep   "")

let decomp_res = ref []
let decomp_res_history  : ( (int*schema*functional_dep) list) list ref = ref []

let dec_schem = ref (get_schema "")
let dec_fdep  = ref (get_fdep   "")

let coerce a = (* JS magic *)  
  (Js.Unsafe.coerce a)

let getElementbyId string = 
  coerce (Dom_html.getElementById string ) 

let getElementsByClassName name =
  Js.Unsafe.meth_call
    Dom_html.document "getElementsByClassName" [|Js.Unsafe.inject name|]

let visible a = 
  a##.style##.visibility := Js.string "visible"

let hidden a = 
  a##.style##.visibility := Js.string "hidden"

let invoke_handler h = 
  ignore $ Js.Unsafe.call h##.onclick ()  [|Js.Unsafe.inject (Dom.Event.make "click") |]

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
          h4 ~a:[a_style "display: inline-block;"] [
            pcdata "Functional Dependencies"; br ();
            textarea  ~a:[a_style "font-size: 13pt"; a_id "input_fdep";a_rows 10;a_cols 60]  (pcdata "" ); 
          ]; 
          span ~a:[a_style "display: inline-block; width: 5ch;"] [pcdata "  "] ;
          h4 ~a:[a_style "display: inline-block;"] [
            pcdata "Output"; br ();
            textarea ~a:[ a_style "font-size: 13pt"; a_id "output_fdep";a_rows 10;a_cols 80;a_readonly ()] (pcdata "" )
          ]; 

          h3 [pcdata "Testing equivalence"];     
          span ~a:[a_id "error_fdep2"; a_style "color: red;"] [pcdata ""] ; br ();
          h4 ~a:[a_style "display: inline-block;"] [
            pcdata "First Dependency"; br ();
            textarea  ~a:[a_style "font-size: 13pt"; a_id "input_fdep2";a_rows 5;a_cols 70]  (pcdata "" ); 
          ]; 
          span ~a:[a_style "display: inline-block; width: 5ch;"] [pcdata "  "] ;
          h4 ~a:[a_style "display: inline-block;"] [
            pcdata "Second Dependency"; br ();
            textarea ~a:[ a_style "font-size: 13pt"; a_id "input_fdep3";a_rows 5;a_cols 70] (pcdata "" )
          ]; 
          br ();        
          button ~a:[a_id "equiv_button"] [pcdata "Check equivalence"] ;
          span ~a:[a_style "display: inline-block; width: 5ch;"] [pcdata "  "] ;
          strong [pcdata "Result:  "]; input ~a:[a_id "output_fdep2"; a_size 100; a_readonly()] (); 
        ]; 

        div ~a:[a_class ["tabcontent"]; a_id "fd_gen"] [ 
          p [pcdata "Produces a set of random Functional Dependencies, based on a number of chosen predicates"];
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

          strong [pcdata "# of keys (upper bound)  "]; 
          input ~a:[a_value "3"; a_id "choice_keys"; a_size 1;] (); 
          span ~a:[a_style "display: inline-block; width: 5ch;"] [pcdata "  "] ;

          strong [pcdata "Cardinality of keys (upper bound) "]; 
          input ~a:[a_value "4"; a_id "choice_card"; a_size 1;] (); 
          span ~a:[a_style "display: inline-block; width: 5ch;"] [pcdata "  "] ;

          span ~a:[a_id "error_gen"; a_style "color: red;"] [pcdata ""] ;  br ();   br (); 
          button ~a:[a_id "gen_button"] [pcdata "Generate dependency"] ;
          h4 [
            pcdata "Output"; br ();
            textarea ~a:[ a_style "font-size: 13pt"; a_id "output_gen";a_rows 15;a_cols 140;a_readonly ()] (pcdata "" )
          ];

        ];

        div ~a:[a_class ["tabcontent"]; a_id "synth"] [
          p [pcdata "Presents the Synthesis algorithm to achieve 3NF, in a step-wise fashion"];

          strong [pcdata "Schema  "]; input ~a:[a_id "input_schema_synth"] (); 
          span ~a:[a_style "display: inline-block; width: 5ch;"] [pcdata "  "] ;
          span ~a:[a_id "error_synth"; a_style "color: red;"] [pcdata ""] ;  br ();
          h4 [
            pcdata "Functional Dependency"; br ();
            textarea  ~a:[a_style "font-size: 13pt"; a_id "input_fdep_synth";a_rows 5;a_cols 80]  (pcdata "" ); 
          ];
          button ~a:[a_id "synth_button"] [pcdata "Proceed"] ;          
          button ~a:[a_id "synth_button3"] [pcdata "Step back"] ;  
          button ~a:[a_id "synth_button2"] [pcdata "Start again"] ;  
          h4 [
            pcdata "Output"; br ();
            textarea ~a:[ a_style "font-size: 13pt"; a_id "output_synth";a_rows 20;a_cols 80;a_readonly ()] (pcdata "" )
          ];
        ];

        div ~a:[a_class ["tabcontent"]; a_id "decomp" ] [
          p [pcdata "Interactively presents the Decomposition algorithm to achieve BCNF"];

          strong [pcdata "Schema  "]; input ~a:[a_id "input_schema_decomp"] (); 
          span ~a:[a_style "display: inline-block; width: 5ch;"] [pcdata "  "] ;
          span ~a:[a_id "error_decomp"; a_style "color: red;"] [pcdata ""] ;  br ();
          h4 [
            pcdata "Functional Dependency"; br ();
            textarea  ~a:[a_style "font-size: 13pt"; a_id "input_fdep_decomp";a_rows 5;a_cols 80]  (pcdata "" ); 
          ];    
          button ~a:[a_id "decomp_button"] [pcdata "Proceed"];
          button ~a:[a_id "decomp_button3"] [pcdata "Step back"] ;             
          button ~a:[a_id "decomp_button4"] [pcdata "Start again"] ;  
          span ~a:[a_style "display: inline-block; width:2c h;"] [pcdata "  "] ;   
          button ~a:[a_id "decomp_button2"; a_style "visibility: hidden"] [pcdata "Decompose"] ;
          span ~a:[a_style "display: inline-block; width: 5ch;"] [pcdata "  "] ;        
          strong  ~a:[a_id "old_fdep"; a_style "visibility: hidden"] [pcdata "Next dependency:  "; select ~a:[a_id "choice_dep"] []; ]; 
          h4  [
            pcdata "Output"; br ();
            textarea ~a:[ a_style "font-size: 13pt"; a_id "output_decomp";a_rows 20;a_cols 80;a_readonly ()] (pcdata "" )
          ];
        ]
      ]  ) in
  Dom.appendChild body html_div;

  (*  Fetch the useful elements defined above *)

  let input_schema        = getElementbyId "input_schema"         in
  let input_fdep          = getElementbyId "input_fdep"           in
  let input_fdep2         = getElementbyId "input_fdep2"          in
  let input_fdep3         = getElementbyId "input_fdep3"          in
  let output_fdep         = getElementbyId "output_fdep"          in
  let output_fdep2        = getElementbyId "output_fdep2"         in
  let error_fdep          = getElementbyId "error_fdep"           in
  let error_fdep2         = getElementbyId "error_fdep2"          in
  let equiv_button        = getElementbyId "equiv_button"         in
  let gen_button          = getElementbyId "gen_button"           in
  let error_gen           = getElementbyId "error_gen"            in
  let choice_nf           = getElementbyId "choice_nf"            in
  let choice_keys         = getElementbyId "choice_keys"          in
  let choice_card         = getElementbyId "choice_card"          in
  let output_gen          = getElementbyId "output_gen"           in
  let input_schema_gen    = getElementbyId "input_schema_gen"     in
  let input_schema_synth  = getElementbyId "input_schema_synth"   in
  let error_synth         = getElementbyId "error_synth"          in
  let input_fdep_synth    = getElementbyId "input_fdep_synth"     in
  let synth_button        = getElementbyId "synth_button"         in
  let synth_button2       = getElementbyId "synth_button2"        in
  let synth_button3       = getElementbyId "synth_button3"        in
  let output_synth        = getElementbyId "output_synth"         in
  let input_schema_decomp = getElementbyId "input_schema_decomp"  in
  let error_decomp        = getElementbyId "error_decomp"         in
  let input_fdep_decomp   = getElementbyId "input_fdep_decomp"    in
  let decomp_button       = getElementbyId "decomp_button"        in
  let decomp_button2      = getElementbyId "decomp_button2"       in
  let decomp_button3      = getElementbyId "decomp_button3"       in
  let decomp_button4      = getElementbyId "decomp_button4"       in
  let choice_dep          = getElementbyId "choice_dep"           in
  let output_decomp       = getElementbyId "output_decomp"        in
  let old_fdep            = getElementbyId "old_fdep"             in


  (* Install the needed handlers *)

  input_schema_synth##.oninput := Dom_html.handler (fun _ -> 
      input_schema_gen##.value :=  input_schema_synth##.value;
      input_schema##.value := input_schema_synth##.value;
      input_schema_decomp##.value := input_schema_synth##.value;
      Js._true);  

  input_schema_decomp##.oninput := Dom_html.handler (fun _ -> 
      input_schema_gen##.value :=  input_schema_decomp##.value;
      input_schema##.value := input_schema_decomp##.value;
      input_schema_synth##.value := input_schema_decomp##.value;
      Js._true);

  input_schema_gen##.oninput := Dom_html.handler (fun _ -> 
      input_schema_synth##.value :=  input_schema_gen##.value;
      input_schema_decomp##.value :=  input_schema_gen##.value;
      input_schema##.value := input_schema_gen##.value;
      Js._true);

  input_fdep##.oninput := input_schema##.oninput := Dom_html.handler (fun _ -> (
        input_schema_synth##.value :=  input_schema##.value;   
        input_schema_decomp##.value :=  input_schema##.value;
        input_schema_gen##.value := input_schema##.value;
        error_fdep##.innerHTML := (Js.string "");
        try (  
          Firebug.console##log  input_fdep##.value;
          let fdep   = get_fdep $ Js.to_string input_fdep##.value in 

          Firebug.console##log  input_schema##.value ;
          let schema = get_schema $ Js.to_string input_schema##.value in 

          Firebug.console##log  (Js.string $ "Schema: " ^ (sch_to_string schema));
          Firebug.console##log  (Js.string  $ "Fdep: " ^(fdep_to_string fdep));

          output_fdep##.value := (Js.string $ latex_transformer (schema,fdep) () )
        ) with _ -> 
          error_fdep##.innerHTML := (Js.string "Couldn't parse your input. 🙁"); (); 
      );
      Js._true );

  equiv_button##.onclick := Dom_html.handler (fun _ -> (
        error_fdep2##.innerHTML := (Js.string "");
        try (  
          Firebug.console##log  input_fdep2##.value;
          let fdep2   = get_fdep $ Js.to_string input_fdep2##.value in 
          Firebug.console##log  input_fdep3##.value;
          let fdep3   = get_fdep $ Js.to_string input_fdep3##.value in 

          Firebug.console##log  (Js.string  $ "Fdep: " ^ (fdep_to_string fdep2));
          Firebug.console##log  (Js.string  $ "Fdep: " ^ (fdep_to_string fdep3));

          output_fdep2##.value := (Js.string $ equiv_test fdep2 fdep3 )
        ) with _ -> 
          error_fdep2##.innerHTML := (Js.string "Couldn't parse your input. 🙁"); (); 
      );
      Js._true );

  gen_button##.onclick :=  Dom_html.handler (fun _ -> (
        error_gen##.innerHTML := (Js.string "");
        try (  
          Firebug.console##log  choice_nf##.value;
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
        ) with _ ->
          error_gen##.innerHTML := (Js.string "Couldn't parse your input. 🙁"); (); 
      );
      Js._true );


  synth_button##.onclick :=  Dom_html.handler (fun _ ->  (
        error_synth##.innerHTML := (Js.string "");  
        let change_counter s_new f_new =  
          if ((Schema.equal (!syn_schem) s_new) && equal_fdep (!syn_fdep) f_new) then 
            incr syn_count
          else 
            syn_count := 0 in
        try (  
          Firebug.console##log  input_schema_synth##.value;
          let schema = get_schema $ Js.to_string   input_schema_synth##.value in
          Firebug.console##log  input_fdep_synth##.value;
          let fdep = get_fdep $ Js.to_string   input_fdep_synth##.value in   

          change_counter schema fdep;
          Firebug.console##log  (Js.string $ Printf.sprintf "\nLast Schema: %s\nLast Fdep: %s\nCounter: %n" 
                                   (sch_to_string (!syn_schem)) (fdep_to_string !(syn_fdep)) (!syn_count) );

          output_synth##.value := (Js.string $ (synthesis_procedure (schema,fdep) (!syn_count)   ()   ) );
          syn_fdep  := fdep;
          syn_schem := schema;        
        ) with error -> 
          error_synth##.innerHTML := (Js.string "Couldn't parse your input. 🙁" ); (); 
      );
      Js._true );    

  synth_button2##.onclick := Dom_html.handler (fun _ -> (
        syn_count := -1;
        invoke_handler synth_button
      ); 
      Js._true;);    

  synth_button3##.onclick := Dom_html.handler (fun _ -> (
        ( if(!syn_count = 0) then
            syn_count := -1
          else
            syn_count := !syn_count -2);
        invoke_handler synth_button
      ); 
      Js._true;);  

  let reset s_new f_new =  
    not $ ((Schema.equal (!dec_schem) s_new) && equal_fdep (!dec_fdep) f_new) in

  decomp_button##.onclick :=  Dom_html.handler (fun _ -> (

        error_decomp##.innerHTML := (Js.string "");
        try (  
          Firebug.console##log  input_schema_decomp##.value;
          let schema = get_schema $ Js.to_string   input_schema_decomp##.value in
          Firebug.console##log  input_fdep_decomp##.value;
          let fdep = get_fdep $ Js.to_string   input_fdep_decomp##.value in   

          if reset schema fdep then (
            Firebug.console##log  (Js.string "new schema or fdep different from old one");
            decomp_res := [(0,schema, fdep)];
            decomp_res_history := [];
            hidden old_fdep;
            hidden decomp_button2;
          );

          Firebug.console##log  
            (Js.string $ Printf.sprintf "\nLast Schema: %s\nLast Fdep: %s" 
               (sch_to_string (!dec_schem)) (fdep_to_string !(dec_fdep)) );
          let out_string,violating = check_violating_deps !decomp_res in 

          choice_dep##.innerHTML := Js.string "";
          List.iter (fun (a,b) -> 
              let opt = Dom_html.createOption (Dom_html.document) in
              opt##.innerHTML := 
                (Js.string $ Printf.sprintf "(%s → %s)\n" (sch_to_string a) (sch_to_string b) );
              Dom.appendChild choice_dep opt;
            )  violating; 

          if (violating != []) then (
            visible old_fdep;
            visible decomp_button2;
            hidden decomp_button
          ) else (
            hidden old_fdep;
            hidden decomp_button2;
          );

          output_decomp##.value := (Js.string out_string );
          dec_schem  := schema;
          dec_fdep   := fdep;
        ) with error -> 
          error_decomp##.innerHTML := (Js.string "Couldn't parse your input. 🙁" ); (); 
      );
      Js._true );

  decomp_button2##.onclick := Dom_html.handler (fun _ -> (
        error_decomp##.innerHTML := (Js.string "");
        try (  
          Firebug.console##log  input_schema_decomp##.value;
          let schema = get_schema $ Js.to_string   input_schema_decomp##.value in
          Firebug.console##log  input_fdep_decomp##.value;
          let fdep = get_fdep $ Js.to_string   input_fdep_decomp##.value in   

          if reset schema fdep then (
            Firebug.console##log  (Js.string "new schema or fdep different from old one");

            decomp_res := [(0,schema, fdep)];
            decomp_res_history := []
          )else (
            let k = choice_dep##.selectedIndex in      
            let out_string, res = decomposition_procedure k !decomp_res in 
            output_decomp##.value := (Js.string out_string );

            decomp_res_history := !decomp_res::!decomp_res_history;
            decomp_res := res;

          );
          hidden  old_fdep;
          hidden  decomp_button2;       
          visible decomp_button
        ) with error -> 
          error_decomp##.innerHTML := (Js.string "Couldn't parse your input. 🙁" ); (); 
      );      
      Js._true);

  decomp_button3##.onclick := Dom_html.handler (fun _ -> (
        if (!decomp_res_history != []) then (
          decomp_res         := List.hd !decomp_res_history;
          decomp_res_history := List.tl !decomp_res_history
        );
        invoke_handler decomp_button;
      );
      Js._true);

  decomp_button4##.onclick := Dom_html.handler (fun _ -> (
        if (!decomp_res_history != []) then (
          decomp_res         := List.nth !decomp_res_history ((List.length !decomp_res_history)-1);
          decomp_res_history := []
        );
        invoke_handler decomp_button;
      );
      Js._true);


  Js._false

let _ = Dom_html.window##.onload := Dom_html.handler onload