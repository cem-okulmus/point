open Printf
open Normform_base
open Schema

(*  An interactive tool to walk through the decomposition and synthesis algorithm.  *)

(* Compute the transitive closure *)

let extend fdep : functional_dep = (* compute one transitive step *)
    let extend (a1,b1) = match List.find_opt (fun (a2,b2) -> subset a2 (b1 @@ a1) && not ( subset b2 (b1 @@ a1) )) fdep with
    | None          -> (a1, b1)
    | Some (a3,b3)  -> (a1,  (b1 @@ b3) -- a1 ) in
    List.map extend fdep 

let right_extend fdep :functional_dep = (* add all subsets of betas *)
    let add a bs = List.map (fun b -> (a,b)) bs in 
    List.flatten $ List.map (fun (a,b) ->  add a (powerset b) ) fdep

let closure (schema:schema) (fdep:functional_dep) : functional_dep = 
    let trivial_deps = List.map (fun a -> (a,empty)) (powerset schema) in 
    let temp = sanity $ fixpoint extend (fdep @ trivial_deps) in
    sanity $ right_extend temp


let attr_extend fdep s : schema = match List.find_opt (fun (a2,b2) -> subset a2 s && not ( subset b2 s)) fdep with
    | None          -> s
    | Some (a3,b3)  -> s @@ b3

let get_cover fdep subschema = 
     fixpoint (attr_extend fdep) subschema

let get_superkeys schema fdep : schema list = 
    List.filter (fun a -> equal (get_cover fdep a) schema) (powerset schema)

let get_key_cand schema fdep =   (* find minimal superkeys wrt. subset *)
    let superkeys = get_superkeys schema fdep in 
    List.filter (fun a -> not $ List.exists (fun b -> subset b a && a <> b) superkeys ) superkeys

(* Check BCNF condition, and produce violating dep *)

let get_bcnf_violating_deps schema fdep =
    let skeys = get_superkeys schema fdep  in 
    List.filter (fun (a,b) -> not $ list_mem a skeys ) fdep

let is_in_bcnf schema fdep = 
    [] = get_bcnf_violating_deps schema fdep 

(* Decomposition step *)
let fitting_deps schema fdep = 
    List.filter (fun (c,d) -> subset c schema && not $$ is_empty $ inter schema d) fdep
    |> List.map (fun (c,d) -> (c, (inter d schema))) 
    |> sanity

let decompose_schema schema fdep  (a,b) = 
    let fdep = closure schema fdep in 
    let schema_a = a @@ b in 
    let schema_b = schema -- b in
    let fdep_a = fitting_deps schema_a fdep in 
    let fdep_b = fitting_deps schema_b fdep in 
    (schema_a, fdep_a),(schema_b, fdep_b)

(* Synthesis algorithm  *)

(* Check 3rd NF condition *)

let is_in_3NF schema fdep = 
    let fdep = sanity fdep in 
    let keys = get_key_cand schema fdep in
    let is_superkey a = list_mem a (get_superkeys schema fdep) in 
    let subset_of_key a = for_all (fun c -> List.exists  (fun x -> mem c x) keys) a  in
    List.for_all (fun (a,b) -> is_superkey a || subset_of_key b) fdep

let get_smallest_cover fdep a b :schema= 
    List.filter (fun a -> subset b (get_cover fdep a)) (powerset a) 
    |>List.fold_left (fun prev next -> 
        if subset next prev then next else prev ) a

let leftred fdep : functional_dep= 
    List.map (fun (a,b) -> ( get_smallest_cover fdep a b,b)) fdep
    |> sanity

(* true means redundant, false means attr cannot be removed *)
let redundancy_check fdep a b attr =
    let new_fdep = (a, remove attr b )::(remove_dep fdep a b) in 
    mem attr  ( get_cover  new_fdep a ) 

let minimize_dependency fdep (a,b) = 
    let redundant = choose $ filter (fun x -> redundancy_check fdep a b x) b in
    rem_dup_sch $ (a, remove redundant b)::(remove_dep fdep a b)

let rightred_step  fdep = 
    match List.find_opt (fun (a,b) -> exists (fun x -> redundancy_check  fdep a b x) b ) fdep  with
    | None      -> fdep 
    | Some(a,b) -> minimize_dependency fdep (a,b)

let rightred fdep : functional_dep = 
    sanity $ fixpoint rightred_step fdep

let combine_step (fdep:functional_dep) = 
    let left_sides = rem_dup $$ fst $ List.split fdep in 
    let collect_betas a = List.split $ List.filter (fun (x,b) -> a=x) fdep in
    List.map (fun a -> (a, flatten $ snd (collect_betas a))) left_sides

let canonical = 
    combine_step $$ rightred $$ leftred 

let create_schemas clos fdep : (schema * functional_dep) list = 
    List.map (fun (a,b) -> ( a@@b ,fitting_deps (a@@b) clos)) fdep

let check_for_prim_key schema fdep = 
    let keys = get_key_cand schema fdep in 
    List.find_opt (fun (s,f) -> List.exists (fun (a,b) -> list_mem a keys) f)


(* Parser functions *)

let get_schema s :schema =
  let lexbuf = Lexing.from_string s in 
  (Normform_pars.schema Normform_lex.token) lexbuf;;

let get_fdep s :functional_dep =
  let lexbuf = Lexing.from_string s in 
  Normform_pars.fun_dep Normform_lex.token lexbuf;;

(* Interactive procedures *)

    (*   n :=  increasing counter, needed for res  *)
    (* res :=  { (n,s,f) | n .. number, s ..schema, f .. fun. dep. } *)
let rec decomposition_procedure n res = 
    printf "\n\nCurrent schemas: \n\n"; 
    List.iter (fun (num,s,f) -> 
        printf "\tSchema_%n: %s \n \tFunctional Dep: %s \n\n" 
                num (sch_to_string s) (fdep_to_string  f )) res;
    match List.find_opt (fun (n,s,f) -> not $ is_in_bcnf s f) res with
    | None -> 
        printf "Schemas are all in BCNF. Nothing to be done.\n"
    | Some (num,schema,fdep) ->
    (   
        printf "\n\n\tSchema_%n: %s is not in BCNF! \n\n" 
                num (sch_to_string schema); 

        printf  "Following violating dependencies found: \n";
        let violating =  get_bcnf_violating_deps schema fdep in 
        List.iteri (fun i (a,b) -> 
            printf "%n. (%8s → %-8s)\n" i (sch_to_string a) (sch_to_string b) ) violating;

        printf "\nChoose how to proceed, enter a number between [0 .. %n]:" 
                                                ((List.length violating)-1);
        let k = robust read_int ()  in 
        let (s,f) = (List.nth violating k) in 

        printf "\n\nDecomposing existing Schema_%n: %s via %s: \n" 
                num (sch_to_string schema) (fdep_to_string [(s,f)]);
        let (s1,f1),(s2,f2) = decompose_schema schema fdep (s,f)  in 
        printf "\n\tSchema_%n: %s \nFunctional Dep: %s  \n" 
                (n) (sch_to_string s1 )  (fdep_to_string $ canonical f1);
        printf "\n\tSchema_%n: %s \nFunctional Dep: %s \n" 
                (n+1) (sch_to_string s2 )  (fdep_to_string $  canonical f2);

        decomposition_procedure (n+2) $ List.filter ( (<>) (num,schema,fdep) ) (res@[((n),s1,canonical f1);((n+1),s2,canonical f2)])  
    )

let decomposition (schema,fdep) () : unit = 
    decomposition_procedure 1 [(0,schema, fdep)]

let synthesis_procedure (s,f) () =
    let proceed () = 
        printf "Press enter to proceed ...:";
        ignore $ read_line ()   in
    printf "\nWorking schema: \n\n"; 
    printf "Schema: \t\t%s \nFun. dependencies: \t%s \nKeys: \t\t\t{ %s } \n\n" 
            (sch_to_string s) (fdep_to_string f ) (String.concat "," (List.map sch_to_string (get_key_cand s f)));
    if (is_in_3NF s f) then 
        printf "Already in 3rd normal form, nothing to be done. \n"
    else 
    (
        printf "\t \t 1. Calculating canonical form of schema:\n";
        printf "\t \t ========================================\n";

        printf "Left-reduction: \n\n";
        let left = leftred f in 
        printf "%s" (fdep_to_string left);

        printf "\n\nRight-reduction: \n\n";
        let right = rightred left in 
        printf "%s" (fdep_to_string right);

        printf "\n\nCombination step: \n\n";
        let comb = combine_step right in 
        printf "%s\n\n" (fdep_to_string comb); 

        proceed ();
        printf "\t \t 2. Creating new schemas based on canonical form:\n";
        printf "\t \t ================================================\n";

        let schemas = create_schemas (closure s f) comb in  (* need closure here*)
        List.iter (fun (s,f) -> 
            printf "\tSchema: %s \n \tFunctional dependencies: %s \n\n"
                    (sch_to_string s) (fdep_to_string $ canonical f); ) schemas;
        proceed ();
        printf "\t \t 3. Checking if a key candidate present in schemas:\n";
        printf "\t \t ==================================================\n";

        let keys = get_key_cand s f in
        printf "Key candidates are: { %s }\n\n" 
                $ String.concat "," (List.map sch_to_string keys);

        let key_schemas =  match check_for_prim_key s f schemas with 
            | Some (s,f) -> 
                printf "Schema %s already contains a key candidate.\n\n"
                        (sch_to_string s);
                schemas
            | None       -> 
            (   
                printf "No key candidate covered, proceeding to make additional schema\n\n";
                let key = List.nth keys 0; (* just pick the first one, must always exist *) in 
                let key_dep = fitting_deps key (closure s f) in

                printf "\tSchema: %s \n \tFunctional dependencies: %s \n\n" 
                        (sch_to_string key) (fdep_to_string key_dep );
                (key,key_dep)::schemas 
            ) in

        proceed ();
        printf "\t \t 4. Removing all schemas which are subsets of others:\n";
        printf "\t \t ====================================================\n";

        let final_schemas = 
            List.filter (fun (s,f) -> not $ List.exists (fun (s2,f2) -> subset s2 s && not $ equal s2 s) key_schemas) key_schemas in 

        List.iter (fun (s,f) -> 
            printf "\tSchema: %s \n \tFunctional dependencies: %s \n\n"
                    (sch_to_string s) (fdep_to_string $ canonical f); ) final_schemas;


        printf "Schemas are all in 3rd Normalform. \n\n"
    )

(* Predicates for key_exercises  *)
let neither schema fdep = 
    not $ is_in_3NF schema fdep 

let third_only schema fdep = 
    (is_in_3NF schema fdep ) && (not $ is_in_bcnf schema fdep)

(* Try to generate useful fdeps for some predicate normform *)
let key_exercises normform input () =
    let fdep_generate schema : functional_dep = 
        let shuffle d =
            let nd = List.map (fun c -> (Random.bits (), c)) d in
            let sond = List.sort Pervasives.compare nd in
            List.map snd sond in    
        let get_subschema schema = List.nth (shuffle $ List.filter (fun a -> cardinal a > 0) (powerset schema)) 0 in
        Random.self_init ();
        let num_deps = (Random.int 2 ) + 4 in
        List.init num_deps (fun _ -> (get_subschema schema ,get_subschema schema))  in
    let rec repeat_until p f x = 
        let temp = f x in 
        if p temp then temp
        else repeat_until p f x  in 
    let predicate (fdeps:functional_dep ) = 
        (normform input fdeps) &&
        (List.length $ get_key_cand input fdeps) <= 3 &&  
        (List.for_all (fun a -> cardinal a < 4) $ get_key_cand input fdeps)  in

    if equal input empty then  
    (   printf "Can't work on empty schema \n"; () )
    else    
    (  printf "\n\nSchema \t\t %s" (sch_to_string input);

        printf "\n\nProducing random dependency: \n"; 
        let fdep = repeat_until predicate fdep_generate input in 

        printf "\n\t\t%s,\nCanonical: \t%s"  
               (fdep_to_string fdep ) (fdep_to_string $ canonical fdep );
        printf "\nWith keys: \t{ %s }" $ 
               String.concat ", " ( List.map sch_to_string $ get_key_cand input fdep)  
    )

(* TODO extend this to give out more information too *)
let latex_transformer (s,f) () = 
    let fdep_to_latex (t:functional_dep) = 
        let buf = Buffer.create ((List.length t)*10) in
        Buffer.add_string buf "\\{";
        List.map  (fun (a,b) -> String.concat "" ["\\fd{";sch_to_string a;"}{";sch_to_string b;"}"] ) t 
        |> (Buffer.add_string buf $$ String.concat "," ) ;
        Buffer.add_string buf "\\}";
        Buffer.contents buf in

    printf "\nWorking schema: %s\n\n" (sch_to_string s); 
    printf "Functional dep.\t %s \n" (fdep_to_string  f);
    printf "Latex import\t %s \n\n" (fdep_to_latex  f);

    printf "Keys\t { %s } \n\n" 
          $$ String.concat ", " $ List.map sch_to_string (get_key_cand s f)

let rec robust_read parser = 
    try ( parser (robust read_line ()) )
    with _ -> printf "\nWrong input, try again:"; robust_read parser

let get_input_fdep () = 
    printf "Enter the functional dependencies (Format: [ATR {-,->,→} ATR], ... )  : ";
    robust_read get_fdep 

let rec get_input_schema () = 
    printf "\n\nEnter the schema (use format ABCDEF...  Order does not matter) : ";
    robust_read get_schema     

let get_input_full () = 
    let implicit_schema fdep = 
        List.fold_right (fun (a,b) c -> c @@ b @@ a) fdep empty in
    let s = get_input_schema () in 
    let f = get_input_fdep () in 
    if equal s empty then 
    (  printf "\nEmpty schema entered, derived minimal schema %s based on dependencies.\n"
        (sch_to_string $ implicit_schema f) ;
       implicit_schema f, f )
    else  s,f

let rec ask_repeat f read main () : unit = 
    let rec ask_repeat f input read main () : unit = 
        f input ();
        printf "\n\nRun the algorithm again? [Y]es (Same Input)/[D]ifferent Input/[B]ack to selection/[Q]uit:";
        let answer = String.concat "" [robust read_line ();" "]  in 
        match (Char.uppercase_ascii $ String.get answer 0 ) with 
        | 'Y' | 'y'     -> ask_repeat f input read main ()  
        | 'D' | 'd'     -> ask_repeat f (read ()) read main () 
        | 'B' | 'b'     -> main ()
        |  _            -> () in
    ask_repeat f (read ()) read main ()

let rec main () : unit = 
   let options =  
        [("Synthesis algorithm (for 3NF)"          ,`Full   synthesis_procedure           ); 
         ("Decomposition algorithm (for BCNF)"     ,`Full   decomposition                 );
         ("Produce random dependencies (Not 3NF )" ,`Schema (key_exercises neither        ));
         ("Produce random dependencies (3NF only)" ,`Schema (key_exercises third_only     ));
         ("Produce random dependencies (In BCNF)"  ,`Schema (key_exercises is_in_bcnf     ));
         ("Functional Dependency tools "           ,`Full   latex_transformer             )]
    in

    List.iteri (fun i (a,_) -> printf "%n.\t %s\n" i a; ) options;
    printf "%n.\t I had enough of Normalforms for today (quit program)\n" (List.length options);

    printf "\n\nWhich algorithm do you want to see presented? Your choice: ";
    match robust read_int () with 
         n when n >= 0 && n < List.length options     -> 
            (   match List.nth options n with 
                | _,`Full   b   ->  ask_repeat b get_input_full   main ()
                | _,`Schema b   ->  ask_repeat b get_input_schema main ()
                | _,`Fdep   b   ->  ask_repeat b get_input_fdep   main ()  )
        | n when n = List.length options    -> ()
        | _                                 -> printf "You chose poorly, try again\n\n"; main () 

(* Don't start the program, if run in the OCaml toplevel *)
let () = 
    if (not !Sys.interactive) then
    (
        printf "\t\t    _____   ____ _____ _   _ _______ \n";
        printf "\t\t   |  __ \\ / __ \\_   _| \\ | |__   __|\n";
        printf "\t\t   | |__) | |  | || | |  \\| |  | |   \n";
        printf "\t\t   |  ___/| |  | || | | . ` |  | |   \n";
        printf "\t\t   | |    | |__| || |_| |\\  |  | |   \n";
        printf "\t\t   |_|     \\____/_____|_| \\_|  |_|   \n";
        printf "\t\t                                     \n";
        printf "\tPresentation Of Interactive Normalform Transformations \n\n";
        printf "This tool showcases transformations to achieve 3rd and Boyce-Codd Normalform\n";
        printf "(and contains some other useful tools)\n\n";
        main ()
    )