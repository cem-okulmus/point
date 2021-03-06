(* 
 *  Provides the basic functionality and acts as a library for
 *  the two interfaces, one for command line, one for the web. 
 * 
 *)

open Printf
open Base
open Schema

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
  List.filter (fun (a,b) -> (not $ list_mem a skeys) && (not $ subset b a ) ) fdep

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
  let schema_b = schema -- (b -- a) in
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

let decompose (fdep:functional_dep ) = 
  List.flatten $ List.map (fun (a,b) -> List.map (fun c -> a, singleton c) $ to_list b) fdep 

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
  |> List.sort_uniq (fun (s1,_) (s2,_) -> Schema.compare s1 s2)

let check_for_prim_key schema fdep = 
  let keys = get_key_cand schema fdep in 
  List.find_opt (fun (s,f) -> List.exists (fun (a,b) -> list_mem a keys) f)

(* Parser functions *)

let get_schema s :schema =
  let lexbuf = Lexing.from_string s in 
  (Pars.schema Lex.token) lexbuf;;

let get_fdep s :functional_dep =
  let lexbuf = Lexing.from_string s in 
  Pars.fun_dep Lex.token lexbuf;;

(* Interactive procedures *)

let check_violating_deps res = 
  let current = 
    "Current schemas: \n\n" ^
    (List.fold_left (fun old (num,s,f) -> 
         old ^ sprintf "Schema_%n:\t%s\nFunctional Dep:\t%s \n\n" 
           num (sch_to_string s) (fdep_to_string  f )) "" res) in
  match List.find_opt (fun (_,s,f) -> not $ is_in_bcnf s f) res with
  | None                   ->     
    current ^ "Already in BCNF, nothing to be done here. 😀\n",[] 
  | Some (num,schema,fdep) -> (   
      let violating =  get_bcnf_violating_deps schema fdep in 
      current ^
      sprintf "\tSchema_%n: %s is not in BCNF! \n\n" num (sch_to_string schema)                          ^
      "Following violating dependencies found: \n" ^
      ((List.mapi (fun i (a,b) -> sprintf "%n. (%8s → %-8s)\n" 
                      i (sch_to_string a) (sch_to_string b) ) violating)
       |> String.concat "") ^

      "\nChoose how to proceed.",violating
    )

(* res :=  { (n,s,f) | n .. number, s ..schema, f .. fun. dep. } *)
let decomposition_procedure k res =  
  let n =  (*   n :=  increasing counter, determined from res  *)
    (+) 1 $ List.fold_left (fun tmp (k,_,_) -> max tmp k ) 0 res in
  match List.find_opt (fun (_,s,f) -> not $ is_in_bcnf s f) res with
  | None                   ->    "",[] (* indicates that all is done *)  
  | Some (num,schema,fdep) -> (   
      let violating =  get_bcnf_violating_deps schema fdep in 
      let (s,f) = (List.nth violating k) in 
      let (s1,f1),(s2,f2) = decompose_schema schema fdep (s,f)  in 

      sprintf "\n\nDecomposing existing Schema_%n: %s via %s: \n" 
        num (sch_to_string schema) (fdep_to_string [(s,f)]) ^
      sprintf "\n\tSchema_%n: %s \nFunctional Dep: %s  \n" 
        (n) (sch_to_string s1 )  (fdep_to_string $ canonical f1) ^
      sprintf "\n\tSchema_%n: %s \nFunctional Dep: %s \n" 
        (n+1) (sch_to_string s2 )  (fdep_to_string $  canonical f2),

      List.filter ( (<>) (num,schema,fdep) ) (res@[((n),s1,canonical f1);((n+1),s2,canonical f2)] )
    )

let synthesis_procedure (s,f)  () : string list =
  (*     let proceed () = 
          printf "Press enter to proceed ...:";
          ignore $ read_line ()   in *)
  if (is_in_3NF s f) then 
    let to_tuple a = [a;a;a;a](* aaaaah *) in 
    to_tuple "Already in 3rd normal form, nothing to be done here. 😀\n"
  else 
    (
      let left = leftred f in 
      let right = rightred left in 
      let comb = combine_step right in 

      let schemas = create_schemas (closure s f) comb in  (* need closure here*)
      let keys = get_key_cand s f in

      let key_schemas =  match check_for_prim_key s f schemas with 
        | Some (s,f) -> schemas
        | None       -> 
          let key = List.nth keys 0; (* just pick the first one, must always exist *) in 
          let key_dep = [] in (* not hard to see why this must be empty*)
          (key,key_dep)::schemas  in

      let final_schemas = 
        List.filter (fun (s,f) -> 
            not $ List.exists (fun (s2,f2) -> subset s s2 && not $ equal s2 s) key_schemas) key_schemas in 
      let redundant_schemas = 
        let final_schemas = List.map fst final_schemas in 
        let key_schemas = List.map fst key_schemas in 
        let pick_superset s = 
          List.nth ( List.filter (fun s2 -> subset s s2) final_schemas ) 0 in
        List.filter (fun a -> not $ list_mem a final_schemas) key_schemas 
        |> List.map (fun a -> a, pick_superset a ) in 

      let first = 

        "\t \t 1. Calculating canonical form of schema:\n" ^
        "\t \t ========================================\n" ^

        "Left-reduction: \n" ^
        sprintf "\t\t%s\n" (fdep_to_string left) ^
        sprintf "(with decompose)%s" (fdep_to_string $$ leftred $ decompose f) ^

        "\n\nRight-reduction: \n" ^
        sprintf "\t\t%s\n" (fdep_to_string right) ^ 
        sprintf "(with decompose)%s" (fdep_to_string $$ rightred $$ leftred $ decompose f) ^

        "\n\nCombination step: \n\n" ^
        sprintf "%s\n\n" (fdep_to_string comb) in

      let second = 
        "\t \t 2. Creating new schemas based on canonical form:\n" ^
        "\t \t ================================================\n" ^

        (List.fold_left (fun old (s,f) ->  
             old ^
             sprintf "\tSchema: %s\t with keys %s \n \tFunctional dependencies: %s \n\n"
               (sch_to_string s) (slist_to_string $ get_key_cand s f) (fdep_to_string $ canonical f) ) "" schemas) in

      let third = 
        "\t \t 3. Checking if a key candidate present in schemas:\n" ^
        "\t \t ==================================================\n" ^

        sprintf "Key candidates are: %s \n\n" (slist_to_string keys) ^

        (   match check_for_prim_key s f schemas with
            | None  ->    
              "No key candidate covered, proceeding to make additional schema\n\n" ^
              let key,key_dep = List.nth key_schemas 0 in
              sprintf "\tSchema: %s\t with keys %s \n \tFunctional dependencies: %s \n\n" 
                (sch_to_string key)(slist_to_string $ get_key_cand key key_dep) (fdep_to_string key_dep )
            | Some (s,f) ->
              sprintf "Schema %s already contains a key candidate.\n\n"(sch_to_string s) 
        ) in

      let fourth = 
        "\t \t 4. Removing all schemas which are subsets of others:\n" ^
        "\t \t ====================================================\n" ^
        ( if ( redundant_schemas != []) then 
            "\nRemoved following schemas:\n" ^

            (List.fold_left (fun old (s,s2) -> 
                 old ^ sprintf "\tSchema: %s ⊆ %s \n" (sch_to_string s) (sch_to_string s2) )  "" redundant_schemas) 
          else "") ^
        "\nFinal schemas:    \n" ^
        (List.fold_left (fun old (s,f) -> 
             old ^ sprintf "\tSchema: %s\t with keys %s \n \tFunctional dependencies: %s \n\n"
               (sch_to_string s)  (slist_to_string $ get_key_cand s f) (fdep_to_string $ canonical f) ) "" final_schemas ) ^
        "Schemas are all in 3rd Normalform. \n\n" in
      [first;second;third;fourth]
    )

(* Predicates for key_exercises  *)

let predicate ?active:(a=true) p =
  if a then 
    p
  else 
    (fun a b -> true)

let all_violating schema fdep = 
  let keys = get_key_cand schema fdep in
  List.for_all (fun (a,_) -> not $ list_mem a keys ) fdep 

let neither schema fdep = 
  not $ is_in_3NF schema fdep 

let third_only schema fdep = 
  (is_in_3NF schema fdep ) && (not $ is_in_bcnf schema fdep)

(* Try to generate useful fdeps for some predicate normform *)
let key_exercises normform size_subschema num_keys length_keys input () =
  let fdep_generate () : functional_dep = 
    let rec get_subschema_rec temp  =
      let choose s  = List.nth (to_list s) (Random.int (cardinal s)) in
      if cardinal temp >= size_subschema then 
        temp 
      else 
        get_subschema_rec (add (choose (input -- temp)) temp) in 
    let get_subschema () =
      if size_subschema <= 0 then 
        empty
      else if (size_subschema >= cardinal input) then 
        input
      else 
        get_subschema_rec empty in     
    Random.self_init ();
    let num_deps = (Random.int 2 ) + 4 in
    List.init num_deps (fun _ -> (get_subschema () ,get_subschema ()))   in
  let rec repeat_until p f = 
    let temp = f () in 
    if p temp then temp
    else repeat_until p f  in 
  let predicate (fdeps:functional_dep ) = 
    (normform input fdeps) &&
    (List.length $ get_key_cand input fdeps) = num_keys &&  
    (List.for_all (fun a -> cardinal a = length_keys) $ get_key_cand input fdeps)  in

  if equal input empty then  
    "Can't work on empty schema 🙁\n" 
  else    
    let fdep = repeat_until predicate fdep_generate in 
    sprintf "\n\nSchema \t\t %s" (sch_to_string input)
    ^
    "\n\nProducing random dependency: \n"
    ^
    sprintf "\n\t\t%s\nCanonical: \t%s"  
      (fdep_to_string fdep ) (fdep_to_string $ canonical fdep ) 
    ^
    sprintf "\nWith keys: \t %s "  (slist_to_string $  get_key_cand input fdep)


(* TODO extend this to give out more information too *)
let latex_transformer (s,f) () = 
  let fdep_to_latex (t:functional_dep) = 
    let buf = Buffer.create ((List.length t)*10) in
    Buffer.add_string buf "\\{";
    List.map  (fun (a,b) -> "\\fd{" ^ sch_to_string a ^ "}{" ^ sch_to_string b^ "}" ) t 
    |> (Buffer.add_string buf $$ String.concat "," ) ;
    Buffer.add_string buf "\\}";
    Buffer.contents buf in    
  (sprintf "Functional dep.\t %s \n"   (fdep_to_string  f))              ^
  (sprintf "               \t %s \n\n" (fdep_to_latex   f))              ^
  (sprintf "Canonical Cover\t %s \n"   (fdep_to_string $ canonical  f))  ^
  (sprintf "               \t %s \n\n" (fdep_to_latex  $ canonical  f )) ^
  (sprintf "Keys\t\t %s\n\n" (slist_to_string $ get_key_cand s f ) ) ^
  if (is_in_bcnf s f) then 
    "Is in Boyce-Codd Normalform."
  else if (is_in_3NF s f) then     
    "Is in 3rd Normal Form, but not BCNF."
  else
    "Neither in 3NF, nor BCNF."

let equiv_test fdep1 fdep2 = 
  try 
    let a,b = List.find (fun (a,b) -> not ( subset b (get_cover fdep2 a) ) ) fdep1 in
    sprintf "The dependency ( %s → %s ) of the first is not present in the other FD" 
      (sch_to_string a) (sch_to_string b)
  with _ -> 
  try
    let a,b = List.find (fun (a,b) -> not ( subset b (get_cover fdep1 a) ) ) fdep2 in
    sprintf "The dependency ( %s → %s ) of the second is not present in the other FD" 
      (sch_to_string a) (sch_to_string b) 
  with _ -> 
    "The dependencies are equivalent."