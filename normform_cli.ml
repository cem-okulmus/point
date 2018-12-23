(* 
 *  The command line interface for POINT.
 *  
 * 
 *)
open Printf
open Normform
open Normform_base
open Schema


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
        let answer = robust read_line () ^ " "  in 
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
         ("Produce random dependencies (Not 3NF )" ,`Schema (key_exercises neither)       );
         ("Produce random dependencies (3NF only)" ,`Schema (key_exercises third_only)    );
         ("Produce random dependencies (In BCNF)"  ,`Schema (key_exercises is_in_bcnf)    );
         ("Functional Dependency tools "           ,`Full   (fun a -> printf "%s" $$ latex_transformer a  )            )]
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