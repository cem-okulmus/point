
module Schema = Set.Make(Char)
open Schema

type schema = Schema.t  (* a 'schema' is the type for sets of attributes *)

(* module FDep =
    struct
        type t = schema * schema
        let compare (x0,y0) (x1,y1) =
            match Schema.compare x0 x1 with
            | 0 -> Schema.compare y0 y1
            | c -> c
    end

module FunDep = Set.Make(FDep)
 *)
type functional_dep = (schema * schema) list

let ( $ ) f a = f (a)
let ( $$ ) f g a = f (g a) (* infix functional composition *)
let (@@) a b = union a b
let (--) a b = diff a b

let to_list t = Schema.fold (fun e res -> e::res) t []

let flatten s = List.fold_right (fun a b -> a @@ b) s empty

let sch_to_string (t:Schema.t) = 
    let buf = Buffer.create (Schema.cardinal t) in 
    List.iter (Buffer.add_char buf $$ Char.uppercase_ascii) (List.rev $ to_list t);
    Buffer.contents buf

let print_schema fmt t = 
    Format.fprintf fmt "%s" $ sch_to_string t
    
let fdep_to_string (t:functional_dep) = 
    let buf = Buffer.create ((List.length t)*10) in
    Buffer.add_string buf "{ ";
    List.map  (fun (a,b) -> String.concat "" ["(";sch_to_string a;" → ";sch_to_string b;")"] ) t 
    |> (Buffer.add_string buf $$ String.concat ", " ) ;
    Buffer.add_string buf " }";
    Buffer.contents buf 

let sch_to_string t = 
    String.concat "" ["\x1B[3m";sch_to_string t;"\x1B[0m"]

let print_fdep fmt t  =     
    Format.fprintf fmt "%s" $ fdep_to_string t  ;;

let rem_dup a = List.sort_uniq Schema.compare a

let rem_dup_sch a = 
    let compare (x0,y0) (x1,y1) =match Schema.compare x0 x1 with
        | 0 -> Schema.compare y0 y1
        | c -> c in
    List.sort_uniq compare a

let remove_dep fdep (a:schema) (b:schema) = 
    List.filter (fun (c,d) -> not (equal a c && equal d b)) fdep  

(*  clean up empty dependencies, sort for comparability *)
let sanity fdep = 
    List.map (fun (a,b) -> (a, diff b a)) fdep
    |> List.filter (fun (a,b)-> not (is_empty a || is_empty b)) 
    |> rem_dup_sch

let powerset xs = 
    let xs = to_list xs in
    List.fold_right (fun x bag -> bag @ (List.map (fun ys -> x::ys) bag)) xs [[]]
    |> List.map of_list

let rec fixpoint f x =
    if  x = (f x) then x
    else fixpoint f (f x) 

let list_mem a s = 
    List.exists (fun x -> equal a x) s

let rec robust f x = 
    try (f x ) with _ -> Printf.printf "\nWrong input. Try again:"; robust f x