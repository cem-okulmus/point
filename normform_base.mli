module Schema :
  sig
    type elt = Char.t
    type t = Set.Make(Char).t
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val map : (elt -> elt) -> t -> t
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val min_elt_opt : t -> elt option
    val max_elt : t -> elt
    val max_elt_opt : t -> elt option
    val choose : t -> elt
    val choose_opt : t -> elt option
    val split : elt -> t -> t * bool * t
    val find : elt -> t -> elt
    val find_opt : elt -> t -> elt option
    val find_first : (elt -> bool) -> t -> elt
    val find_first_opt : (elt -> bool) -> t -> elt option
    val find_last : (elt -> bool) -> t -> elt
    val find_last_opt : (elt -> bool) -> t -> elt option
    val of_list : elt list -> t
  end
type schema = Schema.t
type functional_dep = (schema * schema) list
val ( $ ) : ('a -> 'b) -> 'a -> 'b
val ( $$ ) : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b
val ( @@ ) : schema -> schema -> schema
val ( -- ) : schema -> schema -> schema
val to_list : schema -> Schema.elt list
val flatten : schema list -> schema
val sch_to_string : schema -> string
val print_schema : Format.formatter -> schema -> unit
val fdep_to_string : functional_dep -> string
val print_fdep : Format.formatter -> functional_dep -> unit
val rem_dup : 'a list -> 'a list
val rem_dup_sch : functional_dep -> functional_dep
val remove_dep : functional_dep -> schema -> schema ->  functional_dep
val sanity : functional_dep -> functional_dep
val powerset : schema-> schema list
val fixpoint : ('a -> 'a) -> 'a -> 'a
val list_mem : schema -> schema list -> bool
val robust : ('a -> 'b) -> 'a -> 'b 
