type 'a env = (string * 'a) list

val empty : 'a env

val find : string -> 'a env -> 'a option

val find_key: 'a -> 'a env -> string option

val add : string -> 'a -> 'a env -> 'a env

val remove: string -> 'a env -> 'a env 

val map : ('a -> 'b) -> 'a env -> 'b env

val union : 'a env -> 'a env -> 'a env