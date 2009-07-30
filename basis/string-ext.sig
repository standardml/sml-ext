
signature STRING_EXT =
sig

include STRING

val isCapitalized: string -> bool        
val capitalize: string -> string
val upcase: string -> string
val downcase: string -> string

(**
 * Return the first index of a char in a string. 
 *)
val index: string -> char -> int option

(**
 * Expensive!  Copies string.  
 *)
val update: string * int * char -> string
val space: int -> string
val nullTerminate: string -> string

(**
 * splitString #"_" "to_be_or_not_to_be" ~~> ["to","be","or","not","to","be"] 
 *)
val splitString: char -> string -> string list

(**
 * Simple tokenizer based on single character separators
 *)
val chop: char list -> string -> string list

(** [abc] *)
val bracket: string -> string

(** <abc> *)
val abracket: string -> string

(** [|abc|] *)
val arrbracket: string -> string

(** a,b,c *) 
val commas: string list -> string

(** a, b, c *)                               
val scommas: string list -> string 

(** {abc} *)
val curly: string -> string

(** (abc) *)
val paren: string -> string

(** a;b;c *)
val semis: string list -> string

(** a; b; c *)
val ssemis: string list -> string 

(** "s" *)
val quote: string -> string

(** a b c *)
val spaces: string list -> string 

(* Truncate a string at the first null character *) 
val truncateCString: string -> string

end
