
signature INT_EXT =
sig

include INTEGER

val log: int -> int -> int
(** log base n *)

val sqrt: int -> int
val log2: int -> int
val log10: int -> int
val pow: int * int -> int
val sum: int list -> int
val odd: int -> bool
val even: int -> bool
val i0: IntInf.int
(* the constant 0 *)

val i1: IntInf.int
(* the constant 1 *)

val infToReal: IntInf.int -> real
val maxList: int list -> int
val minList: int list -> int

end
