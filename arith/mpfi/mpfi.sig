
signature MPFI =
sig

type t

val toString: t -> string
val fromString: string -> t
val fromInt: int -> t
val add: t * t -> t
val sub: t * t -> t
val mul: t * t -> t
val div: t * t -> t
val neg: t -> t
val sqr: t -> t
val sqrt: t -> t
val inv: t -> t
val abs: t -> t
val atan: t -> t
val zero: t
val isZero: t -> bool
val one: t
val pi: t
val nan: t
val finite: t -> bool
val compare: t * t -> order option
(* val intersect: t * t -> t *)
(* val union: t * t -> t *)

val left: t -> MPFR.t
val right: t -> MPFR.t
val make: MPFR.t * MPFR.t -> t

end
