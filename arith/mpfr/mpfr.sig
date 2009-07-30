
signature MPFR =
sig

type t

val toString: t -> string
val fromString: string -> t
val fromReal: real -> t
val fromInt: int -> t
val toReal: t -> real

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
val infinite: t -> bool
val finite: t -> bool
val isNan: t -> bool
val isZero: t -> bool
val zero: t
val one: t
val pi: t
val nan: t
val compare: t * t -> order 

(* FIXME: This is a hack to use MPFR with MPFI.  
 Is there a better way? *) 
val fromPtr: MLton.Pointer.t -> t
val asPtr: t * (MLton.Pointer.t -> 'a) -> 'a

end
