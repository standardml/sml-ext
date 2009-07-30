

signature REAL_EXT =
sig

include REAL

val nan : real

(** 
 * Generate the bits (in hex) of a [real] 
 *)
val to64BitString : real -> string

(**
 * Assumes 64 bit float, and we are given a 16 digit (hex) string. 
 *)
val from64BitString : string -> real option

val npow : real * int -> real

end
