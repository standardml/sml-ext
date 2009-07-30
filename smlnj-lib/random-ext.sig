
signature RANDOM_EXT =
sig

include RANDOM

(**
 * Generate a random vector [v] of reals such that
 * lower_i <= v_i <= upper_i 
 * Assumes lower/upper have the same length and that
 * lower_i <= upper_i
 *)
val randRealVector: rand -> {lower: real vector,
                             upper: real vector} -> real vector
val randRealArray: rand -> {lower: real vector,
                            upper: real vector} -> real array

(* see split in Haskell's library *)
val split: rand -> rand * rand
val splitN: rand * int -> rand list

end
