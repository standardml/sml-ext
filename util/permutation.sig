
signature PERMUTATION =
sig

type t 

exception Permutation of string

(**
 * @exception Permutation iff [isPerm l] ~~ [false]
 *)
val fromList: int list -> t

(**
 * extend ([1,3,5],7) ~~> a permutation representing [1,3,5,0,2,4,6]
 *)
val extend: int list * int -> t

(** 
 * [true] iff the list represents a 
 * permutation, i.e. a nonempty list without duplicates of the numbers from 0 -- (n-1)
 *)
val isPerm: int list -> bool

(* Smallest non-identity index. *)
val size: t -> int

val compose: t * t -> t

(**
 * apply (compose(p,inverse p)) l = l
 *)
val inverse: t -> t

(**
 * If the input list is too short, use the supplied element 
 *)
val applyList: t -> 'a -> 'a list -> 'a list
val applyVec: t -> 'a -> 'a vector -> 'a vector
val applyList': t -> 'a list -> 'a list
val applyVec': t -> 'a vector -> 'a vector

end
