
signature VECTOR_EXT =
sig

include VECTOR

val toList: 'a vector -> 'a list
val toString: ('a -> string) -> 'a vector -> string

(**
 * @exception Domain if the vectors have uneven length
 *)
val map2: ('a * 'b -> 'c) -> 'a vector * 'b vector -> 'c vector

val slice: 'a vector * int * int option -> 'a vector

(**
 * Copy a vector to a (mutable) array
 *)
val array: 'a vector -> 'a array

val all2: ('a * 'b -> bool) -> 'a vector * 'b vector -> bool

val alli: (int * 'a -> bool) -> 'a vector -> bool

end
