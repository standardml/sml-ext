
signature NAME = 
sig

type t

structure Set: ORD_SET_EXT where type Key.ord_key = t
structure Map: ORD_MAP_EXT where type Key.ord_key = t

val new: unit -> t
val newNamed: string -> t
val freshen: t -> t
val fromString: string -> t
val toString: t -> string
val name: t -> string
val pp: t -> PP.pp
val compare: t * t -> order
val eq: t * t -> bool
val hash: t -> Word.word
val reset: unit -> unit

end
