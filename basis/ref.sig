
signature REF =
sig

val incr: int ref -> unit
val incr': LargeInt.int ref -> unit
val decr: int ref -> unit
val repeat: int * ('a -> unit) * 'a -> unit
val ! : 'a ref -> 'a
val := : 'a ref * 'a -> unit

end
