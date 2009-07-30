
signature FUN = 
sig

val id: 'a -> 'a
val const: 'a -> 'b -> 'a
val flip: ('a -> 'b -> 'c) -> 'b -> 'a -> 'c
val swap: ('a * 'b -> 'c) -> 'b * 'a -> 'c
val pass: 'a -> ('a -> 'b) -> 'b
val curry: ('a * 'b -> 'c) -> 'a -> 'b -> 'c
val uncurry: ('a -> 'b -> 'c) -> 'a * 'b -> 'c
val ` : ('a -> 'b) * 'a -> 'b
val apply: 'a -> ('a -> 'b) -> 'b
val repeat: ('a -> 'a) * 'a -> 'a
val repeatN: int * ('a -> 'a) * 'a -> 'a
val compose: ('a -> 'c) * ('b -> 'a) -> 'b -> 'c

end
