
signature GENERAL_EXT =
sig

exception Impossible
exception Unimplemented
val printl: string -> unit
val fst: 'a * 'b -> 'a
val snd: 'a * 'b -> 'b
val isNone: 'a option -> bool
val option: 'b -> ('a -> 'b) -> 'a option -> 'b
val id: 'a -> 'a
val curry: ('a * 'b -> 'c) -> 'a -> 'b -> 'c
val swap: ('a * 'b -> 'c) -> 'b * 'a -> 'c
val flip: ('a -> 'b -> 'c) -> 'b -> 'a -> 'c

datatype ('a, 'b) either = Left of 'a
                         | Right of 'b

end
