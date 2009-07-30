
signature GENERAL_EXT =
sig

exception Impossible
exception Unimplemented
val printl: string -> unit
val fst: 'a * 'b -> 'a
val snd: 'a * 'b -> 'b
val isNone: 'a option -> bool

datatype ('a, 'b) either = Left of 'a
                         | Right of 'b

end
