
signature PAIR =
sig

datatype ('a, 'b) product = & of 'a * 'b
                                 
val pair: 'a -> 'b -> 'a * 'b
val pair1: 'a -> 'a * 'a
val swap: 'a * 'b -> 'b * 'a
val papp: ('a -> 'b) -> 'a * 'a -> 'b * 'b
val pappp: ('a -> 'b) -> ('c -> 'd) -> 'a * 'c -> 'b * 'd
val papp1: ('a -> 'b) -> 'a * 'c -> 'b * 'c
val papp2: ('b -> 'c) -> 'a * 'b -> 'a * 'c
val fst: 'a * 'b -> 'a
val snd: 'a * 'b -> 'b
val toString: ('a -> string) -> ('b -> string) -> 'a * 'b -> string

end
