
signature OPTION_EXT =
sig

include OPTION

val compare: ('a * 'a -> order) -> 'a option * 'a option -> order
val extract: 'a option * exn -> 'a

end
