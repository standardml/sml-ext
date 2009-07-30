
signature HASH_TABLE_EXT =
sig

include HASH_TABLE

val all: ('a * 'b -> bool) -> ('a, 'b) hash_table -> bool
val pp: ('a * 'b -> PP.pp) -> ('a, 'b) hash_table -> PP.pp

end
