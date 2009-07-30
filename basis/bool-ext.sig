
signature BOOL_EXT =
sig

include BOOL

val and': bool * bool -> bool
val or': bool * bool -> bool
val compare: bool * bool -> order

structure Ops: 
             sig
                val && : bool * bool -> bool
                val || : bool * bool -> bool
             end

end
