
structure GeneralExt :> GENERAL_EXT where type ('a, 'b) either = ('a, 'b) Either.either =
struct 

exception Impossible
exception Unimplemented
fun printl s = print (s ^ "\n")
fun fst (x,_) = x
fun snd (_,y) = y
fun isNone NONE = true
  | isNone _ = false
val option = OptionExt.option
val id = Fun.id
val curry = Fun.curry
val swap = Fun.swap
val flip = Fun.flip
fun list x = [x]
open Either

end
