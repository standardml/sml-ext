
structure GeneralExt :> GENERAL_EXT =
struct 

exception Impossible
exception Unimplemented
fun printl s = print (s ^ "\n")
fun fst (x,_) = x
fun snd (_,y) = y
fun isNone NONE = true
  | isNone _ = false
val option = OptionExt.option
datatype ('a, 'b) either = Left of 'a
                         | Right of 'b
val id = Fun.id
val curry = Fun.curry

end
