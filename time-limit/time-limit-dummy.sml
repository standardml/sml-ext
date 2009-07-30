
structure TimeLimit : 
             sig
                exception TimeOut
                val timeLimit : Time.time -> ('a -> 'b) -> 'a -> 'b
             end = 
struct

exception TimeOut
val _ = TimeOut
fun timeLimit _ f x = f x

end
