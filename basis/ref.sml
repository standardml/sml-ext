
structure Ref :> REF =
struct

structure N = LargeInt

fun incr x = x := !x + 1

fun incr' (x: N.int ref) = x := !x + 1

fun decr x = x := !x - 1

local
   fun repeat' (0,(_:'a->unit),_) = ()
     | repeat' (n,f,x) = (f x; repeat' (n-1,f,x))
in
fun repeat (args as (n,_,_)) = 
    if n < 0 then 
       raise Domain
    else repeat' args
end (* local *) 

val ! = !
val op:= = op:=

end
