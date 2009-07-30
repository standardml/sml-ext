
structure Timing :> TIMING =
struct

open GeneralExt 
open PP.Ops

type center = unit
type sum = unit
val init = Fun.id
val newCenter = Fun.const ()
val reset = Fun.id
fun time _ f x = f x
fun sumCenter _ = ()
fun pp _ = $"Timing is off"
val ppSum = pp

end
