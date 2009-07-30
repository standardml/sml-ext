
structure TimerExt: TIMER_EXT =
struct 

structure T = Timer
open T

fun checkCPUTimer' t = 
    let
       val {usr, sys} = T.checkCPUTimer t
    in
       Time.+(usr, sys)
    end
       
end
