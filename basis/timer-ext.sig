
signature TIMER_EXT =
sig

include TIMER

(* Give the sum of the sys and user time *) 
val checkCPUTimer': cpu_timer -> Time.time

end
