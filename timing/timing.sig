
(* Copied/Modified from Frank Pfenning's Twelf timers *)

signature TIMING =
sig

val init : unit -> unit

type center
val newCenter: string -> center
val reset: center -> unit
val time: center -> ('a -> 'b) -> ('a -> 'b)

type sum
val sumCenter: string * center list -> sum

val pp: center -> PP.pp
val ppSum: sum -> PP.pp

end
