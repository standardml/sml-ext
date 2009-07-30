
(* Count calls instead of time *)
structure Counting :> TIMING =
struct

open PP.Ops

type center = string * int ref
type sum = string * center list

fun init () = ()

fun newCenter (name) = (name, ref 0)

fun reset (_, counters) = (counters := 0)

fun time (_, counters) (f:'a -> 'b) (x:'a) =
    let
       val _ = counters := !counters + 1
    in
       f x
    end

fun sumCenter (name, l) = (name, l)

fun pp' (name, n) = %[$name, $": ", PP.int n]

fun pp (name, ref n) = pp' (name, n)

fun ppSum (name, centers) = 
    let fun sumup (nil, total) = pp' (name, total)
	  | sumup ((_, ref n)::centers, total) =
	    sumup (centers, total+n)
    in 
       sumup (centers, 0)
    end

end
