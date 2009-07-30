

(* 
 Trace is a debugging facility.  It prints entry and exit
 to a function call, optionally printing inputs and outputs.
 
 Note the the implementations should be functors over whether 
 to do tracing or not.  This will allow MLton to eliminate
 all the trace calls.  (Right?)
 *) 
signature TRACE = 
sig

val traceP: bool

(* Act like Ocaml trace *) 
val trace: string -> ('a -> 'b) -> ('a -> 'b)
val traceArgs: string * ('a -> PP.pp) * ('b -> PP.pp) 
               -> ('a -> 'b) -> ('a -> 'b)

(* Print a message whilein trace mode *) 
val message: PP.pp -> unit

end
