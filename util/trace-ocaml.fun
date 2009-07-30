
(* Do tracing like Ocaml. 

 This simulates the OCaml #trace directive, e.g.

  # let rec fib n = match n with 0 -> 1 | 1 -> 1 | n -> fib (n-1) + fib (n-2);;
  val fib : int -> int = <fun>
  # fib 5;;
  - : int = 8
  # #trace fib;;
  fib is now traced.
  # fib 5;;
  fib <-- 5
  fib <-- 3
  fib <-- 1
  ...
  fib --> 2
  fib --> 3
  fib --> 5
  fib --> 8
  - : int = 8

  Exceptions look like this:

  fib raises Failure "abc"
 *) 

functor TraceOcamlFn(val traceP: bool) :> TRACE =
struct 

open PP.Ops

val traceP = traceP

fun message p = if traceP then PP.pp p else ()

(* -------------------------------------------------------------------------- *)
(*  Ocaml                                                                     *)
(* -------------------------------------------------------------------------- *)

fun handleExn (name, exn) = 
    let in 
       PP.pp (%[$name, $" raises ", $(exnName exn)])
     ; raise exn
    end

fun trace name f x = 
    if not traceP then f x else
    let
       val inp = %[$name, $" <-- "]
       val outp = %[$name, $" --> "]
    in 
       PP.pp inp
     ; f x before PP.pp outp
    end 
    handle exn => handleExn(name, exn)

fun traceArgs (name, ppA, ppB) f x = 
    if not traceP then f x else
    let
       val inp = %[$name, $" <-- ", ppA x]
       val _ = PP.pp inp
       val res = f x
       val outp = %[$name, $" --> ", ppB res]
    in 
       PP.pp outp
     ; res
    end 
    handle exn => handleExn(name, exn)

end
