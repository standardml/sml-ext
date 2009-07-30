
(* Do tracing by indentation *) 

functor TraceDepthFn(val traceP: bool) :> TRACE =
struct 

open PP.Ops

val traceP = traceP

(* -------------------------------------------------------------------------- *)
(*  Depth                                                                     *)
(* -------------------------------------------------------------------------- *)

local
   val depth = ref 0
in

fun indent () = Ref.incr depth
fun outdent () = Ref.decr depth
fun printCol () = !depth

end (* local *) 

fun message p = if traceP
                   then PP.pp (%[PP.space (printCol()), p])
                else ()

fun handleExn (name, exn) = 
    let in 
       outdent ()
     ; PP.pp (%[PP.space (2 + printCol()), $name, $" raises ", $(exnName exn)])
     ; raise exn
    end

fun trace name f x = 
    if not traceP then f x else
    let
       val col = printCol()
       val inp = %[PP.space col, $name, $" <-- "]
       val _ = PP.pp inp
       val _ = indent()
       val res = f x
       val outp = %[PP.space col, $name, $" --> "]
    in 
       outdent()
     ; PP.pp outp
     ; res
    end 
    handle exn => handleExn(name, exn)

fun traceArgs (name, ppA, ppB) f x = 
    if not traceP then f x else
    let
       val col = printCol()
       val top = %[PP.space col, $name, $" <-- ", ppA x]
       val _ = PP.pp top
       val _ = indent()
       val res = f x
       val bot = %[PP.space col, $name, $" --> ", ppB res]
                                   
    in 
       outdent()
     ; PP.pp bot
     ; res
    end
    handle exn => handleExn(name, exn)

end
