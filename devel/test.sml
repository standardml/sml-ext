
structure Test =
struct 

structure T = Trace

fun fib 0 = 1
  | fib 1 = 1
  | fib n = fib' (n-1) + fib' (n-2)

and fib' n = T.traceArgs ("fib", PP.int, PP.int) fib n

fib 5


end
