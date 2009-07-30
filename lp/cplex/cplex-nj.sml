
structure Cplex :> CPLEX =
struct 

  type lp = unit

  datatype dir = Minimize | Maximize

  datatype kind = Leq
                | Eq
                | Geq

  exception Infeasible
  exception Unbounded
  exception Error

  fun fail () = raise Fail "You can't do that in NJ"
  fun make _ = fail ()
  fun numrows _ = fail ()
  fun numcols _ = fail ()
  fun opt _ = fail ()
  fun obj _ = fail ()
  fun prim_vars _ = fail ()
  fun dual_vars _ = fail ()
  fun write _ = fail ()
  fun feasible _ = fail ()

end
