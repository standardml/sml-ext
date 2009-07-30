
signature CPLEX =
sig

  type lp 

  exception Infeasible
  exception Unbounded
  exception Error

  datatype dir = Minimize | Maximize

  datatype kind = Leq
                | Eq
                | Geq

  val make : {name : string,
              numrows : int,
              numcols : int,
              dir : dir,
              obj : real array,
              rhs : real array,
              sense : kind array,
              matbeg : int array,
              matcnt : int array,
              matind : int array,
              matval : real array,
              lb : real array,
              ub : real array,
              rowname : string array,
              colname : string array} -> lp
              
  val numrows : lp -> int

  val numcols : lp -> int
 

  (**
   * Optimize the LP.  Raises [Error] if there's a problem.
   *)
  val opt : lp -> unit

  (**
   * true iff the problem is both primal and dual feasible 
   *)
  val feasible : lp -> bool

  (**
   * Get the objective value after a call to [opt]
   * Raises [Unbounded] if the solution doesn't exits
   *)
  val obj : lp -> real

  (**
   * Get the values of the primal (column) variables
   *)
  val prim_vars : lp -> real array

  (**
   * Get the values of the dual (row) variables
   *)
  val dual_vars : lp -> real array

  (**
   * Write problem to file
   *)
  val write : lp * string -> unit

end
