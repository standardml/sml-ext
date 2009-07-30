
(* 
[BASE] creates a problem in "standard" form:
      
Minimize c^T x
 s.t.     A x >= b
            x >= 0
  
 It might seem strange to have the ['a lp] be a polymorphic type.
 The reason is that if we wish to do an accurate verification using
 interval arithmetic, all the constants must be given as intervals.
 Thus, the user would enter the problem as an [interval lp], it would
 be transformed into a [real lp] to be passed to a solver.  The solution
 would use the dual variables to give approximate multiplers for the 
 rows, and the actual linear combinations of the rows would again be
 done by interval arithmetic on the original interval constants.  
 This is completely safe, as the multipliers can safely be approximate,
 and converted to (empty) intervals.  

 If the user only cares about real linear programs (the common case), 
 they can use the REAL_LP signatures.

 STANDARD_LP_BASE is currently implemeneted by bindings to
 GLPK and CPLEX.
 *)

signature STANDARD_LP_BASE =
   sig
      structure Var: NAME
      structure Name: NAME

      type 'a monom = 'a * Var.t

      type 'a lcomb = 'a monom list

      datatype 'a constr = Constr of {name: Name.t,
                                      body: 'a lcomb,
                                      bound: 'a}

      datatype 'a lp = Lp of {name: string,
                              vars: Var.t list,
                              obj: 'a lcomb,
                              constrs: 'a constr list}

      (** Write the problem in CPLEX format to a file. *)
      val writeProblem: real lp * string -> unit

      type answer

      val solve: real lp -> answer

      datatype status = OK
		      | Infeasible
		      | Unbounded
		      | Error

      val status: answer -> status
      val objective: answer -> real
      val colPrimal: answer * Var.t -> real
      val rowDual: answer * Name.t -> real
   end
   
signature STANDARD_LP = 
   sig
      include STANDARD_LP_BASE

      val morphLp: ('a -> 'b) -> 'a lp -> 'b lp
      val pp: ('a -> PP.pp) -> 'a lp -> PP.pp
      val ppAnswer: 'a lp * answer -> PP.pp
   end
