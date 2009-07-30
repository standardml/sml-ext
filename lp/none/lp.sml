
structure StandardLp :> STANDARD_LP = 
   struct 

      structure Var = NameFn(val name = "Var")
      structure Name = NameFn(val name = "Name")
      open GeneralExt 

      type 'a monom = 'a * Var.t 

      type 'a lcomb = 'a monom list

      datatype 'a constr = Constr of {name: Name.t,
                                      body: 'a lcomb,
                                      bound: 'a}

      datatype 'a lp = Lp of {name: string,
                              vars: Var.t list,
                              obj: 'a lcomb,
                              constrs: 'a constr list}

      fun writeProblem _ = raise Unimplemented 

      type answer = unit

      fun solve _ = raise Unimplemented 

      datatype status = OK
		      | Infeasible
		      | Unbounded
		      | Error

      fun status _ = raise Unimplemented 
      fun objective _ = raise Unimplemented 
      fun colPrimal _ = raise Unimplemented 
      fun rowDual _ = raise Unimplemented 
      fun morphLp _ _ = raise Unimplemented 
      fun pp _ _ = raise Unimplemented 
      fun ppAnswer _ = raise Unimplemented 

   end

structure Lp = LpFn(StandardLp)
