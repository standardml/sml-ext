
(**
 * The standard form of linear programs is rather inflexible.  Thus,
 * we extend the syntax to allow free variables, maximization, etc.
 * These extensions are purely syntactic, in the sense that the problem
 * is transformed to the standard from using [standardize].   
 *)
signature LP =
sig

structure Var: NAME
structure Name: NAME
structure S: STANDARD_LP

datatype 'a var_ty = NonNegative 
		   | Free
		   | Geq of 'a
		   | Leq of 'a
		   | Eq of 'a
		   | Boxed of {lo: 'a, hi: 'a} 

datatype 'a bound = CGeq of 'a
		  | CLeq of 'a
		  | CEq of 'a
		  | CBoxed of {lo: 'a, hi: 'a} 
	                      
datatype dir = Minimize | Maximize

type 'a monom = 'a * Var.t

type 'a lcomb = 'a monom list

datatype 'a constr = Constr of {name: Name.t,
                                body: 'a lcomb,
                                bound: 'a bound}

(* Generic operators.  This allows things like interval arithmetic
 checking of dual variables.  *) 

datatype 'a ops = Ops of {zero: 'a,
                          one: 'a,
                          opp: 'a -> 'a,
                          leq: 'a * 'a -> bool}

val realOps: real ops

(* Linear programs *) 

datatype 'a lp = Lp of {name: string,
	                dir: dir,
                        vars: (Var.t * 'a var_ty) list,
                        obj: 'a lcomb,
                        constrs: 'a constr list}

val morphLp: ('a -> 'b) -> 'a lp -> 'b lp

val standardize: 'a ops -> 'a lp -> 'a S.lp

type answer

val solve: real lp -> answer

datatype status = OK
		| Infeasible
		| Unbounded
		| Error

val liftStatus: S.status -> status

val status: answer -> status
val objective: real lp * answer -> real
val colPrimal: answer * Var.t -> real
val rowDual: answer * Name.t -> real

(* Write the problem in standard form to a file *) 
val writeProblem: real lp * string -> unit

(* Printing *) 
val pp: ('a -> PP.pp) -> 'a lp -> PP.pp
val ppStatus: status -> PP.pp

end
