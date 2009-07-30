
functor LpFn(S: STANDARD_LP): LP =
struct 

open GeneralExt 
structure L = ListExt
structure S = S
structure Var = S.Var
structure Name = S.Name

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

datatype 'a ops = Ops of {zero: 'a,
                          one: 'a,
                          opp: 'a -> 'a,
                          leq: 'a * 'a -> bool}

val realOps = Ops {zero = 0.0,
		   one = 1.0,
		   opp = Real.~,
		   leq = Real.<=}

datatype 'a lp = Lp of {name: string,
	                dir: dir,
                        vars: (Var.t * 'a var_ty) list,
                        obj: 'a lcomb,
                        constrs: 'a constr list}

(* --------------------------------  morph  --------------------------------- *)

fun morphMonom f (x, y) = (f x, y)

fun morphLcomb f l = map (morphMonom f) l

fun morphConstrTy f (CGeq x) = CGeq (f x)
  | morphConstrTy f (CLeq x) = CLeq (f x)
  | morphConstrTy f (CEq x) = CEq (f x)
  | morphConstrTy f (CBoxed {lo, hi}) = CBoxed {lo = f lo, hi = f hi}

fun morphConstr f (Constr {name, body, bound}) = 
    Constr {name = name,
            body = morphLcomb f body,
            bound = morphConstrTy f bound}

fun morphVarTy _ NonNegative = NonNegative
  | morphVarTy f (Geq x) = Geq (f x)
  | morphVarTy f (Leq x) = Leq (f x)
  | morphVarTy f (Eq x) = Eq (f x)
  | morphVarTy f (Boxed {lo, hi}) = Boxed ({lo = f lo, hi = f hi})
  | morphVarTy _ Free = Free

fun morphLp f (Lp {name, dir, vars, obj, constrs}) =
    Lp {name = name,
        dir = dir,
        vars = map (fn (x, t) => (x, morphVarTy f t)) vars,
        obj = morphLcomb f obj,
        constrs = map (morphConstr f) constrs}

(*** Substitute a pair of variables for a single variable ***)

fun splitVars {old, new1, new2} vars = 
    [(new1, NonNegative), (new2, NonNegative)] @ L.rem (fn (x, _) => Var.eq(x, old)) vars

fun splitMonom (Ops {opp, ...}) {old, new1, new2} (m as (c, x)) = 
    if Var.eq(x, old) then [(c, new1), (opp c, new2)] else [m]

fun splitLcomb ops sub l = L.concat ((map (splitMonom ops sub) l))

fun splitConstr ops sub (Constr {name, body, bound}) =
    Constr {name = name,
            body = splitLcomb ops sub body,
            bound = bound}

(*** Negate a variable ***)

fun negateMonomVar (Ops {opp, ...}) v (m as (c, x)) = 
    if Var.eq(x, v) then (opp c, v) else m

fun negateLcombVar ops v l = map (negateMonomVar ops v) l

fun negateConstrVar ops v (Constr {name, body, bound}) = 
    Constr {name = name,
            body = negateLcombVar ops v body,
            bound = bound}

(*** replace the type of variable [x] with [ty] in [vars] ***)

fun replaceTyVar (x, ty) (var as (x', _)) = if Var.eq(x, x') then (x, ty) else var
fun replaceTyVars xty vars = map (replaceTyVar xty) vars

(* Put variables in standard form, i.e. all with bounds v >= 0, possibly splitting
 * and/or adding constraints *)
fun standardizeVariable (ops as Ops {zero, one, opp, leq})
			((x: Var.t, ty:'a var_ty),
                         lp as Lp {name, dir, vars, obj, constrs}) =
    case ty of
       (* Nonnegative variables don't need to be changed *)
       NonNegative => lp
     (* Free variables need to be split into two. *)
     | Free => 
       let
	  val sub = {old = x,
                     new1 = Var.freshen x,
                     new2 = Var.freshen x} 
       in
	  Lp {name = name,
	      dir = dir,
	      vars = splitVars sub vars,
	      obj = splitLcomb ops sub obj,
	      constrs = map (splitConstr ops sub) constrs}
       end
     (* Variables bounded below are handled whether the constant
      * is greater or less than 0 *)
     | Geq b => 
       (* If greater than zero, we just need to add a constraint. *)
       if leq(zero, b) then Lp {name = name,
			       dir = dir,
			       vars = replaceTyVars(x, NonNegative) vars,
			       obj = obj,
			       constrs = Constr {name = Name.newNamed (Var.toString x ^ "Bound"),
				                 body = [(one, x)],
				                 bound = CGeq b}::
                                         constrs}
       (* Otherwise splitting is necessary. We treat v as free, with a constraint *)
       else
          let
             val var' = (x, Free) 
          in
             standardizeVariable ops
               (var', Lp {name = name,
	                  dir = dir,
	                  vars = replaceTyVars var' vars,
	                  obj = obj,
	                  constrs = 
                          Constr {name = Name.newNamed (Var.toString x ^ "Bound"),
			          body = [(one, x)],
			          bound = CGeq b}::constrs})
          end
     (* negate x throughout the linear program and call the Geq case *)
     | Leq b => 
       let 
          val var' = (x, Geq (opp b))
       in
          standardizeVariable ops (var',
                                   Lp {name = name,
			               dir = dir,
			               vars = replaceTyVars var' vars,
			               obj = negateLcombVar ops x obj,
			               constrs = map (negateConstrVar ops x) constrs})
       end
     (* equality is transformed to boxed *)
     | Eq b => 
       let 
          val var' = (x, Boxed {lo = b, hi = b})
       in
          standardizeVariable ops (var',
                                   Lp {name = name,
			               dir = dir,
			               vars = replaceTyVars var' vars,
			               obj = obj,
			               constrs = constrs})
       end
     (* boxed becomes either Leq or Geq plus a constraint *)
     | Boxed{lo, hi} => 
       (* the domain is positive *)
       if leq(zero, lo) then
          let
             val var' = (x, Geq lo)
          in
             standardizeVariable ops (var',
	                              Lp {name = name,
	                                  dir = dir,
	                                  vars = replaceTyVars var' vars,
	                                  obj = obj,
	                                  constrs = Constr {name = Name.newNamed (Var.toString x ^ "Bound"),
		                                            body = [(one, x)],
		                                            bound = CLeq hi}
                                                    ::constrs})
          end
       (* the domain is negative *)
       else if leq(hi, zero) then
          let
             val var' = (x, Leq hi)
          in
             standardizeVariable ops (var',
	                              Lp {name = name,
	                                  dir = dir,
	                                  vars = replaceTyVars var' vars,
	                                  obj = obj,
	                                  constrs = Constr {name = Name.newNamed (Var.toString x ^ "Bound"),
		                                     body = [(one, x)],
		                                     bound = CGeq lo}
                                                    ::constrs})
          end
       (* domain is mixed *)
       else
          let
             val var' = (x, Free)
          in
             standardizeVariable ops 
              (var',
	       Lp {name = name,
	           dir = dir,
	           vars = replaceTyVars var' vars,
	           obj = obj,
	           constrs = Constr {name = Name.newNamed (Var.toString x ^ "LowerBound"),
		                     body = [(one, x)],
		                     bound = CGeq lo}::
                             Constr {name = Name.newNamed (Var.toString x ^ "UpperBound"),
		                     body = [(one, x)],
		                     bound = CLeq hi}::
                             constrs})
          end

fun standardizeVariables ops (lp as Lp {vars, ...}) = 
    foldr (standardizeVariable ops) lp vars

fun negateMonom (Ops {opp, ...}) (c, x) = (opp c, x)

fun negateLcomb ops l = map (negateMonom ops) l

fun standardizeConstr (ops as Ops {opp, ...}) (Constr {name, body, bound}) =
    case bound of
       (* c1 x1 + ... + cn xn >= b is in standard form already *)
       CGeq b => [S.Constr {name = name,
                            body = body,
                            bound = b}]
     (* c1 x1 + ... + cn xn <= b ~~> -c1 x1 + ... + -cn xn >= -b *)
     | CLeq b => [S.Constr {name = name,
                            body = negateLcomb ops body,
                            bound = opp b}]
     (* c1 x1 + ... + cn xn = b ~~>
      c1 x1 + ... + cn xn >= b /\ -c1 x1 + ... + -cn xn >= -b *)
     | CEq b =>
       [S.Constr {name = Name.newNamed(Name.name name ^ "Gt"),
                  body = body,
                  bound = b},
        S.Constr {name = Name.newNamed (Name.name name ^ "Lt"),
                  body = negateLcomb ops body,
                  bound = opp b}]
     (* lo <= c1 x1 + ... + cn xn <= hi ~~>
      c1 x1 + ... + cn xn >= lo /\ -c1 x1 + ... + -cn xn >= -hi *)
     | CBoxed {lo, hi} =>
       [S.Constr{name = Name.newNamed(Name.name name ^ "Gt"),
                 body = body,
                 bound = lo},
        S.Constr{name = Name.newNamed(Name.name name ^ "Lt"),
                 body = negateLcomb ops body,
                 bound = opp hi}]


fun standardize ops lp =
    let
       val Lp {name, dir, vars, obj, constrs} = standardizeVariables ops lp
       (* Variables should now be nonnegative *)
       val  _ = if L.exists (fn (_, NonNegative) => false | _ => true) vars then 
		   raise Fail "non-NonNegative variable in problem" else ()
       val vars' = map #1 vars
       val obj' = case dir of Minimize => obj
                            | Maximize => negateLcomb ops obj
       val constrs' = L.concat (map (standardizeConstr ops) constrs)
    in
       S.Lp {name = name,
             vars = vars', 
             obj = obj',
             constrs = constrs'}
    end

type answer = S.answer

datatype status = OK
		| Infeasible
		| Unbounded
		| Error

fun liftStatus S.OK = OK
  | liftStatus S.Infeasible = Infeasible
  | liftStatus S.Unbounded = Unbounded
  | liftStatus S.Error = Error

fun status answer = liftStatus (S.status answer)

fun solve lp = 
    let
       val slp = standardize realOps lp
    in
       S.solve slp
    end

fun objective (Lp {dir, ...}, ans) = 
    let
       val res = S.objective ans
    in 
       case dir of 
	  Minimize => res
	| Maximize => ~ res 
    end

val colPrimal = S.colPrimal

val rowDual = S.rowDual

fun writeProblem (lp, file) = 
    let
       val slp = standardize realOps lp
    in
       S.writeProblem(slp, file)
    end

(* -------------------------------------------------------------------------- *)
(*  Printing                                                                  *)
(* -------------------------------------------------------------------------- *)

open PP.Ops                   

fun ppDir Minimize = $"Minimize"
  | ppDir Maximize = $"Maximize"

fun ppMonom f (c, x) = %[f c, $" * ", Var.pp x]

fun ppLcomb f l = % (L.separate ($" + ") (map (ppMonom f) l))

fun ppBound f (CGeq x) = %[$" >= ", f x]
  | ppBound f (CLeq x) = %[$" <= ", f x]
  | ppBound f (CEq x) = %[$" = ", f x]
  | ppBound f (CBoxed {lo, hi}) = %[$" >< ", PP.pair(f lo, f hi)]

fun ppConstr f (Constr {name, body, bound}) = 
    %[Name.pp name, $": ", ppLcomb f body, ppBound f bound]

fun ppVbound f (v, b) = 
    case b of 
       NonNegative => PP.empty
     | Free => %[Var.pp v, $" free"]
     | Geq b => %[Var.pp v, $" >= ", f b]
     | Leq b => %[Var.pp v, $" <= ", f b]
     | Eq b => %[Var.pp v, $" = ", f b]
     | Boxed {lo, hi} => %[f lo, $" <= ", Var.pp v, $" <= ", f hi]

fun pp f (Lp {name, dir, vars, obj, constrs}) = 
    &[$"Linear Program in General Form",
      ~,
      %[$"Name: ", $name],
      ~,
      %[$"Variables: ", PP.listHoriz (Var.pp o Pair.fst) vars],
      ~,
      ppDir dir,
      ~,
      %[\\, ppLcomb f obj],
      ~,
      $"Subject to",
      ~,
      %[\\, &(map (ppConstr f) constrs)],
      ~,
      $"Bounds",
      %[\\, PP.listVert (ppVbound f) vars],
      ~,
      $"End",
      ~]

fun ppStatus OK = $"OK"
  | ppStatus Infeasible = $"Infeasible"
  | ppStatus Error = $"Error"
  | ppStatus Unbounded = $"Unbounded"

end
