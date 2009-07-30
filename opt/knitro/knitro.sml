
(* 
 * Note: don't try your own starting point.  If it's not
 * feasible, Knitro just stops.
 *)

structure Knitro :> KNITRO = 
struct 

open GeneralExt
infixr 0 `

structure A = ArrayExt
structure L = ListExt
structure V = VectorExt
structure P = MLton.Pointer
structure R = RandomExt

infix 8 & ?
fun x & n = V.sub(x, n)
fun x ? n = A.sub(x, n)

nonfix %
fun % x = A.vector x

type ptr = P.t 
type env = ptr

type func = real vector -> real * real vector * real vector vector

datatype verbose = None
                 | Summary
                 | Iter10
                 | Iter
                 | IterVerbose
                 | IterX
                 | All

datatype opt = Opt of {obj: func,
                       constrs: func list,
                       lowerBnds: real vector,
                       upperBnds: real vector,
                       jacobianValid: bool,
                       hessianValid: bool,
                       verbose: verbose,
                       maxIters: int,
                       repeat: int,
                       checkFirstDerivs: bool}

datatype eval_status = Yes | No 

datatype solver_result = SUCCESS
                       | KTR_RC_EVALFC (* eval point and constraints at x *)
                       | KTR_RC_EVALGA (* eval objGrad and jacobian at x *)
                       | KTR_RC_EVALH (* eval hessian at x *)
                       | KTR_RC_EVAL_ERR (* div by 0, etc. *)

(* -------------------------------------------------------------------------- *)
(*  #define variables                                                         *)
(* -------------------------------------------------------------------------- *)

val (getKtrObjgoalMinimize, _) = _symbol "ktr_objgoal_minimize": (unit -> int) * (int -> unit);
val ktrObjgoalMinimize = getKtrObjgoalMinimize()

val (getKtrObjgoalMaximize, _) = _symbol "ktr_objgoal_maximize": (unit -> int) * (int -> unit);
val ktrObjgoalMaximize = getKtrObjgoalMaximize()

val (getKtrObjtypeGeneral, _) = _symbol "ktr_objtype_general": (unit -> int) * (int -> unit);
val ktrObjtypeGeneral = getKtrObjtypeGeneral()

val (getKtrInfbound, _) = _symbol "ktr_infbound": (unit -> real) * (real -> unit);
val ktrInfbound = getKtrInfbound()

val (getKtrContypeGeneral, _) = _symbol "ktr_contype_general": (unit -> int) * (int -> unit);
val ktrContypeGeneral = getKtrContypeGeneral()

val (getKtrRcOptimal, _) = _symbol "ktr_rc_optimal": (unit -> int) * (int -> unit);
val ktrRcOptimal = getKtrRcOptimal()

val (getKtrRcEvalfc, _) = _symbol "ktr_rc_evalfc": (unit -> int) * (int -> unit);
val ktrRcEvalfc = getKtrRcEvalfc()

val (getKtrRcEvalga, _) = _symbol "ktr_rc_evalga": (unit -> int) * (int -> unit);
val ktrRcEvalga = getKtrRcEvalga()

val (getKtrRcEvalh, _) = _symbol "ktr_rc_evalh": (unit -> int) * (int -> unit);
val ktrRcEvalh = getKtrRcEvalh()

val (getKtrParamGradopt, _) = _symbol "ktr_param_gradopt": (unit -> int) * (int -> unit);
val ktrParamGradopt = getKtrParamGradopt()

val (getKtrParamMultistart, _) = _symbol "ktr_param_multistart": (unit -> int) * (int -> unit);
val ktrParamMultistart = getKtrParamMultistart()

val (getKtrParamMsmaxsolves, _) = _symbol "ktr_param_msmaxsolves": (unit -> int) * (int -> unit);
val ktrParamMsmaxsolves = getKtrParamMsmaxsolves()

val (getKtrParamMaxit, _) = _symbol "ktr_param_maxit": (unit -> int) * (int -> unit);
val ktrParamMaxit = getKtrParamMaxit()

val (getKtrGradoptExact, _) = _symbol "ktr_gradopt_exact": (unit -> int) * (int -> unit);
val ktrGradoptExact = getKtrGradoptExact()

(* val (getKtrGradoptForward, _) = _symbol "ktr_gradopt_forward": (unit -> int) * (int -> unit); *)
(* val ktrGradoptForward = getKtrGradoptForward() *)

val (getKtrGradoptCentral, _) = _symbol "ktr_gradopt_central": (unit -> int) * (int -> unit);
val ktrGradoptCentral = getKtrGradoptCentral()

val (getKtrParamHessopt, _) = _symbol "ktr_param_hessopt": (unit -> int) * (int -> unit);
val ktrParamHessopt = getKtrParamHessopt()

val (getKtrHessoptExact, _) = _symbol "ktr_hessopt_exact": (unit -> int) * (int -> unit);
val ktrHessoptExact = getKtrHessoptExact()

val (getKtrHessoptBfgs, _) = _symbol "ktr_hessopt_bfgs": (unit -> int) * (int -> unit);
val ktrHessoptBfgs = getKtrHessoptBfgs()

(*** Verbosity ***)

val (getKtrParamOutlev, _) = _symbol "ktr_param_outlev": (unit -> int) * (int -> unit);
val ktrParamOutlev = getKtrParamOutlev()

val (getKtrOutlevNone, _) = _symbol "ktr_outlev_none": (unit -> int) * (int -> unit);
val ktrOutlevNone = getKtrOutlevNone()

val (getKtrOutlevSummary, _) = _symbol "ktr_outlev_summary": (unit -> int) * (int -> unit);
val ktrOutlevSummary = getKtrOutlevSummary()

val (getKtrOutlevIter10, _) = _symbol "ktr_outlev_iter_10": (unit -> int) * (int -> unit);
val ktrOutlevIter10 = getKtrOutlevIter10()

val (getKtrOutlevIter, _) = _symbol "ktr_outlev_iter": (unit -> int) * (int -> unit);
val ktrOutlevIter = getKtrOutlevIter()

val (getKtrOutlevIterVerbose, _) = _symbol "ktr_outlev_iter_verbose": (unit -> int) * (int -> unit);
val ktrOutlevIterVerbose = getKtrOutlevIterVerbose()

val (getKtrOutlevIterX, _) = _symbol "ktr_outlev_iter_x": (unit -> int) * (int -> unit);
val ktrOutlevIterX = getKtrOutlevIterX()

val (getKtrOutlevAll, _) = _symbol "ktr_outlev_all": (unit -> int) * (int -> unit);
val ktrOutlevAll = getKtrOutlevAll()

val (getKtrRcEvalErr, _) = _symbol "ktr_rc_eval_err": (unit -> int) * (int -> unit);
val ktrRcEvalErr = getKtrRcEvalErr()

(* -------------------------------------------------------------------------- *)
(*  Util                                                                      *)
(* -------------------------------------------------------------------------- *)

fun evalStatusToInt Yes = 0
  | evalStatusToInt No = 1

fun solverResultFromInt n = 
    if n = ktrRcOptimal orelse n < 0 then SUCCESS
    else if n = ktrRcEvalfc then KTR_RC_EVALFC
    else if n = ktrRcEvalga then KTR_RC_EVALGA
    else if n = ktrRcEvalh then KTR_RC_EVALH
    else if n = ktrRcEvalErr then KTR_RC_EVAL_ERR
    else raise Fail ("unknown solver status: " ^ Int.toString n)

(* upper triangle pairs n ~~>
 [(0, 0), (0, 1), ..., (0, n-1), (1, 1), ..., (1, n-1), (2, 2), ..., (n-1, n-1) *)
local
   fun upperPairs' n (i, j) store = 
       if i = n then rev store
       else if j = n then upperPairs' n (i+1, i+1) store
       else upperPairs' n (i, j+1) ((i, j)::store)
in
fun upperPairs n = upperPairs' n (0, 0) []
end

fun addMat (m1, m2) = V.map2 (fn (r1, r2) => V.map2 (op+) (r1, r2)) (m1, m2)
fun sumMat ms = L.foldl1 addMat ms
fun scaleMat (c, m) = V.map (V.map (fn x => c * x)) m

(* -------------------------------------------------------------------------- *)
(*  Knitro Library                                                            *)
(* -------------------------------------------------------------------------- *)

val ktrNew = _import "KTR_new": unit -> ptr;

val ktrInitProblem = _import "KTR_init_problem": 
                     env * (* kc: context pointer *)
                     int * (* n: number of variables *)
                     int * (* objGoal: 0 for min, 1 for max *)
                     int * (* objType *)
                     real vector * (* xLoBnds: lower bound array of length n*)
                     real vector * (* xUpBnds: upper bound array of length n*)
                     int * (* m: number of constraints *)
                     int vector * (* cType: array of length m: constraint types
                                   * 0 for general/nonlinear
                                   * 1 for linear
                                   * 2 for quadratic *)
                     real vector * (* cLoBnds: lower bound array of length m for constrains *)
                     real vector * (* cUpBnds: upper bound array of length m for constrains *)
                     int * (* nnzJ : number of nonzero jacobian values *)
                     int vector * (* jacIndexVars: variable indices of jacobian *)
                     int vector * (* jacIndexCons: constraint indices of jacobian *)
                     int * (* nnzH : number of nonzero hessian values *)
                     int vector * (* hessIndexRows : hessian row indices *)
                     int vector * (* hessIndexCols : hessian column indices *)
                     ptr * (* xInitial: initial guess for x, not used *)
                     ptr (* lambdaInitial: not used *)
                     -> int;

(* returns 0 iff OK *)
val ktrSetIntParam = _import "KTR_set_int_param": env * int * int -> int;

val ktrSolve = _import "KTR_solve":
               env * (* INPUT kc: context pointer*)
               real array * (* OUTPUT x: solution, array of length n *)
               real array * (* OUTPUT lambda: multipliers, array of length n + m *)
               int * (* INPUT eval_status:
                      * 0 means application was able to compute values at x, lambda 
                      * 1 otherwise *)                    
               real ref * (* INPUT/OUTPUT obj: value of obj function *)
               real array * (* INPUT c:
                             values of constraints *)
               real array * (* INPUT objGrad: 
                             values of derivatives of obj *)
               real array * (* INPUT jac: 
                             values of derivatives of constraints. *)
               real array * (* INPUT hess: 
                             values of Lagrangian of Hessian of objective. *)
               ptr * (* hessVector: not used *)
               ptr (* userParams: not used *)
               -> int;

val ktrCheckFirstDers = 
    _import "KTR_check_first_ders":
    env (* INPUT kc: context pointer*)
    * real array (* INPUT/OUTPUT x: point at which to check derivatives,
                  * solution, array of length n *)
    * int (* INPUT finiteDiffMethod *)
    * real (* INPUT absThreshold *)
    * real (* INPUT relThreshold *)
    * int (* INPUT eval_status:
           * 0 means application was able to compute values at x, lambda 
           * 1 otherwise *)                    
    * real ref (* INPUT/OUTPUT obj: value of obj function *)
    * real array (* INPUT c:
                  values of constraints *)
    * real array (* INPUT objGrad: 
                  values of derivatives of obj *)
    * real array (* INPUT jac: 
                  values of derivatives of constraints. *)
    * ptr (* userParams: not used *)
    -> int;

val ktrRestart = _import "KTR_restart":
                 env * (* INPUT kc: context pointer*)
                 real array * (* OUTPUT x: solution, array of length n *)
                 real array (* OUTPUT lambda: multipliers, array of length n + m *)
                 -> int;

(* returns 0 iff OK *)
val free = _import "mlton_knitro_free": env -> int;

(* -------------------------------------------------------------------------- *)
(*  SML Library                                                               *)
(* -------------------------------------------------------------------------- *)

fun optimize dir (Opt {obj, constrs, lowerBnds, upperBnds,
                       jacobianValid, hessianValid,
                       verbose, repeat, maxIters, checkFirstDerivs}) =
    let
       (* val _ = printl "starting optimzation" *)
       val env = ktrNew ()
       fun setIntParam (i, j) = 
           let 
              val res = ktrSetIntParam(env, i, j)
           in
              if res = 0 then () else raise Fail "can't set int param"
           end
       val seed = R.rand (7384912, 12021906)

       (* maybe run from multiple starting points *)
       val _ = if repeat <= 1 then () else
               let in
                  setIntParam(ktrParamMultistart, 1)
                ; setIntParam(ktrParamMsmaxsolves, repeat)
               end

       (* set max iterations *) 
       val _ = setIntParam(ktrParamMaxit, maxIters)

       (**** constants ****)
       val numvars = V.length lowerBnds
       (* val _ = printl ("num vars: " ^ Int.toString numvars) *)
       val numconstrs = length constrs
       (* val _ = printl ("num constrs: " ^ Int.toString numconstrs) *)
       val nnzJ = numconstrs * numvars
       val nnzH = (numvars + 1) * numvars div 2 (* just the upper triangle *)

       (**** static storage ****)

       val jacIndexCons = V.tabulate(nnzJ, fn i => i div numvars)
       val jacIndexVars = V.tabulate(nnzJ, fn i => i mod numvars)
       val upairs = upperPairs numvars
       val (rows, cols) = L.unzip upairs
       val hessIndexRows = V.fromList rows
       val hessIndexCols = V.fromList cols
       val ctypes = V.tabulate(numconstrs, Fun.const ktrContypeGeneral)
       val clower = V.tabulate(numconstrs, Fun.const (~ ktrInfbound))
       val cupper = V.tabulate(numconstrs, Fun.const 0.0)

       (* set the jacobian option *)
       val _ = if not jacobianValid then
                  setIntParam(ktrParamGradopt, ktrGradoptCentral)
               else 
                  setIntParam(ktrParamGradopt, ktrGradoptExact)
       (* set the hessian option *)
       val _ = if not hessianValid then
                  setIntParam(ktrParamHessopt, ktrHessoptBfgs)
               else 
                  setIntParam(ktrParamHessopt, ktrHessoptExact)
       (* set verbosity *)
       val _ = case verbose of 
                  None => setIntParam(ktrParamOutlev, ktrOutlevNone)
                | Summary => setIntParam(ktrParamOutlev, ktrOutlevSummary)
                | Iter10 => setIntParam(ktrParamOutlev, ktrOutlevIter10)
                | Iter => setIntParam(ktrParamOutlev, ktrOutlevIter)
                | IterVerbose => setIntParam(ktrParamOutlev, ktrOutlevIterVerbose)
                | IterX => setIntParam(ktrParamOutlev, ktrOutlevIterX)
                | All => setIntParam(ktrParamOutlev, ktrOutlevAll)
       
       (**** dynamic storage ****)

       val objVal = ref 0.0
       (* val xInit = R.randRealVector seed {lower = lowerBnds, *)
       (* upper = upperBnds} *)
       val x = R.randRealArray seed {lower = lowerBnds,
                                     upper = upperBnds}
       val lambda = A.array(numconstrs + numvars, 0.0)
       val c = A.array(numconstrs, 0.0)
       val objGrad = A.array(numvars, 0.0)
       val jac = A.array(nnzJ, 0.0)
       val hess = A.array(nnzH, 0.0)

       (**** initialize the problem ****)

       (* val _ = printl "initializing" *)
       val res =
           ktrInitProblem(env,
                          numvars,
                          dir,
                          ktrObjtypeGeneral,
                          lowerBnds,
                          upperBnds,
                          numconstrs,
                          ctypes,
                          clower,
                          cupper,
                          nnzJ,
                          jacIndexVars,
                          jacIndexCons,
                          nnzH,
                          hessIndexRows,
                          hessIndexCols,
                          (* xInit, *)
                          P.null,
                          P.null)
       val _ = if res <> 0 then raise Fail "initProblem: error" else ()

       (**** recursive solver ****)

       fun solve (status, xinit) =
           let
              val numRestarts = ref 0
              val maxRestarts = 30
              val res = 
                  if checkFirstDerivs then
                     ( printl "Checking derivs"
                     ; ktrCheckFirstDers (env,
                                          x,
                                          2, (* central differences *)
                                          1E~10,
                                          1E~10,
                                          0,
                                          objVal,
                                          c,
                                          objGrad,
                                          jac,
                                          P.null) 
                       before printl "Done checking derivs"
                     )
                  else
                     case xinit of 
                        NONE => 
                        ktrSolve (env,
                                  x,
                                  lambda,
                                  evalStatusToInt status,
                                  objVal,
                                  c,
                                  objGrad,
                                  jac,
                                  hess,
                                  P.null,
                                  P.null) 
                      | SOME xs => 
                        ktrRestart (env,
                                    xs,
                                    lambda)
           in
              (* Note: See the Knitro user's manual, p.31,
               * "Computing the sparse Jacobian matrix",
               * "Computing the sparse Hessian matrix" sections
               * for the algebraic manipulations here. *)
              case solverResultFromInt res of
                 SUCCESS => 
                 ( (* printl "success" *)
                  (!objVal, %x)
                 )
               | KTR_RC_EVALFC =>
                 let
                    val (v, _, _) = obj (%x)
                    val () = objVal := v
                    fun appFun (i, f) =
                        let
                           val (v, _, _) = f (%x)
                        in
                           A.update(c, i, v)
                        end
                    val () = L.appi appFun constrs
                 in
                  (* printl "evalfc" *)
                    solve (Yes, NONE)
                 end
               | KTR_RC_EVALGA =>
                 let
                    val (_, dobj, _) = obj (%x)
                    val _ = V.appi (fn (i, v) => A.update(objGrad, i, v)) dobj
                    val (_, dconstrs, _) = L.unzip3 (map (Fun.apply (%x)) constrs)
                 in
                    (* printl "evalfga" *)
                    L.appi (fn (i, c) => V.appi (fn (j, v) => A.update(jac, numvars * i + j, v)) c) dconstrs
                  ; solve (Yes, NONE)
                 end
               | KTR_RC_EVALH =>
                 let
                    val (_, _, ddobj) = obj (%x)
                    val (_, _, ddconstrs) = L.unzip3 (map (Fun.apply (%x)) constrs)
                    val ddconstrs' = L.mapi (fn (i, m) => scaleMat(lambda ? i, m)) ddconstrs
                    val lagrangeHessian = sumMat (ddobj::ddconstrs')
                    fun appFn (n, (i, j)) = A.update(hess, n, lagrangeHessian & i & j)
                 in
                    (* printl "evalfh" *)
                    L.appi appFn upairs
                  ; solve (Yes, NONE)
                 end
               | KTR_RC_EVAL_ERR => 
                 let
                    val _ = Ref.incr numRestarts
                    val x = R.randRealArray seed {lower = lowerBnds,
                                                  upper = upperBnds}
                 in
                    if !numRestarts < maxRestarts then
                       solve (No, SOME x)
                    else raise Fail ("Knitro failed after " ^ Int.toString maxRestarts ^ " restarts!")
                 end
           end
           handle Fail _ => solve (No, NONE)
                | Domain => solve (No, NONE)
                | Div => solve (No, NONE)
                | exn => (printl ("unknown exn: " ^ exnName exn)
                        ; raise exn)
       val result = solve (Yes, NONE)
       val code = free env
    in
       if code <> 0 then
          raise Fail "Can't free Knitro problem" 
       else result
    end

fun minimize inp = optimize ktrObjgoalMinimize inp
fun maximize inp = optimize ktrObjgoalMaximize inp

(* -------------------------------------------------------------------------- *)
(*  Simple interface                                                          *)
(* -------------------------------------------------------------------------- *)

type simp_func = real vector -> real

datatype simp = Simp of {obj: simp_func,
                         constrs: simp_func list,
                         lowerBnds: real vector,
                         upperBnds: real vector,
                         verbose: verbose,
                         maxIters: int,
                         repeat: int}

local
   val dummy = V.tabulate(0, fn _ => 0.0)
   val dummy2 = V.tabulate(0, fn _ => dummy)
   fun extend f x = (f x, dummy, dummy2)
in

fun simpleMinimize (Simp {obj, constrs, lowerBnds, upperBnds, verbose, maxIters, repeat}) =
    minimize (Opt {obj = extend obj,
                   constrs = map extend constrs,
                   lowerBnds = lowerBnds,
                   upperBnds = upperBnds,
                   jacobianValid = false,
                   hessianValid = false,
                   verbose = verbose,
                   maxIters = maxIters,
                   repeat = repeat,
                   checkFirstDerivs = false})

fun simpleMaximize (Simp {obj, constrs, lowerBnds, upperBnds, verbose, maxIters, repeat}) =
    maximize (Opt {obj = extend obj,
                   constrs = map extend constrs,
                   lowerBnds = lowerBnds,
                   upperBnds = upperBnds,
                   jacobianValid = false,
                   hessianValid = false,
                   verbose = verbose,
                   maxIters = maxIters,
                   repeat = repeat,
                   checkFirstDerivs = false})

end

end
