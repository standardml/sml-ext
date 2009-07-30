(**
 * CFSQP relies on callbacks between SML and C.  Since the SML functions
 * take array arguments, it is not possible to manage the arguments from C,
 * as arrays must be created on the SML heap.  Thus, we have one global
 * array for the arguments to the functions [functionArgs], 
 * and set them elementwise from C [mltonSetArg].
 *)
structure Cfsqp:> CFSQP =
struct 

structure L = ListExt
structure A = ArrayExt
structure V = VectorExt
structure D = Random
structure R = RealExt

datatype opt = Opt of {obj: real vector -> real,
                       constrs: (real vector -> real) list,
                       lowerBnds: real vector,
                       upperBnds: real vector,
                       repeat: int,
                       verbose: int}

val functionArgs: real array ref = ref (A.array(0, 0.0))

val mltonSetArg = _export "mlton_cfsqp_set_arg": (int * real -> unit) -> unit;

fun setFun (i, x) = A.update(!functionArgs, i, x)

val _ = mltonSetArg setFun

val mltonObjFun = _export "mlton_cfsqp_obj_fun": (unit -> real) -> unit;

val mltonConstrFun = _export "mlton_cfsqp_constr_fun": (int -> real) -> unit;

val cfsqpMinimize = _import "cfsqp_minimize": 
                    int * int * real vector * real vector * real vector * int -> real;
    
local
   val rand = D.rand(587493, 1987342) (* just some arbitrary constants for the seed *)
in
(* given two arrays of length n, return an array of length
 * n with random values between those of the corresponding arrays.
 * Assumes lo[i] <= hi[i] 
 *)
fun randRealVector (lo, hi) =
    let 
       val lo' = V.toList lo
       val hi' = V.toList hi 
       fun randReal(lo, hi) = lo + (D.randReal rand * (hi - lo))
    in
       V.fromList(L.map2 randReal (lo', hi'))
    end

end (* local *)

fun modifyObjFun (f: real vector -> real) (): real =
    f (A.vector (!functionArgs))
    handle _ => R.nan

fun modifyConstrFun (f: int * real vector -> real) n: real = 
    f (n, A.vector (!functionArgs))
    handle _ => 0.0

fun minimizeOnce (Opt {obj, constrs, lowerBnds, upperBnds, verbose, ...}) =
    if not (V.all2 Real.<= (lowerBnds, upperBnds)) then raise Fail "minimize: bound mismatch" else
    let
       val numArgs = V.length lowerBnds
       val () = functionArgs:= A.array(numArgs, 0.0)
       val init = randRealVector(lowerBnds, upperBnds) 
       val objFun' = modifyObjFun obj
       fun constrFun (i, v) = L.nth(constrs, i-1) v (* cfsqp constraints are indexed from 1 *)
       val constrFun' = modifyConstrFun constrFun
       val _ = mltonObjFun objFun'
       val _ = mltonConstrFun constrFun'
       val numConstrs = length constrs
       val opt = cfsqpMinimize (numArgs, numConstrs, lowerBnds, upperBnds, init, verbose)
    in
       (opt, A.vector (!functionArgs))
    end

fun min xs = 
    let
       (* remove nans *)
       val xs = L.filter (fn (x, _) => not (R.isNan x)) xs
       fun foldFn (a as (x:real, _), b as (y, _)) = 
           if x < y then a else b
    in
       case xs of
          [] => (R.nan, V.fromList [])
        | _ => L.foldl1 foldFn xs
    end
    
fun minimize (inp as Opt {repeat,...}) =
    min (L.tabulate(repeat, (fn _ => minimizeOnce inp)))
    handle Subscript => raise Fail "minimize: subscript"

(* -------------------------------------------------------------------------- *)
(*  Maximize                                                                  *)
(* -------------------------------------------------------------------------- *)

fun maximize (Opt {obj, constrs, lowerBnds, upperBnds, repeat, verbose}) =
    let
       fun negateFun f x = ~ (f x)
       val (v, vs) = minimize (Opt {obj = negateFun obj,
                                    constrs = constrs,
                                    lowerBnds = lowerBnds,
                                    upperBnds = upperBnds,
                                    repeat = repeat,
                                    verbose = verbose})
    in
       (~ v, vs)
    end

end

