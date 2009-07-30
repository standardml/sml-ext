
signature LP_TEST_STRUCTS = 
   sig
      structure B: STANDARD_LP_BASE
      val strName: string
   end

functor LpTestFn(X: LP_TEST_STRUCTS):> UNIT_TEST =
struct 

open X
open GeneralExt

val _ = strName

structure U = UnitTests
structure R = Real
structure S = StandardLpFn(B)
structure Lp = LpFn(S)

val epsilon = 1E~10
fun epsilonClose(x:real,y) = Real.abs(x - y) < epsilon

(* -------------------------------------------------------------------------- *)
(*  Standard                                                                  *)
(* -------------------------------------------------------------------------- *)

val (vars' as (x1,x2,x3)) = Tuple3.map S.Var.newNamed ("x1","x2","x3")
val vars = Tuple3.toList vars'

val (p,q,r) = Tuple3.map S.Name.newNamed ("p","q","r")

val realLp = B.Lp {name = "sample",
                   vars = vars,
                   obj = [(~10.0,x1),(~6.0,x2),(~4.0,x3)],
                   constrs = map B.Constr 
                                 [{name = p,
                                   body = [(~ 1.0,x1),(~ 1.0,x2),(~ 1.0,x3)],
                                   bound = ~ 100.0},
                                  {name = q,
                                   body = [(~ 10.0,x1),(~ 4.0,x2),(~5.0,x3)],
                                   bound = ~600.0},
                                  {name = r,
                                   body = [(~2.0,x1),(~2.0,x2),(~6.0,x3)],
                                   bound = ~300.0}]}

val x1Res = 100.0 / 3.0
val x2Res = 200.0 / 3.0
val x3Res = 0.0
val zRes = 733.0 + 1.0 / 3.0

local
   open PP.Ops                   
in
val t1 = U.TestLabel("t1", U.TestCase (fn () =>
    let 
       val ans = S.solve realLp
       val _ = printl "ans"
       val z = S.objective ans
       val _ = printl "z"
       val x1 = S.colPrimal(ans,x1)
       val _ = printl "x1"
       val x2 = S.colPrimal(ans,x2)
       val _ = printl "x2"
       val x3 = S.colPrimal(ans,x3)
       val _ = printl "x3"
    in
       PP.message (&[~,
                    S.pp PP.real realLp,
                    ~,
                    S.ppAnswer (realLp,ans),
                    ~])
     ; U.assert (epsilonClose(x1,x1Res) andalso 
                 epsilonClose(x2,x2Res) andalso 
                 epsilonClose(x3,x3Res) andalso 
                 epsilonClose(z,R.~ zRes))
    end))
end (* local *) 

(* -------------------------------------------------------------------------- *)
(*  LP                                                                        *)
(* -------------------------------------------------------------------------- *)

val lp = Lp.Lp
    {name = "testLp",
     dir = Lp.Maximize,
     vars = [(x1,Lp.NonNegative),(x2,Lp.NonNegative),(x3,Lp.NonNegative)],
     obj = [(10.0, x1), (6.0, x2), (4.0, x3)],
     constrs = map Lp.Constr
                   [{name = p,
                     body = [(1.0, x1), (1.0, x2), (1.0, x3)],
                     bound = Lp.CLeq 100.0},
                    {name = q,
                     body = [(10.0, x1),(4.0, x2),(5.0, x3)],
                     bound = Lp.CLeq 600.0},
                    {name = r,
                     body = [(2.0, x1), (2.0, x2), (6.0, x3)],
                     bound = Lp.CLeq 300.0}]}

val t2 = U.TestLabel("t2", U.TestCase (fn () =>
  U.assert (                              
  let val ans = Lp.solve lp in
    case Lp.status ans of
       Lp.OK => epsilonClose(Lp.objective (lp, ans), zRes)
     | _ => false
  end)))

(* -------------------------------------------------------------------------- *)
(*  Infeasible                                                                *)
(* -------------------------------------------------------------------------- *)

val lp = Lp.Lp
    {name = "testLp",
     dir = Lp.Maximize,
     vars = [(x1,Lp.NonNegative)],
     obj = [(1.0, x1)],
     constrs = map Lp.Constr
                   [{name = p,
                     body = [(1.0, x1)],
                     bound = Lp.CGeq 1.0},
                    {name = q,
                     body = [(1.0, x1)],
                     bound = Lp.CLeq (~1.0)}]}

val t3 = U.TestLabel("t3", U.TestCase (fn () =>
    let in 
       U.assert
       let 
          val ans = Lp.solve lp
       in
          case Lp.status ans of
             Lp.Infeasible => true
           | Lp.Error => (printl "Error?"; false)
           | Lp.Unbounded => (printl "Unbounded?"; false)
           | Lp.OK => (printl "OK???"; false)
       end
    end))

val test = U.TestLabel(strName, U.TestList[t1, t2, t3])

end
