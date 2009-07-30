
structure GlpkBaseTest:> UNIT_TEST =
struct 

open GeneralExt

structure U = UnitTests
structure A = ArrayExt
structure G = Glpk
structure R = RealExt

open U.Ops
     
val epsilon = 1E~10
fun epsilonClose(x:real,y) = R.abs(x - y) < epsilon

(* -------------------------------------------------------------------------- *)
(*  Test basic Glpk interface                                                 *)
(* -------------------------------------------------------------------------- *)

fun mkLp () =
    let
       val lp = G.createProb()
       val ia = A.fromList [~1,  1,  1,  1,  2,   3,  2,  3,  2,  3]
       val ja = A.fromList [~1,  1,  2,  3,  1,   1,  2,  2,  3,  3]
       val ar = A.fromList [~1.0,1.0,1.0,1.0,10.0,2.0,4.0,2.0,5.0,6.0]
    in
       G.setProbName(lp,"test")
     ; G.setObjDir(lp,G.GLP_MAX)
     ; G.addRows(lp,3)
     ; G.setRowName(lp,1,"p")
     ; G.setRowBnds(lp,1,G.GLP_UP 100.0)
     ; G.setRowName(lp,2,"q")
     ; G.setRowBnds(lp,2,G.GLP_UP 600.0)
     ; G.setRowName(lp,3,"r")
     ; G.setRowBnds(lp,3,G.GLP_UP 300.0)
     ; G.addCols(lp,3)
     ; G.setColName(lp,1,"x1")
     ; G.setColBnds(lp,1,G.GLP_LO 0.0)
     ; G.setObjCoef(lp,1,10.0)
     ; G.setColName(lp,2,"x2")
     ; G.setColBnds(lp,2,G.GLP_LO 0.0)
     ; G.setObjCoef(lp,2,6.0)
     ; G.setColName(lp,3,"x3")
     ; G.setColBnds(lp,3,G.GLP_LO 0.0)
     ; G.setObjCoef(lp,3,4.0)
     ; G.loadMatrix(lp,9,ia,ja,ar)
     ; lp
    end

val x1Res = 100.0 / 3.0
val x2Res = 200.0 / 3.0
val x3Res = 0.0
val zRes = 733.0 + 1.0 / 3.0
fun test x y = U.assertEqual epsilonClose R.toString x y

fun t1 () =
    let
       val lp = mkLp ()
       (* val () = G.simplex (lp,G.defaultParams) *)
       val () = G.simplex (lp,{msgLev = G.GLP_MSG_OFF,
			       presolve = G.GLP_OFF,
			       itLim = 1000000,
			       tmLim = 1000000})
       val _ = G.writeCpxlp(lp,"test.lp")
       val z = G.getObjVal lp
       val x1 = G.getColPrim(lp,1)
       val x2 = G.getColPrim(lp,2)
       val x3 = G.getColPrim(lp,3)
       val p = G.getRowDual(lp,1)
       val q = G.getRowDual(lp,2)
       val r = G.getRowDual(lp,3)
    in
       printl (concat["dual for row p = ",Real.toString p,"\n",
                      "dual for row q = ",Real.toString q,"\n",
                      "dual for row r = ",Real.toString r,"\n"])
     ; test x1 x1Res
     ; test x2 x2Res
     ; test x3 x3Res
     ; test z zRes
    end
val t1 = $("t1",%t1)

val test = $("GlpkBase",&[t1])

end
