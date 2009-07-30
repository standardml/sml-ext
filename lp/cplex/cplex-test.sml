
structure CplexTest:> UNIT_TEST =
struct 

  open GeneralExt
  infixr 0 ` 

  structure U = UnitTests
  structure A = ArrayExt
  structure C = Cplex

  (*
   *      maximize
   * 
   *        z = 10 x1 + 6 x2 + 4 x3
   * 
   *      subject to
   * 
   *        x1 + x2 + x3 <= 100
   *        10 x1 + 4 x2 + 5 x3 <= 600
   *        2x1 + 2x2 + 6x3 <= 300
   * 
   *      bounds
   * 
   *       x1 >= 0, x2 >= 0, x3 >= 0
   *)

  val lp = C.make {name = "my_prob",
                   numrows = 3,
                   numcols = 3,
                   dir = C.Maximize,
                   obj = A.from_list [10.0,6.0,4.0],
                   rhs = A.from_list [100.0,600.0,300.0],
                   sense = A.array(3,C.Leq),
                   matbeg = A.from_list [0,3,6],
                   matcnt = A.from_list [3,3,3],
                   matind = A.from_list [0,1,2,0,1,2,0,1,2],
                   matval = A.from_list [1.0,10.0,2.0,
                                         1.0,4.0,2.0,
                                         1.0,5.0,6.0],
                   lb = A.from_list [0.0,0.0,0.0],
                   ub = A.array(3,Real.posInf),
                   rowname = A.from_list ["p","q","r"],
                   colname = A.from_list ["x1","x2","x3"]}

                        (* 
                         abc
                            def
                              

  val t1 = $("t1",%(fn () =>
      let 
        val _ = C.opt lp
        val obj = C.obj lp
      in
        C.write (lp,"t1.lp")
      ; printl ` concat ["lp is feasible: ",Bool.toString (C.feasible lp)]
      ; printl ` concat ["obj is: ",Real.toString obj]
      end))


val test = $("Cplex",&[t1])

end

structure I = RealInterval

structure CplexLpTest = LpTestFun(structure B = CplexLpBase
                                  structure I = I
                                  val str_name = "CplexLpTest")
