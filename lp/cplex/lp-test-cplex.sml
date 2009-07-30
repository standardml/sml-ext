
structure I = RealInterval

structure GlpkLpTest = LpTestFun(structure B = GlpkLpBase
                                 structure I = I
                                 val str_name = "GlpkLpTest")

structure CplexLpTest = LpTestFun(structure B = CplexLpBase
                                  structure I = I
                                  val str_name = "CplexLpTest")
