
structure GlpkTest :> UNIT_TEST =
struct 

structure U = UnitTests
structure GlpkLpTest = LpTestFn(struct 
                                   structure B = GlpkLpBase
                                   val strName = "GlpkLp"
                                end)

val test = U.TestLabel("Glpk", U.TestList[GlpkBaseTest.test, GlpkLpTest.test]) 

end
