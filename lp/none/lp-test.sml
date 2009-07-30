
structure LpTest :> UNIT_TEST =
struct 

structure U = UnitTests
val test = fn () => U.TestLabel("Dummy", U.TestList[]) 

end
