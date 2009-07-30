
(* All SML-Ext tests in one package *) 

structure SMLExtTest :> UNIT_TEST =
struct 

structure U = UnitTests

val test = 
 fn () => U.TestLabel ("SML-Ext",
                       U.TestList [ ListExtTest.test()
                                  , RealExtTest.test()
                                  , FloatTest.test()
                                  , FPUTest.test()
                                  , StreamTest.test()
                                  , ListPermutationTest.test()
                                  , VectorPermutationTest.test()
                                  , BigWordTest.test()
                                  , BitsTest.test()
                                  ])

end
