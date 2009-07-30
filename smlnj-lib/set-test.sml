

structure SetTest :> UNIT_TEST = 
struct

structure U = UnitTests

structure BitSet1024 = BitSetExtFn(val size = 1024)

structure BitSetTest = SetTestFn(struct 
                                    val strName = "BitSet"
                                    structure S = BitSet1024
                                 end)

structure IntSplaySet = OrdSetExtFn(struct 
                                       type ord_key = int
                                       val compare = Int.compare
                                       val ppItem = PP.int
                                    end)

structure OrdSetTest = SetTestFn(struct 
                                    val strName = "IntSplaySet"
                                    structure S = IntSplaySet
                                 end)

val test = fn () => U.TestLabel("SetTest", 
                                U.TestList[ BitSetTest.test()
                                          , OrdSetTest.test()])

end 
