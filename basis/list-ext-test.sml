
structure ListExtTest: UNIT_TEST =
struct

structure L = ListExt
structure U = UnitTests

(* ---------------------------------  sort  --------------------------------- *)

val sort = L.sortSetify Int.compare

val sort0 = U.TestCase 
                     (fn () => U.assert (sort [] = []))

val sort1 = U.TestCase
                      (fn () => U.assert (sort [3,5,1,1,7,3,3,5] = [1,3,5,7]))

val sortTests = U.TestLabel("sort",U.TestList [sort0,sort1])

(* ---------------------------------  mem  ---------------------------------- *)

val mem0 = U.TestCase (fn () => U.assert (L.mem(3,[3,5,1,1,7,3,3,5])))
val mem1 = U.TestCase (fn () => U.assert (not (L.mem(4,[3,5,1,1,7,3,3,5]))))

val memTests = U.TestLabel("mem",U.TestList [mem0,mem1])

(* ---------------------------------  end  ---------------------------------- *)

val test = fn () => 
    U.TestLabel ("ListExt Tests", 
                 U.TestList [sortTests,memTests])


end
