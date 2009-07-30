
(** 
 * Test function values against Hales' sphere.m file
 *)

signature SET_TEST_ARGS =
   sig 
      val strName: string
      structure S: ORD_SET_EXT where type Key.ord_key = int
   end

functor SetTestFn(X: SET_TEST_ARGS):> UNIT_TEST =
struct

open X

structure U = UnitTests

fun ` x = S.fromList x

fun fail () = raise Fail "Test Failed!!!"

fun testEq (name, f) = 
    U.TestLabel(name, U.TestCase(fn () => if S.equal(f()) then () else fail()))

fun testNeq (name, f) = 
    U.TestLabel(name, U.TestCase(fn () => if S.equal(f()) then fail() else ()))

fun testT (name, f) = 
    U.TestLabel(name, U.TestCase(fn () => if f() then () else fail()))

fun testF (name, f) = 
    U.TestLabel(name, U.TestCase(fn () => if f() then fail() else ()))

fun testSub (name, f) = 
    U.TestLabel(name, U.TestCase(fn () => if S.isSubset(f()) then () else fail()))

fun testNsub (name, f) = 
    U.TestLabel(name, U.TestCase(fn () => if S.isSubset(f()) then fail() else ()))

val t0 = testEq("t0", fn () => (`[], S.empty))
val t1 = testEq("t1", fn ()=> (`[1, 2, 3, 1000], `[1000, 3, 2, 1]))
val t2 = testEq("t2", fn () => (S.union(`[1, 2, 3, 1000], `[4, 2, 5]), `[1,2,3,4,5, 1000]))
val t3 = testEq("t3", fn () => (S.difference(`[1000, 1, 2, 3, 5, 4], `[4, 2, 5]), `[1000, 1,3]))
val t4 = testEq("t4", fn () => (S.delete(`[1, 2, 3, 5, 1000], 1000), `[3, 5, 2, 1]))
val t5 = testT("t5", fn () => S.member(`[1, 2, 3, 5, 1000], 1000))
val t6 = testF("t6", fn () => S.member(`[1, 2, 3, 5], 1000))
val t7 = testSub("t7", fn () => (`[1,2,3, 1000], `[1,2,3,4, 1000]))
val t8 = testSub("t8", fn () => (S.empty, `[1,2,3,4]))
val t9 = testNsub("t9", fn () => (`[1,2,3,4], `[1000, 1,2,5]))
val t10 = testEq("t10", fn () => (`[1,2,3,4, 1000], S.map (fn x => x + 1) (`[0,1,2,3, 999])))

val test = fn () => 
              U.TestLabel(strName, 
                          U.TestList[ t0, t1, t2, t3, t4, t5
                                    , t6, t7, t8, t9, t10]
                         )

end 

