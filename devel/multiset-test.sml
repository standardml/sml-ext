
structure MultisetTest:> UNIT_TEST =
struct

structure U = UnitTests
structure S = SplayMultisetFn(struct 
                                 type ord_key = string
                                 val compare = String.compare
                              end)

open U.Ops

fun eqSet (s1,s2) = S.listAllItems s1 = S.listAllItems s2

val t1 = $("t1",%(fn () =>
    let
       val s1 = S.addManyList(S.empty,[("a",5),("b",2),("c",54)])
       val s2 = S.addManyList(S.empty,[("a",3),("d",7),("c",46)])
       val s3 = S.addManyList(S.empty,[("a",8),("b",2),("c",100),("d",7)])
       val _ = U.assert (eqSet(S.union(s1,s2),s3))
       val s4 = S.delete(s3,"c")
       val _ = U.assert (S.card(s4,"c") = 99)
       val s5 = S.addManyList(S.empty,[("a",3),("c",46)])
       val _ = U.assert (eqSet(S.intersection(s1,s2),s5))
       val s6 = S.addManyList(S.empty,[("a",2),("b",2),("c",8)])
       val _ = U.assert (eqSet(S.difference(s1,s2),s6))
    in
       ()
    end))

val test = $("Multiset",&[t1])

end

(* 
CM.make "sources.cm"; 
UnitTests.runAll()
val s1 = S.addManyList(S.empty,[("a",5),("b",2),("c",54)])
S.listAllItems s1
val s2 = S.addManyList(S.empty,[("a",3),("d",7),("c",46)])
*) 
