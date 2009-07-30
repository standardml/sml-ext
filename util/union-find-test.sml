
structure UnionFindTest :> UNIT_TEST =
struct

structure U = UnitTests
structure L = ListExt

(* -------------------------------------------------------------------------- *)
(*  Persistent                                                                *)
(* -------------------------------------------------------------------------- *)

structure IntKey = 
   struct 
      type ord_key = int
      val compare = Int.compare
   end
structure P = PersistentUnionFindFn(IntKey)

val p1 = U.TestLabel("p1",U.TestCase(fn () =>
    let
       val equal = [(1,2),(3,4),(5,6)]
       val u = foldr (fn ((a,b), p) => P.union p (a,b)) P.empty equal
       val eqs = P.toList u
    in
       U.assert (P.equal u (1,1))
     ; U.assert (P.equal u (3,4))
     ; U.assert (P.equal u (3,4))
     ; U.assert (P.equal u (5,6))
     ; U.assert (not (P.equal u (3,6)))
     ; U.assertEqual op= (L.toString (L.toString Int.toString)) ([[1,2],[3,4],[5,6]], eqs)
    end))

val p2 = U.TestLabel("p2",U.TestCase(fn () =>
    let
       val equal1 = [(1,2),(3,4),(5,6)]
       val u1 = foldr (fn ((a,b), p) => P.union p (a,b)) P.empty equal1
       val equal2 = [(1,3),(4,6)]
       val u2 = foldr (fn ((a,b), p) => P.union p (a,b)) P.empty equal2
       val u = P.merge(u1,u2)
       val eqs = P.toList u
    in
       U.assert (P.equal u (1,1))
     ; U.assert (P.equal u (3,4))
     ; U.assert (P.equal u (3,4))
     ; U.assert (P.equal u (5,6))
     ; U.assertEqual op= (L.toString (L.toString Int.toString)) ([[1,2,3,4,5,6]], eqs)
    end))

(* -------------------------------------------------------------------------- *)
(*  Imperative                                                                *)
(* -------------------------------------------------------------------------- *)

structure IntKey = 
   struct 
      type hash_key = int
      val sameKey = op=
      val hashVal = Word.fromInt
   end
structure I = ImperativeUnionFindFn(IntKey)

val i1 = U.TestLabel("i1",U.TestCase(fn () =>
    let
       val uni = I.create 10
       val equal = [(1,2),(3,4),(5,6)]
       val _ = app (fn (a,b) => I.union uni (a,b)) equal
       val eqs = I.toList uni
    in
       U.assert (I.equal uni (1,2))
     ; U.assert (I.equal uni (3,4))
     ; U.assert (I.equal uni (5,6))
     ; U.assert (not (I.equal uni (3,6)))
     ; U.assertEqual op= (L.toString (L.toString Int.toString)) ([[1,2],[3,4],[5,6]], map (L.sortSetify Int.compare) eqs)
    end))

(* -------------------------------------------------------------------------- *)
(*  End                                                                       *)
(* -------------------------------------------------------------------------- *)

val test = fn () => U.TestList[U.TestLabel("PersistentUnionFind",U.TestList[p1,p2]),
                               U.TestLabel("ImperativeUnionFind",U.TestList[i1])]

end
