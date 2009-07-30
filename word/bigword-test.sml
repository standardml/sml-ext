
(* 
 We can test against the existing word structures.  In this case, we 
 use Word32. 
 *)

structure BigWordTest:> UNIT_TEST =
struct

structure I = IntInf
structure W = Word32
structure W' = BigWordFn(val wordSize = 32)
structure U = UnitTests

open U.Ops

(* -----------------------------  Conversions  ------------------------------ *)

val t1 = %(fn () => U.assert (W'.fromInt 0 = W'.fromInt 0))

val t2 = %(fn () => U.assert ((ignore (W'.fromInt ~1); true) handle _ => false))

(*
 *  Dont' worry about this test failing in Poly/ML.  The behavior there is incorrect.
 * 
 * > Word32.fromLargeInt ~1;
 * val it = 0wx3FFFFFFFFFFFFFFF : Word32.word
 * 
 * That looks suspiciously longer than 32 bits...
 *)
val t3 = %(fn () => U.assert 
                       ((ignore (Option.valOf (W'.fromString "ffffffff")); true) 
                        handle _ => false))

fun testConv f f' x =
    let
       val v = f x
       val v' = f' x
    in
       % (fn () => U.assert (W'.toLargeInt v' = W.toLargeInt v))
    end

val t4 = testConv W.fromLargeInt W'.fromLargeInt 0xfffffff

val t5 = testConv W.fromLargeInt W'.fromLargeInt ~1

val t6 = %(fn () => U.assert (W'.fromLargeInt 0 = W'.fromLargeInt 0))

val t7 = % (fn () => U.assert ((ignore (W'.fromLargeInt ~1); true) handle _ => false))

val t8 = % (fn () => U.assert ((ignore (W'.fromLargeInt 0xfffffffff); true) handle _ => false))

val t9 = % (fn () => U.assertEqual op= Int.toString (W'.toIntX (W'.fromInt ~1), ~1))

val maxUnsigned = W'.fromLargeInt (I.pow(I.fromInt 2,W'.wordSize) - 1)

val t10 = % (fn () => U.assert (W'.toIntX maxUnsigned = ~1))

(* ------------------------------  Arithmetic  ------------------------------ *)

fun testBinop bop bop' x y = 
    U.assert
       (W.toLargeInt(bop(W.fromLargeInt x,W.fromLargeInt y)) = 
        W'.toLargeInt(bop'(W'.fromLargeInt x,W'.fromLargeInt y)))

fun testUnop uop uop' x = 
    U.assert
       (W.toLargeInt(uop(W.fromLargeInt x)) =
        (W'.toLargeInt(uop'(W'.fromLargeInt x))))

val testPlus = testBinop (W.+) (W'.+)
val t11 = % (fn () => testPlus 0 0)
val t12 = % (fn () => testPlus 34972348 82897492)
val t13 = % (fn () => testPlus 0x3fffffff 0x3fffffff) (* overflow *)

val testMinus = testBinop (W.-) (W'.-)
val t14 = %(fn () => testMinus 0 1000) (* underflow *)
val t15 = % (fn () => testMinus 82897492 34972348)
val testMul = testBinop ( W.* ) ( W'.* )
val t16 = % (fn () => testMul 0xfffffff 0xfffffff) (* overflow *)

val testAndb = testBinop W.andb W'.andb 
val t17 = % (fn () => testAndb 0xfffffff 0xfffffff)

val testNotb = testUnop W.notb W'.notb 
val t18 = % (fn () => testNotb 0xfffffff)

fun testBinop' bop bop' x y = 
    U.assert
       (W.toLargeInt(bop(W.fromLargeInt x,Word.fromInt y)) = 
        W'.toLargeInt(bop'(W'.fromLargeInt x,Word.fromInt y)))

val testShiftr = testBinop' W.>> W'.>>
val t19 = % (fn () => testShiftr 0xfffffff 5)
val t20 = % (fn () => testShiftr 0xfffffff 100)

val testShiftl = testBinop' W.<< W'.<<
val t21 = % (fn () => testShiftl 0xfffffff 5)
val t22 = % (fn () => testShiftl 0xfffffff 100)

val testAshiftl = testBinop' W.~>> W'.~>>
val t23 = % (fn () => testAshiftl 0xfffffff 5)
val t24 = % (fn () => testAshiftl 0xfffffff 100)
val t25 = % (fn () => 
                         U.assert
                            (W.toLargeInt(W.~>>(W.fromLargeInt 0xffffffff,Word.fromInt 5)) = 
                             W'.toLargeInt(W'.~>>(W'.fromLargeInt 0xffffffff,Word.fromInt 5))))

val test = fn () => $("Bigword tests",
                      & [t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12,
                         t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23,
                         t24, t25])

end
