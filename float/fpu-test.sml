
structure FPUTest:> UNIT_TEST =
struct 

infix ==
infix !=

structure F = Float64
structure R = RealExt
structure I = IEEERealExt
structure U = UnitTests

open Bits.Ops
open U.Ops

(* -------------------------------------------------------------------------- *)
(*  Comparisons                                                               *)
(* -------------------------------------------------------------------------- *)

val nan = 0.0 / 0.0
val op== = R.==


(* 
 We need to save the current rounding mode to avoid messing with the
 processor after tests.  We restore it as the last test
 *) 
val currentMode = ref I.TO_NEAREST

val save_mode = $("save_mode",%(fn () => currentMode := I.getRoundingMode ()))


val inf1 = $("inf1",%(fn () => U.assert (R.posInf == R.posInf)))
val inf3 = $("inf3",%(fn () => U.assert (not (R.posInf > R.posInf))))
val inf4 = $("inf4",%(fn () => U.assert (not (R.negInf > R.negInf))))
val inf5 = $("inf5",%(fn () => U.assert (R.negInf < R.posInf)))

val nan1 = $("nan1",%(fn () => U.assert (not (R.negInf == nan))))
val nan2 = $("nan2",%(fn () => U.assert (not (R.negInf < nan))))
val nan3 = $("nan3",%(fn () => U.assert (not (R.negInf > nan))))
val nan4 = $("nan4",%(fn () => U.assert (not (5.0 > nan))))
val nan5 = $("nan5",%(fn () => U.assert (not (5.0 < nan))))
val nan6 = $("nan6",%(fn () => U.assert (R.isNan nan)))
val nan7 = $("nan7",%(fn () => U.assert (R.isNan (nan + 5.0))))
val nan8 = $("nan8",%(fn () => U.assert (R.isNan (nan * 5.0))))
val nan9 = $("nan9",%(fn () => U.assert (R.isNan (nan / nan))))
val nan10 = $("nan10",%(fn () => U.assert (R.isNan (nan / 0.0))))
val nan11 = $("nan11",%(fn () => U.assert (R.isNan (~ nan))))

(* max(nan,x) = x *)
val nan12 = $("nan12",%(fn () => U.assert (R.==(R.max(nan,0.0),0.0))))
val nan13 = $("nan13",%(fn () => U.assert (R.==(R.min(nan,0.0),0.0))))

(* -------------------------------------------------------------------------- *)
(*  Constants                                                                 *)
(* -------------------------------------------------------------------------- *)

fun testConstants x y =
    U.assertEqualReal (F.bitfloatToReal x, y)

fun testConstants' x y =
    U.assert (F.bitwiseEqual(x, F.realToBitfloat y))
    
val c0 = $("c0",%(fn () => testConstants F.maxPosNormalized R.maxFinite))
val c1 = $("c1",%(fn () => testConstants F.minPosDenormalized R.minPos))
val c2 = $("c2",%(fn () => testConstants F.minPosNormalized R.minNormalPos))
val c3 = $("c3",%(fn () => testConstants F.posZero 0.0))
val c4 = $("c4",%(fn () => testConstants F.negZero ~0.0))
val c5 = $("c5",%(fn () => testConstants' F.posInfinity R.posInf))
val c6 = $("c6",%(fn () => testConstants' F.negInfinity R.negInf))

(* -------------------------------------------------------------------------- *)
(*  Next FP                                                                   *)
(* -------------------------------------------------------------------------- *)

fun testNextFp x y =
    U.assertEqualReal (F.bitfloatToReal (F.nextFp x), F.bitfloatToReal y)

val n0 = $("n0",%(fn () => testNextFp F.posZero F.minPosDenormalized))
val n1 = $("n1",%(fn () => testNextFp F.negZero F.minPosDenormalized))
val n2 = $("n2",%(fn () => testNextFp F.maxPosDenormalized F.minPosNormalized))

(* -------------------------------------------------------------------------- *)
(*  Rounding Modes                                                            *)
(* -------------------------------------------------------------------------- *)

(**
 * Check the behavior of important functions for correctness with respect to
 * IEEE 754 rounding modes.  Most important, apart from the essential +,-,*,/,
 * is that Math.sqrt is rounded correctly, as specified in IEEE 754.
 *)

fun up () = I.setRoundingMode I.TO_POSINF
fun down () = I.setRoundingMode I.TO_NEGINF
fun testRound x y = U.assertEqualReal (F.bitfloatToReal x, y)

(*

 In[7]:= RealDigits[1 / 10,2,61]

                   Out[7]= {{1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0,
                             0, 1, 1, 0, 0, 1, 1, 0, 0, 1,
                             1, 0, 0, 1, 1, 0, 0, 1, 1, 0,
                             0, 1, 1, 0, 0, 1, 1, 0, 0, 1,
                             1, 0, 0, 1, 1, 0, 0, 1, 1, 0,
                             0, 1, 1, 0, 0, 1, 1, 0, 0, 1},-3}

 *)

val m1 = $("m1",%(fn () =>
    let
       val tenthLo = F.makeBitfloat(O,F.intToFloatExponent (~4),
                                    [I, O, O, I, I, O, O, I, I, O,
                                     O, I, I, O, O, I, I, O, O, I,
                                     I, O, O, I, I, O, O, I, I, O,
                                     O, I, I, O, O, I, I, O, O, I,
                                     I, O, O, I, I, O, O, I, I, O,
                                     O, I])

       val mtenthLo = (down(); Option.valOf (Real.fromString "1.0") /
                               (Option.valOf (Real.fromString "10.0")))
       val mtenthLo' = (down(); Option.valOf (Real.fromString "0.1"))
       val tenthHi = F.makeBitfloat(O,F.intToFloatExponent (~4),
                                    [I, O, O, I, I, O, O, I, I, O,
                                     O, I, I, O, O, I, I, O, O, I,
                                     I, O, O, I, I, O, O, I, I, O,
                                     O, I, I, O, O, I, I, O, O, I,
                                     I, O, O, I, I, O, O, I, I, O,
                                     I, O])
       val mtenthHi = (up(); Option.valOf (R.fromString "1.0") / 
                             (Option.valOf (R.fromString "10.0")))
       val mtenthHi' = (up(); Option.valOf (Real.fromString "0.1"))
    in
       testRound tenthLo mtenthLo
     ; testRound tenthLo mtenthLo'
     ; testRound tenthHi mtenthHi
     ; testRound tenthHi mtenthHi'
    end))

val m2 = $("m2",%(fn () =>
    let
       val negTenthLo = F.makeBitfloat(I,F.intToFloatExponent (~4),
                                       [I, O, O, I, I, O, O, I, I, O,
                                        O, I, I, O, O, I, I, O, O, I,
                                        I, O, O, I, I, O, O, I, I, O,
                                        O, I, I, O, O, I, I, O, O, I,
                                        I, O, O, I, I, O, O, I, I, O,
                                        I, O])
       val negTenthLo' = (down(); Option.valOf (Real.fromString "~0.1"))
       val negTenthHi = F.makeBitfloat(I,F.intToFloatExponent (~4),
                                       [I, O, O, I, I, O, O, I, I, O,
                                        O, I, I, O, O, I, I, O, O, I,
                                        I, O, O, I, I, O, O, I, I, O,
                                        O, I, I, O, O, I, I, O, O, I,
                                        I, O, O, I, I, O, O, I, I, O,
                                        O, I])
       val negTenthHi' = (up(); Option.valOf (Real.fromString "~0.1"))
    in
       testRound negTenthLo negTenthLo'
     ; testRound negTenthHi negTenthHi'
    end))

val m3 = $("m3",%(fn () => 
    let
       (*
        In[4]:= RealDigits[Sqrt[2],2,60]

                          Out[4]= {{1, 0, 1, 1, 0, 1, 0, 1, 0, 0, 0,
                                    0, 0, 1, 0, 0, 1, 1, 1, 1, 0,
                                    0, 1, 1, 0, 0, 1, 1, 0, 0, 1,
                                    1, 1, 1, 1, 1, 1, 0, 0, 1, 1,
                                    1, 0, 1, 1, 1, 1, 0, 0, 1, 1,
                                    0, 0, 1, 0, 0, 1, 0, 0, 0},1}

        *)
       val sqrt2Lo = F.makeBitfloat(O,F.intToFloatExponent 0,
                                    (* don't include leading 1.  It's assumed by normalization. *)
                                    [O, I, I, O, I, O, I, O, O, O,
                                     O, O, I, O, O, I, I, I, I, O,
                                     O, I, I, O, O, I, I, O, O, I,
                                     I, I, I, I, I, I, O, O, I, I,
                                     I, O, I, I, I, I, O, O, I, I,
                                     O, O])
       val msqrt2Lo = (down();Math.sqrt (Option.valOf (R.fromString "2.0")))               
       val sqrt2Hi = F.makeBitfloat(O,F.intToFloatExponent 0,
                                    (* don't include leading 1.  It's assumed by normalization. *)
                                    [O, I, I, O, I, O, I, O, O, O,
                                     O, O, I, O, O, I, I, I, I, O,
                                     O, I, I, O, O, I, I, O, O, I,
                                     I, I, I, I, I, I, O, O, I, I,
                                     I, O, I, I, I, I, O, O, I, I,
                                     O, I])
       val msqrt2Hi = (up();Math.sqrt (Option.valOf (R.fromString "2.0")))
    in                
       testRound sqrt2Lo msqrt2Lo
     ; testRound sqrt2Hi msqrt2Hi
    end))

(* -------------------------------------------------------------------------- *)
(*  Infinities                                                                *)
(* -------------------------------------------------------------------------- *)

(* Infinities should propegate, perhaps converting into nans, but never back
 * into finite numbers.  Comparisons would ideally fail, but it's not terrible
 * if they do the "right thing", e.g. oo > 5.0, or even oo > -oo.
 * oo >= oo should definitely return false, and compare(oo,oo) should raise
 * and exception.  *)

(* -------------------------------------------------------------------------- *)
(*  Nan                                                                       *)
(* -------------------------------------------------------------------------- *)

(* Nan's should propegate through any computation.  They should never be converted
         into infinities.  Comparisons should all return false or raise exceptions. *)

fun testNan x = U.assert (R.isNan x)

(* nan should propegate *)

val b0 = $("b0 ",%(fn () => testNan nan))
val b1 = $("b1 ",%(fn () => testNan (nan + 0.0)))
val b2 = $("b2 ",%(fn () => testNan (nan - 0.0)))
val b3 = $("b3 ",%(fn () => testNan (nan * 0.0)))
val b4 = $("b4 ",%(fn () => testNan (nan / 0.0)))
val b5 = $("b5 ",%(fn () => testNan (~ nan)))

val b6 = $("b6 ",%(fn () => testNan (F.bitfloatToReal F.nan * F.bitfloatToReal F.posInfinity)))
val b7 = $("b7 ",%(fn () => testNan (F.bitfloatToReal F.nan * F.bitfloatToReal F.negInfinity)))
val b8 = $("b8 ",%(fn () => testNan (F.bitfloatToReal F.nan / F.bitfloatToReal F.posZero)))
val b9 = $("b9 ",%(fn () => testNan (F.bitfloatToReal F.nan / F.bitfloatToReal F.negZero)))
val b10 = $("b10 ",%(fn () => testNan (Math.sqrt (~ 1.0))))

val restore_mode = $("restore_mode",%(fn () => I.setRoundingMode (!currentMode)))

val test = fn () => 
              $("FPU",&[ 
                (* !!! must be first to save rounding mode !!! *) 
                save_mode, 
                inf1, inf3, inf4, inf5, nan1, nan2, nan3,
                nan4, nan5, nan6, nan7, nan8, nan9, nan10, nan11, nan12,
                nan13, c0, c1, c2, c3, c4, c5, c6, n0, n1, n2, m1, m2,
                m3, b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10,
                (* !!! must be last to restore rounding mode !!! *)
                restore_mode 
               ])

end
