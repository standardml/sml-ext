
(** 
 * This tests the Float implementation of floating point querying
 * functions.  The tests only use machine floating point numbers that are
 * exactly representable, and should thus succeed on any processor/compiler setup.
 *)
structure FloatTest :> UNIT_TEST =
struct

structure F = Float64
structure U = UnitTests

open Bits.Ops
open U.Ops

(* -------------------------------------------------------------------------- *)
(*  Conversions                                                               *)
(* -------------------------------------------------------------------------- *)

(* -----------------------------  bitsWord64  ------------------------------ *)

fun testConstants x y = U.assertEqualReal (x, F.bitfloatToReal y)
                        
val a0 = $("a0 ",%(fn () => testConstants 
                               1.0 
                               (F.makeBitfloat(O,[I,I,I,I,I,I,I,I,I,I],[]))))

val a1 = $("a1 ",%(fn () => testConstants 
                               1.5
                               (F.makeBitfloat(O,[I,I,I,I,I,I,I,I,I,I],[I]))))

(* -------------------------------------------------------------------------- *)
(*  Infinity                                                                  *)
(* -------------------------------------------------------------------------- *)


(* -------------------------------------------------------------------------- *)
(*  +- 0                                                                      *)
(* -------------------------------------------------------------------------- *)


(* -------------------------------------------------------------------------- *)
(*  nextFP routines                                                           *)
(* -------------------------------------------------------------------------- *)

(* overflow *)
val n3 = $("n3",%(fn () => 
                     U.assert ((ignore (F.nextFp F.maxPosNormalized)
                              ; false) 
                               handle _ => true)))

val test = fn () => $("Float",&[a0,a1,n3])

end
