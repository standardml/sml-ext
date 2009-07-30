
structure RealExtTest :> UNIT_TEST =
struct

structure R = RealExt
structure U = UnitTests
open U.Ops

val mkTest = U.assertEqualReal
               
fun fromBitString0 () = 
    let
       val piBits = R.to64BitString Math.pi
       val pi = case R.from64BitString piBits of
                   SOME p => p
                 | NONE => raise Fail ""
    in
       mkTest (Math.pi, pi)
    end
val fromBitString0 = $("fromBitString0",%fromBitString0)
               
fun fromBitString1 () = 
    let
       val piBits = R.to64BitString (Math.sqrt 2.0)
       val sqrt2 = case R.from64BitString piBits of
                      SOME p => p
                    | NONE => raise Fail ""
    in
       mkTest (sqrt2, Math.sqrt 2.0)
    end
val fromBitString1 = $("fromBitString1",%fromBitString1)

val test = fn () => $("RealExt",&[fromBitString0,fromBitString1])

end
