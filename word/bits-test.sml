
structure BitsTest :> UNIT_TEST =
struct

open GeneralExt

structure B = Bits
structure W = Word8
structure L = ListExt
structure V = Word8Vector
structure U = UnitTests

open B.Ops
open U.Ops

(* --------------------------  word8vectorToBits  --------------------------- *)

fun word8vectorToBitsT vec bits = 
    U.assertEqualEq(bits, B.word8vectorToBits vec)

val c0 = $ ("c0",%(fn () => word8vectorToBitsT (V.fromList [W.fromInt 0]) (L.replicate(8,O))))
val c1 = $ ("c1",%(fn () => word8vectorToBitsT (V.fromList [W.fromInt 255]) (L.replicate(8,I))))
val c2 = $ ("c2",%(fn () => word8vectorToBitsT (V.fromList [W.fromInt 0,W.fromInt 0]) (L.replicate(16,O))))
val c3 = $ ("c3",%(fn () => word8vectorToBitsT (V.fromList [W.fromInt 255,W.fromInt 255]) (L.replicate(16,I))))
val c4 = $ ("c4",%(fn () => word8vectorToBitsT (V.fromList [W.fromInt 128,W.fromInt 1]) (I::L.replicate(14,O)@[I])))

(* --------------------------  bitsToWord8vector  --------------------------- *)

fun bitsToWord8vectorT (vec:V.vector) (bits:B.Ops.bit list) = 
    U.assertEqual (fn xy => V.collate W.compare xy = EQUAL) (fn _ => "<vec>") (vec, B.bitsToWord8vector bits)

val d0 = $ ("d0",%(fn () => bitsToWord8vectorT (V.fromList [W.fromInt 0]) (L.replicate(8,O))))
val d1 = $ ("d1",%(fn () => bitsToWord8vectorT (V.fromList [W.fromInt 255]) (L.replicate(8,I))))
val d2 = $ ("d2",%(fn () => bitsToWord8vectorT (V.fromList [W.fromInt 0,W.fromInt 0]) (L.replicate(16,O))))
val d3 = $ ("d3",%(fn () => bitsToWord8vectorT (V.fromList [W.fromInt 255,W.fromInt 255]) (L.replicate(16,I))))
val d4 = $ ("d4",%(fn () => bitsToWord8vectorT (V.fromList [W.fromInt 128,W.fromInt 1]) (I::L.replicate(14,O)@[I])))

val test = fn () => $("BitsTest",&[c0, c1, c2, c3, c4, d0, d1, d2, d3, d4])

end
