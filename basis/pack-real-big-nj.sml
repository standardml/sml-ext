
structure PackRealBig: PACK_REAL =
struct

open GeneralExt

type real = real
            
fun warn () = printl "Warning: You can't call PackRealBig in NJ!"
fun toBytes _ = (warn (); Word8Vector.tabulate(0,fn _ => Word8.fromInt 0))
fun fromBytes _ = (warn (); Real.posInf)
val bytesPerElem = ~1
val isBigEndian = false
fun subVec _ = (warn (); Real.posInf)
fun subArr _ = (warn (); Real.posInf)
fun update _ = warn ()

end
