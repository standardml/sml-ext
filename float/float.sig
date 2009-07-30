
(*
 *  Conversions between floating point numbers (real) and bit
 *  patterns.  This is convenient when you need to examine the 
 *  fpu of your hardware, or for interval arithmetic when you 
 *  need precise control over the creation of constants.  
 *  One need be cautious of your compiler.  For example, both
 *  MLton and Poly/ML botch this example:
 *
 * 
 * structure R = Real
 * structure I = IEEEReal
 * structure V = Word8Vector
 * structure P = PackRealBig
 *
 * fun up() = I.setRoundingMode I.TO_POSINF
 * fun down() = I.setRoundingMode I.TO_NEGINF
 *
 * val mtenth_lo = (down();1.0 / 10.0)
 * val mtenth_hi = (up();1.0 / 10.0)
 *
 * fun word8vectorToString v = V.foldr (fn(w,s) => Word8.toString w ^ s) "" v
 *
 * val _ = print(word8vectorToString (P.toBytes mtenth_lo) ^ "\n")
 * val _ = print(word8vectorToString (P.toBytes mtenth_hi) ^ "\n")
 *
 * /usr/BACKUP/usr1/seanmcl/save/versioned/projects/kepler/sml/sml-ext
 * $ mlton -cc-opt '-I /usr0/local/include' fpu-bug.sml
 *
 * /usr/BACKUP/usr1/seanmcl/save/versioned/projects/kepler/sml/sml-ext
 * $ fpu-bug
 * 3FB9999999999999
 * 3FB9999999999999
 *
 * It seems they do constant folding, and thus the operations get carried
 * out at compile time, at whatever rounding mode happens to be active.
 * Real.fromString may be unreliable (i.e. not respect rounding modes)  
 * and also may not give you the closest floating point values to the input string.
 *
 *)
signature FLOAT =
   sig

      type bit = Bits.Ops.bit

      type bitfloat
           
      val bitfloatToString : bitfloat -> string

      val exponentSize : int
      val mantissaSize : int
      val consistent : bitfloat -> bool

      (* Raises IEEEReal.Unordered when either argument is nan or infinite *)
      val bitfloatCompare: bitfloat * bitfloat -> order 
      val bitwiseEqual : bitfloat * bitfloat -> bool

      (* mkBitfloat(sign,exp,frac) *)
      val makeBitfloat : bit * bit list * bit list -> bitfloat 
      val destBitfloat : bitfloat -> bit * bit list * bit list

      val intToFloatExponent : int -> bit list

      val sign : bitfloat -> bit
      val exp : bitfloat -> bit list
      val frac : bitfloat -> bit list

      val bitfloatToReal : bitfloat -> real
      val realToBitfloat : real -> bitfloat

      val nextFp : bitfloat -> bitfloat
      val prevFp : bitfloat -> bitfloat
                               
      val posZero : bitfloat
      val negZero : bitfloat
      val isZero : bitfloat -> bool

      val posInfinity : bitfloat
      val negInfinity : bitfloat
      val isInfinite : bitfloat -> bool

      (* One of many possible nans *)
      val nan : bitfloat
      val isNan : bitfloat -> bool

      (* Normalized numbers have a nonzero exponent.  They are interpreted
       * as beginning with a 1 to achieve an extra bit of precision.  *)
      val maxPosNormalized : bitfloat
      val minPosNormalized : bitfloat
      val maxNegNormalized : bitfloat
      val minNegNormalized : bitfloat
      val isNormalized : bitfloat -> bool

      (* Denormalized numbers are very small.  They have exponent 0, 
       * and are interpreted as having no hidden leading 1.  The exponent is
       * ~1023 for a 64 bit float. *)
      val maxExponent : int
      val denormalizedExponent : int
      val maxPosDenormalized : bitfloat
      val minPosDenormalized : bitfloat
      val maxNegDenormalized : bitfloat
      val minNegDenormalized : bitfloat
      val isDenormalized : bitfloat -> bool

   end
