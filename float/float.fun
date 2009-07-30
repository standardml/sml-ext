   
functor FloatFun(val exponentSize : int 
                 val mantissaSize : int):> FLOAT =
struct 

structure P = PackRealBig
structure B = Bits
structure L = ListExt
structure I = IntExt

open B.Ops

infixr 0 `
fun f ` x = f x
            
val exponentSize = exponentSize
val mantissaSize = mantissaSize

fun neg b = case b of O => I | I => O 

(* big-endian conversion *)
                                    
(* We maintain the invariant that all exps have length 11 and
 all fracs have length 52. *)
type bitfloat = {sign : bit,
                 exp : bit list,  (* big endian *)
                 frac : bit list} (* big endian *)

fun consistent {sign=_,exp,frac} = 
    length exp = exponentSize andalso length frac = mantissaSize

fun sign (b:bitfloat) = #sign b
fun exp (b:bitfloat) = #exp b
fun frac (b:bitfloat) = #frac b

(* -----------------------------  fill a list  ------------------------------ *)

fun fillBackK k l = 
    let
       val n = length l
    in
       if n > k then raise Fail ("fillBackK: more than " ^ Int.toString k ^ " digits") else
       l @ L.replicate(k - n,O)
    end

fun fillFrontK k l =
    let 
       val n = length l 
    in
       if n > k then raise Fail ("fillFrontK: more than " ^ Int.toString k ^ " digits") else
       L.replicate(k - n,O) @ l
    end

val fillExp = fillFrontK exponentSize
val fillMant = fillBackK mantissaSize

fun bitfloatToReal {sign,exp,frac} = 
    P.fromBytes ` B.bitsToWord8vector (sign :: exp @ frac)

fun word8vectorToBitfloat v = 
    let
       val bs = B.word8vectorToBits v
       val (sign,rest) = (hd bs,tl bs)
       val exp = List.take(rest,11)
       val frac = List.drop(rest,11)
    in
       {sign=sign, exp=exp, frac=frac}
    end

fun realToBitfloat r = word8vectorToBitfloat ` P.toBytes r

fun makeBitfloat(sign,exp,frac) = 
    if length exp > exponentSize then raise Fail "mkBitfloat: exponent has too many bits (max is 11)" else 
    if length frac > mantissaSize then raise Fail "mkBitfloat: frac has too many bits (max is 52)" else 
    {sign=sign,exp=fillExp exp,frac=fillMant frac}

fun destBitfloat (b:bitfloat) = (#sign b,#exp b,#frac b)

(* -------------------------------------------------------------------------- *)
(*  Constants                                                                 *)
(* -------------------------------------------------------------------------- *)

val minSignificand = L.replicate(mantissaSize - 1,O) @ [I] 
val maxSignificand = L.replicate(mantissaSize,I)
val zeroExponent = L.replicate(exponentSize,O)
val oneExponent = L.replicate(exponentSize - 1,O) @ [I]
val specialExponent = L.replicate(exponentSize,I)
val maxNormalizedExponent = L.replicate(exponentSize-1,I) @ [O]
val minNormalizedExponent = L.replicate(exponentSize-1,O) @ [I]
val zeroSignificand = L.replicate(mantissaSize,O)
val bias = I.pow(2,exponentSize-1) - 1

val maxExponent = bias
val denormalizedExponent = ~bias

fun intToFloatExponent n = fillExp(B.intToBits (n + bias))

(* ----------------------------------  0  ----------------------------------- *)

val posZero = makeBitfloat(O,[],[])
val negZero = makeBitfloat(I,[],[])
fun isZero ({exp,frac,...}:bitfloat) = not(L.mem(I,exp)) andalso not(L.mem(I,frac))

(* ----------------------------------  oo  ---------------------------------- *)

val posInfinity = makeBitfloat(O,specialExponent,[])
val negInfinity = makeBitfloat(I,specialExponent,[])
fun isInfinite ({exp,frac,...}:bitfloat) = not(L.mem(O,exp)) andalso not(L.mem(I,frac))

(* ---------------------------------  NaN  ---------------------------------- *)

val nan = makeBitfloat(O,specialExponent,[I])
fun isNan {sign=_,exp,frac} = not(L.mem(O,exp)) andalso L.mem(I,frac)

(* ------------------------  normalized boundaries  ------------------------- *)

val minPosNormalized = makeBitfloat(O,minNormalizedExponent,zeroSignificand)
val maxPosNormalized = makeBitfloat(O,maxNormalizedExponent,maxSignificand)
val minNegNormalized = makeBitfloat(I,maxNormalizedExponent,maxSignificand)
val maxNegNormalized = makeBitfloat(I,minNormalizedExponent,zeroSignificand)
fun isNormalized (b:bitfloat) = L.mem(I,#exp b) andalso L.mem(O,#exp b)

(* -----------------------  denormalized boundaries  ------------------------ *)

val minPosDenormalized = makeBitfloat(O,zeroExponent,minSignificand)
val maxNegDenormalized = makeBitfloat(I,zeroExponent,minSignificand)
val maxPosDenormalized = makeBitfloat(O,zeroExponent,maxSignificand)
val minNegDenormalized = makeBitfloat(I,zeroExponent,maxSignificand)
fun isDenormalized (b:bitfloat) = not(L.mem(I,#exp b)) 

(* -------------------------------------------------------------------------- *)
(*  Operations on bit lists                                                   *)
(* -------------------------------------------------------------------------- *)

(* -------------------------------  addition  ------------------------------- *)
                                  
(* addtion of a single bit, x + y, with carry c *)
fun bitadd (true,O,O) = (false,I)
  | bitadd (true,O,I) = (true,O)
  | bitadd (true,I,O) = (true,O)
  | bitadd (true,I,I) = (true,I)
  | bitadd (false,O,O) = (false,O)
  | bitadd (false,O,I) = (false,I)
  | bitadd (false,I,O) = (false,I)
  | bitadd (false,I,I) = (true,O)

(** addition on big-endian lists of bits *)
fun add (true, xs, []) = add (false,xs,[I])
  | add (true, [], ys) = add (false,ys,[I])
  | add (false, xs, []) = xs
  | add (false, [], ys) = ys
  | add (carry, x::xs, y::ys) =
    let 
       val (carry',z) = bitadd(carry,x,y)
    in
       z::add(carry',xs,ys)
    end

infix ++ -- 
fun x ++ y = rev(add(false,rev x,rev y))

(* -----------------------------  subtraction  ------------------------------ *)

(* addtion of a single bit, x + y, with carry c *)
fun bitsub (true,O,O) = (true,I)
  | bitsub (true,O,I) = (true,O)
  | bitsub (true,I,O) = (false,O)
  | bitsub (true,I,I) = (true,I)
  | bitsub (false,O,O) = (false,O)
  | bitsub (false,O,I) = (true,I)
  | bitsub (false,I,O) = (false,I)
  | bitsub (false,I,I) = (false,O)

(* subtraction on big-endian lists of bits. assumes y < x *)
fun sub (true, [], []) = raise Fail "sub: x not less than y"
  | sub (true, xs, []) = sub (false,xs,[I])
  | sub (false, xs, []) = xs
  | sub (_,[],_) = raise Fail "sub: x not less than y"
  | sub (carry,x::xs,y::ys) = 
    let
       val (carry',z) = bitsub(carry,x,y)
    in
       z::sub(carry',xs,ys)
    end

fun x -- y = rev(sub(false,rev x,rev y)) 
             
(* ---------------------------------  next  --------------------------------- *)

(* next floating point number away from 0. *)
fun nextFpAway0 x = 
    let
       val (sign,exp,frac) = destBitfloat x
    in
       if not (L.mem(O,exp)) then (* special, i.e. nan, infinity *)
          raise Fail "nextFpAway0: special value"
       else if not (L.mem(I,exp)) then (* denormalized *)
          if not (L.mem(O,frac)) then (* max denormalized, next fp is normalized *)
             makeBitfloat(sign,minNormalizedExponent,zeroSignificand)
          else (* next fp is still denormalized *)
             makeBitfloat(sign,exp,frac ++ minSignificand)
       else (* normalized *)
          if not (L.mem(O,frac)) then (* next fp raises exponent *)
             if exp = maxNormalizedExponent then 
                raise Fail "nextFpAway0: overflow"
             else 
                makeBitfloat(sign,exp ++ oneExponent,zeroSignificand)
          else 
             makeBitfloat(sign,exp,frac ++ minSignificand)
    end

fun nextFpCloser0 x = 
    let
       val (sign,exp,frac) = destBitfloat x
    in
       if not (L.mem(O,exp)) then (* special *)
          raise Fail "nextFpCloser0: special value"
       else if not (L.mem(I,exp)) then (* denormalized *)
          if not (L.mem(I,frac)) then (* zero, next fp is min neg denormalized *)
             makeBitfloat (neg sign,zeroExponent,minSignificand)
          else (* prev fp is still denormalized and nonzero *)
             makeBitfloat(sign,exp,frac -- minSignificand)
       else (* normalized *)
          if not (L.mem(I,frac)) then (* prev fp lowers exponent *)
             if exp = minNormalizedExponent then 
                makeBitfloat(sign,zeroExponent,maxSignificand)
             else 
                makeBitfloat(sign,exp -- oneExponent,maxSignificand)
          else 
             makeBitfloat(sign,exp,frac -- minSignificand)
    end

fun nextFp b = 
    case #sign b of 
       O => nextFpAway0 b
     | I => nextFpCloser0 b

fun prevFp b = 
    case #sign b of
       O => nextFpCloser0 b
     | I => nextFpAway0 b

(* -------------------------------------------------------------------------- *)
(*  Comparison                                                                *)
(* -------------------------------------------------------------------------- *)

fun bitlistCompare([],[]) = EQUAL
  | bitlistCompare([],_) = LESS
  | bitlistCompare(_,[]) = GREATER
  | bitlistCompare(O::xs,O::ys) = bitlistCompare(xs,ys)
  | bitlistCompare(I::_,O::_) = GREATER
  | bitlistCompare(O::_,I::_) = LESS
  | bitlistCompare(I::xs,I::ys) = bitlistCompare(xs,ys)

fun bitwiseEqual(b1:bitfloat,b2) = b1 = b2

fun bitfloatCompare(b1,b2) =
    if isNan b1 orelse isNan b2 orelse 
       isInfinite b1 orelse isInfinite b2 then raise IEEEReal.Unordered else
    let
       val (s1,e1,f1) = destBitfloat b1
       val (s2,e2,f2) = destBitfloat b2
    in
       case (s1,s2) of
          (I,O) => if isZero b1 andalso isZero b2 then EQUAL else LESS
        | (O,I) => if isZero b1 andalso isZero b2 then EQUAL else GREATER
        | (O,O) => (case bitlistCompare(e1,e2) of 
                       EQUAL => bitlistCompare(f1,f2)
                     | LESS => LESS
                     | GREATER => GREATER)
        | (I,I) => case bitlistCompare(e1,e2) of 
                      EQUAL => bitlistCompare(f1,f2)
                    | LESS => GREATER
                    | GREATER => LESS
    end


fun bitfloatToString {sign,exp,frac} = 
    "\n{sign = " ^ B.bitToString sign ^ "\n" ^
    " exp = " ^ L.toString B.bitToString exp ^ "\n" ^
    " frac = " ^ L.toString B.bitToString frac ^ "}\n"

end
