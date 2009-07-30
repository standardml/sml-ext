
structure Bits:> BITS =
struct

open GeneralExt

structure V = Word8Vector
structure W = Word8
structure L = ListExt

structure Ops =
   struct 
      
      datatype bit = O | I

   end

open Ops

fun bitToString b = case b of
                       O => "O" 
                     | I => "I"

fun chopHead p l = 
    case l of
       [] => []
     | h::t => if p h then chopHead p t else l

fun chopZeros l = chopHead (Fun.curry op= O) l

fun intToBits n = 
    let
       val w0 = Word32.fromInt 0
       val w1 = Word32.fromInt 1
       val n' = Word32.fromInt n
       fun raw k = Word32.andb(Word32.<<(w1,Word.fromInt k),n')
       val rawbits = map raw (L.upto1(0,32)) 
       val bits = rev (map (fn x => if w0 < x then I else O) rawbits)
    in
       chopZeros bits
    end

local
   open W
   infix << >>
   infix andb
   val w0 = fromInt 0
   val w1 = fromInt 1
   val w2 = fromInt 2
in

val bitsToWord8 : bit list -> Word8.word = 
    let
       fun bti l = case l of
                      [] => w0
                    | O::t => w2 * bti t
                    | I::t => w1 + w2 * bti t 
    in
       (fn l => if Int.>(length l,8) then raise Overflow else bti (rev l))
    end

fun word8ToBits n = 
    let
       fun raw k = if ((w1 << Word.fromInt k) andb n) > w0 then I else O
       val bits = rev(map raw (L.upto1(0,8)))
    in
       bits
    end

fun slice8 l = 
    let
       val n1 = Int.mod(length l,8)
       val first = List.take(l,n1)
       val l' = List.drop(l,n1)
       fun getRest [] store = store
         | getRest l store = getRest (List.drop(l,8)) (List.take(l,8)::store)
       val rest = rev(getRest l' [])
    in
       case first of 
          [] => rest
        | _ => first::rest
    end

fun bitsToWord8vector bits = 
    let
       val ws = slice8 bits
       fun tabFn n = bitsToWord8 (List.nth(ws,n))
    in
       V.tabulate(length ws,tabFn)
    end

fun word8vectorToBits vec = V.foldr(fn (w,l) => word8ToBits w @ l) [] vec 

end (* local *)

fun word8vectorToString v = V.foldr (fn(w,s) => Word8.toString w ^ s) "" v

end
