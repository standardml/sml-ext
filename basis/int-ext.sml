
structure IntExt: INT_EXT =
struct 

fun sqrt x = Real.floor (Math.sqrt (real x))

(* n^k by repeated squaring, cps form *) 
fun pow (n, k) = 
    let
       fun pow (_, 0) c = c 1
         | pow (n, 1) c = c n
         | pow (n, k) c =
           let
              val k' = k div 2
              val odd = k mod 2 = 1
           in 
              pow (n, k') (fn res => if odd then c (n * res * res) else c (res * res))
           end
    in
       if k >= 0 then pow (n, k) Fun.id else raise Fail "pow: negative exponent"
    end

local 
   fun log' store base n = if n < base then store else log' (1 + store) base (n div base)
in
fun log base n = log' 0 base n
end

val log2 = log 2
val log10 = log 10

fun sum l = foldr op+ 0 l

fun odd x = x mod 2 = 1
fun even x = x mod 2 = 0

val i0 = IntInf.fromInt 0
val i1 = IntInf.fromInt 1

fun infToReal n = 
    let
       open IntInf
       infix ~>>
       datatype sign = Pos | Neg
       fun ! x = fromInt x
       val two = !2
       val sign = if n < !0 then Neg else Pos
       val n = case sign of Pos => n | Neg => ~ n
       fun doit n exp x = 
           if n = !0 then x else
           let
              val b = n mod two
              val x' = if b = !0 then x else (Real.+(x,Math.pow(2.0,real exp)))
           in
              doit (n ~>> 0wx1) (Int.+(exp,1)) x' 
           end
       val unsigned = doit n 0 0.0
    in
       case sign of 
          Pos => unsigned
        | Neg => Real.~ unsigned
    end
    
fun maxList' [] x = x
  | maxList' (h::t) x = if h > x then maxList' t h else maxList' t x

fun maxList (h::t) = maxList' t h
  | maxList [] = raise List.Empty
    
fun minList' [] x = x
  | minList' (h::t) x = if h < x then minList' t h else minList' t x

fun minList (h::t) = minList' t h
  | minList [] = raise List.Empty

open Int

end
