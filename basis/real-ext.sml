
structure RealExt: REAL_EXT = 
struct 

structure P = PackRealBig
structure V = Word8Vector
structure R = Real
structure W = Word8
structure L = ListExt

val nan = 0.0 / 0.0

fun word8ToPairString x = 
    let
       val s = W.toString x 
    in
       if String.size s = 2 then s else "0" ^ s
    end

fun word8vectorToString v = V.foldr (fn(w,s) => word8ToPairString w ^ s) "" v

(* Assumes 64 bit float, and we are given either a 16 digit hexidecimal string. *)
exception Done

fun word8vectorFromString s : Word8Vector.vector option = 
    let
       val cs = explode s
       val n = length cs
       val _ = if n <> 16
                  (* not 16 hexidecimal chars *) 
                  then raise Done 
               else ()
       val ps = L.split(2,cs)
       val ps' = map (W.fromString o implode) ps
    in
       if L.mem (NONE,ps')
          then NONE
       else SOME (V.fromList (map valOf ps'))
    end handle Done => NONE

fun to64BitString x = word8vectorToString (P.toBytes x)

fun from64BitString x = 
    Option.map P.fromBytes (word8vectorFromString x)

val npow = 
    let
       fun npow(_,0,acc) = acc
         | npow(x,n,acc) = npow(x,n-1,acc * x)
       fun npow'(x,n) = 
           if n < 0 then raise Fail "npow: negative" 
           else npow(x,n,1.0)
    in
       npow'
    end
    
open R

end
