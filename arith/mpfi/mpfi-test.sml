
structure MPFITest =
struct 

structure I = MPFI
(* structure R = MPFR *)

open GeneralExt 

(* 
 * In[1]:= Harmonic[n_,store_] := If[n == 0,store,Harmonic[n-1,store + 1/n]]
 * In[2]:= Harmonic[n_] := Harmonic[n,0]
 * In[6]:= N[Harmonic[100],20]
 * Out[6]= 5.1873775176396202608
 *)

fun harmonicAux n store =
    if n = 0 then store 
    else harmonicAux (n - 1) (I.add(store, I.div(I.one, I.fromInt n)))

fun harmonic n = harmonicAux n I.zero

fun doit () = 
    let
       val x = I.fromString "1.7"
       val _ = printl ("x = " ^ I.toString x)
       val y = I.fromString "2.3"
       val _ = printl ("y = " ^ I.toString y)
       val z = I.fromString "0.0"
       val _ = printl ("z = " ^ I.toString z)
       val z' = I.zero
       val _ = printl ("z' = " ^ I.toString z')
       val abs = I.abs(I.sub(z, z'))
       val _ = printl ("abs = " ^ I.toString abs)
       val eps = I.fromString "1.0E-8"
       val _ = printl ("eps = " ^ I.toString eps)
(*        val z = I.add(x, y) *)
(*        val _ = printl ("z = " ^ I.toString z) *)
(*        val z' = I.atan z *)
(*        val _ = printl ("atan z = " ^ I.toString z') *)
(*        val x1 = I.make(R.fromString "-1.1", R.fromString "2.1") *)
(*        val _ = printl ("z = " ^ I.toString z) *)
(*        val x2 = I.abs x1 *)
(*        val _ = printl ("abs x1 = " ^ I.toString x2) *)
    in
(*        case I.compare(x1, x) of *)
       case I.compare(x, y) of
          SOME LESS => printl "x < y"
        | SOME GREATER => printl "x > y"
        | NONE => printl "overlap"
        | _ => raise Impossible 
     ; printl (I.toString (harmonic 100))
     ; case I.compare(abs, eps) of
          SOME LESS => printl "YES"
        | _ => printl "NO"

    end

end
