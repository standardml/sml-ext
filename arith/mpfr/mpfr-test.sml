
structure MPFRTest =
struct 

structure R = MPFR
open GeneralExt 

fun harmonicAux n store =
    if n = 0 then store 
    else harmonicAux (n - 1) (R.add(store, R.div(R.one, R.fromInt n)))

fun harmonic n = harmonicAux n R.zero

fun doit () = 
    let
       val x = R.fromString "1.7"
       val y = R.fromString "2.3"
       val z = R.add(x, y)
       val z' = R.atan z
    in
       printl ("x = " ^ R.toString x)
     ; printl ("y = " ^ R.toString y)
     ; printl ("z = " ^ R.toString z)
     ; printl ("atan z = " ^ R.toString z')
     ; case R.compare(x, y) of 
          LESS => printl "x < y"
        | GREATER => printl "x > y"
        | EQUAL => printl "x = y"
     ; printl (R.toString (harmonic 100))
    end

end
