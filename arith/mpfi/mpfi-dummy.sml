
structure MPFI :> MPFI =
struct 

type t = unit

nonfix div

fun toString _ = ""
fun fromString _ = ()
fun fromInt _ = ()
fun add _ = ()
fun sub _ = ()
fun mul _ = ()
fun div _ = ()
fun neg _ = ()
fun sqr _ = ()
fun sqrt _ = ()
fun inv _ = ()
fun abs _ = ()
fun atan _ = ()
fun infinite _ = false
fun finite _ = false
fun isNan _ = false
fun isZero _ = false
val zero = ()
val one = ()
val pi = ()
val nan = ()
fun compare _ = SOME EQUAL
fun fromReal _ = ()
fun toReal _ = 0.0
fun left _ = MPFR.zero
fun right _ = MPFR.zero
fun make _ = ()

end
