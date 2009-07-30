
structure MathExt : MATH_EXT =
struct 

open Math

fun atanh x = (1.0 / 2.0) * ln ((1.0 + x) / (1.0 - x))

end
