
structure Cfsqp :> CFSQP =
struct 

open GeneralExt

datatype opt = Opt of {obj: real vector -> real,
                       constrs: (real vector -> real) list,
                       lowerBnds: real vector,
                       upperBnds: real vector,
                       repeat: int,
                       verbose: int}

fun minimize _ = raise Unimplemented 
fun maximize _ = raise Unimplemented 

end


structure CfsqpTest :> UNIT_TEST =
struct 

structure U = UnitTests
val test = fn () => U.TestLabel("Dummy", U.TestList[]) 

end
