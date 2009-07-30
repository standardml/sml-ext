
structure Knitro:> KNITRO =
struct 

open GeneralExt 

type func = real vector -> real * real vector * real vector vector

datatype verbose = None
                 | Summary
                 | Iter10
                 | Iter
                 | IterVerbose
                 | IterX
                 | All

datatype opt = Opt of {obj: func,
                       constrs: func list,
                       lowerBnds: real vector,
                       upperBnds: real vector,
                       jacobianValid: bool,
                       hessianValid: bool,
                       verbose: verbose,
                       maxIters: int,
                       repeat: int,
                       checkFirstDerivs: bool}

fun minimize _ = raise Unimplemented
fun maximize _ = raise Unimplemented

type simp_func = real vector -> real

datatype simp = Simp of {obj: simp_func,
                         constrs: simp_func list,
                         lowerBnds: real vector,
                         upperBnds: real vector,
                         verbose: verbose,
                         maxIters: int,
                         repeat: int}

fun simpleMinimize _ = raise Unimplemented
fun simpleMaximize _ = raise Unimplemented

end


structure KnitroTest :> UNIT_TEST =
struct 

structure U = UnitTests
val test = fn () => U.TestLabel("Dummy", U.TestList[]) 

end
