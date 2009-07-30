
(* 
 An interface to the Knitro nonlinear optimization library.
 *) 
signature KNITRO =
sig

type func = real vector -> real * real vector * real vector vector

datatype verbose = None
                 | Summary
                 | Iter10
                 | Iter
                 | IterVerbose
                 | IterX
                 | All

(* constraints are satisfied if the return value is <= 0.0 *)
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

val minimize: opt -> real * real vector
val maximize: opt -> real * real vector

type simp_func = real vector -> real

datatype simp = Simp of {obj: simp_func,
                         constrs: simp_func list,
                         lowerBnds: real vector,
                         upperBnds: real vector,
                         verbose: verbose,
                         maxIters: int,
                         repeat: int}

val simpleMinimize: simp -> real * real vector
val simpleMaximize: simp -> real * real vector

end
