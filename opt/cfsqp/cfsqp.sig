
(*
 An interface to the CFSQP nonlinear optimization library.  
 *)
signature CFSQP =
sig

datatype opt = Opt of {obj: real vector -> real,
                       constrs: (real vector -> real) list,
                       lowerBnds: real vector,
                       upperBnds: real vector,
                       repeat: int,
                       verbose: int} (* 0 = low, 3 = high *)

val minimize: opt -> real * real vector

val maximize: opt -> real * real vector

end
