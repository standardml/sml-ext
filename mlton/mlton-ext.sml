
structure MLtonExt :> MLTON_EXT =
struct 

fun noisyRaise exn =
    let
       val cs = MLton.Exn.history exn
       val () = print (concat ["raising ",
                               exnName exn,
                               " with stack:\n"])
       val () = List.app (fn s => (print s; print "\n")) cs
    in
       raise exn
    end 

end
