 
structure CfsqpTest :> UNIT_TEST =
struct 

open GeneralExt

structure C = Cfsqp
structure U = UnitTests
structure R = RealExt
structure V = VectorExt
structure L = ListExt
open U.Ops
            
val epsilon = 1E~8
fun epsilonClose(x:real, y) = R.abs(x - y) < epsilon

(* -------------------------------------------------------------------------- *)
(*  CFSQP Manual Example                                                      *)
(* -------------------------------------------------------------------------- *)

fun objTest (v : real vector) =
    let
       val x0 = V.sub(v, 0)
       val x1 = V.sub(v, 1)
       val x2 = V.sub(v, 2)
    in
       x0 * x0 + x1 * x1 + x2 * x2
    end

val constrs =
    let 
       fun c1 v = 
           let
              val x0 = V.sub(v, 0)
              val x1 = V.sub(v, 1)
           in
              2.0 * x0 - x1
           end
       fun c2 v = 
           let
              val x1 = V.sub(v, 1)
              val x2 = V.sub(v, 2)
           in
              2.0 * x1 - x2
           end
    in
       [c1, c2]
    end

val low = V.tabulate(3, Fun.const 1.0)
val hi = V.tabulate(3, Fun.const 8.0)

val minimum = $("minimum", %(fn () =>
    let
       val minVal = 21.0
       val minPt = [1.0, 2.0, 4.0]
       val (x, xs) = C.minimize (C.Opt {obj = objTest,
                                        constrs = constrs,
                                        lowerBnds = low,
                                        upperBnds = hi,
                                        repeat = 10,
                                        verbose = 0})
       val () = printl ("minimum is: " ^ R.toString x ^ " at point: " ^ V.toString R.toString xs)
    in
       U.assert (epsilonClose(x, minVal))
     ; U.assert (List.all epsilonClose (L.zip(V.toList xs, minPt)))
    end))

val maximum = $("maximum", %(fn () =>
    let
       val maxVal = 84.0
       val maxPt = [2.0, 4.0, 8.0]
       val (x, xs) = C.maximize (C.Opt {obj = objTest,
                                        constrs = constrs,
                                        lowerBnds = low,
                                        upperBnds = hi,
                                        repeat = 10,
                                        verbose = 0})
       val () = printl ("maximum is: " ^ R.toString x ^ " at point: " ^ V.toString R.toString xs)
    in
       U.assert (epsilonClose(x, maxVal))
     ; U.assert (List.all epsilonClose (L.zip(V.toList xs, maxPt)))
    end))

val test = fn () => $("Cfsqp", &[minimum, maximum])

end
