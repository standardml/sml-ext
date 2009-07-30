 
structure KnitroTest :> UNIT_TEST =
struct 

structure K = Knitro
structure U = UnitTests
structure R = RealExt
structure V = VectorExt
structure L = ListExt

open GeneralExt
open U.Ops
nonfix &&
val && = U.Ops.&

nonfix &
fun & l = V.fromList l
            
(* 1E~8 is too tight *)
val epsilon = 1E~4
fun epsilonClose(x:real,y) = R.abs(x - y) < epsilon

fun obj (v : real vector) =
    let
       val x0 = V.sub(v,0)
       val x1 = V.sub(v,1)
       val x2 = V.sub(v,2)
    in
       (x0 * x0 + x1 * x1 + x2 * x2,
        V.map (fn x => 2.0 * x) v,
        &[&[2.0,0.0,0.0],
          &[0.0,2.0,0.0],
          &[0.0,0.0,2.0]])
    end

val zeros = &[&[0.0,0.0,0.0],
              &[0.0,0.0,0.0],
              &[0.0,0.0,0.0]]

val constrs =
    let 
       fun c1 v = 
           let
              val x0 = V.sub(v,0)
              val x1 = V.sub(v,1)
           in
              (2.0 * x0 - x1,
               &[2.0,~1.0,0.0],
               zeros)
           end
       fun c2 v = 
           let
              val x1 = V.sub(v,1)
              val x2 = V.sub(v,2)
           in
              (2.0 * x1 - x2,
               V.fromList [0.0,2.0,~1.0],
               zeros)
           end
    in
       [c1,c2]
    end

val low = V.tabulate(3,Fun.const 1.0)
val hi = V.tabulate(3,Fun.const 8.0)

val minimum = $("minimum",%(fn () => 
    let
       val minVal = 21.0
       val minPt = [1.0,2.0,4.0]
       val (x,xs) = K.minimize (K.Opt {obj = obj,
                                       constrs = constrs,
                                       lowerBnds = low,
                                       upperBnds = hi,
                                       jacobianValid = true,
                                       hessianValid = true,
                                       verbose = K.None,
                                       maxIters = 100,
                                       repeat = 3,
                                       checkFirstDerivs = false})
       val () = printl ("minimum is: " ^ R.toString x ^ " at point: " ^ V.toString R.toString xs)
    in
       U.assert (epsilonClose(x,minVal))
     ; U.assert (List.all epsilonClose (L.zip(V.toList xs,minPt)))
    end))

val maximum = $("maximum",%(fn () => 
    let
       val maxVal = 84.0
       val maxPt = [2.0,4.0,8.0]
       val (x,xs) = K.maximize (K.Opt {obj = obj,
                                       constrs = constrs,
                                       lowerBnds = low,
                                       upperBnds = hi,
                                       jacobianValid = true,
                                       hessianValid = true,
                                       verbose = K.None,
                                       maxIters = 100,
                                       repeat = 3,
                                       checkFirstDerivs = false})
       val () = printl ("maximum is: " ^ R.toString x ^ " at point: " ^ V.toString R.toString xs)
    in
       U.assert (epsilonClose(x,maxVal))
     ; U.assert (List.all epsilonClose (L.zip(V.toList xs,maxPt)))
    end))

(* XXX this isn't working *)
(* 
fun check () =
    let

       val zeros = &[&[0.0,0.0,0.0],
                     &[0.0,0.0,0.0],
                     &[0.0,0.0,0.0]]

       fun obj (v : real vector) =
           let
              val x0 = V.sub(v,0)
              val x1 = V.sub(v,1)
              val x2 = V.sub(v,2)
           in
              (x0 * x0 + x1 * x1 + x2 * x2,
               V.map (fn x => 3.5 * x) v,
               zeros)
           end

       val constrs =
           let 
              fun c1 v = 
                  let
                     val x0 = V.sub(v,0)
                     val x1 = V.sub(v,1)
                  in
                     (2.0 * x0 - x1,
                      &[2.7,~1.8,0.5],
                      zeros)
                  end
              fun c2 v = 
                  let
                     val x1 = V.sub(v,1)
                     val x2 = V.sub(v,2)
                  in
                     (2.0 * x1 - x2,
                      V.fromList [0.74,2.3,~5.0],
                      zeros)
                  end
           in
              [c1,c2]
           end

       val (x,xs) = K.maximize{obj = obj,
                               constrs = constrs,
                               lowerBnds = low,
                               upperBnds = hi,
                               jacobianValid = true,
                               hessianValid = false,
                               verbose = true,
                               repeat = 1,
                               checkFirstDerivs = true}
       val () = printl ("maximum is: " ^ R.toString x ^ " at point: " ^ V.toString R.toString xs)
    in
       ()
    end
*) 

val test = fn () => $("Knitro",&&[minimum,maximum])

end

