
(* The types here are too complicated to be worth abstracting *)

structure RecordUpdate =
struct

open GeneralExt 
val id = Fun.id

datatype ('x, 'y) u = X of 'x | Y of 'y
datatype ('a, 'b) product = & of 'a * 'b
fun $ (a, f) = f a
infix &

val makeUpdate =
 fn z =>
    Fold.fold
       (((), (),
      fn f => f o X,
      fn (a, u) => case u of X x => x | _ => a),
     fn (p, up, _, _) => 
     fn (p2r, p2r', r2p) => 
     fn r =>
        Fold.fold ((p2r' (p id), up, r2p r),
                fn (_, _, p) => p2r p))
       z
       
val A =
 fn z =>
    Fold.step0
       (fn (_, _, p, up) =>
           (p, up, fn f => p (f o X) & (f o Y),
         fn (a & b, u) =>
            (case u of X x => up (a, x) | _ => a)
               & (case u of Y y => y | _ => b)))
       z

fun makeUpdate2 z = makeUpdate A A $ z
fun makeUpdate3 z = makeUpdate A A A $ z
(* fun makeUpdate4 z = makeUpdate A A A A $ z *)
(* fun makeUpdate5 z = makeUpdate A A A A A $ z *)
(* fun makeUpdate6 z = makeUpdate A A A A A A $ z *)
(* fun makeUpdate7 z = makeUpdate A A A A A A A $ z *)
(* fun makeUpdate8 z = makeUpdate A A A A A A A A $ z *)
(* fun makeUpdate9 z = makeUpdate A A A A A A A A A $ z *)
(* fun makeUpdate10 z = makeUpdate A A A A A A A A A A $ z *)
(* fun makeUpdate11 z = makeUpdate A A A A A A A A A A A $ z *)
(* fun makeUpdate12 z = makeUpdate A A A A A A A A A A A A $ z *)
(* fun makeUpdate13 z = makeUpdate A A A A A A A A A A A A A $ z *)

fun U s v = Fold.step0 (fn (r, up, p) => (r, up, up (p, s r v)))

end
