
structure Fold :> FOLD =
struct

val id = Fun.id
fun $ (a, f) = f a

type ('a, 'b, 'c, 'd) step = 'a * ('b -> 'c) -> 'd

type ('a, 'b, 'c, 'd) t = ('a, 'b, 'c, 'd) step -> 'd

type ('a1, 'a2, 'b, 'c, 'd) step0 =
     ('a1, 'b, 'c, ('a2, 'b, 'c, 'd) t) step

type ('a11, 'a12, 'a2, 'b, 'c, 'd) step1 =
     ('a12, 'b, 'c, 'a11 -> ('a2, 'b, 'c, 'd) t) step

fun fold (a: 'a, f: 'b -> 'c)
         (g: ('a, 'b, 'c, 'd) step): 'd =
    g (a, f)

fun step0 (h: 'a1 -> 'a2)
          (a1: 'a1, f: 'b -> 'c): ('a2, 'b, 'c, 'd) t =
    fold (h a1, f)

fun step1 (h: 'a11 * 'a12 -> 'a2)
          (a12: 'a12, f: 'b -> 'c)
          (a11: 'a11): ('a2, 'b, 'c, 'd) t =
    fold (h (a11, a12), f)

fun lift0 (s: ('a1, 'a2, 'a2, 'a2, 'a2) step0)
          (a: 'a1, f: 'b -> 'c): ('a2, 'b, 'c, 'd) t =
    fold (fold (a, id) s $, f)
    
fun post (w: ('a, 'b, 'c1, 'd) t,
          g: 'c1 -> 'c2)
         (s: ('a, 'b, 'c2, 'd) step): 'd =
    w (fn (a, h) => s (a, g o h))

(* MLton gives unused warning here.  Weird*)
val _ = (post, lift0, step1)
val (_ : (int, int, int, int, int, int) step1 -> 'a) = fn _ => raise Fail ""

end
