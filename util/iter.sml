
structure Iter:> ITER =
struct

infix 2 to bigto downto
infix 1 @@ when by
infix 0 >>= **
infix &

type 'a t = ('a -> unit) -> unit

val return = Fun.pass
fun (iA >>= a2iB) f = iA (Fun.flip a2iB f)

val none = ignore

fun (l to u) (f : int -> unit) = 
    let fun `l = if l<u then (f l; `(l+1)) else () in `l end
fun (l bigto u) (f : IntInf.int -> unit) = 
    let open IntInf fun `l = if l<u then (f l; `(l+1)) else () in `l end
fun (u downto l) (f : int -> unit) = 
    let fun `u = if u>l then (f (u-1); `(u-1)) else () in `u end

fun in_list ? = Fun.flip List.app ?
fun in_vector ? = Fun.flip Vector.app ?
fun in_array ? = Fun.flip Array.app ?

fun opt fno fso = fn NONE => fno () | SOME ? => fso ?

fun using get s (f : 'a -> unit) = let fun `s = opt (Fun.const ()) (fn (x, s) => (f x; `s)) (get s) in `s end

fun (iA when p) f = iA (fn a => if p a then f a else ())
fun (iA by g) f = iA (f o g)
fun (iA @@ iB) f = (iA f : unit; iB f)
val op& = Pair.&
fun (iA ** iB) f = iA (fn a => iB (fn b => f (a & b)))

val for = Fun.id

fun triangle (l, u) = l to u >>= (fn i => i to u >>= (fn j => return (i, j)))
fun upper_triangle (l, u) = l to u >>= (fn i => i+1 to u >>= (fn j => return (i, j)))

end
