
structure Deque :> CATENABLE_DEQUE =
struct 

open GeneralExt 

structure List = ListExt 

type 'a t = 'a list * 'a list

val empty = ([], [])

fun isEmpty (f, b) = null f andalso null b

fun cons (x, (f, b)) = (x::f, b)

datatype 'a viewl = EmptyL
                 | Cons of 'a * 'a t

fun viewl ([], []) = EmptyL
  | viewl (x :: f, b) = Cons(x, (f, b))
  | viewl ([], b) = Cons(List.last b, ([], List.butlast b))

datatype 'a viewr = EmptyR
                 | Snoc of 'a t * 'a

fun viewr ([], []) = EmptyR
  | viewr (f, x :: b) = Snoc((f, b), x)
  | viewr (f, []) = Snoc((List.butlast f, []), List.last f)

fun snoc ((f, b), x) = (f, x::b)

fun cat ((f1, b1), (f2, b2)) = (f1 @ rev b1, b2 @ rev f2)

fun toList (f, b) = f @ rev b
fun fromList l = (l, [])
fun singleton x = ([x], [])

end
