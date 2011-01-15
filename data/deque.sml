
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

fun take (0, _) = []
  | take (n, q) = case viewl q of 
                     EmptyL => []
                   | Cons(x, q) => x::take(n-1, q)

fun drop (0, q) = q
  | drop (n, q) = case viewl q of 
                     EmptyL => q
                   | Cons(_, q) => drop(n-1, q)

datatype 'a viewr = EmptyR
                 | Snoc of 'a t * 'a

fun viewr ([], []) = EmptyR
  | viewr (f, x :: b) = Snoc((f, b), x)
  | viewr (f, []) = Snoc((List.butlast f, []), List.last f)

fun snoc ((f, b), x) = (f, x::b)

fun cat ((f1, b1), (f2, b2)) = (f1 @ rev b1, b2 @ rev f2)

fun toList (f, b) = f @ rev b

fun foldr f x = List.foldr f x o toList

fun fromList l = (l, [])
fun singleton x = ([x], [])
fun exists p (f, b) = List.exists p f orelse List.exists p b

datatype 'a viewl2 = EmptyL2
                  | SingL2 of 'a
                  | Cons2 of 'a * 'a * 'a t

fun viewl2 t =
    case viewl t of
       EmptyL => EmptyL2 
     | Cons(x1, t) => 
       case viewl t of
          EmptyL => SingL2 x1
        | Cons(x2, t) => Cons2(x1, x2, t)

datatype 'a viewr2 = EmptyR2
                  | SingR2 of 'a
                  | Snoc2 of 'a t * 'a * 'a

fun viewr2 t =
    case viewr t of
       EmptyR => EmptyR2 
     | Snoc(t, x2) =>
       case viewr t of
          EmptyR => SingR2 x2
        | Snoc(t, x1) => Snoc2(t, x2, x1)

fun map f (a, b) = (List.map f a, List.map f b)

end
