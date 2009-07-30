
structure Fun:> FUN =
struct

infixr 0 `

fun apply x f = f x

fun const x _ = x

fun flip f x y = f y x

fun swap f (x,y) = f(y,x)

fun id x = x

fun pass x f = f x

fun curry f x y = f(x,y)

fun uncurry f (x,y) = f x y

fun f ` x = f x

fun repeat (f,x) = repeat (f,f x) handle _ => x

fun repeat' (0,_,x) = x
  | repeat' (n,f,x) = repeat'(n-1,f,f x)

fun repeatN (t as (n,_,_)) = 
    if n < 0 then raise Fail "repeat: negative" else repeat' t

fun compose (f, g) = f o g

end
