
structure Either :> EITHER =
struct

datatype ('a, 'b) either = Left of 'a
                         | Right of 'b

fun either f g e = 
    case e of 
       Left x => f x
     | Right x => g x

val rec lefts: ('a, 'b) either list -> 'a list = 
 fn [] => []
 | Left x :: xs => x :: lefts xs
 | Right _ :: xs => lefts xs

val rec rights: ('a, 'b) either list -> 'b list = 
 fn [] => []
 | Left _ :: xs => rights xs
 | Right x :: xs => x :: rights xs

val rec partition: ('a, 'b) either list -> 'a list * 'b list =
 fn [] => ([], [])
 | x :: xs => 
   let
      val (vs, ws) = partition xs
   in 
      case x of 
         Left v => (v::vs, ws)
       | Right w => (vs, w::ws)
   end

end
