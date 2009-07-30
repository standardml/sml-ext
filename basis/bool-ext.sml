
structure BoolExt : BOOL_EXT  =
struct 

fun and'(x,y) = x andalso y
fun or'(x,y) = x orelse y

structure Ops =
   struct 
      val && = and'
      val || = or'
   end

fun compare (true, true) = EQUAL
  | compare (true, false) = GREATER
  | compare (false, true) = LESS
  | compare (false, false) = EQUAL

open Bool

end
