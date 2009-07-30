
(* Names that can be generated uniquely *)
functor NameFn(val name: string) = 
struct 

open GeneralExt  

type t = string * int 

(* Start counter from 1 so user-defined symbols can
   have index 0 *) 
local
   val ctr = ref 1
in
fun newNamed s = (s, !ctr) before Ref.incr ctr
fun reset () = ctr := 1
fun new () = newNamed name
fun freshen (s, _) = newNamed s
end

fun name (s, _) = s

fun fromString f = (f, 0)

fun toString (f, 0) = f 
  | toString (f, n) = f ^ "_" ^ Int.toString n

val pp = PP.string o toString


fun marshal (s, i, _) = (s, i)
fun unmarshal (s, i) = (s, i, false)

val compare = Order.lexOrder String.compare Int.compare 

fun eq m12 = compare m12 = EQUAL

fun hash (s, n) = Word.+(HashString.hashString s, Word.fromInt n)

structure Set = OrdSetExtFn(struct 
                               type ord_key = t
                               val compare = compare 
                               val ppItem = PP.string o toString
                            end)

structure Map = OrdMapExtFn(struct 
                               type ord_key = t
                               val compare = compare 
                            end)

end
