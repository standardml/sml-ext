
(* comes before ListExt, so don't use list functions. *)
structure StringExt: STRING_EXT =
struct 

structure C = Char
structure L = List
structure S = String
structure A = Array

fun shuffle ([],l2) = l2
  | shuffle (l1,[]) = l1
  | shuffle (h1::t1,h2::t2) = h1::h2::shuffle(t1,t2)

fun paren s = "(" ^ s ^ ")"
fun bracket s = "[" ^ s ^ "]"
fun arrbracket s = "[|" ^ s ^ "|]"
fun quote s = "\"" ^ s ^ "\""
fun curly s = "{" ^ s ^ "}"
fun abracket s = "<" ^ s ^ ">"

val commas = S.concatWith ","
val semis = S.concatWith ";"
val scommas = S.concatWith ", "
val ssemis = S.concatWith "; "
val spaces = S.concatWith " "
fun space n = S.implode(L.tabulate(n,fn _ => #" "))

fun capitalize s = 
    if s = "" then "" else
    let
       val (c,cs) = 
           case explode s of
              c::cs => (c,cs) 
            | _ => raise Fail "Impossible"
    in
       implode(C.toUpper c::map C.toLower cs)
    end

val upcase = S.map C.toUpper
val downcase = S.map C.toLower

fun splitString c s = 
    let
       val cs = explode s
       fun foldFn(c',(s,l)) = if c = c' then ([],s::l) else (c'::s,l)
       val (s,l) = foldr foldFn ([],[]) cs
    in
       map implode (s::l)
    end

val null = S.str (C.chr 0)
fun nullTerminate x = x ^ null

fun chop seps s = 
    let
       fun chop [] charStore wordStore = 
           let 
              val wstore = if charStore = [] 
                              then wordStore
                           else charStore::wordStore 
              fun implode' [] = NONE
                | implode' s = SOME (implode s)
           in
              rev (List.mapPartial (implode' o rev) wstore)
           end
         | chop (c::cs) charStore wordStore =
           if L.exists (fn x => x = c) seps then chop cs [] (charStore::wordStore)
           else chop cs (c::charStore) wordStore
    in
       chop (explode s) [] []
    end

(* index'' f l n *)
fun index'' _ [] _ = NONE
  | index'' f (h::t) n = if f h then SOME n else index'' f t (n+1)

fun index' f l = index'' f l 0

fun index s c = index' (fn x => x = c) (explode s)

fun arrayToList v = A.foldr op:: [] v

fun update (s,n,c) =
    let
       val ar = A.fromList (explode s)
       val () = A.update(ar,n,c)
    in
       implode (arrayToList ar)        
    end

fun isCapitalized s = S.size s <> 0 andalso Char.isUpper(S.sub(s,0))

local
   val null = Char.chr 0
in
fun truncateCString s = 
    let
       fun index' _ [] _ = NONE
         | index' f (h::t) n = if f h then SOME n else index' f t (n+1)
       fun index f l = index' f l 0
       val s' = explode s
    in
       case index (fn x => x = null) s' of
          NONE => raise Fail "truncateCString: Not a C string"
        | SOME n => String.substring(s, 0, n)
    end
end (* local *) 

open S

end
