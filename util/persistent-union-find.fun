
(* 
Union-Find 

Follows Tarjan.  Of the two main optimizations, ranking and
path compression, we only do ranking. 
We can't implement path compression without
imperative features.  We want to stay out of the state monad
for now, so we elect to ignore it.  

Adapeted from John Harrison's O'Caml code for his book Automated Theorem Proving
*) 

functor PersistentUnionFindFn(K: ORD_KEY)
  :> PERSISTENT_UNION_FIND where type K.ord_key = K.ord_key =
struct 

structure K = K
structure L = ListExt

open GeneralExt 

structure M = OrdMapExtFn(K)

val |-> = M.|->
infixr 5 |-> 

datatype pnode = Nonterminal of K.ord_key
               | Terminal of K.ord_key * int

datatype union = Union of pnode M.map

fun getElem (Nonterminal x) = x
  | getElem (Terminal (x, _)) = x

fun terminus (ptn as Union f) a =
    case M.find(f, a) of
       SOME (Nonterminal b) => terminus ptn b
     | SOME (Terminal pq) => SOME pq
     | NONE => NONE 

fun tryterminus ptn a =
    case terminus ptn a of
       SOME x => x
     | NONE => (a,1)

fun inDomain (ptn, a) = not (isSome (terminus ptn a)) 

fun find ptn a = fst(tryterminus ptn a)

fun equal eqv (a, b) = 
    case K.compare(find eqv a, find eqv b) of
       EQUAL => true
     | _ => false

fun union (ptn as Union f) (a,b) =
  let
     val (a',na) = tryterminus ptn a
     val (b',nb) = tryterminus ptn b 
     val f' = case K.compare(a', b') of
                 EQUAL => f
               | _ => 
                 if na <= nb then
                    (a' |-> Nonterminal b') ((b' |-> Terminal(b',na+nb)) f)
                 else
                    (b' |-> Nonterminal a') ((a' |-> Terminal(a',na+nb)) f)
  in
     Union f'
  end

fun merge (u1, u2 as Union m2) = 
    let
       fun mapFn x = (x, find u2 x)
       val items = map getElem (M.listItems m2) @ M.listKeys m2
       val xs2 = map mapFn items
    in
       foldr (fn (xy, u) => union u xy) u1 xs2
    end

val empty = Union M.empty

fun toList (ptn as Union f) = 
    let
       val items = M.listItems f
       val items = map getElem items
       val items = M.listKeys f @ items
       fun foldFn (x, m) = 
           let
              val rep = find ptn x
           in
              case M.find(m, rep) of
                 SOME l => M.insert(m, rep, x::l)
               | NONE => M.insert(m, rep, [x])
           end
       val vmap = foldr foldFn M.empty items
       val ls = M.listItems vmap
    in
       map (L.sortSetify K.compare) ls
    end

end
