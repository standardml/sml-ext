
structure ListExt: LIST_EXT =
struct

structure L = List
structure S = StringExt
exception Impossible

infix !!

fun cons h t = h::t

fun list x = [x]

fun dest (h::t) = (h,t)
  | dest [] = raise Fail "dest: empty"

fun l !! n = L.nth(l,n)

fun mapi f l =
    let 
       fun mm _ nil = nil
         | mm n (h::t) = f (n, h) :: mm (n + 1) t
    in
       mm 0 l
    end

fun appi f l =
    let 
       fun mm _ nil = ()
         | mm n (h::t) = (ignore (f (n, h)); mm (n + 1) t)
    in
       mm 0 l
    end

local  
   fun select' (_,[],store) = rev store
     | select' (xs,n::ns,store) = select'(xs,ns,L.nth(xs,n)::store) 
in
fun select (xs,ns) = select' (xs,ns,[])
end

fun unzip x = ListPair.unzip x

val zip = ListPair.zipEq

fun unzip3 abcList =
    let fun unzip3Loop [] (aa,bb,cc) = (rev aa, rev bb, rev cc)
          | unzip3Loop ((a,b,c)::rest) (aa,bb,cc) = unzip3Loop rest (a::aa,b::bb,c::cc)
    in unzip3Loop abcList ([],[],[])
    end

fun zip3 (a,b,c) = map (fn ((a,b),c) => (a,b,c)) 
                       (zip (zip (a, b), c))

fun unzip4 abcdList =
    let fun unzip4Loop [] (aa,bb,cc,dd) = (rev aa, rev bb, rev cc, rev dd)
          | unzip4Loop ((a,b,c,d)::rest) (aa,bb,cc,dd) = unzip4Loop rest (a::aa,b::bb,c::cc,d::dd)
    in unzip4Loop abcdList ([],[],[],[])
    end

fun zip4 (a,b,c,d) = map (fn ((a,b),(c, d)) => (a,b,c,d)) 
                         (zip (zip (a, b), (zip (c, d))))

val map2 = ListPair.mapEq
val app2 = ListPair.appEq

fun map2i f l =
    let 
       fun mm _ (nil,nil) = nil
         | mm n ((h1::t1),(h2::t2)) = f (n, h1, h2) :: mm (n + 1) (t1,t2)
         | mm _ _ = raise Fail "map2i: unequal lengths"
    in
       mm 0 l
    end

fun chop' (l,0) acc = (rev acc,l)
  | chop' ([],_) _ = raise Fail "chop: not enough elements"
  | chop' ((h::t),n) acc = chop' (t,n-1) (h::acc)
fun chop (l,n) = chop' (l,n) []

fun replicate (n,x) = L.tabulate(n,fn _ => x)

val upto = 
    let
       fun upto (n,m) acc = 
           if n > m then acc else
           upto (n+1,m) (n::acc) 
    in
       (fn x => rev(upto x []))
    end

fun upto1 (n,m) = upto(n,m-1)

fun fromN (n,m) = upto(n,n+m-1)

fun toString p = S.bracket o S.scommas o map p

(* uniq f (l,acc), assumes l is sorted on input 
 * only put the last of any chain of equal elements in the accumulator *)
fun uniq _ ([],acc) = acc
  | uniq _ ([h],acc) = h::acc
  | uniq f (h1::(t' as h2::_),acc) = 
    case f(h1,h2) of 
       EQUAL => uniq f (t',acc)
     | _ => uniq f (t',h1::acc)

(* should be optimized to do less consing. (split mainly) *)
fun sort cmp l =
    let
       fun split l =
           let fun s a1 a2 nil = (a1, a2)
                 | s a1 a2 (h::t) = s a2 (h::a1) t
           in s nil nil l
           end

       fun merge a nil = a
         | merge nil b = b
         | merge (aa as (a::ta)) (bb as (b::tb)) =
           case cmp (a, b) of
              EQUAL => (a :: b :: merge ta tb)
            | LESS => (a :: merge ta bb)
            | GREATER => (b :: merge aa tb)

       fun ms nil = nil
         | ms [s] = [s]
         | ms [a,b] = merge [a] [b]
         | ms ll = 
           let val (a,b) = split ll
           in merge (ms a) (ms b)
           end
    in 
       ms l
    end

fun sortSetify f l = rev(uniq f (sort f l,[]))
                     
fun foldli f base l = 
    let
       fun foldFn(x,(i,b)) = (i+1,f(i,x,b))
    in
       #2(foldl foldFn (0,base) l)
    end

fun foldri f base l = 
    let
       fun foldFn(x,(i,b)) = (i+1,f(i,x,b))
    in
       #2(foldr foldFn (0,base) l)
    end

(* use the first element as a base *)
fun foldl1 f l = case l of 
                    [] => raise Fail "foldlHd: empty"
                  | _ => foldl f (hd l) (tl l)

(* use the last element as a base *)
fun foldr1 f l = case l of 
                    [] => raise Fail "foldrTl: empty"
                  | _ => foldr f (L.last l) (L.take(l,length l - 1))

fun cleave' 0 l acc = (rev acc,l)
  | cleave' _ [] acc = (rev acc,[])
  | cleave' n (h::t) acc = cleave' (n-1) t (h::acc)

fun cleave n l = if n < 0 then raise Subscript else cleave' n l []
                                                    
fun split' (n,l,acc) = 
    let
       val (l1,l2) = cleave n l
    in
       case l2 of
          [] => rev (l1::acc)
        | _ => split'(n,l2,l1::acc)
    end

fun split (n,l) = if n <= 0 then raise Fail "split: nonpositive" else split'(n,l,[])
             
val allPairs =
 fn f => 
    let
       fun allPairs ([],_) = []
         | allPairs (h::t,l) = map (fn x => f (h,x)) l @ allPairs(t,l)
    in
       allPairs
    end

fun allCombs [] = [[]]
  | allCombs (l::ls) = 
    let
       val cs = allCombs ls
    in
       L.concat (map (fn x => map (fn c => x::c) cs) l)
    end

fun allMapsBy _ ([], _) = [[]]
  | allMapsBy f (h::t, l) = 
    let
       val maps = allMapsBy f (t, l)
       val firsts = L.mapPartial (fn ? => f(h, ?)) l 
    in
       L.concat (map (fn m => map (fn e => (h,e)::m) firsts) maps)
    end

fun allMaps p = allMapsBy (fn (_,y) => SOME y) p


(* insertAll x [l1, ..., ln] --> [[(x::l1), l2, ..., ln], 
                                  [l1, (x::l2), ..., ln], 
                                  ...
                                  [l1, l2, ..., (x::ln)]]
 *) 
fun insertAll _ [] = []
  | insertAll x (l::ls) = 
    let
       val lsts = insertAll x ls 
    in
       ((x::l) :: ls) :: map (fn ? => l :: ?) lsts
    end

fun allPartitionsBy _ [] = [[]]
  | allPartitionsBy f (x::xs) =
    let
       val parts = allPartitionsBy f xs
       fun mapFn p =
           let 
              val ps = ([x]::p) :: (insertAll x p)
           in
              L.filter f ps
           end
    in
       L.concat (map mapFn parts)
    end

fun allPartitions l = allPartitionsBy (fn _ => true) l

val all2 = ListPair.allEq

fun exists2 f xys = not (all2 f xys)

fun genAssoc f (x',(x,y)::rest) = if f(x,x') then SOME y else genAssoc f (x',rest)
  | genAssoc _ (_,[]) = NONE

fun genAssoc' f = valOf o genAssoc f

fun assoc x = genAssoc op= x
fun assoc' x = case assoc x of
                  SOME t => t
                | NONE => raise Fail "assoc': key not present"

fun genRevAssoc f (y',(x,y)::rest) = if f(y,y') then SOME x else genRevAssoc f (y',rest)
  | genRevAssoc _ (_,[]) = NONE

fun revAssoc x = genRevAssoc op= x

fun group [] = []
  | group (h::t) = 
    let
       val gs = group t
    in
       case gs of 
          [] => [[h]]
        | (g::gs') => 
          case g of
             [] => raise Fail "Impossible"
           | (h'::_) => if h = h' then (h::g)::gs'
                        else [h]::gs
    end

local
   fun assocRemAux _ (_,[]) _ = NONE
     | assocRemAux p (x,(h as (y,z))::t) store = 
       if p(x, y) then SOME (z,rev store @ t) 
       else assocRemAux p (x,t) (h::store)
in

fun genAssocRem p (x,xs) = assocRemAux p (x,xs) []
fun assocRem x = genAssocRem op= x

end (* local *) 

fun findRemFirst f =
    let
       fun findRemAux [] _ = NONE 
         | findRemAux (h::t) store = 
           case f h of 
              SOME x => SOME (x,rev store @ t) 
            | NONE => findRemAux t (h::store)
    in
       (fn xs => findRemAux xs [])
    end

fun findRem f = findRemFirst (fn x => if f x then SOME x else NONE)    

(* findApp = Option.map f (List.find (isSome o f) l) *) 
fun findApp _ [] = NONE 
  | findApp f (h::t) = case f h of 
                          NONE => findApp f t
                        | some => some

fun shuffle ls =
    let
       fun shuffle(l,[]) store = rev store @ l
         | shuffle([],l) store = rev store @ l
         | shuffle(x::xs,y::ys) store = shuffle(xs,ys) (y::x::store)
    in
       shuffle ls []
    end

(* index' f l n *)
fun index' _ [] _ = NONE
  | index' f (h::t) n = if f h then SOME n else index' f t (n+1)

fun index f l = index' f l 0

fun getIndex' _ [] _ = NONE
  | getIndex' f (h::t) n = if f h then SOME (n, h) else getIndex' f t (n+1)

fun getIndex f l = getIndex' f l 0

(* fun partition p [] = ([],[]) *)
(*   | partition p (h::t) =  *)
(*     let *)
(*        val (l,r) = partiton p t *)
(*     in  *)
(*        if p h then (h::l, r) else (l, h::r) *)
(*     end *)

(* separate x xs *)
fun separate _ [] = []
  | separate x xs = 
    let
       val xs' = replicate(length xs - 1,x)
    in
       shuffle(xs,xs')
    end

(* put xs between every adjacent pair of ys *)
(* separateMany xs ys *)
fun separateMany _ [] = []
  | separateMany xs ys = 
    let
       val xss = replicate(length ys - 1,xs)
    in
       L.concat (shuffle(map list ys,xss))
    end

fun concatMap _ [] = []
  | concatMap f (h::t) = f h @ concatMap f t

(* rem' pred l *)
fun rem' _ [] store = rev store
  | rem' p (h::t) store = if p h then rev store @ t else rem' p t (h::store)

fun rem p l = rem' p l []

fun remEq x l = rem (fn x' => x = x') l

fun cyclePairs' [] _ _ = raise Impossible 
  | cyclePairs' [h] beg acc = (h,beg)::acc
  | cyclePairs' (h :: (hs as (h'::_))) beg acc = cyclePairs' hs beg ((h,h')::acc)

fun cyclePairs [] = []
  | cyclePairs [_] = []
  | cyclePairs [h1,h2] = [(h1,h2)]
  | cyclePairs (hs as (h::_)) = cyclePairs' hs h []
                              
val cyclePairs : 'a list -> ('a * 'a) list
  = cyclePairs

(* remAll' pred l acc *)
fun remAll' _ [] store = rev store
  | remAll' p (h::t) store = if p h then remAll' p t store else remAll' p t (h::store)

fun remAll p l = remAll' p l []

fun number l = mapi (fn x => x) l

fun butlast l = L.take (l,length l - 1)

fun mapButlast (f,g) l =
    let
       val n = length l
       fun mapFn (m,x) = if m = (n-1) then g x else f x
    in
       mapi mapFn l
    end

fun mem (x,l) = Option.isSome (L.find (fn y => x = y) l)

fun genMem f (x,l) = Option.isSome (L.find (fn y => f(x,y)) l)

local
   fun setify _ ([],acc) = rev acc
     | setify f (h::t,acc) =
       if genMem f (h,acc) then setify f (t,acc)
       else setify f (t,h::acc)
in
val setify = fn f => fn l => setify f (l,[])
end

fun setifyEq l = setify op= l

local
   fun genDiff' _ ([],_,store) = rev store
     | genDiff' f (h::t,l,store) = 
       if genMem f (h,l) then genDiff' f (t,l,store)
       else genDiff' f (t,l,h::store)
in
fun genDiff f (l1,l2) = genDiff' f (l1,l2,[])
fun difference ls = genDiff op= ls
end

fun unique [] = true
  | unique [_] = true
  | unique (h::t) = not (mem(h,t)) andalso unique t

local
   fun insert' (l,0,x,store) = rev store @ (x::l)
     | insert' ([],_,_,_) = raise Fail "insert: not enough elements"
     (* n > 0 *)                                
     | insert' (h::t,n,x,store) = insert'(t,n-1,x,h::store)
in
fun insert (l,n,x) =
    if n < 0 then raise Fail "insert: negative" 
    else insert'(l,n,x,[])
end 

fun lexSort _ ([],[]) = EQUAL
  | lexSort _ (_,[]) = GREATER
  | lexSort _ ([],_) = LESS
  | lexSort f (h1::t1,h2::t2) = 
    case f(h1,h2) of
       EQUAL => lexSort f (t1,t2)
     | c => c

fun subset (l1,l2) = L.all (fn x => mem(x,l2)) l1

fun subsets [] = [[]]
  | subsets (h::t) = 
    let
       val ss = subsets t
    in
       ss @ map (cons h) ss
    end

fun take' (_, 0) = []
  | take' ([], _) = []
  | take' (h::t, n) = h::take'(t, n-1)

fun drop' (l, 0) = l
  | drop' ([], _) = []
  | drop' (_::t, n) = drop'(t, n-1)

fun takeLast (l, n) = drop'(l, length l - n) 

fun mapRem' _ [] _ = []
  | mapRem' f (h::t) acc = f(h, rev acc @ t) :: mapRem' f t (h::acc)

fun mapRem f l = mapRem' f l []

(* 
 allInjectiveMaps ([1,2], [a,b,c]) -->

 [[(1,a), (2,b)], [(1,a), (2,c)],
  [(1,b), (2,a)], [(1,b), (2,c)],
  [(1,c), (2,a)], [(1,c), (2,b)]]
*) 

fun allInjectiveMapsBy _ ([], _) = [[]]
  | allInjectiveMapsBy f (x::xs, ys) =
    let
       fun mapFn (y, ys') =
           case f(x,y) of 
              NONE => []
            | SOME fxy => 
              let
                 val rest = allInjectiveMapsBy f (xs, ys')
              in
                 map (fn m => fxy :: m) rest
              end
    in
       L.concat (mapRem mapFn ys)
    end

fun allInjectiveMaps l = allInjectiveMapsBy (SOME o Fun.id) l

fun lookup f l = case L.find f l of
                    SOME x => x
                  | NONE => raise Impossible

fun intersect (l1, l2) = L.filter (fn x => mem (x, l2)) l1

fun nubBy _ [] = []
  | nubBy p (x::xs) = x :: nubBy p (remAll (fn y => p(x, y)) xs)

fun nub l = nubBy op= l

open L

end
