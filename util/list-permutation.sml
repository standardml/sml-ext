
structure ListPermutation:> PERMUTATION =
struct 

structure L = ListExt
structure A = ArrayExt
structure V = VectorExt

type t = int list

exception Permutation of string

fun size p = length p

fun isPerm l = 
    let
       val n = length l
       val ord = L.upto(0,n-1)
       val l' = L.sort Int.compare l 
    in
       ord = l'
    end

fun fromList l = if isPerm l then l else raise Permutation "not a permutation"

fun extend (l,n) = 
    let
       val ns = L.upto(0,n-1)
       val ns' = L.difference (ns,l)
    in
       l @ ns'
    end

fun compose (p1,p2) =
    let
       val n = length p1
       val m = length p2
       val k = Int.max(n,m)
       fun tabFn i = L.nth(p2,L.nth(p1,i))
    in
       L.tabulate(k,tabFn)
    end

fun inverse p =
    let
       val arr = A.array(length p,0) 
       fun appFn (i,x) = A.update(arr,x,i)
       val _ = L.appi appFn p
    in
       A.toList arr
    end

fun applyList p x l = 
    let
       val n = size p
       val m = length l
       val k = Int.max(n,m)
       fun tabFn i = 
           if i >= n then L.nth(l,i) else
           let
              val j = L.nth(p,i)
           in
              if j >= m then x else L.nth(l,j) 
           end
       val ret = L.tabulate (k,tabFn) 
    in
       ret
    end

fun applyList' p l = 
    let
       val n = size p
       val m = length l
       val _ = if m < n then raise Permutation "Not enough elements" else ()
       fun tabFn i = 
           if i >= n then L.nth(l,i) else
           let
              val j = L.nth(p,i)
           in
              L.nth(l,j) 
           end
    in
       L.tabulate (m,tabFn) 
    end

fun applyVec p x vec = V.fromList (applyList p x (V.toList vec))
fun applyVec' p vec = V.fromList (applyList' p (V.toList vec))

end
