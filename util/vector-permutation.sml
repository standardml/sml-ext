
structure VectorPermutation:> PERMUTATION =
struct 

structure L = ListExt
structure A = ArrayExt
structure V = VectorExt

type t = int vector

exception Permutation of string

open GeneralExt 

fun size p = V.length p

fun isPerm l = 
    if l = [] then false else
    let
       val n = length l
       val ord = L.upto(0,n-1)
       val l' = L.sort Int.compare l 
    in
       ord = l'
    end

fun fromList l = 
    if isPerm l then V.fromList l else raise Permutation "Not a permutation"

fun extend (l,n) = 
    let
       val ns = L.upto(0,n-1)
       val ns' = L.difference (ns,l)
    in
       fromList (l @ ns')
    end

fun applyVec p x vec = 
    let
       val n = size p
       val m = V.length vec
       val k = Int.max(n,m)
       fun tabFn i = 
           if i >= n then V.sub(vec,i) else
           let
              val j = V.sub(p,i)
           in
              if j < m then 
                 V.sub(vec,j) 
              else
                 x
           end
    in
       V.tabulate (k,tabFn) 
    end

fun applyVec' p vec = 
    let
       val n = size p
       val m = V.length vec
       val _ = if m < n then raise Permutation "Not enough elements" else ()
       fun tabFn i = 
           if i >= n then V.sub(vec,i) else
           let
              val j = V.sub(p,i)
           in
              V.sub(vec,j) 
           end
    in
       V.tabulate (m,tabFn) 
    end

fun applyList p x l = V.toList (applyVec p x (V.fromList l))
fun applyList' p l = V.toList (applyVec' p (V.fromList l))

fun compose (p1,p2) =
    let
       val n = size p1
       val _ = if size p2 <> n then raise Permutation "unqual lengths" else ()
       val arr = A.array(n,0) 
       fun appFn(i,_) = A.update(arr,i,V.sub(p2,V.sub(p1,i)))
       val _ = A.appi appFn arr
    in
       A.vector arr
    end

fun inverse p =
    let
       val arr = A.array(size p,0) 
       fun appFn(i,_) = A.update(arr,V.sub(p,i),i)
       val _ = A.appi appFn arr
    in
       A.vector arr
    end

end
