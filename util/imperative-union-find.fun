
(* 
 From the O'Caml implementation
 *) 

functor ImperativeUnionFindFn(K: HASH_KEY) 
        :> IMPERATIVE_UNION_FIND where type K.hash_key = K.hash_key =
struct 

open GeneralExt 

structure K = K
structure H = HashTableFn(K)
structure L = ListExt

type union = K.hash_key H.hash_table

exception UnionFind of string

fun create n = H.mkTable(n, UnionFind "Generic Error") 

fun find tbl element =
    let
       fun getParent elt = case H.find tbl elt of 
                              SOME p => p
                            | NONE => 
                              let in
                                 H.insert tbl (elt, elt)
                               ; elt
                              end
       (* The find function *)
       fun findrec element =
	   let 
              val parent = getParent element
           in
	      if K.sameKey(parent, element) then
	         element
	      else
	         findrec parent
           end
       val parent = getParent element
    in
       if K.sameKey(parent, element) 
          then element 
       else 
          let
             val parent2 = getParent parent
          in
             if K.sameKey(parent2, parent) 
                then parent 
             else 
                (* The path compression function *)
                let
                   fun compressrec res element =
	               let 
                          val parent = getParent element 
                       in
	                  if K.sameKey(parent, element) 
                             then () 
                          else
                             let in
	                        H.insert tbl (element, res)
                              ; compressrec res parent
                             end
	               end
                in
                   (* The algorithm *)
                   let
                      val res = findrec parent2 
                   in
                      compressrec res element
                    ; res
                   end
                end
          end
    end

fun union tbl  (element1, element2) =
    let
       val root1 = find tbl element1
       val root2 = find tbl element2
    in 
       H.insert tbl (root1, root2)
    end

fun equal eqv (a, b) = K.sameKey(find eqv a, find eqv b) 

fun toList tbl = 
    let
       val n = H.numItems tbl
       val tbl2 = create n
       fun appFn x = 
           let
              val rep = case H.find tbl x of
                           SOME p => p
                         | NONE => raise Impossible 
           in
              case H.find tbl2 rep of
                 SOME l => H.insert tbl2 (rep, x::l)
               | NONE => H.insert tbl2 (rep, [x])
           end
       fun appFn' (x,y) = 
           let in
              appFn x
            ; appFn y
           end
       val _ = H.appi appFn' tbl
       val ls = H.listItems tbl2
    in
       map (L.setify K.sameKey) ls
    end

end
