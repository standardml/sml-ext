
structure VectorExt: VECTOR_EXT =
   struct 

     structure S = StringExt
     structure V = Vector

     fun toList v = V.foldr op:: [] v

     fun toString f v = "[<" ^ S.concatWith ", " (map f (toList v)) ^ ">]"

     fun map2 f (vec1,vec2) = 
         let
            val len = V.length vec1
            val _ = if V.length vec2 <> len then raise Domain else ()
         in
            V.tabulate (len,fn i => f(V.sub(vec1,i),V.sub(vec2,i)))
         end

     fun slice (vec,beg,num) = 
         let
            val len = case num of SOME n => n | NONE => V.length vec - beg
         in
            V.tabulate(len,fn n => V.sub(vec,beg+n))
         end

     fun array v = Array.tabulate (V.length v,fn i => V.sub(v,i))

     fun all2 f (vec1, vec2) = 
         let
            exception False
         in
            let
               val len = V.length vec1
               val _ = if V.length vec2 <> len then raise Domain else ()
               fun appFn (i,x) = if f(x,V.sub(vec2,i)) then () else raise False
            in
               V.appi appFn vec1;
               true
            end handle False => false
         end

     fun alli f vec = 
         case V.findi (not o f) vec of
            SOME _ => false
          | NONE => true



     open V

   end
