
structure ArrayExt: ARRAY_EXT =
   struct 

     structure S = StringExt
     structure A = Array

     fun toList v = A.foldr op:: [] v

     fun toString f v = "[|" ^ S.concatWith ", " (map f (toList v)) ^ "|]"

     fun map f arr = A.tabulate(A.length arr,(fn i => f(A.sub(arr,i))))

     fun map2 f (arr1,arr2) = 
         let
            val len = A.length arr1
            val _ = if A.length arr2 <> len then raise Domain else ()
         in
            A.tabulate (len,fn i => f(A.sub(arr1,i),A.sub(arr2,i)))
         end

     fun slice (arr,beg,num) = 
         let
            val len = case num of SOME n => n | NONE => A.length arr - beg
         in
            A.tabulate(len,fn n => A.sub(arr,beg+n))
         end
         
     fun all2 f arr1 arr2 = 
         let
            exception False
         in
            let
               val len = A.length arr1
               val _ = if A.length arr2 <> len then raise Domain else ()
               fun appFn (i,x) = if f(x,A.sub(arr2,i)) then () else raise False
            in
               A.appi appFn arr1;
               true
            end handle False => false
         end

     open A

   end
