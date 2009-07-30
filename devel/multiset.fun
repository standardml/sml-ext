
signature MULTISET_STRUCTS =
   sig
      structure M: ORD_MAP
   end

functor MultisetBaseFn(X: MULTISET_STRUCTS): MULTISET_BASE =
   struct 
      
      open X
      structure Key = M.Key
      open GeneralExt 

      type item = Key.ord_key
      type set = int M.map

      val empty = M.empty
      fun singleton x = M.singleton (x,1)
      fun deleteMany (m,x,k) = 
          case M.find(m,x) of 
             SOME n => (case Int.compare(n,k) of
                           GREATER => M.insert(fst(M.remove(m,x)),x,n-k)
                         | EQUAL => fst(M.remove(m,x))
                         | LESS => raise Fail "not enough elements")
           | NONE => raise Fail "not found"
      fun card (m,x) = case M.find (m,x) of
                          SOME n => n
                        | NONE => 0
      fun delete (m,x) = deleteMany (m,x,1)
      fun deleteAll (m,x) = 
          case M.find (m,x) of
             SOME _ => fst (M.remove(m,x))
           | NONE => m
      fun addMany (m,x,n) = 
          if n <= 0 then raise Fail "must add a positive number of elements" else
          case M.find(m,x) of 
             SOME k => M.insert(fst(M.remove(m,x)),x,n+k)
           | NONE => M.insert(m,x,n)
      fun addManyList (m,l) = 
          foldl (fn ((x,n),m') => addMany(m',x,n)) m l

      fun add (m,x) = addMany (m,x,1)
      fun add' (x,m) = add(m,x)
      fun addList (m,l) = foldl add' m l
      fun fromList l = addList(empty,l)
      fun member (m,x) = isSome(M.find(m,x))
      val isEmpty = M.isEmpty
      val compare = M.collate Int.compare
      fun equal m12 = compare m12 = EQUAL 

      fun isSubset (m1,m2) = 
          let
             fun foldFun (k,n,b) = 
                 case M.find(m2,k) of
                    SOME m => b andalso m >= n 
                  | NONE => false
          in
             M.foldli foldFun true m1
          end
      fun numItems m = M.foldl op+ 0 m
      val listItems = M.listKeys
      val listAllItems = M.listItemsi
      val union = M.unionWith op+
      val intersection = M.intersectWith Int.min
      fun difference (m1,m2) = 
          let
             fun mergeFun (SOME n1, SOME n2) =
                 if n1 >= n2 then SOME (n1 - n2) else NONE
               | mergeFun (SOME n, NONE) = SOME n
               | mergeFun (NONE,_) = NONE
          in
             M.mergeWith mergeFun (m1,m2)
          end
      fun map f m = M.foldli (fn (k,n,m') => addMany(m',f k,n)) empty m
      fun app f m = M.appi (fn (k,n) => Ref.repeat(n,f,k)) m
      (* ignore number of each element *)
      fun foldl f x m = M.foldli (fn (l,_,b) => f(l,b)) x m
      (* ignore number of each element *)
      fun foldr f x m = M.foldri (fn (l,_,b) => f(l,b)) x m
      fun partition _ _ = raise Unimplemented
      fun filter _ _ = raise Unimplemented
      fun find _ _ = raise Unimplemented
      fun exists _ _ = raise Unimplemented
   end

functor MultisetFn(X: MULTISET_STRUCTS): MULTISET =
   struct 
      structure S = MultisetBaseFn(X)
      structure S' = OrdSetExtFn(struct 
                                    type ord_key = S.item
                                    val compare = S.Key.compare
                                    val ppItem = fn _ => raise Fail ""
                                 end)
      open S
      open S'
   end

functor SplayMultisetFn(K: ORD_KEY): MULTISET 
  = MultisetFn(struct 
                  structure K = K
                  structure M = SplayMapFn(K)
               end)
