
signature ARGS = 
   sig
      structure Set: ORD_SET
      val ppItem: Set.item -> PP.pp
   end

functor OrdSetExtFn'(Args: ARGS) : ORD_SET_EXT =
   struct 
      structure L = ListExt
      open Args
      open GeneralExt
      open PP.Ops
      infixr 0 ` 

      fun eqKey k12 = Set.Key.compare k12 = EQUAL

      fun choose s = 
          case Set.find (Fun.const true) s of
             NONE => NONE 
           | SOME x => SOME (Set.delete(s,x),x)

      fun choose' s = 
          case Set.find (Fun.const true) s of
             NONE => raise Empty
           | SOME x => x

      fun deleteIfMem (sx as (s,_)) = 
          Set.delete sx 
          handle LibBase.NotFound => s

      fun findRem p s =
          case Set.find p s of
             NONE => NONE 
           | SOME x => SOME (x,Set.delete(s,x))

      fun unions l = L.foldl Set.union Set.empty l

      fun isSupset (s, t) = Set.isSubset(t, s)

      fun intersections [] = Set.empty
        | intersections (h::t) = L.foldl Set.intersection h t

      fun allPairs f (xs, ys) = 
          let
             fun mapFn x = Set.map (fn y => f(x, y)) ys
          in
             Set.foldr (fn (x, acc) => Set.union(acc, mapFn x)) Set.empty xs
          end

      fun mapPartial f s = 
          Set.foldr (fn (x, acc) => case f x of SOME y => Set.add(acc, y) | NONE => acc) Set.empty s

      (* NJ doesn't have this *) 
      fun fromList l = foldl Set.add' Set.empty l

      (* avoid MLton warning *) 
      val _ = fromList

      fun all f xs = not (Set.exists (not o f) xs)

      fun pmap f xs = Set.foldr (fn (x, acc) => f x::acc) [] xs

      fun ppVert s = %[$"{",&(L.mapButlast (fn x => %[x, $","], fn x => x) (map ppItem (Set.listItems s))),$"}"]

      fun pp s = %[$"{",%(L.separate ($", ") (map ppItem (Set.listItems s))),$"}"]

      fun isProperSubset (s, s') = Set.isSubset(s, s') andalso not(Set.equal(s, s'))

      fun foldr1 f s = 
          case choose s of
             NONE => raise Impossible 
           | SOME (s, e) => Set.foldr f e s

      open Set
   end

functor OrdSetExtFn(Args: ORD_SET_EXT_ARGS) : ORD_SET_EXT =
   struct 
      open Args
      structure Set = OrdSetFn(struct 
                                  type ord_key = ord_key
                                  val compare = compare
                               end)
      structure Set = OrdSetExtFn'(struct 
                                      structure Set = Set
                                      val ppItem = ppItem
                                   end)
      open Set
   end
