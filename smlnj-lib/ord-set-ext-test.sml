
structure IntKey = 
   struct 
      type ord_key = int
      val compare = Int.compare
      val ppItem = PP.int
   end

structure IntSet = OrdSetExtFn(IntKey)

val s = IntSet.fromList [1,2,3,4,5]

IntSet.findApp (fn n => if n = 3 then SOME 7 else NONE) s
