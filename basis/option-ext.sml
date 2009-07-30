
structure OptionExt: OPTION_EXT =
   struct 

      structure O = Option

      fun compare _ (NONE,NONE) = EQUAL
        | compare _ (NONE,_) = LESS
        | compare _ (_,NONE) = GREATER
        | compare p (SOME x,SOME y) = p(x,y)

      fun extract (SOME x,_) = x
        | extract (NONE,exn) = raise exn 

      open O

   end
