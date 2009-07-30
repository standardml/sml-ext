
structure IEEERealExt :> IEEE_REAL_EXT =
   struct 

      structure R = IEEEReal

      fun roundUp () = R.setRoundingMode R.TO_POSINF 
      fun roundDown () = R.setRoundingMode R.TO_NEGINF 
      fun roundNear () = R.setRoundingMode R.TO_NEAREST
      fun roundZero () = R.setRoundingMode R.TO_ZERO

      open R

   end
