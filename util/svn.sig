
signature SVN = 
   sig
      val revision: unit -> int option
      val date: unit -> Date.date option
   end

