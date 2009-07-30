
signature IEEE_REAL_EXT =
   sig
      include IEEE_REAL

      val roundUp : unit -> unit
      val roundDown : unit -> unit
      val roundNear : unit -> unit
      val roundZero : unit -> unit
   end
