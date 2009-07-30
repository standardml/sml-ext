
signature ARRAY_EXT =
   sig
      include ARRAY
      val toList: 'a array -> 'a list
      val toString: ('a -> string) -> 'a array -> string
      val map: ('a -> 'b) -> 'a array -> 'b array
      val map2: ('a * 'b -> 'c) -> 'a array * 'b array -> 'c array
      val slice: 'a array * int * int option -> 'a array
      val all2: ('a * 'b -> bool) -> 'a array -> 'b array -> bool
   end
