
signature ORD_MAP_EXT_ARGS = 
   sig
      type ord_key
      val compare: ord_key * ord_key -> order
   end
   
signature ORD_MAP_EXT =
   sig
      include ORD_MAP

      val |-> : Key.ord_key * 'a -> 'a map -> 'a map
      val |=> : Key.ord_key * 'a -> 'a map 

      (* difference(m1,m2) removes the keys from m1 that are in m2 *) 
      val difference: 'a map * 'a map -> 'a map

      val equalBy: ('a * 'a -> bool) -> 'a map * 'a map -> bool
      val equal: ''a map * ''a map -> bool

      val choose: 'a map -> (Key.ord_key * 'a * 'a map) option
                                      
      val search: ('a -> bool) -> 'a map -> 'a option
      val searchi: (Key.ord_key * 'a -> bool) -> 'a map 
                   -> (Key.ord_key * 'a) option

      val exists: ('a -> bool) -> 'a map -> bool
      val existsi: (Key.ord_key * 'a -> bool) -> 'a map -> bool

      val all: ('a -> bool) -> 'a map -> bool
      val alli: (Key.ord_key * 'a -> bool) -> 'a map -> bool

      val fromList: (Key.ord_key * 'a) list -> 'a map

      val ppHoriz: (Key.ord_key * 'a -> PP.pp) 
                   -> 'a map -> PP.pp
      val ppVert: (Key.ord_key * 'a -> PP.pp)
                  -> 'a map -> PP.pp

      val compare: ('a * 'a -> order) -> 'a map * 'a map -> order
   end
