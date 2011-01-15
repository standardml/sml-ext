
signature DEQUE =
   sig
      type 'a t

      val empty   : 'a t
      val isEmpty : 'a t -> bool

      (* insert, inspect, and remove the front element *)
      val cons    : 'a * 'a t -> 'a t

      datatype 'a viewl = EmptyL
                       | Cons of 'a * 'a t

      val viewl: 'a t -> 'a viewl

      datatype 'a viewr = EmptyR
                       | Snoc of 'a t * 'a

      val viewr: 'a t -> 'a viewr

      datatype 'a viewl2 = EmptyL2
                        | SingL2 of 'a
                        | Cons2 of 'a * 'a * 'a t

      val viewl2: 'a t -> 'a viewl2

      datatype 'a viewr2 = EmptyR2
                        | SingR2 of 'a
                        | Snoc2 of 'a t * 'a * 'a

      val viewr2: 'a t -> 'a viewr2

      (* insert, inspect, and remove the rear element *)
      val snoc    : 'a t * 'a -> 'a t

      val singleton: 'a -> 'a t
      val toList: 'a t -> 'a list
      val fromList: 'a list -> 'a t
      val exists: ('a -> bool) -> 'a t -> bool
      val take: int * 'a t -> 'a list
      val drop: int * 'a t -> 'a t
      val foldr: ('a * 'b -> 'b) -> 'b -> 'a t -> 'b 
      val map: ('a -> 'b) -> 'a t -> 'b t
   end

signature CATENABLE_DEQUE =
   sig
      include DEQUE
      val cat : 'a t * 'a t -> 'a t
   end
