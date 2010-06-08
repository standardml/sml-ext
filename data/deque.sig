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

      (* insert, inspect, and remove the rear element *)
      val snoc    : 'a t * 'a -> 'a t

      val singleton: 'a -> 'a t
      val toList: 'a t -> 'a list
      val fromList: 'a list -> 'a t
   end

signature CATENABLE_DEQUE =
   sig
      include DEQUE
      val cat : 'a t * 'a t -> 'a t
   end
