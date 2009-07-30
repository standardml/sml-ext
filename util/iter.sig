
signature ITER =
   sig
      type 'a t = ('a -> unit) -> unit

      val return : 'a -> 'a t
      val >>= : 'a t * ('a -> 'b t) -> 'b t

      val none : 'a t

      val to : int * int -> int t
      val bigto : IntInf.int * IntInf.int -> IntInf.int t
      val downto : int * int -> int t

      val in_list : 'a list -> 'a t
      val in_vector : 'a vector -> 'a t
      val in_array : 'a array -> 'a t

      val using : ('a, 'b) StringCvt.reader -> 'b -> 'a t

      val when : 'a t * ('a -> bool) -> 'a t
      val by : 'a t * ('a -> 'b) -> 'b t
      val @@ : 'a t * 'a t -> 'a t
      val ** : 'a t * 'b t -> ('a, 'b) Pair.product t

      val for : 'a -> 'a

      val triangle : int * int -> (int * int) t
      val upper_triangle : int * int -> (int * int) t
   end

   (*
    infix 2 to downto
    infix 1 @@ when by
    infix 0 >>= **
    infix &
    *)
