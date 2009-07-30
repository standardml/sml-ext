
signature SLOW_TUPLE_BASE =
   sig
      type 'a tuple
      val size : int
      val toList : 'a tuple -> 'a list
      val fromList : 'a list -> 'a tuple 
   end

signature FAST_TUPLE_BASE = 
   sig
      include SLOW_TUPLE_BASE

      type 'a matrix = 'a tuple tuple

      val const : 'a -> 'a tuple
      val map : ('a -> 'b) -> 'a tuple -> 'b tuple
      val mapi : (int * 'a -> 'b) -> 'a tuple -> 'b tuple
      val map2 : ('a * 'b -> 'c) -> 'a tuple * 'b tuple -> 'c tuple
      val map2i : (int * 'a * 'b -> 'c) -> 'a tuple * 'b tuple -> 'c tuple
      val nth : ('a tuple * int) -> 'a
      val zip : 'a tuple * 'b tuple -> ('a * 'b) tuple
      val unzip : ('a * 'b) tuple -> 'a tuple * 'b tuple
      val zip3 : 'a tuple * 'b tuple * 'c tuple -> ('a * 'b * 'c) tuple
      val unzip3 : ('a * 'b * 'c) tuple -> 'a tuple * 'b tuple * 'c tuple
      val tabulate : (int -> 'a) -> 'a tuple
      val foldl : ('a * 'b -> 'b) -> 'b -> 'a tuple -> 'b
      val foldr : ('a * 'b -> 'b) -> 'b -> 'a tuple -> 'b
      val foldli : (int * 'a * 'b -> 'b) -> 'b -> 'a tuple -> 'b
      val foldri : (int * 'a * 'b -> 'b) -> 'b -> 'a tuple -> 'b
      val all : ('a -> bool) -> 'a tuple -> bool
      val all2 : ('a * 'b -> bool) -> 'a tuple * 'b tuple -> bool
      (* identity "matrix" *)
      val id : {zero : 'a, one : 'a} -> 'a matrix
      (* copy the upper triangle to the lower triangle. *)
      val symmetrize : 'a matrix -> 'a matrix
      val app : ('a -> unit) -> 'a tuple -> unit
      val appi : (int * 'a -> unit) -> 'a tuple -> unit
      val appMatrix : ('a -> unit) -> 'a matrix -> unit
      val diag : 'a matrix -> 'a tuple
   end

signature TUPLE' =
   sig
      include FAST_TUPLE_BASE

      val toString : ('a -> string) -> 'a tuple -> string
      val toStringl : ('a -> string) -> 'a tuple -> string

      val mapMatrix : ('a -> 'b) -> 'a matrix -> 'b matrix
      val map2Matrix : ('a * 'b -> 'c) -> 'a matrix * 'b matrix -> 'c matrix
      val mapiMatrix : (int * int * 'a -> 'b) -> 'a matrix -> 'b matrix

      val foldlMatrix : ('a * 'b -> 'b) -> 'b -> 'a matrix -> 'b
      val foldliMatrix : (int * int * 'a * 'b -> 'b) -> 'b -> 'a matrix -> 'b
      val foldlHd : ('a * 'a -> 'a) -> 'a tuple -> 'a
      val foldrTl : ('a * 'a -> 'a) -> 'a tuple -> 'a
      val foldlHdMatrix : ('a * 'a -> 'a) -> 'a matrix -> 'a
      val foldrTlMatrix : ('a * 'a -> 'a) -> 'a matrix -> 'a
      val allMatrix : ('a -> bool) -> 'a matrix -> bool
      val all2Matrix : ('a * 'b -> bool) -> 'a matrix * 'b matrix -> bool
      val update : 'a tuple * int * 'a -> 'a tuple
                                          
      (**
       * @exception Domain if vector has incorrect length
       *)
      val fromVector : 'a vector -> 'a tuple
      val toVector : 'a tuple -> 'a vector

      (**
       * @exception Domain if vector has incorrect length
       *)
      val fromArray : 'a array -> 'a tuple
      val toArray : 'a tuple -> 'a array

      val ppHoriz : ('a -> PP.pp) -> 'a tuple -> PP.pp
      val ppVert : ('a -> PP.pp) -> 'a tuple -> PP.pp
      val ppMatrix : ('a -> PP.pp) -> 'a matrix -> PP.pp

      (** 
       * @exception Permutation if the permutation is not the right size
       *)
      val permute : Permutation.t -> 'a tuple -> 'a tuple
      val permuteMatrix : Permutation.t -> 'a matrix -> 'a matrix

      val tabulateMatrix : (int * int -> 'a) -> 'a matrix

      val find : ('a -> bool) -> 'a tuple -> 'a option
      val exists : ('a -> bool) -> 'a tuple -> bool
      val exists2 : ('a * 'b -> bool) -> 'a tuple * 'b tuple -> bool

      val findMatrix : ('a -> bool) -> 'a matrix -> 'a option
      val zipMatrix : 'a matrix * 'b matrix -> ('a * 'b) matrix
      val constMatrix : 'a -> 'a matrix
   end

signature TUPLE =
   sig
      include TUPLE'
      val mkIter : 'a Iter.t tuple -> 'a tuple Iter.t
   end

signature TUPLE2 = TUPLE where type 'a tuple = 'a * 'a 
signature TUPLE3 = TUPLE where type 'a tuple = 'a * 'a * 'a 
signature TUPLE4 = TUPLE where type 'a tuple = 'a * 'a * 'a * 'a
signature TUPLE5 = TUPLE where type 'a tuple = 'a * 'a * 'a * 'a * 'a
signature TUPLE6 = TUPLE where type 'a tuple = 'a * 'a * 'a * 'a * 'a * 'a
signature TUPLE7 = TUPLE where type 'a tuple = 'a * 'a * 'a * 'a * 'a * 'a * 'a
signature TUPLE8 = TUPLE where type 'a tuple = 'a * 'a * 'a * 'a * 'a * 'a * 'a * 'a
signature TUPLE9 = TUPLE where type 'a tuple = 'a * 'a * 'a * 'a * 'a * 'a * 'a * 'a * 'a

