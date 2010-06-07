
functor TestFn (U : UNIV) = 
struct
  val (of_int : int -> U.t, to_int) = U.embed ()
  val (of_string : string -> U.t, to_string) = U.embed ()
  val r : U.t ref = ref (of_int 13)
  fun assert true = ()
    | assert false = raise Fail "assertion failed"
  val _ =
      let in
         print "Testing...\n";
         assert (to_int (!r) = SOME 13);
         assert (to_string (!r) = NONE);
         r := of_string "foo";
         assert (to_int (!r) = NONE);
         assert (to_string (!r) = SOME "foo");
         print "Done testing...\n"
      end
end

(* structure UnivTest = TestFn(Univ) *)

structure Univ :> UNIV =
struct
  type t = exn
  val embed: unit -> ('a -> t) * (t -> 'a option) =
   fn () => 
     let
        exception MyExn of 'a
        fun inj x = MyExn x
        fun proj (MyExn x) = SOME x 
          | proj _ = NONE  
     in 
        (inj, proj)
     end
end


