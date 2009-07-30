
signature MULTISET_BASE =
   sig
      include ORD_SET
      val card: set * item -> int
      val deleteAll: set * item -> set
      val addMany: set * item * int -> set
      val addManyList: set * (item * int) list -> set
      val listAllItems: set -> (item * int) list
   end

signature MULTISET = 
   sig
      include MULTISET_BASE
      val choose: set -> (set * item) option
      val selectRemove: (item -> bool) -> set -> (item * set) option

      (* delete the item if its a member.  do nothing otherwise *)
      val deleteIfMem: set * item -> set
      val ppHoriz: (item -> PP.pp) -> set -> PP.pp
      val ppVert: (item -> PP.pp) -> set -> PP.pp
   end
