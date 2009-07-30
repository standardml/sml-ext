
structure StreamTest:> UNIT_TEST =
struct

structure S = Stream
structure U = UnitTests

open S.Ops

val $ = U.Ops.$
val & = U.Ops.&
val %% = U.Ops.%

infix !!

val assertEqualStream = U.assertEqual S.eq (S.toString Int.toString) 
val assertEqualStreamStream = 
    U.assertEqual (fn (x,y) => S.toList (S.map S.toList x) = S.toList (S.map S.toList y) )
                  (S.toString (S.toString Int.toString)) 

(* ---------------------------------  map  ---------------------------------- *)

val map1 = %%
              (fn () =>
                  let
                     val s1 = S.map (fn x => x + 1) (%[0,1,2])
                     val s2 = %[1,2,3]
                  in
                     assertEqualStream (s1, s2)
                  end)

val mapTests = $("map",& [map1])

(* ---------------------------------  all  ---------------------------------- *)

val all1 = %%
              (fn () => U.assert (S.all (fn x => x > 1) (%[5,7,9])))
val all2 = %%
              (fn () => U.assert (S.all (fn _ => false) S.empty))
val all3 = %% 
              (fn () => U.assert (not (S.all (fn x => x > 1) (%[0,7,9]))))

val allTests = $("all",& [all1,all2,all3])

(* --------------------------------  exists  -------------------------------- *)

val exists1 = %%(fn () => U.assert (S.exists (fn x => x > 1) (%[5,7,9])))
val exists2 = %%(fn () => U.assert (not (S.exists (fn _ => true) S.empty)))
val exists3 = %%(fn () => U.assert (not (S.exists (fn x => x > 1) (%[0,~10,~1]))))

val existsTests = $("all",& [exists1,exists2,exists3])

(* --------------------------------  repeat  -------------------------------- *)

val repeat1 = %%(fn () => U.assertEqualInt (S.repeat 5 !! 100, 5))

val repeatTests = $("repeat",& [repeat1])

(* --------------------------------  filter  -------------------------------- *)

val filter1 = %%
                 (fn () => 
                     let
                        val s1 = S.filter (fn x => x > 1) (%[0,1,2])
                        val s2 = %[2]
                     in
                        assertEqualStream(s1, s2)
                     end)

val filter2 = %%
                 (fn () => 
                     let
                        val s1 = S.filter (fn x => x > 1) (%[])
                        val s2 = %[]
                     in
                        assertEqualStream(s1, s2)
                     end)

val filterTests = $("filter",& [filter1,filter2])

(* --------------------------------  append  -------------------------------- *)

val append1 = %%
                 (fn () => 
                     let
                        val s1 = %[0,1,2,4,5,6]
                        val s2 = %[0,1,2]
                        val s3 = %[4,5,6]
                     in
                        assertEqualStream (S.append(s2,s3), s1)
                     end)

val appendTests = $("append",& [append1])

(* --------------------------------  inits  --------------------------------- *)

val inits1 = %%
                (fn () => 
                    let
                       val s1 = %[0,1,2]
                       val s2 = %[%[],%[0],%[0,1],%[0,1,2]]
                    in
                       assertEqualStreamStream (S.inits s1, s2)
                    end)

val initsTests = $("inits",& [inits1])

(* ---------------------------------  nub  ---------------------------------- *)

val nub1 = %%
              (fn () => 
                  let
                     val s1 = %[0,1,2,3,4,4,3,2,1,0]
                     val s2 = %[0,1,2,3,4]
                  in
                     assertEqualStream (S.nub s1, s2)
                  end)

val nubTests = $("nub",& [nub1])

(* ---------------------------------  sort  --------------------------------- *)

val sort1 = %%
               (fn () => 
                   let
                      val s1 = %[0,1,2,3,4,4,3,2,1,0]
                      val s2 = %[0,0,1,1,2,2,3,3,4,4]
                   in
                      assertEqualStream (S.sort Int.compare s1, s2)
                   end)

val sortTests = $("sort",& [sort1])

(* ---------------------------------  join  --------------------------------- *)

val join1 = %%
               (fn () => 
                   let
                      val s1 = %[1,2,3]
                      fun s2 x = %[%[x,x],%[x+1,x+1]]
                      val s3 = %[%[1,1,1],%[1,2,2],%[2,2,2],%[2,3,3],%[3,3,3],%[3,4,4]]
                   in
                      assertEqualStreamStream (S.joinBy S.cons (s1,s2), s3)
                   end)

val joinTests = $("join",& [join1])

(* -----------------------------  findIndices  ------------------------------ *)

val findIndices1 = %%
                   (fn () => 
                       let
                          val s1 = %[0,1,2,~3,4,4,~3,2,~1]
                          val s2 = %[3,6,8]
                       in
                          assertEqualStream (S.findIndices (fn x => x < 0) s1, s2)
                       end)

val findIndicesTests = $("findIndices",& [findIndices1])

(* ---------------------------------  End  ---------------------------------- *)

val test = fn () => $("Stream Tests",
                      & [mapTests,allTests,existsTests,repeatTests,filterTests,appendTests,
                         initsTests,nubTests,sortTests,joinTests,findIndicesTests])

end
