
signature PERMUTATION_TEST_STRUCTS =
   sig
      structure P: PERMUTATION
      val strName: string
   end

functor PermutationTestFn(X: PERMUTATION_TEST_STRUCTS):> UNIT_TEST =
   struct

      open X
      structure U = UnitTests
      open U.Ops

      (* check non-permutation *)
      val a0 = $("a0",
                 %(fn () => 
                      U.assert 
                         (let in
                             ignore (P.fromList [1,2,5,2,4,6])
                           ; false
                          end handle P.Permutation _ => true)))

      val p = P.fromList [0,2,4,1,3,5]
      val p' = P.inverse p
      val pp' = P.compose(p,p')
      val p'p = P.compose(p',p)
      val l = ["a","b","c","d","e","f"] 
      val l2 = ["a","b","c","d","e","f","g","h"] 
      val l3 = ["a","b","c"] 

      val a1 = $("a1",%(fn () => U.assert (P.applyList' pp' l = l)))
      val a2 = $("a2",%(fn () => U.assert (P.applyList' p'p l = l)))
      val a3 = $("a3",%(fn () => U.assert (P.applyList' p l = ["a","c","e","b","d","f"])))
      val a4 = $("a4",%(fn () => U.assert (P.applyList' p l = ["a","c","e","b","d","f"])))
      val a5 = $("a5",%(fn () => U.assert (P.applyList' p l2 = ["a","c","e","b","d","f","g","h"])))
      val a6 = $("a6",%(fn () => U.assert (P.applyList p "z" l3 =
                                           ["a","c","z","b","z","z"])))

      val test = fn () => $(strName,&[a0,a1,a2,a3,a4,a5,a6])

   end

structure ListPermutationTest
  = PermutationTestFn(struct 
                         structure P = ListPermutation
                         val strName = "ListPermutationTest"
                      end)

structure VectorPermutationTest
  = PermutationTestFn(struct 
                         structure P = VectorPermutation
                         val strName = "VectorPermutationTest"
                      end)
