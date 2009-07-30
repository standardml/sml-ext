
functor FastTupleFn(T: FAST_TUPLE_BASE): TUPLE' =
   struct 

      structure L = ListExt
      structure S = StringExt
      structure P = Permutation

      fun foldlMatrix f b mat = 
          let
             fun foldFn (row, store) = T.foldl f store row
          in
             T.foldl foldFn b mat 
          end

      fun foldliMatrix f b mat = 
          let
             fun f1 (i, row, store) = 
                 let
                    fun f2 (j, elem, store') = f(i, j, elem, store')
                 in
                    T.foldli f2 store row
                 end
          in
             T.foldli f1 b mat
          end

      fun foldlHd f t = L.foldl1 f (T.toList t)
      fun foldrTl f t = L.foldr1 f (T.toList t)

      fun matrixToList m = (L.concat o T.toList) (T.map T.toList m)

      fun foldlHdMatrix f m = L.foldl1 f (matrixToList m)
      fun foldrTlMatrix f m = L.foldr1 f (matrixToList m)

      fun allMatrix f = T.all (T.all f) 
      fun all2Matrix f = T.all2 (T.all2 f) 

      fun constMatrix x = T.const (T.const x)

      fun mapMatrix f mat = T.map (T.map f) mat
      fun mapiMatrix f mat = 
          let
             fun f1 (i, row) = 
                 let
                    fun f2 (j, elem) = f(i, j, elem)
                 in
                    T.mapi f2 row
                 end
          in
             T.mapi f1 mat
          end

      fun map2Matrix f m12 = T.map2 (T.map2 f) m12

      fun toString f t = S.paren (S.concatWith ", " (L.map f (T.toList t)))
      fun toStringl f t = S.paren (S.concatWith ", \n" (L.map f (T.toList t)))

      fun fromVector v = T.fromList (VectorExt.toList v)
      fun toVector t = VectorExt.fromList (T.toList t)

      fun fromArray v = T.fromList (ArrayExt.toList v)
      fun toArray t = ArrayExt.fromList (T.toList t)

      fun update (t, n, x) = if n >= T.size then raise Fail "update: index too large"
                           else T.tabulate (fn i => if i = n then x else T.nth(t, i))

      local
         open PP.Ops
         open T
      in

      fun ppHoriz f t = 
          let
             val l = T.toList t
          in
             %%($"("::L.separate ($", ") (L.map f l) @ [$")"])
          end

      fun ppVert f t = 
          let
             val l = T.toList t
             fun mapFn (i, x) = 
                 if i < T.size - 1 
                    then %[f x, $", "]
                 else %[f x, $")"]
             val l' = &(L.mapi mapFn l)
          in
             %[$"(", l']
          end

      fun ppMatrix f m = ppVert (ppHoriz f) m

      end (* local *) 

      fun permute p t = T.fromList (P.applyList' p (T.toList t))
      fun permuteMatrix p mat = permute p (T.map (permute p) mat)

      fun tabulateMatrix f = T.tabulate (fn i => T.tabulate (fn j => f(i, j)))

      fun find f t = L.find f (T.toList t)
      fun exists f t = L.exists f (T.toList t)

      fun findMatrix (f:'a -> bool) (m:'a T.matrix): 'a option =
          let 
             exception Found of 'a
          in
             let in
                (T.appMatrix (fn x => if f x then raise Found x else ()) m; NONE)
             end handle Found x => SOME x
          end 

      fun zipMatrix (m1, m2) = T.map T.zip (T.zip(m1, m2))

      fun exists2 f t12 = not (T.all2 (not o f) t12)

      open T

   end

functor SlowTupleFnAux(T: SLOW_TUPLE_BASE): FAST_TUPLE_BASE =
   struct 

      structure L = ListExt

      open T

      type 'a matrix = 'a tuple tuple

      fun const x = fromList (L.replicate(size, x))
      fun map f x = fromList (L.map f (toList x))
      fun mapi f x = fromList (L.mapi f (toList x))
      fun map2 f (x, y) = fromList (L.map2 f (toList x, toList y))
      fun map2i f (x, y) = fromList (L.map2i f (toList x, toList y))
      fun nth (x, n) = L.nth(toList x, n) handle Subscript => raise Subscript
      fun zip (x1, x2) = fromList (L.zip(toList x1, toList x2))
      fun unzip xs = Pair.pappp fromList fromList (L.unzip (toList xs))
      fun zip3 (x1, x2, x3) = fromList (L.zip3 (toList x1, toList x2, toList x3))
      fun unzip3 xs = 
          let
             val (x1, x2, x3) = L.unzip3 (toList xs)
          in
             (fromList x1, fromList x2, fromList x3)
          end
      fun tabulate f = fromList (L.tabulate(size, f))
      fun foldl f x t = L.foldl f x (toList t)
      fun foldr f x t = L.foldr f x (toList t)
      fun foldli f x t = L.foldli f x (toList t)
      fun foldri f x t = L.foldri f x (toList t)

      fun all f t = L.all f (toList t)
      fun all2 f (t1, t2) = L.all2 f (toList t1, toList t2)

      fun id {zero, one} = 
          tabulate (fn x => tabulate (fn y => if x = y then one else zero))

      fun symmetrize ts = 
          tabulate (fn x => tabulate (fn y => if y >= x then nth(nth(ts, x), y) else nth(nth(ts, y), x)))
      fun app f t = L.app f (toList t)
      fun appi f t = L.appi f (toList t)
      fun appMatrix f m = app (app f) m
      fun diag m = mapi (fn (i, row) => nth(row, i)) m

   end

functor SlowTupleFn(T: SLOW_TUPLE_BASE): TUPLE' =
   struct
      structure T' = SlowTupleFnAux(T)
      structure T'' = FastTupleFn(T')
      open T''
   end

(* -------------------------------------------------------------------------- *)
(*  3                                                                         *)
(* -------------------------------------------------------------------------- *)

structure FastTupleBase3: FAST_TUPLE_BASE =
   struct
      
      type 'a tuple = 'a * 'a * 'a

      type 'a matrix = 'a tuple tuple

      val size = 3

      fun map f (x0, x1, x2) = (f x0, f x1, f x2)

      fun mapi f (x0, x1, x2) = (f(0, x0), f(1, x1), f(2, x2))

      fun map2 f ((x0, x1, x2), (y0, y1, y2)) =
          (f(x0, y0), f(x1, y1), f(x2, y2))

      fun map2i f ((x0, x1, x2), (y0, y1, y2)) =
          (f(0, x0, y0), f(1, x1, y1), f(2, x2, y2))

      fun const x = (x, x, x)

      fun nth ((x0, x1, x2), n) = 
          case n of
             0 => x0
           | 1 => x1     
           | 2 => x2
           | _ => raise Fail "nth"

      fun toList (x0, x1, x2) = [x0, x1, x2]

      fun zip ((x0, x1, x2), (y0, y1, y2)) = ((x0, y0), (x1, y1), (x2, y2))

      fun zip3 ((x0, x1, x2), (y0, y1, y2), (z0, z1, z2)) = ((x0, y0, z0), (x1, y1, z1), (x2, y2, z2))

      fun id {zero=O, one=l} = ((l, O, O),
                               (O, l, O),
                               (O, O, l))

      fun unzip ((x0, y0), (x1, y1), (x2, y2)) = ((x0, x1, x2), (y0, y1, y2))

      fun unzip3 ((x0, y0, z0), (x1, y1, z1), (x2, y2, z2)) = ((x0, x1, x2), (y0, y1, y2), (z0, z1, z2))

      fun symmetrize ((x11, x12, x13),
                      (_, x22, x23),
                      (_, _, x33)) =
          ((x11, x12, x13),
           (x12, x22, x23),
           (x13, x23, x33))

      fun fromList [x1, x2, x3] = (x1, x2, x3)
        | fromList _ = raise Fail "Tuple3.fromList: size"

      fun tabulate f = (f 0, f 1, f 2)

      fun foldl f b (x1, x2, x3) = f(x3, f(x2, f(x1, b)))
      fun foldr f b (x1, x2, x3) = f(x1, f(x2, f(x3, b)))
      fun foldli f b (x1, x2, x3) = f(2, x3, f(1, x2, f(0, x1, b)))
      fun foldri f b (x1, x2, x3) = f(0, x1, f(1, x2, f(2, x3, b)))

      fun all f (x1, x2, x3) = f x1 andalso f x2 andalso f x3
      fun all2 f ((x1, x2, x3), (y1, y2, y3)) = f(x1, y1) andalso f(x2, y2) andalso f(x3, y3)

      fun app f (x1, x2, x3) = (f x1: unit; f x2; f x3)
      fun appi f (x1, x2, x3) = (f (0, x1):unit; f (1, x2); f (2, x3))
      fun appMatrix f ((x1, x2, x3),
                       (y1, y2, y3),
                       (z1, z2, z3)) = 
          (f x1:unit; f x2; f x3;
           f y1; f y2; f y3;
           f z1; f z2; f z3)

      fun diag ((x11, _, _),
                (_, x22, _),
                (_, _, x33)) = (x11, x22, x33)

   end

(* -------------------------------------------------------------------------- *)
(*  6                                                                         *)
(* -------------------------------------------------------------------------- *)

structure FastTupleBase6: FAST_TUPLE_BASE =
   struct
      
      open GeneralExt

      type 'a tuple = 'a * 'a * 'a * 'a * 'a * 'a 

      type 'a matrix = 'a tuple tuple

      val size = 6

      fun map f (x0, x1, x2, x3, x4, x5) = (f x0, f x1, f x2, f x3, f x4, f x5)

      fun mapi f (x0, x1, x2, x3, x4, x5) = (f(0, x0), f(1, x1), f(2, x2), f(3, x3), f(4, x4), f(5, x5))

      fun map2 f ((x0, x1, x2, x3, x4, x5), (y0, y1, y2, y3, y4, y5)) =
          (f(x0, y0), f(x1, y1), f(x2, y2), f(x3, y3), f(x4, y4), f(x5, y5))

      fun map2i f ((x0, x1, x2, x3, x4, x5), (y0, y1, y2, y3, y4, y5)) =
          (f(0, x0, y0), f(1, x1, y1), f(2, x2, y2), f(3, x3, y3), f(4, x4, y4), f(5, x5, y5))

      fun const x = (x, x, x, x, x, x)

      fun nth ((x0, x1, x2, x3, x4, x5), n) = 
          case n of
             0 => x0
           | 1 => x1     
           | 2 => x2
           | 3 => x3     
           | 4 => x4     
           | 5 => x5     
           | _ => raise Fail (concat ["nth: ", Int.toString n, " is not less than tuple size = ", Int.toString size])

      fun toList (x0, x1, x2, x3, x4, x5) = [x0, x1, x2, x3, x4, x5]

      fun zip ((x0, x1, x2, x3, x4, x5), (y0, y1, y2, y3, y4, y5)) = ((x0, y0), (x1, y1), (x2, y2), (x3, y3), (x4, y4), (x5, y5))

      fun zip3 ((x0, x1, x2, x3, x4, x5), (y0, y1, y2, y3, y4, y5), (z0, z1, z2, z3, z4, z5)) =
          ((x0, y0, z0), (x1, y1, z1), (x2, y2, z2), (x3, y3, z3), (x4, y4, z4), (x5, y5, z5))

      fun id {zero=O, one=l} = ((l, O, O, O, O, O),
                               (O, l, O, O, O, O),
                               (O, O, l, O, O, O),
                               (O, O, O, l, O, O),
                               (O, O, O, O, l, O),
                               (O, O, O, O, O, l))

      fun unzip ((x0, y0), (x1, y1), (x2, y2), (x3, y3), (x4, y4), (x5, y5)) = 
          ((x0, x1, x2, x3, x4, x5), (y0, y1, y2, y3, y4, y5))

      fun unzip3 ((x0, y0, z0), (x1, y1, z1), (x2, y2, z2),
                  (x3, y3, z3), (x4, y4, z4), (x5, y5, z5)) =
          ((x0, x1, x2, x3, x4, x5), (y0, y1, y2, y3, y4, y5), (z0, z1, z2, z3, z4, z5))

      fun symmetrize ((x11, x12, x13, x14, x15, x16),
                      (_, x22, x23, x24, x25, x26),
                      (_, _, x33, x34, x35, x36),
                      (_, _, _, x44, x45, x46),
                      (_, _, _, _, x55, x56),
                      (_, _, _, _, _, x66)) =
          ((x11, x12, x13, x14, x15, x16),
           (x12, x22, x23, x24, x25, x26),
           (x13, x23, x33, x34, x35, x36),
           (x14, x24, x34, x44, x45, x46),
           (x15, x25, x35, x45, x55, x56),
           (x16, x26, x36, x46, x56, x66)) 

      fun fromList [x1, x2, x3, x4, x5, x6] = (x1, x2, x3, x4, x5, x6)
        | fromList _ = raise Fail "Tuple6.fromList: size"

      fun tabulate f = (f 0, f 1, f 2, f 3, f 4, f 5)

      fun foldl f b (x1, x2, x3, x4, x5, x6) = f(x6, f(x5, f(x4, f(x3, f(x2, f(x1, b))))))
      fun foldr f b (x1, x2, x3, x4, x5, x6) = f(x1, f(x2, f(x3, f(x4, f(x5, f(x6, b))))))
      fun foldli f b (x0, x1, x2, x3, x4, x5) = f(5, x5, f(4, x4, f(3, x3, f(2, x2, f(1, x1, f(0, x0, b))))))
      fun foldri f b (x0, x1, x2, x3, x4, x5) = f(0, x0, f(1, x1, f(2, x2, f(3, x3, f(4, x4, f(5, x5, b))))))

      fun all f (x1, x2, x3, x4, x5, x6) = f x1 andalso f x2 andalso f x3 andalso 
                                      f x4 andalso f x5 andalso f x6
      fun all2 f ((x1, x2, x3, x4, x5, x6), (y1, y2, y3, y4, y5, y6)) = 
          f(x1, y1) andalso f(x2, y2) andalso f(x3, y3) andalso 
          f(x4, y4) andalso f(x5, y5) andalso f(x6, y6)

      fun app f (x1, x2, x3, x4, x5, x6) = (f x1:unit; f x2; f x3; f x4; f x5; f x6)
      fun appi f (x1, x2, x3, x4, x5, x6) = (f (0, x1):unit; f (1, x2); f (2, x3); f (3, x4); f (4, x5); f (5, x6))
      fun appMatrix f ((x11, x12, x13, x14, x15, x16),
                       (x21, x22, x23, x24, x25, x26),
                       (x31, x32, x33, x34, x35, x36),
                       (x41, x42, x43, x44, x45, x46),
                       (x51, x52, x53, x54, x55, x56),
                       (x61, x62, x63, x64, x65, x66)) =
          (f x11;f x12;f x13;f x14;f x15;f x16:unit;
           f x21;f x22;f x23;f x24;f x25;f x26;
           f x31;f x32;f x33;f x34;f x35;f x36;
           f x41;f x42;f x43;f x44;f x45;f x46;
           f x51;f x52;f x53;f x54;f x55;f x56;
           f x61;f x62;f x63;f x64;f x65;f x66)

      fun diag ((x11, _, _, _, _, _),
                (_, x22, _, _, _, _),
                (_, _, x33, _, _, _),
                (_, _, _, x44, _, _),
                (_, _, _, _, x55, _),
                (_, _, _, _, _, x66)) = (x11, x22, x33, x44, x55, x66)
   end

(* -------------------------------------------------------------------------- *)
(*  9                                                                         *)
(* -------------------------------------------------------------------------- *)

structure FastTupleBase9: FAST_TUPLE_BASE =
   struct
      
      open GeneralExt

      type 'a tuple = 'a * 'a * 'a * 'a * 'a * 'a * 'a * 'a * 'a

      type 'a matrix = 'a tuple tuple

      val size = 9

      fun map f (x0, x1, x2, x3, x4, x5, x6, x7, x8) = (f x0, f x1, f x2, f x3, f x4, f x5, f x6, f x7, f x8)

      fun mapi f (x0, x1, x2, x3, x4, x5, x6, x7, x8) = (f(0, x0), f(1, x1), f(2, x2), f(3, x3), f(4, x4), f(5, x5), f(6, x6), f(7, x7), f(8, x8))

      fun map2 f ((x0, x1, x2, x3, x4, x5, x6, x7, x8), (y0, y1, y2, y3, y4, y5, y6, y7, y8)) =
          (f(x0, y0), f(x1, y1), f(x2, y2), f(x3, y3), f(x4, y4), f(x5, y5), f(x6, y6), f(x7, y7), f(x8, y8))

      fun map2i f ((x0, x1, x2, x3, x4, x5, x6, x7, x8), (y0, y1, y2, y3, y4, y5, y6, y7, y8)) =
          (f(0, x0, y0), f(1, x1, y1), f(2, x2, y2), f(3, x3, y3), f(4, x4, y4), f(5, x5, y5), f(6, x6, y6), f(7, x7, y7), f(8, x8, y8))

      fun const x = (x, x, x, x, x, x, x, x, x)

      fun nth ((x0, x1, x2, x3, x4, x5, x6, x7, x8), n) = 
          case n of
             0 => x0
           | 1 => x1     
           | 2 => x2
           | 3 => x3     
           | 4 => x4     
           | 5 => x5     
           | 6 => x6
           | 7 => x7
           | 8 => x8
           | _ => raise Fail (concat ["nth: ", Int.toString n, " is not less than tuple size = ", Int.toString size])

      fun toList (x0, x1, x2, x3, x4, x5, x6, x7, x8) = [x0, x1, x2, x3, x4, x5, x6, x7, x8]

      fun zip ((x0, x1, x2, x3, x4, x5, x6, x7, x8), (y0, y1, y2, y3, y4, y5, y6, y7, y8)) = ((x0, y0), (x1, y1), (x2, y2), (x3, y3), (x4, y4), (x5, y5), (x6, y6), (x7, y7), (x8, y8))

      fun zip3 ((x0, x1, x2, x3, x4, x5, x6, x7, x8), (y0, y1, y2, y3, y4, y5, y6, y7, y8), (z0, z1, z2, z3, z4, z5, z6, z7, z8)) =
          ((x0, y0, z0), (x1, y1, z1), (x2, y2, z2), (x3, y3, z3), (x4, y4, z4), (x5, y5, z5), (x6, y6, z6), (x7, y7, z7), (x8, y8, z8))

      fun id {zero=O, one=l} = ((l, O, O, O, O, O, O, O, O),
                                (O, l, O, O, O, O, O, O, O),
                                (O, O, l, O, O, O, O, O, O),
                                (O, O, O, l, O, O, O, O, O),
                                (O, O, O, O, l, O, O, O, O),
                                (O, O, O, O, O, l, O, O, O),
                                (O, O, O, O, O, O, l, O, O),
                                (O, O, O, O, O, O, O, l, O),
                                (O, O, O, O, O, O, O, O, l))

      fun unzip ((x0, y0), (x1, y1), (x2, y2), (x3, y3), (x4, y4), (x5, y5), (x6, y6), (x7, y7), (x8, y8)) =
          ((x0, x1, x2, x3, x4, x5, x6, x7, x8), (y0, y1, y2, y3, y4, y5, y6, y7, y8))

      fun unzip3 ((x0, y0, z0), (x1, y1, z1), (x2, y2, z2),
                  (x3, y3, z3), (x4, y4, z4), (x5, y5, z5), 
                  (x6, y6, z6), (x7, y7, z7), (x8, y8, z8)) =
          ((x0, x1, x2, x3, x4, x5, x6, x7, x8), (y0, y1, y2, y3, y4, y5, y6, y7, y8), (z0, z1, z2, z3, z4, z5, z6, z7, z8))

      fun symmetrize ((x11, x12, x13, x14, x15, x16, x17, x18, x19),
                      (_, x22, x23, x24, x25, x26, x27, x28, x29),
                      (_, _, x33, x34, x35, x36, x37, x38, x39),
                      (_, _, _, x44, x45, x46, x47, x48, x49),
                      (_, _, _, _, x55, x56, x57, x58, x59),
                      (_, _, _, _, _, x66, x67, x68, x69),
                      (_, _, _, _, _, _, x77, x78, x79),
                      (_, _, _, _, _, _, _, x88, x89),
                      (_, _, _, _, _, _, _, _, x99)) =
          ((x11, x12, x13, x14, x15, x16, x17, x18, x19),
           (x12, x22, x23, x24, x25, x26, x27, x28, x29),
           (x13, x23, x33, x34, x35, x36, x37, x38, x39),
           (x14, x24, x34, x44, x45, x46, x47, x48, x49),
           (x15, x25, x35, x45, x55, x56, x57, x58, x59),
           (x16, x26, x36, x46, x56, x66, x67, x68, x69),
           (x17, x27, x37, x47, x57, x67, x77, x78, x79),
           (x18, x28, x38, x48, x58, x68, x78, x88, x89),
           (x19, x29, x39, x49, x59, x69, x79, x89, x99))

      fun fromList [x1, x2, x3, x4, x5, x6, x7, x8, x9] = (x1, x2, x3, x4, x5, x6, x7, x8, x9)
        | fromList _ = raise Fail "Tuple6.fromList: size"

      fun tabulate f = (f 0, f 1, f 2, f 3, f 4, f 5, f 6, f 7, f 8)

      fun foldl f b (x1, x2, x3, x4, x5, x6, x7, x8, x9) =
          f(x9, f(x8, f(x7, f(x6, f(x5, f(x4, f(x3, f(x2, f(x1, b)))))))))
      fun foldr f b (x1, x2, x3, x4, x5, x6, x7, x8, x9) = 
          f(x1, f(x2, f(x3, f(x4, f(x5, f(x6, f(x7, f(x8, f(x9, b)))))))))
      fun foldli f b (x0, x1, x2, x3, x4, x5, x6, x7, x8) = 
          f(8, x8, f(7, x7, f(6, x6, f(5, x5, f(4, x4, f(3, x3, f(2, x2, f(1, x1, f(0, x0, b)))))))))
      fun foldri f b (x0, x1, x2, x3, x4, x5, x6, x7, x8) = 
          f(0, x0, f(1, x1, f(2, x2, f(3, x3, f(4, x4, f(5, x5, f(6, x6, f(7, x7, f(8, x8, b)))))))))

      fun all f (x1, x2, x3, x4, x5, x6, x7, x8, x9) = 
          f x1 andalso f x2 andalso f x3 andalso 
          f x4 andalso f x5 andalso f x6 andalso 
          f x7 andalso f x8 andalso f x9 

      fun all2 f ((x1, x2, x3, x4, x5, x6, x7, x8, x9), (y1, y2, y3, y4, y5, y6, y7, y8, y9)) = 
          f(x1, y1) andalso f(x2, y2) andalso f(x3, y3) andalso 
          f(x4, y4) andalso f(x5, y5) andalso f(x6, y6) andalso 
          f(x7, y7) andalso f(x8, y8) andalso f(x9, y9) 

      fun app f (x1, x2, x3, x4, x5, x6, x7, x8, x9) = (f x1:unit; f x2; f x3; f x4; f x5; f x6; f x7; f x8; f x9)
      fun appi f (x1, x2, x3, x4, x5, x6, x7, x8, x9) = (f (0, x1):unit; f (1, x2); f (2, x3); f (3, x4); f (4, x5); f (5, x6); f (6, x7); f(7, x8); f(8, x9))

      fun appMatrix f ((x11, x12, x13, x14, x15, x16, x17, x18, x19),
                       (x21, x22, x23, x24, x25, x26, x27, x28, x29),
                       (x31, x32, x33, x34, x35, x36, x37, x38, x39),
                       (x41, x42, x43, x44, x45, x46, x47, x48, x49),
                       (x51, x52, x53, x54, x55, x56, x57, x58, x59),
                       (x61, x62, x63, x64, x65, x66, x67, x68, x69),
                       (x71, x72, x73, x74, x75, x76, x77, x78, x79),
                       (x81, x82, x83, x84, x85, x86, x87, x88, x89),
                       (x91, x92, x93, x94, x95, x96, x97, x98, x99)) =
          (f x11;f x12;f x13;f x14;f x15;f x16; f x17; f x18; f x19;
           f x21;f x22;f x23;f x24;f x25;f x26; f x27; f x28; f x29;
           f x31;f x32;f x33;f x34;f x35;f x36; f x37; f x38; f x39;
           f x41;f x42;f x43;f x44;f x45;f x46; f x47; f x48; f x49;
           f x51;f x52;f x53;f x54;f x55;f x56; f x57; f x58; f x59;
           f x61;f x62;f x63;f x64;f x65;f x66; f x67; f x68; f x69;
           f x71;f x72;f x73;f x74;f x75;f x76; f x77; f x78; f x79;
           f x81;f x82;f x83;f x84;f x85;f x86; f x87; f x88; f x89;
           f x91;f x92;f x93;f x94;f x95;f x96; f x97; f x98; f x99:unit)
          
      fun diag ((x11, _, _, _, _, _, _, _, _),
                (_, x22, _, _, _, _, _, _, _),
                (_, _, x33, _, _, _, _, _, _),
                (_, _, _, x44, _, _, _, _, _),
                (_, _, _, _, x55, _, _, _, _),
                (_, _, _, _, _, x66, _, _, _),
                (_, _, _, _, _, _, x77, _, _),
                (_, _, _, _, _, _, _, x88, _),
                (_, _, _, _, _, _, _, _, x99)) = 
          (x11, x22, x33, x44, x55, x66, x77, x88, x99)
   end

(* -------------------------------------------------------------------------- *)
(*  Top Level Structures                                                      *)
(* -------------------------------------------------------------------------- *)

(* ----------------------------------  1  ----------------------------------- *)

structure SlowTupleBase1 = 
   struct 
      type 'a tuple = 'a
      val size = 1
      fun toList x = [x]
      fun fromList [x] = x
        | fromList _ = raise Fail "Tuple1"
   end

structure Tuple1: TUPLE = 
   struct 
      structure T = SlowTupleFn(SlowTupleBase1)
      open T
      fun mkIter i1 f = i1 f
   end

(* ----------------------------------  2  ----------------------------------- *)

structure SlowTupleBase2 = 
   struct 
      type 'a tuple = 'a * 'a
      val size = 2
      fun toList (x1, x2) = [x1, x2]
      fun fromList [x1, x2] = (x1, x2)
        | fromList _ = raise Fail "Tuple2"
   end

structure Tuple2: TUPLE = 
   struct 
      structure T = SlowTupleFn(SlowTupleBase2)
      open T
      fun mkIter (i1, i2) f = i1 (fn x1 => i2 (fn x2 => f (x1, x2)))
   end

(* ----------------------------------  3  ----------------------------------- *)

structure Tuple3: TUPLE = 
   struct 
      structure T = FastTupleFn(FastTupleBase3)
      open T
      fun mkIter (i1, i2, i3) f = i1 (fn x1 => i2 (fn x2 => i3 (fn x3 => f (x1, x2, x3))))
   end

(* ----------------------------------  4  ----------------------------------- *)

structure SlowTupleBase4 = 
   struct 
      type 'a tuple = 'a * 'a * 'a * 'a
      val size = 4
      fun toList (x1, x2, x3, x4) = [x1, x2, x3, x4]
      fun fromList [x1, x2, x3, x4] = (x1, x2, x3, x4)
        | fromList _ = raise Fail "Tuple4"
   end

structure Tuple4: TUPLE = 
   struct 
      structure T = SlowTupleFn(SlowTupleBase4)
      open T
      fun mkIter (i1, i2, i3, i4) f = i1 (fn x1 => i2 (fn x2 => i3 (fn x3 => i4 (fn x4 => f (x1, x2, x3, x4)))))
   end

(* ----------------------------------  5  ----------------------------------- *)

structure SlowTupleBase5 = 
   struct 
      type 'a tuple = 'a * 'a * 'a * 'a * 'a
      val size = 4
      fun toList (x1, x2, x3, x4, x5) = [x1, x2, x3, x4, x5]
      fun fromList [x1, x2, x3, x4, x5] = (x1, x2, x3, x4, x5)
        | fromList _ = raise Fail "Tuple5"
   end

structure Tuple5: TUPLE = 
   struct 
      structure T = SlowTupleFn(SlowTupleBase5)
      open T
      fun mkIter (i1, i2, i3, i4, i5) f = 
          i1 (fn x1 => i2 (fn x2 => i3 (fn x3 => 
          i4 (fn x4 => i5 (fn x5 => f (x1, x2, x3, x4, x5))))))
   end

(* ----------------------------------  6  ----------------------------------- *)

structure Tuple6: TUPLE = 
   struct 
      structure T = FastTupleFn(FastTupleBase6)
      open T
      fun mkIter (i1, i2, i3, i4, i5, i6) f = 
          i1 (fn x1 => i2 (fn x2 => i3 (fn x3 => 
          i4 (fn x4 => i5 (fn x5 => i6 (fn x6 => 
            f (x1, x2, x3, x4, x5, x6)))))))
   end

(* ----------------------------------  7  ----------------------------------- *)

structure SlowTupleBase7 = 
   struct 
      type 'a tuple = 'a * 'a * 'a * 'a * 'a * 'a * 'a
      val size = 7
      fun toList (x1, x2, x3, x4, x5, x6, x7) = [x1, x2, x3, x4, x5, x6, x7]
      fun fromList [x1, x2, x3, x4, x5, x6, x7] = (x1, x2, x3, x4, x5, x6, x7)
        | fromList _ = raise Fail "Tuple7"
   end

structure Tuple7: TUPLE = 
   struct 
      structure T = SlowTupleFn(SlowTupleBase7)
      open T
      fun mkIter (i1, i2, i3, i4, i5, i6, i7) f = 
          i1 (fn x1 => i2 (fn x2 => i3 (fn x3 => 
          i4 (fn x4 => i5 (fn x5 => i6 (fn x6 => 
          i7 (fn x7 => f (x1, x2, x3, x4, x5, x6, x7))))))))
   end

(* ----------------------------------  8  ----------------------------------- *)

structure SlowTupleBase8 = 
   struct 
      type 'a tuple = 'a * 'a * 'a * 'a * 'a * 'a * 'a * 'a
      val size = 8
      fun toList (x1, x2, x3, x4, x5, x6, x7, x8) = [x1, x2, x3, x4, x5, x6, x7, x8]
      fun fromList [x1, x2, x3, x4, x5, x6, x7, x8] = (x1, x2, x3, x4, x5, x6, x7, x8)
        | fromList _ = raise Fail "Tuple8"
   end

structure Tuple8: TUPLE = 
   struct 
      structure T = SlowTupleFn(SlowTupleBase8)
      open T
      fun mkIter (i1, i2, i3, i4, i5, i6, i7, i8) f = 
          i1 (fn x1 => i2 (fn x2 => i3 (fn x3 => 
          i4 (fn x4 => i5 (fn x5 => i6 (fn x6 => 
          i7 (fn x7 => i8 (fn x8 => f (x1, x2, x3, x4, x5, x6, x7, x8)))))))))
   end

(* ----------------------------------  9  ----------------------------------- *)

structure Tuple9: TUPLE = 
   struct 
      structure T = FastTupleFn(FastTupleBase9)
      open T
      fun mkIter (i1, i2, i3, i4, i5, i6, i7, i8, i9) f = 
          i1 (fn x1 => i2 (fn x2 => i3 (fn x3 => 
          i4 (fn x4 => i5 (fn x5 => i6 (fn x6 => 
          i7 (fn x7 => i8 (fn x8 => i9 (fn x9 => 
           f (x1, x2, x3, x4, x5, x6, x7, x8, x9))))))))))
   end


