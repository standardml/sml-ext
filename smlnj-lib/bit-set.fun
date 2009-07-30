
(* 
 Set of ints as a bit vector.  The ints you're storing should be in the
range 0..size.  

Data is arranged as little-endian words.  If the word size is 8, then
we have the following layout

[7, 6, 5, 4, 3, 2, 1, 0] [15, 14, 13, 12, 11, 10, 9, 8] [ ... ] ...

we call the place in the vector the "row" and the place 
in the word the "column"

The map, fold, etc. functions are extremely inefficient.  This
representation is optimized for space, union, intersection, etc. 
*)

signature BIT_SET = 
   sig
      include ORD_SET where type Key.ord_key = int 
      val capacity: int
      (* Don't check for membership *)
      val delete': set * item -> set
      val vsize: set -> int
      val wordString: set -> string
   end

signature BIT_SET_EXT = 
   sig
      include ORD_SET_EXT where type Key.ord_key = int 
      val capacity: int
      (* Don't check for membership *)
      val delete': set * item -> set
      val vsize: set -> int
      val wordString: set -> string
   end

(* -------------------------------------------------------------------------- *)
(*  BitSetFn                                                                  *)
(* -------------------------------------------------------------------------- *)

functor BitSetFn(val size: int) :> BIT_SET =
struct 

structure L = ListExt
structure S = StringExt
structure W = Word32
structure V = VectorExt

val >> = W.>>
val << = W.<<
infix >> << 

structure Key = 
   struct 
      type ord_key = int
      val compare = Int.compare
   end

type item = Key.ord_key
type set = W.word vector

val vsize = V.length

val capacity = size

(* Number of rows *) 
val arrSize = size div W.wordSize + 
              (if size mod W.wordSize = 0 then 0 else 1)

(* -------------------------------------------------------------------------- *)
(*  Util                                                                      *)
(* -------------------------------------------------------------------------- *)

fun row n = n div W.wordSize
fun col n = n mod W.wordSize
val zero = W.fromInt 0
val one = W.fromInt 1
fun mask n = one << (Word.fromInt n)

val wstr = StringCvt.padLeft #"0" 8
val wordString = S.concat o map (wstr o W.toString) o V.toList

(* -------------------------------------------------------------------------- *)
(*  Basic set ops                                                             *)
(* -------------------------------------------------------------------------- *)

val empty = V.tabulate(arrSize, fn _ => W.fromInt 0)

fun singleton n = 
    let
       val i = row n
       val j = col n
       fun tabFn k = if k <> i
                        then W.fromInt 0
                     else mask j
    in
       V.tabulate(arrSize, tabFn)
    end

fun add (s, n) = 
    if n > size - 1 
       then raise Fail (S.concat ["Set overflow: capacity = ", Int.toString size, " input = ", Int.toString n])
    else if n < 0
       then raise Fail (S.concat ["Can't add a negative int to a bit set: ", Int.toString n])
    else
       let
          val i = row n
          val j = col n
          val m = mask j
          val w = V.sub(s, i)
          val w' = W.orb(w, m)
       in
          V.update(s, i, w')
       end

fun add' (n, s) = add (s, n)

fun addList (s, ns) = foldr add' s ns

fun fromList ns = addList(empty, ns)

fun member (s, n) = 
    let
       val i = row n
       val j = col n
       val m = mask j
       val w = V.sub(s, i)
    in
       W.andb(w, m) > zero
    end

fun delete' (s, n) = 
    let
       val i = row n
       val j = col n
       val m = mask j
       val m' = W.notb m
       val w = V.sub(s, i)
       val w' = W.andb(w, m')
    in
       V.update(s, i, w')
    end

fun delete (s, n) = 
    if not (member(s, n))
       then raise LibBase.NotFound
    else delete' (s, n)

fun isEmpty s = V.all (fn x => x = zero) s

fun equal (s, t) = V.all2 op= (s, t)

fun compare (s, t) = 
    let
       exception GT
       exception LT
       fun appFn (i, x) = 
           let
              val y = V.sub(t, i)
           in
              if (x:W.word) < y 
                 then raise LT
              else if y < x 
                 then raise GT
              else ()
           end
    in
       let in
          V.appi appFn s
        ; EQUAL
       end handle GT => GREATER
                | LT => LESS
    end

fun isSubset (s, t) = 
    let 
       fun allFn (x, y) = W.andb(x, y) = x
    in
       V.all2 allFn (s, t)
    end

(* Kernighan's method for counting bits, from
 http://www-graphics.stanford.edu/~seander/bithacks.html#CountBitsSetKernighan
 *)
fun numItems1 v0 =
    let
       val c = ref 0 
       val v = ref v0
    in
       while !v <> zero do
          let in 
             Ref.incr c
           ; v := W.andb (!v, !v - one)
          end
     ; !c
    end
    
fun numItems s = V.foldr (fn (x, acc) => acc + numItems1 x) 0 s

fun union (s, t) = V.map2 W.orb (s, t)

fun intersection (s, t) = V.map2 W.andb (s, t)

fun difference1 (x, y) = W.andb(x, W.xorb(x, y))

fun difference (s, t) = V.map2 difference1 (s, t)

val listItems1 : W.word -> int list =
 fn s => 
    let
       val i = ref 0
       val l : int list ref = ref []
    in
       while !i < size do
          let in 
             if W.andb(s >> Word.fromInt (!i), one) > zero 
                then l := !i :: !l 
             else ()
           ; Ref.incr i
          end
     ; !l
    end

fun listItems s = 
    let
       val acc = ref []
       fun appFn (i, w) = 
           let
              val l = listItems1 w
           in
              acc := map (fn x => W.wordSize * i + x) l @ (!acc)
           end
    in
       V.appi appFn s
     ; L.sort Int.compare (!acc)
    end

(* For these, it's probably just as fast to convert to 
 a list, do the operation, and convert back to a bit vector *)
fun map f s = 
    let
       val l = listItems s
       val l' = L.map f l
    in
       fromList l'
    end

fun app f s = L.app f (listItems s)

fun foldl f b s = L.foldl f b (listItems s)
fun foldr f b s = L.foldr f b (listItems s)
fun partition f s = 
    let
       val (yes, no) = L.partition f (listItems s)
    in
       (fromList yes, fromList no)
    end

fun filter f s = fromList (L.filter f (listItems s))
fun exists f s = L.exists f (listItems s)
fun find f s = L.find f (listItems s)

end

(* -------------------------------------------------------------------------- *)
(*  BitSetExtFn                                                               *)
(* -------------------------------------------------------------------------- *)

functor BitSetExtFn(val size: int) :> BIT_SET_EXT =
struct 
   structure Set = BitSetFn(val size = size)
   structure Set' = OrdSetExtFn'(struct 
                                    structure Set = Set
                                    val ppItem = PP.int
                                 end)
   open Set'
   val capacity = Set.capacity
   val vsize = Set.vsize
   val delete' = Set.delete'
   val wordString = Set.wordString
end


