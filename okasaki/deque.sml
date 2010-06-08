exception NotFound
infixr ++
exception NotImplemented
fun stub _ = raise NotImplemented
datatype 'a susp = $ of 'a
fun force ($x) = x

functor BankersDeque (val c : int) : DEQUE =  (* c > 1 *)
struct
  open Stream
  type 'a Queue = int * 'a Stream * int * 'a Stream

  val empty = (0, $Nil, 0, $Nil)
  fun isEmpty (lenf, f, lenr, r) = (lenf+lenr = 0)

  fun check (q as (lenf, f, lenr, r)) =
        if lenf > c*lenr+1 then
          let val i = (lenf+lenr) div 2   val j = lenf + lenr - i
              val f' = take (i, f)        val r' = r ++ reverse (drop (i, f))
          in (i, f', j, r') end
        else if lenr > c*lenf + 1 then
	  let val j = (lenf+lenr) div 2   val i = lenf + lenr - j
              val r' = take (j, r)        val f' = f ++ reverse (drop (j, r))
          in (i, f', j, r') end
        else q

  fun cons (x, (lenf, f, lenr, r)) = check (lenf+1, $(Cons (x, f)), lenr, r)
  fun head (lenf, $Nil, lenr, $Nil) = raise Empty
    | head (lenf, $Nil, lenr, $(Cons (x, _))) = x
    | head (lenf, $(Cons (x, f')), lenr, r) = x
  fun tail (lenf, $Nil, lenr, $Nil) = raise Empty
    | tail (lenf, $Nil, lenr, $(Cons (x, _))) = empty
    | tail (lenf, $(Cons (x, f')), lenr, r) = check (lenf-1, f', lenr, r)

  val snoc = stub
  val last = stub
  val init = stub
end

functor SimpleCatenableDeque (D : DEQUE) : CATENABLE_DEQUE =
struct
  datatype 'a ED = Elem of 'a | Deque of 'a ED D.Queue

  datatype 'a Cat =
      Shallow of 'a ED D.Queue
    | Deep of 'a ED D.Queue * 'a Cat susp * 'a ED D.Queue

  fun tooSmall d = D.isEmpty d orelse D.isEmpty (D.tail d)

  fun dappendL (d1, d2) =
        if D.isEmpty d1 then d2 else D.cons (D.head d1, d2)
  fun dappendR (d1, d2) =
        if D.isEmpty d2 then d1 else D.snoc (d1, D.head d2)

  val empty = Shallow D.empty
  fun isEmpty (Shallow d) = D.isEmpty d
    | isEmpty _ = false

  fun consED (x, Shallow d) = Shallow (D.cons (x, d))
    | consED (x, Deep (f, m, r)) = Deep (D.cons (x, f), m, r)

  fun cons (x, d) = consED (Elem x, d)

  fun headED (Shallow d) = D.head d
    | headED (Deep (f, m, r)) = D.head f

  fun head d = let val Elem x = headED d in x end

  fun tail (Shallow d) = Shallow (D.tail d)
    | tail (Deep (f, m, r)) =
        let val f' = D.tail f
        in
            if not (tooSmall f') then Deep (f', m, r)
            else if isEmpty (force m) then Shallow (dappendL (f', r))
            else let val Deque d = headED (force m)
                 in Deep (dappendL (f', d), $(tail (force m)), r) end
        end

  val snoc = stub
  val last = stub
  val init = stub

  fun (Shallow d1) ++ (Shallow d2) =
        if tooSmall d1 then Shallow (dappendL (d1, d2))
        else if tooSmall d2 then Shallow (dappendR (d1, d2))
        else Deep (d1, $empty, d2)
    | (Shallow d) ++ (Deep (f, m, r)) =
        if tooSmall d then Deep (dappendL (d, f), m, r)
        else Deep (d, $(consED (Deque f, force m)), r)
    | (Deep (f, m, r)) ++ (Shallow d) =
        if tooSmall d then Deep (f, m, dappendR (r, d))
        else Deep (f, $(snoc (force m, Deque r)), d)
    | (Deep (f1, m1, r1)) ++ (Deep (f2, m2, r2)) =
        Deep (f1, $(snoc (force m1, Deque r1) ++ consED (Deque f2, force m2)), r2)
end

structure Deque : CATENABLE_DEQUE = 
struct 
  structure Q = BankersDeque(val c = 2)
  structure D = SimpleCatenableDeque(Q)
  open D
end
