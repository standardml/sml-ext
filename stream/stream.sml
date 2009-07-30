
(*
structure MemoStreamBase:> BASIC_STREAM =
   struct

      type 'a susp = unit -> 'a
      datatype 'a front = Nil | Cons of 'a * 'a stream
      withtype 'a stream = 'a front susp

      fun force s = s()

      fun delay (s : unit -> 'a front) : 'a stream =
          let
             val r = ref (fn () => raise Match)
          in
             r := (fn () => 
                      let val ss = s ()
                      in 
                         r := (fn () => ss);
                         ss
                      end);
             (fn () => (!r) ())
          end

      exception Undefined
      exception Empty

      fun empty () = Nil

      fun undefined () = raise Undefined

   end
*)

functor StreamBaseFn(L: LAZY'):> BASIC_STREAM =
   struct

      datatype 'a front = Nil | Cons of 'a * 'a stream
      withtype 'a stream = unit -> 'a front L.lazy

      fun force s = L.force(s())

      fun delay (s : unit -> 'a front) () =
          L.delay s

      exception Undefined
      exception Empty

      fun empty () = L.inject Nil
      fun undefined () = raise Undefined

   end

structure StreamBase = StreamBaseFn(Lazy')
structure StreamMemoBase = StreamBaseFn(LazyMemo)

functor StreamFn(S: BASIC_STREAM): STREAM = 
   struct 
   
      open S

      fun isUndefined s = 
          (ignore (force s); false)
          handle Undefined => true

      fun cons (x,s) = delay (fn () => Cons(x,s))

      fun map f s = delay (map' f s)
      and map' f s () = 
          case force s of
             Nil => Nil
           | Cons(x,xs) => Cons(f x, map f xs)

      local
         fun mapj f n s = delay (mapj' f n s)
         and mapj' f n s () = 
             case force s of
                Nil => Nil
              | Cons(x,xs) => Cons(f(n,x), mapj f (n+1) xs)
      in
      fun mapi f s = mapj f 0 s
      end

      fun repeat x = delay(fn () => Cons(x,repeat x))

      fun append s12 = delay (append' s12)
      and append' (s1,s2) () =
          case force s1 of
             Nil => force s2
           | Cons(x,xs) => Cons(x, append(xs,s2))

      fun memBy f (x,s) = case force s of
                             Nil => false
                           | Cons(x',xs) => f(x,x') orelse memBy f (x,xs)
                           
      fun mem xs = memBy op= xs

      fun app f s = case force s of
                       Nil => ()
                     | Cons (h, t) => ((f h: unit); app f t)

      fun foldr a b s = case force s of
                           Nil => b
                         | Cons (h, t) => a (h, foldr a b t)

      fun foldl a b s = case force s of
                           Nil => b
                         | Cons (h, t) => foldl a (a (h, b)) t
                                          
      fun all p s = 
          case force s of
             Nil => true
           | Cons(x,xs) =>
             if not (p x) then false
             else all p xs

      fun exists p s = 
          case force s of
             Nil => false
           | Cons(x,xs) =>
             if p x then true
             else exists p xs

      fun filter f s = delay (filter' f s)
      and filter' f s () =
          case force s of
             Nil => Nil
           | Cons(x, xs) => 
             if f x then Cons(x, filter f xs)
             else filter' f xs ()

      fun toList s = foldr op:: nil s

      fun take sn = delay(take' sn)
      and take' (_,0) () = Nil
        | take' (s,n) () = 
          case force s of
             Nil => Nil
           | Cons(x,xs) => Cons(x,take (xs,n-1))

      fun drop (s,0) = s
        | drop (s,n) = 
          case force s of
             Nil => s
           | Cons(_,xs) => drop (xs,n-1)

      fun fromList [] = empty
        | fromList (h::t) = delay (fn () => Cons(h,fromList t))

      local
         fun length' s acc = 
             case force s of
                Nil => acc
              | Cons(_,s') => length' s' (acc+1)
      in
      fun length s = length' s 0
      end

      fun isEmpty s = 
          case force s of
             Nil => true
           | _ => false

      fun head s = case force s of
                      Nil => raise Empty
                    | Cons(x,_) => x

      fun tail s = case force s of
                      Nil => raise Empty
                    | Cons(_,xs) => xs
          
      fun last s = case force s of
                      Nil => raise Empty
                    | Cons(x,xs) => 
                      case force xs of
                         Nil => x
                       | Cons _ => last xs

      local
         fun init1 y s = delay(init1' y s)
         and init1' y s () = 
             case force s of
                Nil => Nil
              | Cons(z,zs) => Cons(y,init1 z zs)
      in
      fun init s = delay(init' s)
      and init' s () = 
          case force s of 
             Nil => raise Empty
           | Cons(x,xs) => force(init1 x xs)
      end

      fun span p s = case force s of
                        Nil => (empty,empty)
                      | Cons(h,t) => 
                        if p h then
                           let 
                              val (yes,no) = span p t
                           in
                              (cons(h,yes),no)
                           end
                        else (empty,s)

      fun concat s = delay (concat' s)
      and concat' s () =
          case force s of
             Nil => Nil
           | Cons(x,xs) => force(append(x,concat xs))

      fun singleton x = delay(fn () => Cons(x,empty))

      fun append1 (s,x) = append(s,singleton x)

      fun inits s = delay(inits' s)
      and inits' s () = 
          case force s of
             Nil => Cons(empty,empty)
           | Cons(x,xs) => 
             force(append(singleton empty, map (fn y => cons(x,y)) (inits xs)))

      fun join f st = delay (join' f st)
      and join' f (s,t) () = 
          case force s of
             Nil => Nil
           | Cons(x,xs) => 
             force(append (map (fn y => f(x,y)) t, join f (xs,t)))

      fun joinBy f st = delay (joinBy' f st)
      and joinBy' f (s,t) () = 
          case force s of
             Nil => Nil
           | Cons(x,xs) => force (append (map (fn y => f(x,y)) (t x), joinBy f (xs,t))) 

      fun eqBy p (s,t) = 
          case (force s,force t) of
             (Nil,Nil) => true
           | (Cons(x,xs),Cons(y,ys)) => p(x,y) andalso eqBy p (xs,ys)
           | _ => false

      fun eq xy = eqBy op= xy

      fun finite s = 
          case force s of
             Nil => true
           | Cons(_,s') => finite s'

(*       fun nubBy eq s = delay(nubBy' eq s) *)
(*       and nubBy' eq s () = *)
(*           case force s of *)
(*              Nil => Nil *)
(*            | Cons(x,xs) => Cons(x,filter (fn y => not(eq(x,y))) (nubBy eq xs)) *)

      fun nubByl eq l s = delay(nubByl' eq l s)
      and nubByl' eq l s () =
          case force s of
             Nil => Nil
           | Cons(x,xs) => if ListExt.genMem eq (x,l) then nubByl' eq l xs ()
                           else Cons(x,nubByl eq (x::l) xs)
      fun nubBy eq s = nubByl eq [] s

      fun nub s = nubBy op= s
                           
      fun nth (s,n) = 
          case (force s,n) of
             (Nil,_) => raise Subscript
           | (Cons(x,_),0) => x
           | (Cons(_,xs),n) => nth(xs,n-1)
          
      fun insert f xs = delay(insert' f xs)
      and insert' f (x,s) () = 
          case force s of
             Nil => Cons(x,empty)
           | Cons(y,ys) => 
             case f(x,y) of
                GREATER => Cons(y,insert f (x,ys))
              | _ => Cons(x,s)

      fun sort f s = foldr (insert f) empty s

      fun lexOrder f (s,t) = 
          case (force s, force t) of
             (Nil,Nil) => EQUAL 
           | (Nil,_) => LESS 
           | (_,Nil) => GREATER
           | (Cons(x,xs),Cons(y,ys)) => 
             case f(x,y) of
                EQUAL => lexOrder f (xs,ys)
              | x => x

      local
         fun index' p s n = 
             case force s of
                Nil => NONE 
              | Cons(x,xs) => 
                if p x then SOME n
                else index' p xs (n+1)
      in
      fun index p s = index' p s 0
      end

      fun find p s = 
          case force s of
             Nil => NONE 
           | Cons(x,xs) => if p x then SOME x else find p xs

      fun findIndices1 p n s = delay(findIndices1' p n s)
      and findIndices1' p n s () = 
          case force s of
             Nil => Nil
           | Cons(x,xs) => 
             if p x then Cons(n,findIndices1 p (n+1) xs)
             else findIndices1' p (n+1) xs ()

      fun findIndices p s = findIndices1 p 0 s

      (* -------------------------------------------------------------------------- *)
      (*  IO                                                                        *)
      (* -------------------------------------------------------------------------- *)

      fun stringStream s =
          let
             val ss = size s
             fun next n = 
                 if n >= ss
                 then empty
                 else cons (CharVector.sub(s, n),
                              next (n + 1))
          in
             next 0
          end

      fun fileStream f =
          let
             val ff = BinIO.openIn f
                      
             fun rd () =
                 case BinIO.input1 ff of
                    NONE => (BinIO.closeIn ff; 
                             empty)
                  | SOME c => cons(chr (Word8.toInt c), rd ())
          in
             rd ()
          end

      fun toString' f s = 
          if isUndefined s then ", UNDEF" else
          case force s of
             Nil => ""
           | Cons(x,xs) => 
             String.concat[",", f x, toString' f xs]

      fun toString f s = 
          if isUndefined s then "UNDEF" else
          case force s of
             Nil => "<>" 
           | Cons(x,xs) => String.concat ["<",f x, toString' f xs,">"]

      fun toString2 f s = toString (toString f) s

      fun outputBy' f (out,s) = 
          let
             fun outp s = TextIO.output(out,s)
          in
             if isUndefined s then outp ", UNDEF" else
             case force s of
                Nil => ()
              | Cons(x,xs) => 
                (outp ","
               ; f(out,x): unit
               ; outputBy' f (out,xs))
          end

      fun outputBy f (out,s) = 
          let
             fun outp s = TextIO.output(out,s)
          in
             if isUndefined s then outp "UNDEF" else
             case force s of
                Nil => outp "<>"
              | Cons(x,xs) => 
                (outp "<"
               ; f(out,x): unit
               ; outputBy' f (out,xs)
               ; outp ">")
          end

      fun print f s = outputBy (f o Pair.snd) (TextIO.stdOut,s)

      structure Ops = 
         struct 
            val !! = nth
            val $ = toList
            val % = fromList
         end
                
   end

structure Stream = StreamFn(StreamBase)
structure StreamMemo = StreamFn(StreamMemoBase)

