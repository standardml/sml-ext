

(**
 * Memoized Streams
 *)

(* -------------------------------------------------------------------------- *)
(*  Basic streams                                                             *)
(* -------------------------------------------------------------------------- *)

signature BASIC_STREAM =
sig

type 'a stream
datatype 'a front = Nil | Cons of 'a * 'a stream
exception Undefined
exception Empty
val force: 'a stream -> 'a front
val delay: (unit -> 'a front) -> 'a stream
val empty: 'a stream
val undefined: 'a stream

end

(* -------------------------------------------------------------------------- *)
(*  Streams                                                                   *)
(* -------------------------------------------------------------------------- *)

signature STREAM =
sig

include BASIC_STREAM
val isUndefined: 'a stream -> bool
val isEmpty: 'a stream -> bool
val map: ('a -> 'b) -> 'a stream -> 'b stream
val mapi: (int * 'a -> 'b) -> 'a stream -> 'b stream
val app: ('a -> unit) -> 'a stream -> unit
val all: ('a -> bool) -> 'a stream -> bool
val exists: ('a -> bool) -> 'a stream -> bool
val repeat: 'a -> 'a stream
val filter: ('a -> bool) -> 'a stream -> 'a stream
val append: 'a stream * 'a stream -> 'a stream
val append1: 'a stream * 'a -> 'a stream
val foldr: ('a * 'b -> 'b) -> 'b -> 'a stream -> 'b
val foldl: ('a * 'b -> 'b) -> 'b -> 'a stream -> 'b
val take: 'a stream * int -> 'a stream
val drop: 'a stream * int -> 'a stream
val index: ('a -> bool) -> 'a stream -> int option
val length: 'a stream -> int
val toList: 'a stream -> 'a list
val find: ('a -> bool) -> 'a stream -> 'a option
val findIndices: ('a -> bool) -> 'a stream -> int stream
val fromList: 'a list -> 'a stream
val head: 'a stream -> 'a
val tail: 'a stream -> 'a stream
val last: 'a stream -> 'a
val init: 'a stream -> 'a stream
val inits: 'a stream -> 'a stream stream
val memBy: ('a * 'a -> bool) -> 'a * 'a stream -> bool
val mem: ''a * ''a stream -> bool
(* raises Empty *)
val span: ('a -> bool) -> 'a stream -> 'a stream * 'a stream
val cons: 'a * 'a stream -> 'a stream
val concat: 'a stream stream -> 'a stream
val singleton: 'a -> 'a stream
val join: ('a * 'b -> 'c) -> 'a stream * 'b stream -> 'c stream
(** 
 * in GHC
 * ? [x:xs|x <- [1..4], xs <- [[5],[6],[7]]]
 * [[1,5],[1,6],[1,7],[2,5],[2,6],[2,7],[3,5],[3,6],[3,7],[4,5],[4,6],[4,7]]
 *)
val joinBy: ('a * 'b -> 'c) -> 'a stream * ('a -> 'b stream) -> 'c stream  
val nth : 'a stream * int -> 'a
(* raises Empty *)
val eq: ''a stream * ''a stream -> bool
val eqBy: ('a * 'a -> bool) -> 'a stream * 'a stream -> bool
val finite: 'a stream -> bool
val nubBy: ('a * 'a -> bool) -> 'a stream -> 'a stream
val nub: ''a stream -> ''a stream
val sort: ('a * 'a -> order) -> 'a stream -> 'a stream
val lexOrder: ('a * 'a -> order) -> 'a stream * 'a stream -> order
val stringStream: string -> char stream
val fileStream: string -> char stream
(* doesn't terminate for infinite streams *)
val toString: ('a -> string) -> 'a stream -> string
val toString2: ('a -> string) -> 'a stream stream -> string
(* write the elements as you force them *)
val outputBy: (TextIO.outstream * 'a -> unit) 
              -> TextIO.outstream * 'a stream -> unit
val print: ('a -> unit) -> 'a stream -> unit
structure Ops: 
             sig
                val !! : 'a stream * int -> 'a (* nth *)
                val $ : 'a stream -> 'a list (* toList *)
                val % : 'a list -> 'a stream (* fromList *)
             end
end


(*
 structure S = Stream
 fun fib 0 = 1 | fib 1 = 1 | fib n = fib (n-1) + fib (n-2);
 fun fibStream n = S.delay (fn () => S.Cons(fib n,fibStream (n+1)))
 val fibStream = fibStream 0

 open S
 infix !!

 fibStream !! 40
 fibStream !! 41
 *)
