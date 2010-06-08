
signature SEQ =
sig

type 'a t

val empty: 'a t
val singleton: 'a -> 'a t
val cons: 'a * 'a t -> 'a t
val snoc: 'a t * 'a -> 'a t
val concat: 'a t * 'a t -> 'a t
val fromList: 'a list -> 'a t
val length: 'a t -> int
val null: 'a t -> bool
val map: ('a -> 'b) -> 'a t -> 'b t
val foldr: ('a * 'b -> 'b) -> 'b -> 'a list -> 'b
val foldl: ('a * 'b -> 'b) -> 'b -> 'a list -> 'b

datatype 'a viewl = EmptyL
                  | Cons of 'a * 'a t

val viewl: 'a t -> 'a viewl

datatype 'a viewr = EmptyR
                  | Snoc of 'a t * 'a

val viewr: 'a t -> 'a viewr



end

(* This should probably be made lazy as in Okasaki.  *) 

structure Seq :> SEQ =

datatype 'a t = Q of 'a list * 'a list 

datatype 'a viewl = EmptyL
                  | Cons of 'a * 'a t

fun viewl (

datatype 'a viewr = EmptyR
                  | Snoc of 'a t * 'a

val empty = Q ([], [])



fun cons (x, Q (front, rear)) = Q (x :: front, rear)
fun snoc (x, Q (front, rear)) = Q (front, x :: rear)
fun concat (Q {front, rear}, Q {front=front', rear=rear') = Q {front = front, rear = x :: rear}
