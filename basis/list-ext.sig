
signature LIST_EXT =
sig

include LIST

val cons: 'a -> 'a list -> 'a list
val dest: 'a list -> 'a * 'a list
val list: 'a -> 'a list
val replicate: int * 'a -> 'a list
val map2: ('a * 'b -> 'c) -> 'a list * 'b list -> 'c list
val app2: ('a * 'b -> unit) -> 'a list * 'b list -> unit
val concatMap: ('a -> 'b list) -> 'a list -> 'b list
val intersect: ''a list * ''a list -> ''a list

(* raises subscript *)
val !! : 'a list * int -> 'a

(** 
 * exception [Fail] if lists are unequal length
 *)
val map2i: (int * 'a * 'b -> 'c) -> 'a list * 'b list -> 'c list
val all2: ('a * 'b -> bool) -> 'a list * 'b list -> bool
val exists2: ('a * 'b -> bool) -> 'a list * 'b list -> bool
val zip: 'a list * 'b list  -> ('a * 'b) list
val unzip: ('a * 'b) list -> 'a list * 'b list  
val zip3: 'a list * 'b list * 'c list -> ('a * 'b * 'c) list
val unzip3: ('a * 'b * 'c) list -> 'a list * 'b list * 'c list
val zip4: 'a list * 'b list * 'c list * 'd list -> ('a * 'b * 'c * 'd) list
val unzip4: ('a * 'b * 'c * 'd) list -> 'a list * 'b list * 'c list * 'd list

(**
 * select([a,b,c,d,e],[0,2,4]) ~~> [a,c,e] 
 * select([a,b,c,d,e],[4,2,0]) ~~> [e,c,a] 
 * n^2 algorithm.
 *)
val select: 'a list * int list -> 'a list

(**
 * upto(3,6) ~~> [3,4,5,6] 
 *)
val upto: int * int -> int list

(**
 * upto1(3,6) ~~> [3,4,5] 
 *)
val upto1: int * int -> int list

(**
 * from_n(3,6) ~~> [3,4,5,6,7,8] 
 *)
val fromN: int * int -> int list
val toString: ('a -> string) -> 'a list -> string
val foldli: (int * 'a * 'b -> 'b) -> 'b -> 'a list -> 'b
val foldri: (int * 'a * 'b -> 'b) -> 'b -> 'a list -> 'b

(* Haskell List.group  
 * group "Mississippi" = ["M","i","ss","i","ss","i","pp","i"] 
 *)
val group: ''a list -> ''a list list

(**
 * use the first element as a base 
 *)
val foldl1: ('a * 'a -> 'a) -> 'a list -> 'a

(**
 * use the last element as a base 
 *)
val foldr1: ('a * 'a -> 'a) -> 'a list -> 'a

(**
 * split(3,[1,2,3,4,5,6,7] ~~> [[1,2,3],[4,5,6],[7]] 
 *)
val split: int * 'a list -> 'a list list

(* allPairs f ([1,2], [3,4,5] ~~> 
  [f(1,3), f(1, 4), f(1,5), f(2,3), f(2,4), f(2,5)] *)
val allPairs: ('a * 'b -> 'c) -> 'a list * 'b list -> 'c list

(* 
allCombs [[1,2], [4,5], [6,7]] 
--> [[1,4,6],[1,4,7],[1,5,6],[1,5,7],[2,4,6],[2,4,7],[2,5,6],[2,5,7]]
*)   
val allCombs: 'a list list -> 'a list list
val allMaps: 'a list * 'b list -> ('a * 'b) list list
val allMapsBy: ('a * 'b -> 'c option) -> 'a list * 'b list -> ('a * 'c) list list
val allInjectiveMaps: 'a list * 'b list -> ('a * 'b) list list
val allInjectiveMapsBy: ('a * 'b -> 'c option) -> 'a list * 'b list -> 'c list list

val allPartitions: 'a list -> 'a list list list
val allPartitionsBy: ('a list list -> bool) -> 'a list -> 'a list list list

val mapi: (int * 'a -> 'b) -> 'a list -> 'b list
val appi: (int * 'a -> unit) -> 'a list -> unit

val assoc: ''a * (''a * 'b) list -> 'b option
val assoc': ''a * (''a * 'b) list -> 'b 
val assocRem: ''a * (''a * 'b) list -> ('b * (''a * 'b) list) option
val genAssocRem: ('a * 'a -> bool) -> 'a * ('a * 'b) list -> ('b * ('a * 'b) list) option
val genAssoc: ('a * 'a -> bool) -> 'a * ('a * 'b) list -> 'b option
val genAssoc': ('a * 'a -> bool) -> 'a * ('a * 'b) list -> 'b 
val genRevAssoc: ('b * 'b -> bool) -> 'b * ('a * 'b) list -> 'a option
val revAssoc: ''b * ('a * ''b) list -> 'a option

(* Remove first occurrance. *)
val rem: ('a -> bool) -> 'a list -> 'a list

(* map over the list, but second argument of function is list
   with current element removed *) 
val mapRem: ('a * 'a list -> 'b) -> 'a list -> 'b list

(* cyclePairs [1,2,3] --> [(1,2), (2,3), (3, 1)] *) 
val cyclePairs: 'a list -> ('a * 'a) list

(* remove first *) 
val remEq: ''a -> ''a list -> ''a list

(* chop (l,n) = (take(l,n),drop(l,n)) but is faster *)
val chop: 'a list * int -> 'a list * 'a list

(* if there aren't enough elements, just take as many as exist *) 
val take': 'a list * int -> 'a list
val drop': 'a list * int -> 'a list

(* takeLast(l, n) = drop(l, length l - n) *) 
val takeLast: 'a list * int -> 'a list

(** 
 * remove all occurances of predicate 
 *)
val remAll: ('a -> bool) -> 'a list -> 'a list

(** 
 * remove first occurance of predicate 
 *)
val findRem: ('a -> bool) -> 'a list -> ('a * 'a list) option
val findApp: ('a -> 'b option) -> 'a list -> 'b option

(* raise Impossible if not present *) 
val lookup: ('a -> bool) -> 'a list -> 'a
val shuffle: 'a list * 'a list -> 'a list
val index: ('a -> bool) -> 'a list -> int option
val getIndex: ('a -> bool) -> 'a list -> (int * 'a) option
val separate: 'a -> 'a list -> 'a list
val separateMany: 'a list -> 'a list -> 'a list
val sort: ('a * 'a -> order) -> 'a list -> 'a list

(** 
 * sort and setify 
 *)
val sortSetify: ('a * 'a -> order) -> 'a list -> 'a list
val setify: ('a * 'a -> bool) -> 'a list -> 'a list
val setifyEq: ''a list -> ''a list
val subset: ''a list * ''a list -> bool

(* All 2^n subsets of a list *) 
val subsets: 'a list -> 'a list list

(** 
 * number a list 
 *)
val number: 'a list -> (int * 'a) list
val butlast: 'a list -> 'a list
val mapButlast: ('a -> 'b) * ('a -> 'b) -> 'a list -> 'b list
val mem: ''a * ''a list -> bool
val genMem: ('a * 'a -> bool) -> 'a * 'a list -> bool
val difference: ''a list * ''a list -> ''a list
val genDiff: ('a * 'a -> bool) -> 'a list * 'a list -> 'a list
val unique: ''a list -> bool

(**
 * insert (l,n,x) inserts x at position n in l 
 *)
val insert: 'a list * int * 'a -> 'a list
val lexSort: ('a * 'a -> order) -> 'a list * 'a list -> order

end
