
signature ORD_SET_EXT_ARGS = 
sig 
  type ord_key
  val compare: ord_key * ord_key -> order
  val ppItem: ord_key -> PP.pp
end

signature ORD_SET_EXT =
sig
  include ORD_SET
  val ppItem: item -> PP.pp
  val eqKey: item * item -> bool
  val choose: set -> (set * item) option
  val choose': set -> item
  val foldr1: (item * item -> item) -> set -> item
  val findRem: (item -> bool) -> set -> (item * set) option
  val unions: set list -> set
  val intersections: set list -> set
  val isSupset: set * set -> bool
  val allPairs: (item * item -> item) -> set * set -> set
  val mapPartial: (item -> item option) -> set -> set
  val all: (item -> bool) -> set -> bool
  val isProperSubset: set * set -> bool

  (* delete the item if its a member.  do nothing otherwise *)
  val deleteIfMem: set * item -> set

  val pmap: (Key.ord_key -> 'a) -> set -> 'a list
  val pp: set -> PP.pp
  val ppVert: set -> PP.pp
end
