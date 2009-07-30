
signature MEMORY_POOL =
sig

type elem
type pelem
type pool

exception Pool

val new: {size: int,
          init: unit -> elem} -> pool

(* Raises Pool if get is called after dealloc *) 
val get: pelem -> elem

val alloc: pool -> pelem
val dealloc: pool * pelem -> unit

end
