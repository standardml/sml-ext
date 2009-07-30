
functor MemoryPoolFn(structure Elem:
                     sig
                        type elem
                        val finalize: elem -> unit
                     end) :> MEMORY_POOL where type elem = Elem.elem =
struct 

structure A = GrowArray

type elem = Elem.elem

datatype pelem = E of {elem: Elem.elem,
                       ind: int,
                       live: bool ref}

datatype pool = P of {arr: Elem.elem A.growarray,
                      q: int list ref,
                      q_end: int ref}

exception Pool

fun get (E {elem, live, ...}) =
    if !live then elem else raise Pool

fun new {size, init} = 
    let
       val x0 = init ()
       val arr = A.growarray size x0
       fun appFn (n, _) = A.update arr n (init())
    in
       A.appi appFn arr
     ; P {arr = arr,
          q = ref [],
          q_end = ref 0}
    end
                          
fun alloc (P {arr, q, q_end}) = 
    case !q of
       [] => E {elem = A.sub arr (!q_end), 
                ind = !q_end,
                live = ref true} before Ref.incr q_end
     | h::t => E {elem = A.sub arr h, 
                  ind = h,
                  live = ref true} before q := t

fun dealloc (P {arr, q, q_end}, E {elem, ind, live}) = 
    let in 
       Elem.finalize elem
     ; live := false
     ; q := ind :: !q
    end

end
