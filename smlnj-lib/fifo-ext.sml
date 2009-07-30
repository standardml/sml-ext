
structure FifoExt: FIFO_EXT =
struct 

structure F = Fifo

fun dequeue' q = 
    SOME (F.dequeue q)
    handle F.Dequeue => NONE 

fun enqueueList (q,l) = foldl (Fun.swap F.enqueue) q l

open F

end
