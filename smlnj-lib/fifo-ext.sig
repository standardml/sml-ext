
signature FIFO_EXT =
sig

include FIFO

val dequeue' : 'a fifo -> ('a fifo * 'a) option
val enqueueList : 'a fifo * 'a list -> 'a fifo

end
