signature FOLD =
sig

type ('a, 'b, 'c, 'd) step = 'a * ('b -> 'c) -> 'd
type ('a, 'b, 'c, 'd) t = ('a, 'b, 'c, 'd) step -> 'd
type ('a1, 'a2, 'b, 'c, 'd) step0 =
     ('a1, 'b, 'c, ('a2, 'b, 'c, 'd) t) step
type ('a11, 'a12, 'a2, 'b, 'c, 'd) step1 =
     ('a12, 'b, 'c, 'a11 -> ('a2, 'b, 'c, 'd) t) step
     
val fold: 'a * ('b -> 'c) -> ('a, 'b, 'c, 'd) t
val lift0: ('a1, 'a2, 'a2, 'a2, 'a2) step0
           -> ('a1, 'a2, 'b, 'c, 'd) step0
val post: ('a, 'b, 'c1, 'd) t * ('c1 -> 'c2)
          -> ('a, 'b, 'c2, 'd) t
val step0: ('a1 -> 'a2) -> ('a1, 'a2, 'b, 'c, 'd) step0
val step1: ('a11 * 'a12 -> 'a2) -> ('a11, 'a12, 'a2, 'b, 'c, 'd) step1

end
