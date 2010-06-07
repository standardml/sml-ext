
signature UNIV = 
sig

type t
val embed: unit -> ('a -> t) * (t -> 'a option)

end
