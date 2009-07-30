
signature ORDER = 
sig

val lexOrder: ('a * 'b -> order) -> ('c * 'd -> order) ->
              ('a * 'c) * ('b * 'd) -> order

val lexOrder3: ('a * 'b -> order) -> ('c * 'd -> order) -> ('e * 'f -> order) ->
               ('a * 'c * 'e) * ('b * 'd * 'f) -> order

val listOrder: ('a * 'b -> order) -> 'a list * 'b list -> order

val optionOrder: ('a * 'b -> order) -> 'a option * 'b option -> order

end
