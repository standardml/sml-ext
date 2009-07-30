
structure HashTableExt : HASH_TABLE_EXT =
struct 

structure H = HashTable
open H

fun pp f tbl = PP.listVert f (H.listItemsi tbl)


local
   exception Nope
in
fun all f tbl = 
    let in 
       H.appi (fn x => if f x then () else raise Nope) tbl
     ; true
    end handle Nope => false
end


end
