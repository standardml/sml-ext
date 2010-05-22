
functor OrdMapExtFn(Args: ORD_MAP_EXT_ARGS): ORD_MAP_EXT =
struct 

open GeneralExt
open PP.Ops

structure M = OrdMapFn(Args)

structure L = ListExt

infix 5 |-> |=>

fun (k |-> v) m = M.insert(m, k, v)
fun (k |=> v) = M.insert(M.empty, k, v)

fun choose m = 
    case M.firsti m of
       NONE => NONE
     | SOME (x, _) => 
       let 
          val (m', y) = M.remove(m, x)
       in
          SOME (x, y, m')
       end

fun delete mx = 
    SOME (M.remove mx) 
    handle LibBase.NotFound => NONE

fun difference (m1, m2) = 
    M.filteri (fn (k,_) => not (isSome (M.find(m2, k)))) m1

local
   exception NotEqual
in
fun equalBy p (m1, m2) =
    let
       (* Test that everything in m1 equals the corresponding entry in m2 *) 
       fun appFn (k,v) =
           case M.find (m2, k) of 
              NONE => raise NotEqual
            | SOME v' => if p(v, v') then () else raise NotEqual
       (* Test that m2 doesn't have anything not in m1 *)
       fun appFn2 (k,_) = 
           case M.find (m1, k) of 
              NONE => raise NotEqual
            | SOME _ => ()
    in
       M.appi appFn m1
     ; M.appi appFn2 m2
     ; true
    end
    handle NotEqual => false
end

fun equal ms = equalBy op= ms

(* -------------------------------------------------------------------------- *)
(*  Exists                                                                    *)
(* -------------------------------------------------------------------------- *)

fun search f (m:'a M.map) = 
    let
       exception FoundIt of 'a
       fun appFn x = if f x then raise FoundIt x else ()
    in
       let in
          M.app appFn m
        ; NONE 
       end
       handle FoundIt x => SOME x
    end

fun searchi f (m:'a M.map) = 
    let
       exception FoundIt of M.Key.ord_key * 'a
       fun appFn x = if f x then raise FoundIt x else ()
    in
       let in
          M.appi appFn m
        ; NONE 
       end
       handle FoundIt x => SOME x
    end

fun exists f (m:'a M.map) = isSome (search f m)
fun existsi f (m:'a M.map) = isSome (searchi f m)

fun all f = not o exists (not o f)
fun alli f = not o existsi (not o f) 

fun fromList l = foldr M.insert' M.empty l 

fun ppVert f s = %[$"{",&(L.mapButlast (fn x => %[x, $", "], fn x => x) (map f (M.listItemsi s))),$"}"]
fun ppHoriz f s = %[$"{",%(L.separate ($", ") (map f (M.listItemsi s))),$"}"]

fun compare f (m1, m2) =
    let
       val items1 = M.listItems m1
       val items2 = M.listItems m2
    in
       Order.listOrder f (items1, items2)
    end

open M

end
