
structure RandomExt :> RANDOM_EXT =
struct 

open GeneralExt

structure R = Random
structure A = ArrayExt
structure V = VectorExt

fun randRealArray seed {lower,upper} = 
    let
       val n = V.length lower
       fun tab_fn i = 
           let
              val r = R.randReal seed
              val lo = V.sub(lower,i)
              val hi = V.sub(upper,i)
              val diff = hi - lo
           in
              lo + diff * r
           end
    in
       A.tabulate (n,tab_fn)
    end

fun randRealVector seed lu = A.vector (randRealArray seed lu)

fun split r = 
    let
       val n1 = R.randInt r
       val n2 = R.randInt r
       val r' = R.rand(n1,n2)
    in
       (r,r')
    end

fun splitN' (_,0) acc = acc
  | splitN' (r,n) acc = 
    let
       val (r1,r2) = split r
    in
       splitN'(r2,n) (r1::acc)
    end
fun splitN rn = splitN' rn []

open R

end
