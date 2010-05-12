
structure PP : PP =
struct 

structure D = SimpleTextIODev
structure S = StringExt
structure L = ListExt
structure X = TextIOExt
open GeneralExt 

structure TextToken =
struct
   type token = string
   type style = unit
   fun string t = t
   fun style _ = ()
   fun size t = String.size t
end

structure PP = PPStreamFn(struct 
                             structure Token = TextToken
                             structure Device = D
                          end)
structure P = PPDescFn(PP)

type stream = PP.stream

fun outStream strm wid = 
    PP.openStream (D.openDev {dst = strm, wid = wid})
fun stdoutStream wid =
    PP.openStream (D.openDev {dst = X.stdOut, wid = wid})

type pp = P.pp_desc

fun hbox l = P.hBox l
fun vBox n l = P.vBox(PP.Rel n, l)
val vbox = vBox 0
fun hvBox n l = P.hvBox(PP.Rel n, l)
val hvbox = hvBox 0
fun hovBox n l = P.hovBox(PP.Rel n, l)
val hovbox = hovBox 0
(* fun box l = P.box(PP.Rel 0, l) *)
val string = P.string
val space = P.space
val cut = P.cut
val newline = P.newline
val nbspace = P.nbSpace
val empty = hbox []
fun vboxNewline l = vbox (L.separate cut l)
fun vboxNewline2 l = vbox (L.separateMany [cut, cut] l)
fun char c = string(Char.toString c)

fun separate x l = hovbox (L.separate x l)

fun tokenize s = 
    let
       val ss = S.chop [#" ", #"\n"] s
       val ds = map P.string ss
    in
       L.separate (P.space 1) ds
    end

fun tokenize' ss = 
    let
       val ss' = map tokenize ss
    in
       List.concat (L.separate [P.space 1] ss')
    end

fun sentences ss = P.box(PP.Rel 0, tokenize' ss)

fun text s = 
    let
       val ss = S.chop [#"."] s
       val ss' = map (fn x => x ^ ".") ss
    in
       sentences ss'
    end

fun doit1 strm dsc = 
    let in
       P.description(strm, dsc)
     ; PP.flushStream strm
    end

fun cutl l = vbox(L.separate P.cut l)
fun cutl2 l = vbox(L.separate P.cut l)

structure Ops =
   struct 
      val op$ = string
      val op$$ = text
      val op% = hbox
      val op%% = hvbox
      fun & x = vboxNewline x
      fun && x = vboxNewline2 x
      val // = cut
      val ~ = empty
      val \ = space 1
      val \\ = nbspace 2
   end

open Ops

local
   val qt = $"\""
   val lb = $"["
   val rb = $"]"
   val ls = $"{"
   val rs = $"}"
   val lp = $"("
   val rp = $")"
   val co = $","
in

fun quote p = %[qt, p, qt]
fun brack p = %[lb, p, rb]
fun curly p = %[ls, p, rs]
fun paren p = %[lp, p, rp]
fun pair (p1, p2) = %[lp, p1, co, \, p2, rp]
fun bool true = $"true"
  | bool false = $"false"
fun int n = $(Int.toString n)
fun option _ NONE = $"NONE"
  | option p (SOME x) = %[$"SOME ", p x]
fun intinf n = $(IntInf.toString n)
fun real x = $(Real.toString x)
fun realFmt n x = $(Real.fmt (StringCvt.GEN (SOME n)) x)
fun time t = $(Time.toString t)
fun date t = $(Date.toString t)

(* Commas after the elements *)
(* fun aggVert lb rb conv f l = %[lb, &(L.mapButlast (fn x => %[f x, co], f) (conv l)), rb] *)

(* Commas before the elements *)
fun aggVert lb rb conv f l = 
    case conv l of 
       [] => %[lb, rb]
     | [h] => %[lb, f h, rb]
     | h :: t => &[%[lb, \, f h]
                  , &(map (fn x => %[co, f x]) t)
                  , rb]

fun aggHoriz lb rb conv f l = %[lb, %(L.separateMany [co, \] (map f (conv l))), rb]

fun listVert f l = aggVert ($"[") ($"]") (fn x => x) f l
fun listHoriz f l = aggHoriz ($"[") ($"]") (fn x => x) f l

fun arrayVert f l = aggVert ($"[|") ($"|]") ArrayExt.toList f l
fun arrayHoriz f l = aggHoriz ($"[|") ($"|]") ArrayExt.toList f l

fun vectorVert f l = aggVert ($"[<") ($">]") VectorExt.toList f l
fun vectorHoriz f l = aggHoriz ($"[<") ($">]") VectorExt.toList f l

fun tupleVert f l = aggVert ($"(") ($")") (fn x => x) f l
fun tupleHoriz f l = aggHoriz ($"(") ($")") (fn x => x) f l

fun array2 f m = 
    let
       val n = Array2.nRows m
       val rows = map (fn i => VectorExt.toList(Array2.row(m, i))) (ListExt.upto(0, n-1))
       val prows = map (listHoriz f) rows
    in
       &prows
    end

val unit = $"()"

fun stripe {length, lhs, rhs, char} = 
    let
       val nL = S.size lhs
       val nR = S.size rhs
       val mid = length - nL - nR 
    in
       $(concat[lhs, StringCvt.padLeft char mid "", rhs])
    end

fun banner (opts as {length, lhs, rhs, ...}) s = 
    let
       val str = stripe opts 
       val middle = concat [lhs, " ", s, " "]
       val nR = S.size rhs
       val mid = concat[StringCvt.padRight #" " (length-nR) middle, rhs]
    in
       &[str,
         $mid,
         str]
    end

end

fun doit stm pp = doit1 stm (&[ pp
                              , ~])

fun pp pp = doit (stdoutStream 80) pp
fun ppl pp = doit (stdoutStream 80) (&[ pp, ~])
(* fun ppl pp = doit (stdoutStream 80) (pp *)

fun writeFileColumn (file, n, p) = 
    let
       val stm = X.openOut file
       val pstm = outStream stm n 
    in
       doit pstm p
     ; X.closeOut stm
    end

fun writeFile (file, p) = writeFileColumn(file, 100000, p)

end
