
structure SimpleRegexp:> SIMPLE_REGEXP =
struct 

open GeneralExt
infixr 0 `

structure RE = RegExpFn (struct 
                            structure P = AwkSyntax
                            structure E = BackTrackEngine
                         end)

type regexp = string

val num = "[0123456789]"

fun inLanguage sregexp line =
    let
       val cregexp = RE.compileString sregexp
       val matches = RE.find cregexp Substring.getc (Substring.full line)
       val n = String.size line
    in
       case matches of
          NONE => false
        | SOME (tree,_) =>
          (case MatchTree.nth(tree,0) of
              NONE => raise Impossible
            | SOME {len,pos=_} => len = n)
    end

end
