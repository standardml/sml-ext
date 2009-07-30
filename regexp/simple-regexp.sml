
structure SimpleRegexp:> SIMPLE_REGEXP =
   struct 

      open GeneralExt
      infixr 0 `

      (* 
       CM.autoload "/usr0/local/smlnj/smlnj-lib/RegExp/regexp-lib.cm";  
       *)

      structure P = AwkSyntax
      structure E = BackTrackEngine (* DfaEngine *)
      structure RE = RegExpFn (structure P = P
                               structure E = E)

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

   (*
    structure R = SimpleRegexp
                     R.
    *)
