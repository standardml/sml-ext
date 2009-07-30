
signature SIMPLE_REGEXP = 
sig

  type regexp = string
  
  val num : regexp

  val inLanguage : regexp -> string -> bool
    (**
     * inLanguage (concat ["[+-]?",R.num,"+.",R.num,"+"]) "~1.78" 
     *)

end

