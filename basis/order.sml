
structure Order :> ORDER =
struct 

fun lexOrder oa ob ((a, b), (aa, bb)) =
    case oa (a, aa) of
       LESS => LESS
     | GREATER => GREATER
     | EQUAL => ob (b, bb)

fun lexOrder3 oa ob oc ((a, b, c), (aa, bb, cc)) =
    case oa (a, aa) of
       LESS => LESS
     | GREATER => GREATER
     | EQUAL => 
       case ob (b, bb) of
          LESS => LESS
        | GREATER => GREATER
        | EQUAL => oc(c, cc)

fun listOrder _ ([],[]) = EQUAL 
  | listOrder _ (_,[]) = GREATER
  | listOrder _ ([],_) = LESS
  | listOrder ord (h1::t1, h2::t2) = lexOrder ord (listOrder ord) ((h1,t1),(h2,t2))

fun optionOrder cmp (SOME a, SOME b) = cmp(a, b)
  | optionOrder _ (SOME _, NONE) = GREATER
  | optionOrder _ (NONE, SOME _) = LESS
  | optionOrder _ (NONE, NONE) = EQUAL

end
