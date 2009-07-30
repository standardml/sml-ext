
functor StandardLpFn(B : STANDARD_LP_BASE): STANDARD_LP =
struct 
         
structure L = ListExt
structure R = RealExt

open PP.Ops                   
     
open B

(* -------------------  convert interval lp to real lp   -------------------- *)

fun morphMonom f (x, y) = (f x, y)

fun morphLcomb f l = map (morphMonom f) l

fun morphConstr f (Constr {name, body, bound}) =
                   Constr {name = name,
                           body = morphLcomb f body,
                           bound = f bound}

fun morphLp f (Lp {name, vars, obj, constrs}) =
    Lp {name = name,
        vars = vars,
        obj = morphLcomb f obj,
        constrs = map (morphConstr f) constrs}

(* -----------------------------  Pretty print  ----------------------------- *)

fun ppMonom f (c, x) = %[f c, $" * ", Var.pp x]

fun ppLcomb f l = %(L.separate ($" + ") (map (ppMonom f) l))

fun ppConstr f (Constr {name, body, bound}) =
    %[Name.pp name, $" : ", ppLcomb f body, $" >= ", f bound]

val stripe = PP.stripe {length = 80,
                        lhs = "",
                        rhs = "",
                        char = #"-"}

fun pp f (Lp {name, vars, obj, constrs}) = 
    &[stripe,
      ~,
      $"Linear Program in Standard Form",
      ~,
      %[\\, $"i.e. ", &[$"Minimize c x",
                      $"         A x >= b",
                      $"         x >= 0"]],
      ~,
      %[$"Name: ", $name],
      ~,
      %[$"Variables: ", PP.listHoriz Var.pp vars],
      ~,
      $"Minimize",
      ~,
      %[\\, ppLcomb f obj],
      ~,
      $"Subject to",
      ~,
      %[\\, &(map (ppConstr f) constrs)],
      ~,
      $"End",
      ~,
      stripe]
    
fun ppStatus OK = $"OK"
  | ppStatus Infeasible = $"Infeasible"
  | ppStatus Unbounded = $"Unbounded"
  | ppStatus Error = $"Error"

fun ppAnswer (Lp {name, vars, constrs, ...}, ans) = 
    let
       fun ppVar v = %[Var.pp v, $" : ", $(R.toString (colPrimal(ans, v)))]
       fun ppConstr (Constr {name, ...}) = 
           %[Name.pp name, $" : ", $(R.toString(rowDual(ans, name)))]
    in
       &[stripe,
         ~,
         %[$"Result for LP: ", $name],
         ~,
         %[$"Status: ", ppStatus (B.status ans)],
         ~,
         %[$"Objective: ", PP.real (B.objective ans)],
         ~,
         $"Column (primal) values: ",
         ~,
         %[\\, &(map ppVar vars)],
         ~,
         $"Row (dual) values: ", 
         %[\\, &(map ppConstr constrs)],
         ~,
         stripe]
    end

end
