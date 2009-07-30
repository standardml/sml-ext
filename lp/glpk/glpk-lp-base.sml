
(*  
 Minimize c^T x
 s.t.     A x >= b
          x >= 0
*)

structure GlpkLpBase :> STANDARD_LP_BASE = 
struct 

structure G = Glpk
structure L = ListExt
structure I = IntExt
structure Var = NameFn(val name = "Var")
structure Name = NameFn(val name = "Name")
open GeneralExt 

type 'a monom = 'a * Var.t 

type 'a lcomb = 'a monom list

datatype 'a constr = Constr of {name: Name.t,
                                body: 'a lcomb,
                                bound: 'a}

datatype 'a lp = Lp of {name: string,
                        vars: Var.t list,
                        obj: 'a lcomb,
                        constrs: 'a constr list}

type answer = real lp * G.lp 

fun findRow (Lp {constrs,...}, n) = 
    case L.index (fn Constr {name,...} => Name.eq(n,name)) constrs of
       NONE => raise Fail ("can't find row: <" ^ Name.toString n ^ ">") 
     | SOME i => i + 1 (* GLPK indices start at 1 *)

fun findCol (Lp {vars,...}, v) = 
    case L.index (fn v' => Var.eq(v,v')) vars of
       NONE => raise Fail ("can't find column: <" ^ Var.toString v ^ ">") 
     | SOME j => j + 1 (* GLPK indices start at 1 *)

fun lpToGlpk (lp as Lp {name,vars,obj,constrs}) = 
    let 
       val glp = G.createProb()
       val ncoefs = I.sum (map (fn Constr {body, ...} => length body) constrs)
       val iarr = Array.array(ncoefs+1,~1)
       val jarr = Array.array(ncoefs+1,~1)
       val farr = Array.array(ncoefs+1,0.0)
       val ctr = ref 1
       fun colFn (i,v) = 
           let 
              val name = Var.toString v
           in
              G.setColName(glp,i+1,name)
            ; G.setColBnds(glp,i+1,G.GLP_LO 0.0)
           end 
       fun rowFn (i, Constr {name,body,bound}) =
           let
              fun monomFn (c,v) = 
                  (Array.update(iarr,!ctr,i+1);
                   Array.update(jarr,!ctr,findCol(lp,v));
                   Array.update(farr,!ctr,c);
                   Ref.incr ctr)
           in
              G.setRowName(glp,i+1,Name.toString name)
            ; G.setRowBnds(glp,i+1,G.GLP_LO bound)
            ; app monomFn body
           end
       fun objFn (c,v) = G.setObjCoef(glp,findCol(lp,v),c)
    in
       G.setProbName(glp,name)
     ; G.setObjDir(glp,G.GLP_MIN)
     ; G.addRows(glp,length constrs)
     ; G.addCols(glp,length vars)
     ; L.appi colFn vars 
     ; L.appi rowFn constrs 
     ; app objFn obj
     ; G.loadMatrix(glp,ncoefs,iarr,jarr,farr)
     ; glp
    end

fun solve lp =
    let
       val glp = lpToGlpk lp
    in
       G.simplex (glp,G.defaultParams)
     ; (lp,glp)
    end 

fun writeProblem (lp,file) = 
    let 
       val glp = lpToGlpk lp
    in
       G.writeCpxlp(glp,file)
    end

datatype status = OK
		| Infeasible
		| Unbounded
		| Error

fun status (_,glp) = case G.getSolnStatus glp of
		        G.GLP_OPT => OK
		      | G.GLP_FEAS => OK
		      | G.GLP_UNBND => Unbounded
		      | G.GLP_INFEAS => Infeasible
		      | G.GLP_NOFEAS => Infeasible
		      | _ => Error

fun check f ans = 
    case status ans of
       OK => f ans
     | Infeasible => raise Fail "GLPK: solution is infeasible"
     | Unbounded => raise Fail "GLPK: solution is unbounded"
     | Error => raise Fail "GLPK: error"

fun objective ans = check (G.getObjVal o Pair.snd) ans
fun colPrimal (ans,v) = check (fn (lp,glp) => G.getColPrim(glp,findCol(lp,v))) ans
fun rowDual (ans,c) = check (fn (lp,glp) => G.getRowDual(glp,findRow(lp,c))) ans

end
