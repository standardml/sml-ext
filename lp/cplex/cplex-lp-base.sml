
structure CplexLpBase :> STANDARD_LP_BASE = 
struct 

  open GeneralExt
  infixr 0 `

  structure C = Cplex
  structure A = ArrayExt
  structure L = ListExt
  structure Var = NameFun()
  structure Name = NameFun()

  open PP.Ops                   

  structure VarKey : ORD_KEY =
  struct 
    type ord_key = Var.name
    val compare = Var.compare
  end

  structure M : ORD_MAP = SplayMapFn(VarKey) 

  type var = Var.name
  type name = Name.name

  type 'a monom = 'a * var 

  type 'a lcomb = 'a monom list

  type 'a constr = {name : name,
                    body : 'a lcomb,
                    bound  : 'a}

  type 'a lp = {name    : string,
                vars    : var list,
                obj     : 'a lcomb,
                constrs : 'a constr list}

  val new_var = Var.new_name
  val var_eq = Var.eq
  val var_to_string = Var.to_string

  val new_name = Name.new_name
  val name_eq = Name.eq
  val name_to_string = Name.to_string

  fun lp_to_cplex {name,vars,obj,constrs} = 
      let 
        val numrows = length constrs
        val numcols = length vars
        (* The index of the variable in [vars] *)
        val vmap = L.foldli (fn (i,v,m) => M.insert(m,v,i)) M.empty vars
        val objective = A.array(numcols,0.0)
        fun obj_fn (c,v) = 
            let
              val i = case M.find(vmap,v) of
                        SOME i => i
                      | NONE => raise Fail ("can't find var: " ^ var_to_string v)
              val x = A.sub(objective,i)
            in
              A.update(objective,i,x + c)
            end
        val _ = app obj_fn obj
        val rhs = A.from_list (map #bound constrs)
        val sense = A.array(numrows,C.Geq)
        (* put the coefficients in column format in [col_vals] *)
        val col_vals = L.tabulate(numcols,(fn _ => (ref [] : (real * int) list ref)))
        fun app_fn row (c,v) = 
            let
              val col = 
                  case M.find(vmap,v) of
                    SOME i => i
                  | NONE => raise Fail ("can't find var: " ^ var_to_string v)
              val cur_vals = L.nth(col_vals,col)
            in
              cur_vals := (c,row)::(!cur_vals)
            end
        val _ = L.appi (fn (row,{body,...} : real constr) => app (app_fn row) body) constrs
        (* now we have the column info *)
        val col_vals = map (fn x => rev ` !x) col_vals
        val all_vals = L.concat col_vals
        val matcnt_list = map length col_vals
        val matcnt = A.from_list matcnt_list
        val (matval,matind) = L.unzip all_vals
        val matind = A.from_list matind
        val matval = A.from_list matval
        fun fold_fn (n,l as h::_) = h + n::l
          | fold_fn _ = raise Impossible
        val matbeg = rev (tl (foldl fold_fn [0] matcnt_list))
        val matbeg = A.from_list matbeg
        val lb = A.array(numrows,0.0)
        val ub = A.array(numrows,Real.posInf)
        val rowname = A.from_list (map (name_to_string o #name) constrs)
        val colname = A.from_list (map var_to_string vars)
      in
(*         PP.message ` &[%[$"name: ",$name], *)
(*                       %[$"numrows: ",PP.int numrows], *)
(*                       %[$"numcols: ",PP.int numcols], *)
(*                       %[$"dir: minimize"], *)
(*                       %[$"obj: ",PP.list_horiz PP.real (A.to_list objective)], *)
(*                       %[$"rhs: ",PP.list_horiz PP.real (A.to_list rhs)], *)
(*                       %[$"sense: ",PP.list_horiz $ (L.tabulate(numrows,const ">="))], *)
(*                       %[$"matbeg: ",PP.list_horiz PP.int (A.to_list matbeg)], *)
(*                       %[$"matcnt: ",PP.list_horiz PP.int (A.to_list matcnt)], *)
(*                       %[$"matind: ",PP.list_horiz PP.int (A.to_list matind)], *)
(*                       %[$"matval: ",PP.list_horiz PP.real (A.to_list matval)], *)
(*                       %[$"lb: ",PP.list_horiz PP.real (A.to_list lb)], *)
(*                       %[$"ub: ",PP.list_horiz PP.real (A.to_list ub)], *)
(*                       %[$"rowname: ",PP.list_horiz $ (A.to_list rowname)], *)
(*                       %[$"colname: ",PP.list_horiz $ (A.to_list colname)]]; *)
        C.make {name = name,
                numrows = numrows,
                numcols = numcols,
                dir = C.Minimize,
                obj = objective,
                rhs = rhs,
                sense = sense,
                matbeg = matbeg,
                matcnt = matcnt,
                matind = matind,
                matval = matval,
                lb = lb,
                ub = ub,
                rowname = rowname,
                colname = colname}
      end

  fun write_problem (lp,file) = C.write(lp_to_cplex lp,file)

  type answer = {rlp : real lp,
                 clp : C.lp}

  fun solve rlp = 
      let 
        val clp = lp_to_cplex rlp
      in
        C.opt clp;
        {rlp = rlp,
         clp = clp}        
      end

  datatype status = OK
		  | Infeasible
		  | Unbounded
		  | Error
  
  fun status {rlp=_,clp} = 
      let (* try *) in 
        if not ` C.feasible clp then Infeasible
        else OK
      end
      handle C.Unbounded => Unbounded
           | C.Error => Error

  fun objective {rlp=_,clp} = C.obj clp

  fun col_primal ({rlp,clp} : answer,v) = 
      let
        val prims = C.prim_vars clp
      in
        case L.index (fn v' => var_eq(v,v')) (#vars rlp) of
          SOME ind => A.sub(prims,ind)
        | NONE => raise Fail ("can't find primal value for " ^ var_to_string v)
      end

  fun row_dual ({rlp,clp} : answer,n) = 
      let
        val duals = C.dual_vars clp
      in
        case L.index (fn n' => name_eq(n,n')) (map #name  (#constrs rlp)) of
          SOME ind => A.sub(duals,ind)
        | NONE => raise Fail ("can't find dual value for " ^ name_to_string n)
      end

end


