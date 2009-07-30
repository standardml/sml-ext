
(**
 * MLton link to the GNU GLPK linear programming library.
 * External function names are from the manual.  
 *)
structure Cplex:> CPLEX = 
struct 

structure F = MLton.Finalizable
structure P = MLton.Pointer
structure A = ArrayExt

type ptr = P.t 
type lp' = ptr
type lp = lp' F.t

(* CPLEX liscense environment *)
type env' = ptr
type env = env' F.t

exception Infeasible
exception Unbounded
exception Error

datatype dir = Minimize | Maximize

datatype kind = Leq
              | Eq
              | Geq

local
   val cpx_open_cplex = _import "CPXopenCPLEX" : int ref -> env';
   val cpx_close_cplex = _import "CPXcloseCPLEX" : env' -> int;
in
val license_env : env = 
    let 
       val status = ref 0
       val env' = cpx_open_cplex status
       val _ = if env' = P.null then raise Fail "Can't get CPLEX license environment" else ()
       fun finalize env = 
           let
              val code = cpx_close_cplex env
           in
              if code = 0 then () else raise Fail "Can't release CPLEX license"
           end
       val env = F.new env'
    in
       F.addFinalizer(env,finalize)
     ; env
    end
end (* local *) 

local
   val cpx_create_prob = _import "CPXcreateprob" : env' * int ref * string -> lp';
   val cpx_free_prob = _import "CPXfreeprob" : env' * lp' -> int;
in
fun create {name} : lp = 
    let
       val status = ref 0
       val lp' : lp' = F.withValue(license_env,(fn env => cpx_create_prob(env,status,name)))
       val _ = if lp' = P.null then raise Fail "Can't create CPLEX problem instance" else ()
       fun finalize lp = 
           let
              val code = F.withValue(license_env,(fn env => cpx_free_prob(env,lp)))
           in
              if code = 0 then () else raise Fail "Can't release CPLEX lp"
           end
       val lp = F.new lp'
    in
       F.addFinalizer(lp,finalize)
     ; lp
    end
end (* local *) 

local
   (* int CPXPUBLIC CPXcopylpwnames(CPXCENVptr env, CPXLPptr lp, int numcols, int numrows, 
                                    int objsen, const double * obj, const double * rhs, 
                                    const char * sense, const int * matbeg, const int * matcnt, 
                                    const int * matind, const double * matval, const double * lb, 
                                    const double * ub, const double * rngval, char ** colname, 
                                    char ** rowname) *)
   val cpx_copy_lp_w_names = _import "CPXcopylpwnames" : 
                             (* CPXCENVptr env *)
                             env' * 
                             (* CPXLPptr lp *)
                             lp' * 
                             (* numcols *)
                             int *
                             (* numrows *) 
                             int *
                             (* objsen *)
                             int * 
                             (* obj *)
                             real array * 
                             (* rhs *)
                             real array * 
                             (* sense *)
                             char array *
                             (* matbeg *) 
                             int array * 
                             (* matcnt *)
                             int array * 
                             (* matind *)
                             int array * 
                             (* matval *)
                             real array * 
                             (* lb *)
                             real array *
                             (* ub *)
                             real array *
                             (* rngval *)
                             real array *
                             (* colname *)
                             string array * 
                             (* rowname *)
                             string array -> int;

   (* From the CPLEX manual *)
   fun dir_to_sense Maximize = ~1
     | dir_to_sense Minimize = 1

   fun kind_to_sense Leq = #"L"
     | kind_to_sense Eq = #"E"
     | kind_to_sense Geq = #"G"

in
fun fill {lp : lp,
          numrows : int,
          numcols : int,
          dir : dir,
          obj : real array,
          rhs : real array,
          sense : kind array,
          matbeg : int array,
          matcnt : int array,
          matind : int array,
          matval : real array,
          lb : real array,
          ub : real array,
          rowname : string array,
          colname : string array} : unit = 
    let
       val null = A.array(0,0.0)
       val code = F.withValue
                     (license_env,
                      (fn env => 
                          F.withValue
                             (lp,
                              (fn lp => 
                                  cpx_copy_lp_w_names
                                     (env,
                                      lp,
                                      numcols,
                                      numrows,
                                      dir_to_sense dir,
                                      obj,
                                      rhs,
                                      A.map kind_to_sense sense,
                                      matbeg,
                                      matcnt,
                                      matind,
                                      matval,
                                      lb,
                                      ub,
                                      null,
                                      colname,
                                      rowname)))))
    in
       if code = 0 then () else raise Fail "Couldn't fill CPLEX matrix"
    end
end (* local *)

fun make {name,numrows,numcols,dir,obj,rhs,sense,matbeg,matcnt,matind,
          matval,lb,ub,rowname,colname} =
    let
       val lp = create {name=name}
    in
       fill {lp = lp,
             numrows = numrows,
             numcols = numcols,
             dir = dir,
             obj = obj,
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
     ; lp
    end

fun withval0 f lp = 
    F.withValue(license_env,
                (fn env => F.withValue(lp,
                                       (fn lp => f(env,lp))))) 

fun withval1 f (lp,x) = 
    F.withValue(license_env,
                (fn env => 
                    F.withValue(lp,(fn lp =>
                                       f(env,lp,x))))) 

fun withval2 f (lp,x,y) = 
    F.withValue(license_env,
                (fn env => 
                    F.withValue(lp,(fn lp =>
                                       f(env,lp,x,y))))) 

fun withval3 f (lp,x,y,z) = 
    F.withValue(license_env,
                (fn env => 
                    F.withValue(lp,(fn lp =>
                                       f(env,lp,x,y,z))))) 

fun withval4 f (lp,x,y,z,w) = 
    F.withValue(license_env,
                (fn env => 
                    F.withValue(lp,(fn lp =>
                                       f(env,lp,x,y,z,w))))) 

local
   (* int CPXPUBLIC CPXgetnumrows(CPXCENVptr env, CPXCLPptr lp) *)
   val cpx_getnumrows = _import "CPXgetnumrows" : env' * lp' -> int;
   (* int CPXPUBLIC CPXgetnumcols(CPXCENVptr env, CPXCLPptr lp) *)
   val cpx_getnumcols = _import "CPXgetnumcols" : env' * lp' -> int;
in
fun numrows lp = withval0 cpx_getnumrows lp
fun numcols lp = withval0 cpx_getnumcols lp
end

local 
   (* int CPXPUBLIC CPXlpopt(CPXCENVptr env, CPXLPptr lp) *)
   val cpx_lpopt = _import "CPXlpopt" : env' * lp' -> int;
in
fun opt lp = 
    let
       val code = withval0 cpx_lpopt lp
    in
       if code = 0 then () else raise Error
    end
end (* local *) 

local
   (* int CPXPUBLIC CPXgetobjval(CPXCENVptr env, CPXCLPptr lp, double * objval_p) *)
   val cpx_getobjval = _import "CPXgetobjval" : env' * lp' * real ref -> int;
in
fun obj lp =
    let
       val res = ref 0.0
       val code = withval1 cpx_getobjval (lp,res)
    in
       if code = 0 then !res else raise Unbounded
    end
end

local
   (* int CPXPUBLIC CPXgetx(CPXCENVptr env, CPXCLPptr lp, double * x, int begin, int end) *)
   val cpx_getx = _import "CPXgetx" : env' * lp' * real array * int * int -> int;
   (* int CPXPUBLIC CPXgetpi(CPXCENVptr env, CPXCLPptr lp, double * x, int begin, int end) *)
   val cpx_getpi = _import "CPXgetpi" : env' * lp' * real array * int * int -> int;
in
fun prim_vars lp = 
    let
       val cols = numcols lp
       val res = Array.array(cols,0.0)
       val code = withval3 cpx_getx (lp,res,0,cols-1)
    in
       if code = 0 then res else raise Fail "Can't get primal variables"        
    end

fun dual_vars lp = 
    let
       val cols = numrows lp
       val res = Array.array(cols,0.0)
       val code = withval3 cpx_getpi (lp,res,0,cols-1)
    in
       if code = 0 then res else raise Fail "Can't get dual variables"        
    end
end

local
   (* int CPXPUBLIC CPXwriteprob(CPXCENVptr env, CPXCLPptr lp, const char * filename_str, const char * filetype_str) *)
   val cpx_writeprob = _import "CPXwriteprob" : env' * lp' * string * string -> int;
in
fun write (lp,file) = 
    let
       val code = withval2 cpx_writeprob (lp,file,"lp")
    in
       if code = 0 then () else raise Fail "Can't write file" 
    end
end

local
   (* int CPXPUBLIC CPXsolninfo(CPXCENVptr env, CPXCLPptr lp, int * solnmethod_p, int * solntype_p, int * pfeasind_p, int * dfeasind_p) *)
   val cpx_solninfo = _import "CPXsolninfo" : env' * lp' * ptr * ptr * int ref * int ref -> int;
in
fun feasible lp = 
    let
       val prim_feas = ref 0
       val dual_feas = ref 0
       val code = withval4 cpx_solninfo (lp,P.null,P.null,prim_feas,dual_feas)
    in
       if code = 0 then !prim_feas <> 0 andalso !dual_feas <> 0 else raise Fail "Can't write file" 
    end
end


end
