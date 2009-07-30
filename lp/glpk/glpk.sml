
(**
 * MLton link to the GNU GLPK linear programming library.
 * External function names are from the manual.  In one case
 * I needed to do a "nm libglpk.a" to find the name 
 * when the symbol was not named as in the manual.
 *)
structure Glpk:> GLPK = 
struct 

structure F = MLton.Finalizable
structure P = MLton.Pointer
structure S = StringExt
open GeneralExt

type ptr = P.t 
type lp' = ptr
type lp = lp' F.t

datatype var = GLP_FR (* free *)
             | GLP_LO of real (* bounded below *)
             | GLP_UP of real (* bounded above *)
             | GLP_DB of real * real (* double bounded *)
             | GLP_FX of real (* fixed *)

datatype dir = GLP_MIN
             | GLP_MAX

datatype solver_status = OK (* GLPK returns 0 in this case *)
		       | GLP_EBADB
		       | GLP_ESING
		       | GLP_ECOND
		       | GLP_EBOUND
		       | GLP_EFAIL
		       | GLP_EOBJLL
		       | GLP_EOBJUL
		       | GLP_EITLIM
		       | GLP_ETMLIM
		       | GLP_ENOPFS
		       | GLP_ENODFS

datatype msg_lev = GLP_MSG_OFF
		 | GLP_MSG_ERR
		 | GLP_MSG_ON
		 | GLP_MSG_ALL

datatype presolve = GLP_ON
		  | GLP_OFF

datatype soln_status = GLP_OPT
		     | GLP_FEAS
		     | GLP_INFEAS
		     | GLP_NOFEAS
		     | GLP_UNBND
		     | GLP_UNDEF

datatype exact_status = LPX_E_OK
		      | LPX_E_FAULT
		      | LPX_E_ITLIM
		      | LPX_E_TMLIM

type params = {msgLev: msg_lev,
	       presolve: presolve,
	       itLim: int,
	       tmLim: int}

(* -------------------------------------------------------------------------- *)
(*  Util                                                                      *)
(* -------------------------------------------------------------------------- *)

fun withval0 f lp = F.withValue(lp,f)
fun withval1 f (lp,x1) = F.withValue(lp,(fn lp' => f(lp',x1)))
fun withval2 f (lp,x1,x2) = F.withValue(lp,(fn lp' => f(lp',x1,x2)))
fun withval3 f (lp,x1,x2,x3) = F.withValue(lp,(fn lp' => f(lp',x1,x2,x3)))
fun withval4 f (lp,x1,x2,x3,x4) = F.withValue(lp,(fn lp' => f(lp',x1,x2,x3,x4)))

val dummy = ~1.0

(* -------------------------------------------------------------------------- *)
(*  C Macros                                                                  *)
(* -------------------------------------------------------------------------- *)

(* Variable types *)
val (getGlpFr,_) = _symbol "glp_fr" : (unit -> int) * (int -> unit);
val glpFr = getGlpFr()

val (getGlpLo,_) = _symbol "glp_lo" : (unit -> int) * (int -> unit);
val glpLo = getGlpLo()

val (getGlpUp,_) = _symbol "glp_up" : (unit -> int) * (int -> unit);
val glpUp = getGlpUp()

val (getGlpDb,_) = _symbol "glp_db" : (unit -> int) * (int -> unit);
val glpDb = getGlpDb()

val (getGlpFx,_) = _symbol "glp_fx" : (unit -> int) * (int -> unit);
val glpFx = getGlpFx()

(* Optimization direction *)
val (getGlpMin,_) = _symbol "glp_min" : (unit -> int) * (int -> unit);
val glpMin = getGlpMin()

val (getGlpMax,_) = _symbol "glp_max" : (unit -> int) * (int -> unit);
val glpMax = getGlpMax()

(* Simplex return status *)
val (getGlpEbadb,_) = _symbol "glp_ebadb" : (unit -> int) * (int -> unit);
val glpEbadb = getGlpEbadb()

val (getGlpEsing,_) = _symbol "glp_esing" : (unit -> int) * (int -> unit);
val glpEsing = getGlpEsing()

val (getGlpEcond,_) = _symbol "glp_econd" : (unit -> int) * (int -> unit);
val glpEcond = getGlpEcond()

val (getGlpEbound,_) = _symbol "glp_ebound" : (unit -> int) * (int -> unit);
val glpEbound = getGlpEbound()

val (getGlpEfail,_) = _symbol "glp_efail" : (unit -> int) * (int -> unit);
val glpEfail = getGlpEfail()

val (getGlpEobjll,_) = _symbol "glp_eobjll" : (unit -> int) * (int -> unit);
val glpEobjll = getGlpEobjll()

val (getGlpEobjul,_) = _symbol "glp_eobjul" : (unit -> int) * (int -> unit);
val glpEobjul = getGlpEobjul()

val (getGlpEitlim,_) = _symbol "glp_eitlim" : (unit -> int) * (int -> unit);
val glpEitlim = getGlpEitlim()

val (getGlpEtmlim,_) = _symbol "glp_etmlim" : (unit -> int) * (int -> unit);
val glpEtmlim = getGlpEtmlim()

val (getGlpEnopfs,_) = _symbol "glp_enopfs" : (unit -> int) * (int -> unit);
val glpEnopfs = getGlpEnopfs()

val (getGlpEnodfs,_) = _symbol "glp_enodfs" : (unit -> int) * (int -> unit);
val glpEnodfs = getGlpEnodfs()

(* Solution status *)
val (getGlpOpt,_) = _symbol "glp_opt" : (unit -> int) * (int -> unit);
val glpOpt = getGlpOpt()

val (getGlpFeas,_) = _symbol "glp_feas" : (unit -> int) * (int -> unit);
val glpFeas = getGlpFeas()

val (getGlpInfeas,_) = _symbol "glp_infeas" : (unit -> int) * (int -> unit);
val glpInfeas = getGlpInfeas()

val (getGlpNofeas,_) = _symbol "glp_nofeas" : (unit -> int) * (int -> unit);
val glpNofeas = getGlpNofeas()

val (getGlpUnbnd,_) = _symbol "glp_unbnd" : (unit -> int) * (int -> unit);
val glpUnbnd = getGlpUnbnd()

val (getGlpUndef,_) = _symbol "glp_undef" : (unit -> int) * (int -> unit);
val glpUndef = getGlpUndef()

(* Terminal output *)
val (getGlpMsgOff,_) = _symbol "glp_msg_off" : (unit -> int) * (int -> unit);
val glpMsgOff = getGlpMsgOff()

val (getGlpMsgErr,_) = _symbol "glp_msg_err" : (unit -> int) * (int -> unit);
val glpMsgErr = getGlpMsgErr()

val (getGlpMsgOn,_) = _symbol "glp_msg_on" : (unit -> int) * (int -> unit);
val glpMsgOn = getGlpMsgOn()

val (getGlpMsgAll,_) = _symbol "glp_msg_all" : (unit -> int) * (int -> unit);
val glpMsgAll = getGlpMsgAll()

fun msgLevelToInt m = case m of 
			    GLP_MSG_OFF => glpMsgOff
			  | GLP_MSG_ERR => glpMsgErr
			  | GLP_MSG_ON => glpMsgOn
			  | GLP_MSG_ALL => glpMsgAll

(* Presolver *)
val (getGlpOn,_) = _symbol "glp_on" : (unit -> int) * (int -> unit);
val glpOn = getGlpOn()

val (getGlpOff,_) = _symbol "glp_off" : (unit -> int) * (int -> unit);
val glpOff = getGlpOff()

fun presolveToInt m = case m of
			   GLP_ON => glpOn
			 | GLP_OFF => glpOff

(* Experimental exact arithmetic solver *)
val (getLpxE,_) = _symbol "lpx_e" : (unit -> int) * (int -> unit);
val lpxE = getLpxE()

val (getLpxE,_) = _symbol "lpx_e" : (unit -> int) * (int -> unit);
val lpxE = getLpxE()

val (getLpxE,_) = _symbol "lpx_e" : (unit -> int) * (int -> unit);
val lpxE = getLpxE()

val (getLpxE,_) = _symbol "lpx_e" : (unit -> int) * (int -> unit);
val lpxE = getLpxE()

val (getParamPtr,_) = _symbol "param_ptr" : (unit -> ptr) * (ptr -> unit);
val paramPtr = getParamPtr()

(* -------------------------------------------------------------------------- *)
(*  API functions                                                             *)
(* -------------------------------------------------------------------------- *)

(* Initialize a glpSmcp for controling [simplex] *)

val maxInt = case Int.maxInt of 
		SOME n => n
	      | NONE => 1073741823 (* if NONE, ints are unbounded. 1073741823 is maxInt on
				                    32 bit machine *)

val defaultParams = {msgLev = GLP_MSG_ERR,
		      presolve = GLP_OFF,
		      itLim = maxInt,
		      tmLim = maxInt}

val setParams = _import "glp_set_params" : int * int * int * int -> unit;

(* val createIndex = _import "glp_create_index" : lp' -> unit; *)
(* val deleteIndex = _import "glp_delete_index" : lp' -> unit; *)
val createProb' = _import "glp_create_prob" : unit -> lp';
val deleteProb = _import "glp_delete_prob" : lp' -> unit;

fun createProb () = 
    let
       val prob = F.new (createProb'())
       fun finalize p = 
           let in
              (* printl "deleting glpk problem instance..."; *)
              (* deleteIndex p; *)
              deleteProb p
           end
    in
       F.addFinalizer(prob,finalize)
     (* ; withval0 createIndex prob *)
     ; prob
    end

(* return is the index of first new row *)
val addRows' = _import "glp_add_rows" : lp' * int -> int;
val addRows = ignore o withval1 addRows'

(* return is the index of first new col *)
val addCols' = _import "glp_add_cols" : lp' * int -> int;
val addCols = ignore o withval1 addCols'

val setProbName' = _import "glp_set_prob_name" : lp' * string -> unit;
fun setProbName (lp,s) = 
    let in 
(*        printl ("setting prob name: " ^ s); *)
       withval1 setProbName' (lp, S.nullTerminate s)
    end

val setRowName' = _import "glp_set_row_name" : lp' * int * string -> unit;
fun setRowName (lp,j,s) = 
    let in
(*        printl ("setting row name: " ^ s); *)
       withval2 setRowName' (lp,j, S.nullTerminate s)
    end

val setColName' = _import "glp_set_col_name" : lp' * int * string -> unit;
fun setColName (lp,j,s) = 
    let in 
(*        printl ("setting col name: " ^ s); *)
       withval2 setColName' (lp,j,S.nullTerminate s)
    end

local 
   fun setBnds f (lp,i,GLP_FR) = withval4 f (lp,i,glpFr,dummy,dummy)
     | setBnds f (lp,i,GLP_LO x) = withval4 f (lp,i,glpLo,x,dummy)
     | setBnds f (lp,i,GLP_UP x) = withval4 f (lp,i,glpUp,dummy,x)
     | setBnds f (lp,i,GLP_DB(x,y)) = withval4 f (lp,i,glpDb,x,y)
     | setBnds f (lp,i,GLP_FX x) = withval4 f (lp,i,glpFx,x,dummy)
in

val setRowBnds' = _import "glp_set_row_bnds" : lp' * int * int * real * real -> unit;
val setRowBnds = setBnds setRowBnds'

val setColBnds' = _import "glp_set_col_bnds" : lp' * int * int * real * real -> unit;
val setColBnds = setBnds setColBnds'

end (* local *) 

val setObjName' = _import "glp_set_obj_name" : lp' * string -> unit;
fun setObjName (lp,s) =
    let in
(*        printl ("setting obj name: " ^ s); *)
       withval1 setObjName' (lp,S.nullTerminate s)
    end

val setObjDir' = _import "glp_set_obj_dir" : lp' * int -> unit;
fun setObjDir (lp,GLP_MIN) = F.withValue(lp,(fn lp' => setObjDir'(lp',glpMin)))
  | setObjDir (lp,GLP_MAX) = F.withValue(lp,(fn lp' => setObjDir'(lp',glpMax)))

val setObjCoef' = _import "glp_set_obj_coef" : lp' * int * real -> unit;
val setObjCoef = withval2 setObjCoef'

val loadMatrix' = _import "glp_load_matrix" : lp' * int * int array * int array * real array -> unit;
val loadMatrix = withval4 loadMatrix'

val findRow' = _import "glp_find_row" : lp' * string -> int;
fun findRow (lp,s) = 
    let
       val n = withval1 findRow' (lp, S.nullTerminate s)
    in
       if n = 0 then NONE else SOME n
    end

val findCol' = _import "glp_find_col" : lp' * string -> int;
fun findCol (lp,s) =
    let
       val n = withval1 findCol' (lp, S.nullTerminate s) 
    in
       if n = 0 then NONE else SOME n
    end

val simplex' = _import "glp_simplex" : lp' * ptr -> int;

fun simplex (lp,{msgLev,presolve,itLim,tmLim}) = 
    let in
       setParams(msgLevelToInt msgLev,presolveToInt presolve,itLim,tmLim);
       case withval1 simplex' (lp,paramPtr) of (* null pointer for default solver options *)
          0 => () (* Success *)
	| n => raise Fail ("simplex: exit code = " ^ Int.toString n)
    end

val exact' = _import "lpx_exact" : lp' -> int;
fun exact lp = 
    case withval0 exact' lp of (* null pointer for default solver options *)
       0 => () (* Success *)
     | n => raise Fail ("exact: exit code = " ^ Int.toString n)

val getStatus' = _import "glp_get_status" : lp' -> int;
fun getSolnStatus lp = 
    let
       val n = withval0 getStatus' lp 
    in
       if n = glpOpt then GLP_OPT
       else if n = glpFeas then GLP_FEAS
       else if n = glpInfeas then GLP_INFEAS
       else if n = glpNofeas then GLP_NOFEAS
       else if n = glpUnbnd then GLP_UNBND
       else if n = glpUndef then GLP_UNDEF
       else raise Fail ("not a known solution status: " ^ Int.toString n)
    end

val getObjVal' = _import "glp_get_obj_val" : lp' -> real;
val getObjVal = withval0 getObjVal'

val getRowPrim' = _import "glp_get_row_prim" : lp' * int -> real;
val getRowPrim = withval1 getRowPrim'

val getRowDual' = _import "glp_get_row_dual" : lp' * int -> real;
val getRowDual = withval1 getRowDual'

val getColPrim' = _import "glp_get_col_prim" : lp' * int -> real;
val getColPrim = withval1 getColPrim'

val getColDual' = _import "glp_get_col_dual" : lp' * int -> real;
val getColDual = withval1 getColDual'

(* return is 0 if success, nonzero otherwise. *)
val writeCpxlp' = _import "_glp_lpx_write_cpxlp" : lp' * string -> int;
fun writeCpxlp (lp,s) = 
    let in
       case withval1 writeCpxlp' (lp, S.nullTerminate s) of
          0 => () (* Success *)
        | n => raise Fail ("writeCpxlp: error code = " ^ Int.toString n)
    end

end
