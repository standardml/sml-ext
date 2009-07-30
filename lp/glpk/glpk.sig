
(**
 *  SML interface to GLPK, the GNU linear programming library.  
 *  The problem must be stated in "Standard Form" (see GLPK manual, p.4).  
 * 
 *  Minimize c^T x
 *  s.t.  A x = b
 *        lbs <= x <= ubs
 *)
signature GLPK =
sig

type lp 

datatype var = GLP_FR                  (* free *)
             | GLP_LO of real          (* bounded below *)
             | GLP_UP of real          (* bounded above *)
             | GLP_DB of real * real   (* double bounded *)
             | GLP_FX of real          (* fixed *)

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

type params = {msgLev: msg_lev,     (* terminal output, default GLP_MSG_ALL *)
	       presolve: presolve , (* use presolver, default GLP_OFF *)
	       itLim: int,          (* max iterations, default INT_MAX *)
	       tmLim: int}          (* milliseconds, default INT_MAX *)

val defaultParams: params

val createProb: unit -> lp 
val addRows: lp * int -> unit
val addCols: lp * int -> unit
val setProbName: lp * string -> unit
val setRowName: lp * int * string -> unit
val setColName: lp * int * string -> unit
val setRowBnds: lp * int * var -> unit
val setColBnds: lp * int * var -> unit
val setObjName: lp * string -> unit
val setObjDir: lp * dir -> unit
val setObjCoef: lp * int * real -> unit
val loadMatrix: lp * int * int array * int array * real array -> unit

val simplex: lp * params -> unit
val exact: lp -> unit 

val getSolnStatus: lp -> soln_status
val getObjVal: lp -> real
val getRowPrim: lp * int -> real
val getRowDual: lp * int -> real
val getColPrim: lp * int -> real
val getColDual: lp * int -> real

val writeCpxlp:  lp * string -> unit

end
