
signature QSOPT_STUBS =
sig

type mpq_t

type mpq_QSprob 

type mpq_QSdata

datatype status = QS_LP_OPTIMAL
                | QS_LP_INFEASIBLE
                | QS_LP_UNBOUNDED
                | QS_LP_UNSOLVED
                | QS_LP_MODIFIED

datatype simplex = Primal
                 | Dual

val mpq_init: mpq_t -> unit

val QSexact_set_precision: int -> unit

val mpq_QSget_rowcount: mpq_QSprob -> int

val mpq_QSget_colcount: mpq_QSprob -> int

val QSexact_solver: mpq_QSdata * mpq_t option * mpq_t option * simplex -> status 

val mpq_QSget_status: mpq_QSdata * status -> unit

val mpq_QSget_objval: mpq_QSdata -> mpq_t

val mpq_QSfree_prob: mpq_t -> unit

val mpq_clear: mpq_t -> unit

val mpq_QSload_prob: string * int * int * 

end
