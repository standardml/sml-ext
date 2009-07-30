
#include "knitro.h"

int ktr_objgoal_minimize = KTR_OBJGOAL_MINIMIZE; 
int ktr_objgoal_maximize = KTR_OBJGOAL_MAXIMIZE; 
int ktr_rc_eval_err = KTR_RC_EVAL_ERR;
int ktr_objtype_general = KTR_OBJTYPE_GENERAL; 

double ktr_infbound = KTR_INFBOUND;

int ktr_contype_general = KTR_CONTYPE_GENERAL; 

int ktr_rc_optimal = KTR_RC_OPTIMAL;
int ktr_rc_evalfc = KTR_RC_EVALFC;
int ktr_rc_evalga = KTR_RC_EVALGA;
int ktr_rc_evalh = KTR_RC_EVALH;
int ktr_rc_evalhv = KTR_RC_EVALHV;

int ktr_param_gradopt = KTR_PARAM_GRADOPT;
int ktr_gradopt_exact = KTR_GRADOPT_EXACT;
int ktr_gradopt_forward = KTR_GRADOPT_FORWARD;
int ktr_gradopt_central = KTR_GRADOPT_CENTRAL;

int ktr_param_hessopt = KTR_PARAM_HESSOPT;
int ktr_hessopt_bfgs = KTR_HESSOPT_BFGS;
int ktr_hessopt_exact = KTR_HESSOPT_EXACT;

int ktr_param_outlev = KTR_PARAM_OUTLEV;
int ktr_outlev_none = KTR_OUTLEV_NONE;
int ktr_outlev_summary = KTR_OUTLEV_SUMMARY;
int ktr_outlev_iter_10 = KTR_OUTLEV_ITER_10;
int ktr_outlev_iter = KTR_OUTLEV_ITER;
int ktr_outlev_iter_verbose = KTR_OUTLEV_ITER_VERBOSE;
int ktr_outlev_iter_x = KTR_OUTLEV_ITER_X;
int ktr_outlev_all = KTR_OUTLEV_ALL;

int ktr_param_multistart = KTR_PARAM_MULTISTART;
int ktr_param_msmaxsolves = KTR_PARAM_MSMAXSOLVES;
int ktr_param_maxit = KTR_PARAM_MAXIT;

int mlton_knitro_free(KTR_context_ptr kc){
  return KTR_free(&kc);
}
