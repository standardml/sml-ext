
/* 
   MLton can't import #define macros.  Thus we need to force
   them to be actual ints here.
*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "glpk.h"

/* -------------------------------------------------------------------------- */
/*  GLPK macros                                                               */
/* -------------------------------------------------------------------------- */

// Variable types
int glp_fr = GLP_FR;
int glp_lo = GLP_LO;
int glp_up = GLP_UP;
int glp_db = GLP_DB;
int glp_fx = GLP_FX;

// Optimization direction
int glp_min = GLP_MIN;
int glp_max = GLP_MAX;

// Simplex return status
int glp_ebadb = GLP_EBADB;
int glp_esing = GLP_ESING;
int glp_econd = GLP_ECOND;
int glp_ebound = GLP_EBOUND;
int glp_efail = GLP_EFAIL;
int glp_eobjll = GLP_EOBJLL;
int glp_eobjul = GLP_EOBJUL;
int glp_eitlim = GLP_EITLIM;
int glp_etmlim = GLP_ETMLIM;
int glp_nopfs = GLP_ENOPFS;
int glp_nodfs = GLP_ENODFS;

// Solution status
int glp_opt = GLP_OPT;
int glp_feas = GLP_FEAS;
int glp_infeas = GLP_INFEAS;
int glp_nofeas = GLP_NOFEAS;
int glp_unbnd = GLP_UNBND;
int glp_undef = GLP_UNDEF;

// Terminal output
int glp_msg_off = GLP_MSG_OFF;
int glp_msg_err = GLP_MSG_ERR;
int glp_msg_on = GLP_MSG_ON;
int glp_msg_all = GLP_MSG_ALL;

// Presolver
int glp_on = GLP_ON;
int glp_off = GLP_OFF;

// Experimental exact arithmetic solver
int lpx_e_ok = LPX_E_OK;
int lpx_e_fault = LPX_E_FAULT;
int lpx_e_itlim = LPX_E_ITLIM;
int lpx_e_tmlim = LPX_E_TMLIM;

// Parameters to control simplex
glp_smcp params; // allocate on the global C heap
glp_smcp* param_ptr = &params;

void glp_set_params(int msg_lev, int presolve, int it_lim, int tm_lim){
  glp_init_smcp(&params);
  params.msg_lev = msg_lev;
  params.presolve = presolve;
  params.it_lim = it_lim;
  params.tm_lim = tm_lim;
  return;
}
