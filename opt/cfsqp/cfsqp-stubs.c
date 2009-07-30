
#include "stdio.h"
#include "cfsqpusr.h" 
#include "cfsqp-sml.h"

/* -------------------------------------------------------------------------- */
/*  Util                                                                      */
/* -------------------------------------------------------------------------- */

void print_arr(int n,double* arr){
  printf("[");
  int i;
  for(i=0;i<n-1;i++){
    printf("%f,",arr[i]);
  }
  printf("%f]\n",arr[n-1]);
}

/* -------------------------------------------------------------------------- */
/*  CFSQP                                                                     */
/* -------------------------------------------------------------------------- */

/* call the objective function (see the cfsqp manual, p. 27) */
void obj_fun(int nparam,     // (in) dimension of x
             int j,          // (in) number of the obj function to be computed.  We always have a single obj fun, so this is fixed at 1.
             double* xs,     // (in) evaluate at this point
             double* result, // (out) pointer to the value of the jth obj fun at x
             void* dummy)    // (inout) not used 
{
  int i;
  for(i=0;i<nparam;i++){
    mlton_cfsqp_set_arg(i,(Real64) xs[i]);
  }
  *result = (double) mlton_cfsqp_obj_fun();
  return;
}

/* call the objective function (see the cfsqp manual, p. 27) */
void constr_fun(int nparam,     // (in) dimension of x
                int j,          // (in) number of the constr function to be computed.  
                double* xs,     // (in) evaluate at this point
                double* result, // (out) pointer to the value of the jth obj fun at x
                void* dummy)    // (inout) not used 
{
  int i;
  for(i=0;i<nparam;i++){
    mlton_cfsqp_set_arg(i,(Real64) xs[i]);
  }
  *result = (double) mlton_cfsqp_constr_fun((Int32) j);
  return;
}

Real64 cfsqp_minimize(Int32 numargs,
                      Int32 nconstrs,
                      Pointer xmin_,   // lower bounds
                      Pointer xmax_,   // upper bounds
                      Pointer x_,  // initial guess
                      int verbose) 
{
  //printf("minimizing c...\n");
  double* xmin0 = (double*) xmin_;
  double* xmax0 = (double*) xmax_;
  double* x0 = (double*) x_;

  /* There are about 30 arguments to the primitive csfqp function.
     We only use a few. We keep the names from the manual, p. 17. */
  int nparam = (int) numargs;   // dimension of x
  int nineqn = (int) nconstrs;  // number of nonlinear inequality constraints
  double xmin[numargs];
  double xmax[numargs];
  double x[numargs];
  int i;
  for(i=0;i<numargs;i++){
    xmin[i] = xmin0[i];
    xmax[i] = xmax0[i];
    x[i] = x0[i];
  }

  int nineq = nineqn;            // total number of ineq constrs.  All constraints are nonlinear, so this is equal to nineqn
  int inform = 0;                // (out) return code
  int nf = 1;                    // number of obj functions
  int mode = 100;                // C=1, B=0, A=0. see manual, p.18
  int iprint = verbose;          // how much status output (verbosity, 0 = low, 3 = high)
  int miter = 500;               // max number of iterations
  double bigbnd=1.e10;           // a big number
  double eps=1.0e-8;             // a small number
  double udelta=0.0;             // p.21, should be set to 0.0 if user has no idea how to choose it
  double epsneq = 0.0;           // XXX I have no idea what this does

  int nfsr = 0;                  // we don't use sequentially related functions
  int ncsrl = 0;                 // "
  int ncsrn = 0;                 // "
  int mesh_pts[1] = {0};         // "
  int neqn = 0;                  // we don't use equality constraints
  int neq = 0;                   // "
  void *cd = NULL;               // not used, it's for tricky info passing between functions during optimization

  // Arrays for cfsqp to work
  double *f = (double*) calloc(nf,sizeof(double));
  double *g = (double*) calloc(nineq+neq,sizeof(double));
  double* lambda = (double*) calloc(nineq+neq+nf+nparam,sizeof(double));

  //printf("starting cfsqp...\n");

  cfsqp(nparam,
        nf,
        nfsr,
        nineqn,
        nineq,
        neqn,
        neq,
        ncsrl,
        ncsrn,
        mesh_pts,
        mode,
        iprint,
        miter,
        &inform,
        bigbnd,
        eps,
        epsneq,
        udelta,
        xmin,                   // lower bounds
        xmax,                   // upper bounds
        x,                      // initial guess
        f,
        g,
        lambda,
        obj_fun,                // our objective fun
        constr_fun,             // our constrint funs
        grobfd,                 // from cfsqpusr.h
        grcnfd,                 // from cfsqpusr.h
        cd);                    // dummy
  
  //printf("done with cfsqp...\n");

  double r;  // obj_fun puts the value of the obj fun at the optimal point in r

  obj_fun(nparam,0,x,&r,cd);

  //printf("r: %f\n",r);
  //print_arr(numargs,x);

  // free storage
  free(f);
  free(g);
  free(lambda);

  // flush printing buffer so errors and warnings
  // are printed immediately

  //printf("finished c...\n");
  fflush(stdout);
  fflush(stderr);

  return (Real64) r;

}
