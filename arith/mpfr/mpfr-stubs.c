
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "mpfr.h"

#define BASE10 10
#define MAX_MPFR_LENGTH 200
#define MAX_EXP_LENGTH 16

char* mlton_mpfr_to_string(mpfr_t* x) {
  char *bp;
  size_t size;
  FILE *stream;
  stream = (FILE*) open_memstream (&bp, &size);
  mpfr_out_str(stream,10,0,*x, GMP_RNDN);
  fflush (stream);
  char* s = (char*) mlton_mk_str(strlen(bp) + 1);
  strcpy(s, bp);
  fclose (stream);
  free(bp);
  return s;
}

mpfr_t* mlton_mpfr_new() {
  mpfr_t* i = malloc(sizeof(mpfr_t));
  mpfr_init(*i);
  return i;
}

void mlton_mpfr_free(mpfr_t* i) {
  mpfr_clear(*i);
  free(i);
  return;
}

void mlton_mpfr_set_str(mpfr_t* i, char* s) {
  mpfr_set_str(*i, s, 10, GMP_RNDN);
  return;
}

int mlton_mpfr_add(mpfr_t* z, mpfr_t* x, mpfr_t* y) {
  return mpfr_add(*z, *x, *y, GMP_RNDN);
}

int mlton_mpfr_sub(mpfr_t* z, mpfr_t* x, mpfr_t* y) {
  return mpfr_sub(*z, *x, *y, GMP_RNDN);
}

int mlton_mpfr_mul(mpfr_t* z, mpfr_t* x, mpfr_t* y) {
  return mpfr_mul(*z, *x, *y, GMP_RNDN);
}

int mlton_mpfr_div(mpfr_t* z, mpfr_t* x, mpfr_t* y) {
  return mpfr_div(*z, *x, *y, GMP_RNDN);
}

int mlton_mpfr_neg(mpfr_t* z, mpfr_t* x) {
  return mpfr_neg(*z, *x, GMP_RNDN);
}

int mlton_mpfr_sqr(mpfr_t* z, mpfr_t* x) {
  return mpfr_sqr(*z, *x, GMP_RNDN);
}

int mlton_mpfr_sqrt(mpfr_t* z, mpfr_t* x) {
  return mpfr_sqrt(*z, *x, GMP_RNDN);
}

int mlton_mpfr_abs(mpfr_t* z, mpfr_t* x) {
  return mpfr_abs(*z, *x, GMP_RNDN);
}

int mlton_mpfr_atan(mpfr_t* z, mpfr_t* x) {
  return mpfr_atan(*z, *x, GMP_RNDN);
}

int mlton_mpfr_inf_p(mpfr_t* z) {
  return mpfr_inf_p(*z);
}

int mlton_mpfr_number_p(mpfr_t* z) {
  return mpfr_number_p(*z);
}

int mlton_mpfr_zero_p(mpfr_t* z) {
  return mpfr_zero_p(*z);
}

int mlton_mpfr_nan_p(mpfr_t* z) {
  return mpfr_nan_p(*z);
}

int mlton_mpfr_cmp(mpfr_t* x, mpfr_t* y) {
  return mpfr_cmp(*x, *y);
}

double mlton_mpfr_get_d(mpfr_t* x) {
  return mpfr_get_d(*x, GMP_RNDN);
}

double mlton_mpfr_set_d(mpfr_t* x, double r) {
  return mpfr_set_d(*x, r, GMP_RNDN);
}

/* int main1() { */
/*   mpfr_t x; */
/*   mpfr_init(x); */
/* } */
