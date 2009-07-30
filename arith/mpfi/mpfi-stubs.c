
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "mpfr.h"
#include "mpfi.h"

#define BASE10 10
#define MAX_IVAL_LENGTH 200
#define MAX_EXP_LENGTH 12 /* This is an overestimate. The max exp is 2^11 for 64 bit floats */

mpfi_t* mlton_mpfi_new() {
  mpfi_t* i = malloc(sizeof(mpfi_t));
  mpfi_init(*i);
  return i;
}

void mlton_mpfi_free(mpfi_t* i) {
  mpfi_clear(*i);
  free(i);
}

char* mlton_mpfi_to_string(mpfi_t* x) {
  char *bp;
  size_t size;
  FILE *stream;
  stream = (FILE*) open_memstream (&bp, &size);
  mpfi_out_str(stream,10,0,*x);
  fflush (stream);
  char* s = (char*) mlton_mk_str(strlen(bp) + 1);
  strcpy(s, bp);
  fclose (stream);
  free(bp);
  return s;
}

void mlton_mpfi_set_str(mpfi_t* i, char* s) {
  mpfi_set_str(*i, s, 10);
}

int mlton_mpfi_add(mpfi_t* z, mpfi_t* x, mpfi_t* y) {
  return mpfi_add(*z, *x, *y);
}

int mlton_mpfi_sub(mpfi_t* z, mpfi_t* x, mpfi_t* y) {
  return mpfi_sub(*z, *x, *y);
}

int mlton_mpfi_mul(mpfi_t* z, mpfi_t* x, mpfi_t* y) {
  return mpfi_mul(*z, *x, *y);
}

int mlton_mpfi_div(mpfi_t* z, mpfi_t* x, mpfi_t* y) {
  return mpfi_div(*z, *x, *y);
}

int mlton_mpfi_neg(mpfi_t* z, mpfi_t* x) {
  return mpfi_neg(*z, *x);
}

int mlton_mpfi_sqr(mpfi_t* z, mpfi_t* x) {
  return mpfi_sqr(*z, *x);
}

int mlton_mpfi_sqrt(mpfi_t* z, mpfi_t* x) {
  return mpfi_sqrt(*z, *x);
}

int mlton_mpfi_inv(mpfi_t* z, mpfi_t* x) {
  return mpfi_inv(*z, *x);
}

int mlton_mpfi_is_zero(mpfi_t* z) {
  return mpfi_is_zero(*z);
}

int mlton_mpfi_abs(mpfi_t* z, mpfi_t* x) {
  return mpfi_abs(*z, *x);
}

int mlton_mpfi_atan(mpfi_t* z, mpfi_t* x) {
  return mpfi_atan(*z, *x);
}

int mlton_mpfi_cmp(mpfi_t* x, mpfi_t* y) {
  return mpfi_cmp(*x, *y);
}

int mlton_mpfi_bounded_p(mpfi_t* x) {
  return mpfi_bounded_p(*x);
}

int mlton_mpfi_interv_fr(mpfi_t* x, mpfr_t* l, mpfr_t* r) {
  return mpfi_interv_fr(*x, *l, *r);
}

mpfr_t* mlton_mpfi_get_left(mpfi_t* x) {
  mpfr_t* z = malloc(sizeof(mpfr_t));
  mpfr_init(*z);
  mpfi_get_left(*z, *x);
  return z;
}

mpfr_t* mlton_mpfi_get_right(mpfi_t* x) {
  mpfr_t* z = malloc(sizeof(mpfr_t));
  mpfr_init(*z);
  mpfi_get_right(*z, *x);
  return z;
}

/* int main() { */
/*   mpfi_t x; */
/*   mpfi_init(x); */
/*   mpfr_t l; */
/*   mpfr_init(l); */
/*   mpfi_set_str(x, "[1.7, 1.9]", 10); */
/*   mpfi_out_str(stdout, 10, 20, x); */
/*   printf("\n"); */
/*   fflush(stdout); */
/*   mpfi_get_left(l, x); */
/*   mpfr_out_str(stdout, 10, 20, l, GMP_RNDN); */
/*   printf("\n"); */
/*   int n = mpfi_cmp(x, x); */
/*   printf("cmp: %d\n", n); */
/*   fflush(stdout); */
/*   mpfr_clear(l); */
/*   mpfi_clear(x); */
/*   return 0; */
/* } */
