# -*- Mode: Makefile -*-

default: nj

mlton:
	mlton -stop tc -default-ann 'allowFFI true' \
	  -mlb-path-map build/sml-ext.map build/sml-ext.mlb

nj:
	sml sources.cm

# ------------------------------------------------------------------------------
#  Edit the following to suit your FFI requirements 
# ------------------------------------------------------------------------------

# CFSQP

USE_CFSQP := false
CFSQP_INCLUDE_DIR := /usr/local/cfsqp
CFSQP_LIB_DIR := /usr/local/cfsqp

# KNITRO

USE_KNITRO := false
KNITRO_INCLUDE_DIR := /usr/local/knitro/include
KNITRO_LIB_DIR := /usr/local/knitro/lib
ZIENA_LICENSE := /home/sean/save/versioned/program-data/knitro/mac

# CPLEX

USE_CPLEX := false
CPLEX_INCLUDE_DIR := 
CPLEX_LIB_DIR := 

# GLPK

USE_GLPK := false
GLPK_INCLUDE_DIR := /sw/include
GLPK_LIB_DIR := /sw/lib

# MPFR

USE_MPFR := false
MPFR_INCLUDE_DIR := /usr/local/include
MPFR_LIB_DIR := /usr/local/lib

# MPFI

USE_MPFI := false
MPFI_INCLUDE_DIR := /usr/local/include
MPFI_LIB_DIR := /usr/local/lib

clean:
	-find . -name ".cm" -exec rm -rf {} \; 
