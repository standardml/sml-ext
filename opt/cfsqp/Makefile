
GENERATED += cfsqp-sml.h cfsqp-stubs.o 

include ../../Makefile

cfsqp-sml.h : cfsqp.mlb $(SML)
	$(MLTON) -export-header $@ -stop tc $<

cfsqp-stubs.o : cfsqp-stubs.c cfsqp-sml.h
	gcc -I $(CFSQP_INCLUDE_DIR) -c -o $@ $<

OPTS += $(CFSQP_OPT)

OBJS += $(CFSQP_OBJ) 



