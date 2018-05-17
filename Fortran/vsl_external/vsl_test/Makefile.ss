FFLAGS = -O2 -g -check bounds -traceback -fpe0 
MKL_INC =  -I$(MKLROOT)/include/intel64/lp64 -I$(MKLROOT)/include
MKL_LIB =  $(MKLROOT)/lib/intel64/libmkl_lapack95_lp64.a -L$(MKLROOT)/lib/intel64 -lmkl_intel_lp64 -lmkl_core -lmkl_sequential -lpthread -lm
INCLUDES = -I$(PROJECT_ROOT) $(MKL_INC)
LIBRARIES =  $(MKL_LIB)
