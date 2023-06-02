# C compiler and C compiler flags

#CC      = $(CC)
CCFLAGS = $(CFLAGS) -O3 -cpp -I. -fopenmp

# Fortran compiler and Fortan compiler flags (required for the Fortran
# interface)
F90      = $(FC)
F90FLAGS = $(FFLAGS) -O3 -I. -ffree-line-length-0 -cpp -fcheck=bounds -fopenmp

LINKS = $(LDFLAGS) -lm -fopenmp

# Include and lib directories for non standard locations required by hsd_reader
INCDIRS  += -I$(PREFIX)/include
LIBDIRS  += -L$(PREFIX)/lib
LINKS    += -lhdf5 -lhdf5_fortran -lhdf5_hl_fortran -lnetcdff -lnetcdf
