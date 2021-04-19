# C compiler and C compiler flags

CC      = $(GCC)
CCFLAGS = $(CFLAGS)

# Fortran compiler and Fortan compiler flags (required for the Fortran
# interface)
F90      = $(GFORTRAN)
F90FLAGS = $(FFLAGS)

LINKS = $(LDFLAGS) -lm

# Include and lib directories for non standard locations required by SEVIRI_util
INCDIRS  += -I$(PREFIX)/include -I.
LIBDIRS  += -L$(PREFIX)/lib
LINKS    += -lhdf5 -lhdf5_fortran -lhdf5hl_fortran -lnetcdff -lnetcdf
