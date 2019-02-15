# C compiler and C compiler flags

CC      = $(basename $(GCC))
CCFLAGS = $(CFLAGS)

# Fortran compiler and Fortan compiler flags (required for the Fortran
# interface)
F90      = $(basename $(GFORTRAN))
F90FLAGS = $(FFLAGS)

LINKS = -lm

# Uncomment to compile the Fortran interface and examples
OBJECTS          += seviri_util_f90.o
OPTIONAL_TARGETS += example_f90

# Include and lib directories for non standard locations required by SEVIRI_util
INCDIRS          += -I${PREFIX}/include
LIBDIRS          += -L${PREFIX}/lib
LINKS            += -lhdf5 -lnetcdf -ltiff -lm
