# C compiler and C compiler flags

#CC      = $(CC)
CCFLAGS = $(CFLAGS) -O2

# Fortran compiler and Fortan compiler flags (required for the Fortran
# interface)
F90      = $(FC)
F90FLAGS = $(FFLAGS) -O2

LINKS = -lm

# Uncomment to compile the Fortran interface and examples
OBJECTS          += seviri_util_f90.o
OPTIONAL_TARGETS += example_f90

# Include and lib directories for non standard locations required by SEVIRI_util
INCDIRS           += -I${LIB_DIR}/include
LIBDIRS           += -L${LIB_DIR}/lib
LINKS             += $(LDFLAGS) -lhdf5 -lnetcdf -ltiff -lm
