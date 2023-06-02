# Directory for object files
OBJS = obj

# Define Fortran 77 compiler
F77 = $(GFORTRAN)

# Define Fortran 90 compiler
F90 = $(GFORTRAN)

# Define C compiler
CC = $(GCC)

# Define C++ compiler
CXX = $(GXX)

# Linker already defined by conda environment

# Set Fortran 77 compiler flags
F77FLAGS  = -O3 -cpp -ffree-line-length-0 -g -fcheck=bounds -fallow-argument-mismatch
# Uncomment if extra warnings are required.  Take out -Werror if that annoys you.
#FF77LAGS += -Wall -Werror -Wno-conversion -Wno-uninitialized -Wno-unused-dummy-argument -Wtabs

# Set Fortran 90 compiler flags
FFLAGS    = -O3 -cpp -ffree-line-length-0 -g -fcheck=bounds -fallow-argument-mismatch
# Uncomment if OpenMP support is desired. Highly recommented.
FFLAGS   += -fopenmp
# Uncomment if extra warnings are required.  Take out -Werror if that annoys you.
#FFLAGS   += -Wall -Werror -Wno-conversion -Wno-uninitialized -Wno-unused-dummy-argument -Wtabs

LFLAGS = -fopenmp

# Set C compiler  flags
CFLAGS = -O3 -g
# Uncomment if extra warnings are required.
#CFLAGS += -Wall -Werror

AUXFLAGS = -J $(OBJS)

# Set Bison/Flex parser flags
FLEXFLAGS =
BISONFLAGS =
# Uncomment if debugging the parser
#FLEXFLAGS += -d
#BISONFLAGS += -t
