# Directory for object files
OBJS = obj

# Define Fortran 77 compiler
F77 = gfortran

# Define Fortran 90 compiler
F90 = gfortran

# Define C compiler
CC = gcc

# Define C++ compiler
CXX = g++

# Set Fortran 77 compiler flags
FF77LAGS  = -O3 -cpp -ffree-line-length-0 -g
# Uncomment if extra warnings are required.  Take out -Werror if that annoys you.
#FF77LAGS += -Wall -Werror -Wno-conversion -Wno-uninitialized -Wno-unused-dummy-argument -Wtabs

# Set Fortran 90 compiler flags
FFLAGS    = -O3 -cpp -ffree-line-length-0 -g
# Uncomment if OpenMP support is desired. Highly recommented.
FFLAGS   += -fopenmp
# Uncomment if extra warnings are required.  Take out -Werror if that annoys you.
#FFLAGS   += -Wall -Werror -Wno-conversion -Wno-uninitialized -Wno-unused-dummy-argument -Wtabs

LFLAGS = -fopenmp

# Set C compiler  flags
CFLAGS = -O3 -g -Wall -Werror

AUXFLAGS = -J $(OBJS)
