# Directory for object files
OBJS = obj

# Define Fortran 77 compiler
F90 = ifort

# Define Fortran 90 compiler
F90 = ifort

# Define C compiler
CC = gcc

# Define C++ compiler
CXX = g++

# Set Fortran 77 compiler flags
FF77LAGS = -O3 -cpp -g

# Set Fortran 90 compiler flags
FFLAGS   = -O3 -cpp -g
# Uncomment if OpenMP support is desired
FFLAGS  += -openmp

LFLAGS = -openmp

# Set C compiler  flags
CFLAGS = -O3 -g

AUXFLAGS = -module $(OBJS)
