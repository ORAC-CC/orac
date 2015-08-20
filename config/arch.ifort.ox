# folder for object files
OBJS=obj

AUXFLAGS=-module $(OBJS)

# Define fortran 90 compiler
F90=ifort

# Define C compiler
CC=gcc

# optimised compiling, but keeping code resonably portable
# ifort
FFLAGS=-cpp -O2 -heap-arrays
LFLAGS=-cpp -O2 -heap-arrays
CFLAGS=-O2

# maximum optimisation for current machine - only do this
# if you are intending to run the code on a machine with the
# same processor as the machine you are compiling on
# ifort
#FFLAGS=-cpp -O3 -heap-arrays -xHOST
#LFLAGS=-cpp -O3 -heap-arrays -xHOST
#CFLAGS=-O3 -march=native

FFLAGS+= -openmp
LFLAGS+= -openmp
