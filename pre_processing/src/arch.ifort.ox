# folder for object files
OBJS=obj

# Define fortran 90 compiler 
F90=ifort
#F90=gfortran

# Define C compiler
CC=gcc

#set compiler flags
# ifort debugging flags
FFLAGS=-g -check all -heap-arrays -traceback
LFLAGS=-g -check all -heap-arrays -traceback
# gcc debugging flags for C code
CFLAGS=-g -Wall
# gfortran debugging flags
#FFLAGS=-g -Wall -xf95-cpp-input -fbacktrace
#LFLAGS=-g -Wall -fbacktrace

# optimised compiling, but keeping code resonably portable
# ifort
#FFLAGS=-O3 -heap-arrays
#LFLAGS=-O3 -heap-arrays
# gcc for C code
#CFLAGS=-O3

AUXFLAGS=-module $(OBJS)

# maximum optimisation for current machine - only do this
# if you are intending to run the code on a machine with the
# same processor as the machine you are compiling on
# ifort
#FFLAGS=-cpp -pg -CB -heap-arrays -O3 -xHOST
#LFLAGS=-cpp -pg -CB -heap-arrays -O3 -xHOST
# gcc for C code
#CFLAGS=-O3 -march=native