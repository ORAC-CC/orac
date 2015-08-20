# folder for object files
OBJS=obj

AUXFLAGS=-module $(OBJS)

# ifort debugging flags
F90=ifort

# Define C compiler
CC=gcc

FFLAGS=-cpp -heap-arrays -g -C -traceback -fstack-protector-all -assume protect_parens
LFLAGS=-cpp -heap-arrays -g -C -traceback -fstack-protector-all -assume protect_parens

# gfortran debugging flags
#F90=gfortran
#FFLAGS=-g -Wall -xf95-cpp-input -fbacktrace
#LFLAGS=-g -Wall -fbacktrace

# gcc debugging flags for C code
CFLAGS=-g -Wall

# add debugging library
LIBS+= -L$(HOME)/bin/Fortran -ldump
INC+= -I$(HOME)/bin/Fortran
