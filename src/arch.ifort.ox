# folder for object files
OBJS=obj

#ifort (arch.irfort.oxford)
F90=ifort
# debug
#FFLAGS=-cpp -g -pg -CB -check all -traceback -heap-arrays
#LFLAGS=-cpp -g -pg -CB -check all -traceback -heap-arrays
# optimised compiling, but keeping code resonably portable
#FFLAGS=-cpp -pg -CB -heap-arrays -O3
#LFLAGS=-cpp -pg -CB -heap-arrays -O3
# maximum optimisation for current machine - only do this
# if you are intending to run the code on a machine with the
# same processor as the machine you are compiling on
FFLAGS=-cpp -pg -CB -heap-arrays -O3 -xHOST
LFLAGS=-cpp -pg -CB -heap-arrays -O3 -xHOST
AUXFLAGS=-module