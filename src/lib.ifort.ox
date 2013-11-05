#ifort
LIBBASE=/home/jupiter/eodg/gthomas/orac/orac_for_cci/fortran_preprocessing/libs
NCDFLIB=$(LIBBASE)/netcdf/lib
NCDFINCLUDE=$(LIBBASE)/netcdf/include


#Set up libraries and includes
LIBS=-L$(NCDFLIB) -lnetcdf -lnetcdff -lgomp
INC=-I./  -I$(NCDFINCLUDE)
