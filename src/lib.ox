# (lib.ifort.oxford)
LIBBASE=/home/jupiter/eodg/povey/orac/libs
NCDFLIB=$(LIBBASE)/netcdf/lib
NCDFINCLUDE=$(LIBBASE)/netcdf/include

#Set up libraries and includes
LIBS=-L$(NCDFLIB) -lnetcdf -lnetcdff -lgomp
INC=-I./ -I$(NCDFINCLUDE)

