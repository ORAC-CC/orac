#Base directory for all libraries and include files
LIBBASE=/home/jupiter/eodg/gthomas/orac/orac_for_cci/fortran_preprocessing/libs

#Set path to hdf library and include directory. hdf4 has to be compiled with
#netcdf support turned OFF! otherwise you run into problems when using netcdf
#explicitly s.b.
HDFLIB=$(LIBBASE)/hdf4/lib
HDFINCLUDE=$(LIBBASE)/hdf4/include

HDF5LIB=$(LIBBASE)/hdf5/lib
HDF5INCLUDE=$(LIBBASE)/hdf5/include

SZLIB=/home/jupiter/eodg/gthomas/hdf/hdf4/szip-2.1/szip/lib
ZLIB=/home/jupiter/eodg/gthomas/hdf/hdf4/zlib-1.2.5/lib
JPEGLIB=/home/jupiter/eodg/gthomas/hdf/hdf4/jpeg-6b/lib

#Set path to HDF-EOS library. Should be compiled with the same version of the HDF4
#library specified above (HDFLIB)
EOSLIB=$(LIBBASE)/hdfeos/lib
EOSINCLUDE=$(LIBBASE)/hdfeos/include

#Set path to netcdf library and include directory.
NCDFLIB=$(LIBBASE)/netcdf/lib
NCDFINCLUDE=$(LIBBASE)/netcdf/include
#NCDFLIB=/home/jupiter/eodg2/povey/orac/libs/netcdf/lib
#NCDFINCLUDE=/home/jupiter/eodg2/povey/orac/libs/netcdf/include

#Setup GRIB API library and JASPER
GRIBLIB=$(LIBBASE)/grib/lib
GRIBINCLUDE=$(LIBBASE)/grib/include

JASPERLIB=$(LIBBASE)/jasper/lib
JASPERINCLUDE=$(LIBBASE)/jasper/include

#Setup RTTOV library
RTTOVLIB=$(LIBBASE)/rttov/lib
RTTOVINCLUDE=$(LIBBASE)/rttov/include
RTTOVMODULE=$(LIBBASE)/rttov/mod

#Setup BEAM library
BEAMLIB=$(LIBBASE)/epr_api/lib
BEAMINCLUDE=$(LIBBASE)/epr_api/include

OPENLIB=/home/jupiter/eodg2/povey/orac/libs

#Set up libraries and includes
LIBS= -L$(BEAMLIB) -lepr_api \
      -L$(GRIBLIB) -lgrib_api_f90 -lgrib_api_f77 -lgrib_api \
      -L$(JASPERLIB) -ljasper \
      -L$(NCDFLIB) -lnetcdff -lnetcdf \
      -L$(HDFLIB) -lmfhdf -ldf \
      -L$(JPEGLIB) -ljpeg \
      -L$(ZLIB) -lz \
      -L$(SZLIB) -lsz -lm \
      -L$(EOSLIB) -lhdfeos -lGctp \
      -L$(HDF5LIB) -lhdf5_fortran -lhdf5 \
      -L$(RTTOVLIB) -lrttov10.2.0_emis_atlas -lrttov10.2.0_main \
	            -lrttov10.2.0_coef_io -lrttov10.2.0_other \
      -L$(OPENLIB) -lopenjpeg

INC=-I./ -I$(GRIBINCLUDE) -I$(HDFINCLUDE) -I$(EOSINCLUDE) -I$(HDF5INCLUDE) \
	 -I$(NCDFINCLUDE) -I$(JASPERINCLUDE) -I$(RTTOVINCLUDE) -I$(RTTOVMODULE) \
	 -I$(BEAMINCLUDE)

CINC=-I./ -I$(BEAMINCLUDE)
