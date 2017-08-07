# Base directory for all libraries and include files taken from the
# environmental variable ORAC_LIBBASE.
LIBBASE = $(ORAC_LIBBASE)

# Separate base directory for Fortran 90 libraries since Fortran 90 libraries
# are compiler dependent.  If your C and Fortran libraries are all in one
# directory just set the environmental variable ORAC_LIBBASE_FORTRAN to the same
# as ORAC_LIBBASE.
LIBBASE_FORTRAN = $(ORAC_LIBBASE_FORTRAN)

# EMOS
EMOSLIB = $(LIBBASE_FORTRAN)/emos

# HDF-EOS
EOSLIB = $(LIBBASE_FORTRAN)/hdfeos/lib
EOSINCLUDE = $(LIBBASE_FORTRAN)/hdfeos/include

# EPR_API
EPR_APILIB = $(LIBBASE)/epr-api/lib
EPR_APIINCLUDE = $(LIBBASE)/epr-api/include

# Fu and Liou
FULIOULIB = $(LIBBASE_FORTRAN)/fuliou/lib
FULIOUINCLUDE = $(LIBBASE_FORTRAN)/fuliou/lib/mod

# GRIB API
GRIBLIB = $(LIBBASE_FORTRAN)/grib_api/lib
GRIBINCLUDE = $(LIBBASE_FORTRAN)/grib_api/include

# HDF4 has to be compiled without the HDF4 versions of NetCDF APIs.
HDFLIB = $(LIBBASE_FORTRAN)/hdf/lib
HDFINCLUDE = $(LIBBASE_FORTRAN)/hdf/include

# HDF5
HDF5LIB = $(LIBBASE_FORTRAN)/hdf5/lib
HDF5INCLUDE = $(LIBBASE_FORTRAN)/hdf5/include

# NetCDF
NCDFLIB = $(LIBBASE)/netcdf/lib
NCDFINCLUDE = $(LIBBASE)/netcdf/include
NCDF_FORTRAN_LIB = $(LIBBASE_FORTRAN)/netcdf-fortran/lib
NCDF_FORTRAN_INCLUDE = $(LIBBASE_FORTRAN)/netcdf-fortran/include

# RTTOV
RTTOVLIB = $(LIBBASE_FORTRAN)/rttov/lib
RTTOVINCLUDE = $(LIBBASE_FORTRAN)/rttov/include
RTTOVMODULE = $(LIBBASE_FORTRAN)/rttov/mod

# Himawari_HSD_Reader
HIMAWARI_HSD_READER_LIB = $(LIBBASE_FORTRAN)/Himawari_HSD_Reader
HIMAWARI_HSD_READER_INCLUDE = $(LIBBASE_FORTRAN)/Himawari_HSD_Reader

# seviri_native_util
SEVIRI_NATIVE_UTIL_LIB = $(LIBBASE_FORTRAN)/seviri_native_util
SEVIRI_NATIVE_UTIL_INCLUDE = $(LIBBASE_FORTRAN)/seviri_native_util

SZLIB = $(LIBBASE)/szip/lib

# Set up libraries and includes
LIBS = -L$(EMOSLIB) -lemosR64 \
       -L$(EOSLIB) -lhdfeos -lGctp \
       -L$(EPR_APILIB) -lepr_api \
       -L$(GRIBLIB) -lgrib_api_f77 -lgrib_api_f90 -lgrib_api \
       -L$(HDFLIB) -lmfhdf -ldf \
       -L$(NCDFLIB) -lnetcdf \
       -L$(NCDF_FORTRAN_LIB) -lnetcdff \
       -L$(RTTOVLIB) -lrttov12_coef_io -lrttov12_emis_atlas -lrttov12_hdf \
                     -lrttov12_parallel -lrttov12_main -lrttov12_other \
       -L$(HDF5LIB) -lhdf5 -lhdf5_fortran -lhdf5_hl -lhdf5hl_fortran \
       -L$(SZLIB) -lsz \
       -ljpeg -lm -lz -lstdc++

INC = -I./ \
      -I$(EOSINCLUDE) \
      -I$(EPR_APIINCLUDE) \
      -I$(GRIBINCLUDE) \
      -I$(HDFINCLUDE) \
      -I$(HDF5INCLUDE) \
      -I$(NCDFINCLUDE) \
      -I$(NCDF_FORTRAN_INCLUDE)  \
      -I$(RTTOVINCLUDE) \
      -I$(RTTOVMODULE)

CINC = -I./ -I$(EPR_APIINCLUDE)

# Uncomment if you want to enable OpenMP for RTTOV computations.
INC  += -DINCLUDE_RTTOV_OPENMP

# Uncomment if Fu_Liou support is desired for broadband fluxes.
#LIBS += -L$(FULIOULIB) -lEd3Fu_201212
#INC  += -I$(FULIOUINCLUDE) -DINCLUDE_FU_LIOU_SUPPORT

# Uncomment if Himawari support is desired.
#LIBS += -L$(HIMAWARI_HSD_READER_LIB) -lhimawari_util
#INC  += -I$(HIMAWARI_HSD_READER_INCLUDE) -DINCLUDE_HIMAWARI_SUPPORT

# Uncomment if SEVIRI support is desired.
#LIBS += -L$(SEVIRI_NATIVE_UTIL_LIB) -lseviri_native_util
#INC  += -I$(SEVIRI_NATIVE_UTIL_INCLUDE) -DINCLUDE_SEVIRI_SUPPORT
