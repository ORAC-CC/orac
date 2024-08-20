# Base directory for all libraries and include files taken from the
# environmental variable CONDA_PREFIX.

# Set up libraries and includes
LIBS = -L$(CONDA_PREFIX)/lib \
       -lemosR64 -lemos -lfftw3 \
       -lhdfeos -lGctp \
       -leccodes_f90 -leccodes \
       -lmfhdf -lmfhdf_fortran -lmfhdf_fcstub -lhdf \
       -lnetcdff \
       -lnetcdf \
       -lrttov13_coef_io -lrttov13_emis_atlas -lrttov13_hdf \
       -lrttov13_parallel -lrttov13_main -lrttov13_other \
       -lhdf5 -lhdf5_fortran -lhdf5_hl -lhdf5_hl_fortran \
       -lsz \
       -ljpeg -lm -lz -lstdc++ -lblas -llapack

INC = -I./ \
      -I$(CONDA_PREFIX)/include \
      -I$(CONDA_PREFIX)/mod

CINC = -I./ \
       -I$(CONDA_PREFIX)/include


# Configuration options

# Uncomment if you want to enable OpenMP for RTTOV computations.
INC  += -DINCLUDE_RTTOV_OPENMP

# Uncomment if ATSR support is desired.
LIBS += -lepr_api
INC  += -DINCLUDE_ATSR_SUPPORT
CINC += -DINCLUDE_ATSR_SUPPORT

# Uncomment if Numerical Recipes is available for cubic spline profile
# interpolation and bilinear LUT interpolation.
LIBS += -lnr
INC  += -DINCLUDE_NR

# Uncomment if Fu_Liou support is desired for broadband fluxes.
LIBS += -lEd3Fu_201212
INC  += -DINCLUDE_FU_LIOU_SUPPORT

# Uncomment if Himawari support is desired.
LIBS += -lhimawari_util
INC  += -DINCLUDE_HIMAWARI_SUPPORT

# Uncomment if SEVIRI support is desired.
LIBS += -lseviri_util
INC  += -DINCLUDE_SEVIRI_SUPPORT

# Uncomment if SEVIRI_ML support is desired.
# Please modify Python -lpython3.X to your Python version.
LIBS += -lsevann -lpython3.10
INC  += -DINCLUDE_SEVIRI_NEURALNET
