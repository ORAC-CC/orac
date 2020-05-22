#!/bin/bash

set -e

# Ensure the build path doesn't leak into our build
export FC=$(basename ${GFORTRAN})
export F77="${FC}"
export F90="${FC}"
export CC=$(basename ${GCC})
export CXX=$(basename ${GXX})

# Don't use OpenMP on JASMIN. Do use it everywhere else.
#export FFLAGS="${FFLAGS//-fopenmp}"
#export INC="${CPPFLAGS}"
export CFLAGS="${CFLAGS} -fopenmp"
export CXXFLAGS="${CXXFLAGS} -fopenmp"
export INC="${CPPFLAGS} -DINCLUDE_RTTOV_OPENMP"

export FFLAGS="${FFLAGS} -w -cpp -ffree-line-length-0"
export F77FLAGS="${FFLAGS}"
export CFLAGS="${CFLAGS} -w"
export CXXCFLAGS="${CXXFLAGS} -w"
export OBJS="obj"
export AUXFLAGS="-J ${OBJS}"
export FLEXFLAGS=
export BISONFLAGS=

# Replicate the definitions of config/lib.inc
export LIBS="${LDFLAGS} -L${PREFIX}/lib -lemosR64 -lemos -lfftw3 -lhdfeos -lGctp"
export LIBS="${LIBS} -lemosR64 -lemos -lfftw3 -lhdfeos -lGctp"
export LIBS="${LIBS} -leccodes_f90 -leccodes -lmfhdf -ldf -lnetcdff -lnetcdf"
export LIBS="${LIBS} -lrttov12_coef_io -lrttov12_emis_atlas -lrttov12_hdf -lrttov12_parallel -lrttov12_main -lrttov12_other"
export LIBS="${LIBS} -lhdf5 -lhdf5_fortran -lhdf5_hl -lhdf5hl_fortran"
export LIBS="${LIBS} -lsz -ljpeg -lm -lz -lstdc++ -lblas -llapack -lgomp"
export LIBS="${LIBS} -lepr_api -lnr -lEd3Fu_201212 -lhimawari_util -lseviri_util"
export INC="${INC} -I./ -I${PREFIX}/include -I${PREFIX}/mod"
export INC="${INC} -DINCLUDE_ATSR_SUPPORT -DINCLUDE_NR -DINCLUDE_FU_LIOU_SUPPORT"
export INC="${INC} -DINCLUDE_HIMAWARI_SUPPORT -DINCLUDE_SEVIRI_SUPPORT"
export CINC="-I./ -I${PREFIX}/include -DINCLUDE_ATSR_SUPPORT"

if [ ! -d common/obj ]; then
    mkdir common/obj
    mkdir pre_processing/obj
    mkdir src/obj
    mkdir post_processing/obj
    mkdir derived_products/broadband_fluxes/obj
    mkdir derived_products/broadband_fluxes/obj/bugsrad
    mkdir derived_products/broadband_fluxes/obj/fu_liou
fi

make -j ${CPU_COUNT}

find . -name "*.a" -exec mv {} ${PREFIX}/lib \;
find . -name "*.mod" -exec mv {} ${PREFIX}/include \;
mv common/rttov_version ${PREFIX}/bin
mv pre_processing/orac_preproc ${PREFIX}/bin
mv src/orac ${PREFIX}/bin
mv post_processing/orac_postproc ${PREFIX}/bin

# Add configuration files, as the Python scripts use them to load libraries
mkdir -p ${PREFIX}/share/orac
cp config/arch.gfortran.inc ${PREFIX}/share/orac
cp config/lib.inc ${PREFIX}/share/orac

# Install the python scripts; user will need to override local_defaults.py
cd tools
${PYTHON} setup.py install

# Add the [de]activate scripts for environment variables
for CHANGE in "activate" "deactivate"; do
    mkdir -p "${PREFIX}/etc/conda/${CHANGE}.d"
    cp "${RECIPE_DIR}/${CHANGE}.sh" "${PREFIX}/etc/conda/${CHANGE}.d/${CHANGE}-${PKG_NAME}.sh"
done
