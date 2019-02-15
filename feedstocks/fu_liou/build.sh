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
export CFLAGS="${CFLAGS} -fopenmp"
export CXXFLAGS="${CXXFLAGS} -fopenmp"

# They use a non-standard name for the compilation flags
export F90COMP="${FFLAGS} -c -w"
export FCOMP="${F90COMP}"

mkdir "${PREFIX}/include"

cd Ed4_LaRC_FuLiou
make -j ${CPU_COUNT}

mv lib/*.a ${PREFIX}/lib
mv lib/mod/*.mod ${PREFIX}/include
