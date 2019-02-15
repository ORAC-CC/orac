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

export FFLAGS="${FFLAGS} -w -cpp -I. -ffree-line-length-0 -fcheck=bounds"
export F77FLAGS="${FFLAGS}"
export CFLAGS="${CFLAGS} -w -cpp -I."

# Can't use -j ${CPU_COUNT} as dependencies are incomplete
make

mv *.a "${PREFIX}/lib"
mv *.mod "${PREFIX}/include"
mv AHI "${PREFIX}/bin"
