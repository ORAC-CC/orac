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

# Turn off warnings
export CFLAGS="${CFLAGS} -w"
export CXXLAGS="${CXXFLAGS} -w"

mkdir -vp ${PREFIX}/bin

autoreconf -vfi
./configure --prefix="${PREFIX}"

make -j${CPU_COUNT}
make check
make install -j${CPU_COUNT}
