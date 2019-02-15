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

# Use CMake rather than the default file from the repository
rm -rf makefile

cmake -DCMAKE_INSTALL_PREFIX="${PREFIX}" \
      -DCMAKE_LIBRARY_PATH="${PREFIX}/lib" \
      -DCMAKE_BUILD_TYPE="Release" \
      -DBUILD_TESTS="yes"

make -j ${CPU_COUNT}
make test
make install -j ${CPU_COUNT}
