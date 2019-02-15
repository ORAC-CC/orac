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
export FFLAGS="${FFLAGS} -w -I${PREFIX}/include"
export F77FLAGS="${FFLAGS}"
export CFLAGS="${CFLAGS} -w"
export CXXFLAGS="${CXXFLAGS} -w"
export CPPFLAGS="${CPPFLAGS} -I${PREFIX}/include"
export LDFLAGS="${LDFLAGS} -L${PREFIX}/lib"
export LD_LIBRARY_PATH="${PREFIX}/lib:${LD_LIBRARY_PATH}"

autoreconf -vfi

# Build static, ignoring DAP as it doesn't work
./configure --prefix="${PREFIX}" \
            --enable-netcdf-4 \
            --enable-hdf4 \
            --enable-logging \
            --disable-dap \
            --disable-examples

make -j ${CPU_COUNT}
make check
make install -j ${CPU_COUNT}
