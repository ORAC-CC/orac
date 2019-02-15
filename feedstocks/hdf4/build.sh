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

# Build static library the shared isn't compatible with Fortran and I haven't
# been able to build both.
./configure --prefix="${PREFIX}" \
            --enable-linux-lfs \
            --enable-silent-rules \
            --with-ssl \
            --with-zlib="${PREFIX}" \
            --with-szlib="${PREFIX}" \
            --with-jpeg="${PREFIX}" \
            --disable-netcdf \
            --enable-fortran \
            --disable-shared

make -j ${CPU_COUNT}
make check
make install -j ${CPU_COUNT}
make installcheck

# Remove man pages.
rm -rf ${PREFIX}/share

# Avoid clashing names with netcdf.
mv ${PREFIX}/bin/ncdump ${PREFIX}/bin/h4_ncdump
mv ${PREFIX}/bin/ncgen ${PREFIX}/bin/h4_ncgen
