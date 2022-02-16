#!/bin/bash

set -e

# Stop the build environment leaking into h4cc
export FC=$(basename ${GFORTRAN})
export CC=$(basename ${GCC})
export CXX=$(basename ${GXX})

autoreconf -vfi

# Build static library the shared isn't compatible with Fortran and I haven't
# been able to build both.
./configure --prefix="${PREFIX}" \
            --enable-linux-lfs \
            --enable-silent-rules \
            --with-ssl \
            --with-szlib="${PREFIX}" \
            --with-zlib="${PREFIX}" \
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

# People usually Google these.
rm -rf ${PREFIX}/examples
