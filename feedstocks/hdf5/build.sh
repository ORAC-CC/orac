#!/bin/bash

#set -e

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
export CXXFLAGS="${CXXFLAGS}"
export CPPFLAGS="${CPPFLAGS} -I${PREFIX}/include"
export LDFLAGS="${LDFLAGS} -L${PREFIX}/lib"
export LD_LIBRARY_PATH="${PREFIX}/lib:${LD_LIBRARY_PATH}"

export LIBRARY_PATH="${PREFIX}/lib"

autoreconf -vfi

./configure --prefix="${PREFIX}" \
            --with-pic \
            --host="${HOST}" \
            --build="${BUILD}" \
            --enable-linux-lfs \
            --with-zlib="${PREFIX}" \
            --with-szlib="${PREFIX}" \
            --with-pthread=yes  \
            --enable-cxx \
            --enable-fortran \
            --enable-fortran2003 \
            --with-default-plugindir="${PREFIX}/lib/hdf5/plugin" \
            --enable-threadsafe \
            --enable-build-mode=production \
            --enable-unsupported \
            --enable-using-memchecker \
            --enable-clear-file-buffers \
            --with-ssl

make -j ${CPU_COUNT}
if [ $(make check) -ne 0 ]; then
    # This tends to fail the first time with a BlockingIOError. If you do it
    # again, it magically works!
    make check
fi
make install -j ${CPU_COUNT}
make check-install

rm -rf ${PREFIX}/share/hdf5_examples
