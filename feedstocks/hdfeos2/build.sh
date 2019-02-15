#!/bin/sh

set -e

export FC="${PREFIX}/bin/h4fc"
export F77="${FC}"
export F90="${FC}"
export CC="${PREFIX}/bin/h4cc"
export DYLD_FALLBACK_LIBRARY_PATH="${PREFIX}/lib"

# Don't use OpenMP on JASMIN. Do use it everywhere else.
#export FFLAGS="${FFLAGS//fopenmp}"
export CFLAGS="${CFLAGS} -fopenmp"
export CXXFLAGS="${CXXFLAGS} -fopenmp"

# Don't define flags as they're already in h4cc and h4fc
export CFLAGS="${CFLAGS} -Df2cFortran"
export CXXFLAGS="${CXXFLAGS} -Df2cFortran"
export CPPFLAGS=""
export LDFLAGS=""
export LD_LIBRARY_PATH="${PREFIX}/lib:${LD_LIBRARY_PATH}"

chmod -R 0700 .
autoreconf -vfi

./configure --prefix="${PREFIX}" \
            --with-hdf4="${PREFIX}" \
            --with-zlib="${PREFIX}" \
            --with-szlib="${PREFIX}" \
            --with-jpeg="${PREFIX}" \
            --enable-install-include

# Don't use -j as the dependencies are mightly buggered
make
if [ $(make check) -ne 0 ]; then
    # This tends to fail the first time with a BlockingIOError. If you do it
    # again, it magically works!
    make check
fi
make install

pushd include
make install-includeHEADERS
popd
