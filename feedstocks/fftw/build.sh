#!/usr/bin/env bash

set -e

# Ensure the build path doesn't leak into our build
export FC=$(basename ${GFORTRAN})
export F77="${FC}"
export F90="${FC}"
export CC=$(basename ${GCC})
export CXX=$(basename ${GXX})

# Don't use OpenMP on JASMIN. Do use it everywhere else.
#export FFLAGS="${FFLAGS//-fopenmp}"
#CONFIGURE="./configure --prefix=$PREFIX --with-pic --enable-shared --enable-threads"
export CFLAGS="${CFLAGS} -fopenmp"
export CXXFLAGS="${CXXFLAGS} -fopenmp"
CONFIGURE="./configure --prefix=$PREFIX --with-pic --enable-shared --enable-threads  --enable-openmp"

export CFLAGS="${CFLAGS} -I${PREFIX}/include -fomit-frame-pointer -fstrict-aliasing -ffast-math"
export LDFLAGS="${LDFLAGS} -L${PREFIX}/lib"

# (Note exported LDFLAGS and CFLAGS vars provided above.)
BUILD_CMD="make -j${CPU_COUNT}"
INSTALL_CMD="make install"

# Test suite
# tests are performed during building as they are not available in the
# installed package.
# Additional tests can be run with "make smallcheck" and "make bigcheck"
TEST_CMD="eval cd tests && make check-local && cd -"

#
# We build 3 different versions of fftw:
#
build_cases=(
    # single
    "$CONFIGURE --enable-sse2 --enable-avx"
    # double
    "$CONFIGURE --enable-float --enable-sse --enable-sse2 --enable-avx"
    # long double (SSE2 and AVX not supported)
    "$CONFIGURE --enable-long-double"
)

autoreconf -vfi
for config in "${build_cases[@]}"
do
    :
    $config
    ${BUILD_CMD}
    ${INSTALL_CMD}
    ${TEST_CMD}
done
