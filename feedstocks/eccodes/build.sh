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
export CFLAGS="${CFLAGS} -fopenmp"
export CXXFLAGS="${CXXFLAGS} -fopenmp"

# Update the build environment
export PATH="${PATH}:${BUILD_PREFIX}/${HOST}/sysroot/usr/lib"
export PYTHON=
export LDFLAGS="${LDFLAGS} -L${PREFIX}/lib -Wl,-rpath,${PREFIX}/lib"
export CFLAGS="${CFLAGS} -I${PREFIX}/include"

rm -rf build
mkdir build
cd build
cmake -DCMAKE_INSTALL_PREFIX"=${PREFIX}" \
      -DCMAKE_LIBRARY_PATH="${PREFIX}/lib" \
      -DBUILD_SHARED_LIBS="BOTH" \
      -DENABLE_JPG=1 \
      -DENABLE_NETCDF=1 \
      -DENABLE_PNG=1 \
      -DENABLE_PYTHON=0 \
      -DENABLE_FORTRAN=1 \
      -DENABLE_AEC=1 ..

# Get the right version of jasper
JASPER_VERSION=`$PREFIX/bin/jasper --version 2> /dev/null`
JASPER_SHORT_VERSION=${JASPER_VERSION%%.*}
sed -e "s#JASPER_VERSION_MAJOR.*#JASPER_VERSION_MAJOR ${JASPER_SHORT_VERSION}#" \
    -i eccodes_config.h

make -j ${CPU_COUNT}
export ECCODES_TEST_VERBOSE_OUTPUT=1
export LD_LIBRARY_PATH=${PREFIX}/lib
ctest -j ${CPU_COUNT}
make install

# Replace any leaked build env
find ${PREFIX}/include -type f -print0 | xargs -0 sed -i -e "s#$BUILD_{PREFIX}/${HOST}/sysroot#\${BUILD_PREFIX}/\${HOST}/sysroot#g" -e "s#${BUILD_PREFIX}#${PREFIX}#g"
find ${PREFIX}/lib/pkgconfig -type f -print0 | xargs -0 sed -i -e "s#${BUILD_PREFIX}/${HOST}/sysroot#\$ENV{BUILD_PREFIX}/\$ENV{HOST}/sysroot#g" -e "s#${BUILD_PREFIX}#${PREFIX}#g"
find ${PREFIX}/share/eccodes/cmake -type f -print0 | xargs -0 sed -i -e "s#${BUILD_PREFIX}/${HOST}/sysroot#\$ENV{BUILD_PREFIX}/\$ENV{HOST}/sysroot#g" -e "s#${BUILD_PREFIX}#${PREFIX}#g"

