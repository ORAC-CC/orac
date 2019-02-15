#!/bin/bash

set -e

# Ensure the build path doesn't leak into our build
export FC=$(basename ${GFORTRAN})
export F77="${FC}"
export F90="${FC}"
export CC=$(basename ${GCC})
export CXX=$(basename ${GXX})

# Don't use OpenMP. Ever.
export FFLAGS="${FFLAGS//-fopenmp} -ffree-line-length-none -I${PREFIX}/include"
export F77FLAGS="${FFLAGS}"
export CFLAGS="${CFLAGS//-fopenmp} -I${PREFIX}/include "
export CXXFLAGS="${CXXFLAGS//-fopenmp} -I${PREFIX}/include "
export LDFLAGS="${LDFLAGS} -L${PREFIX}/lib"

export DYLD_LIBRARY_PATH="${PREFIX}/lib:${DYLD_LIBRARY_PATH}"
export LD_LIBRARY_PATH="${PREFIX}/lib:${LD_LIBRARY_PATH}"
export PATH="${PREFIX}/bin:${PATH}"

rm -rf build
mkdir build
cd build

cmake -DCMAKE_INSTALL_PREFIX="${PREFIX}" \
      -DCMAKE_LIBRARY_PATH="${PREFIX}/lib" \
      -DENABLE_GRIBEX_ABORT=OFF \
      -DFFTW_USE_STATIC_LIBS=ON \
      -DFFTW_PATH="${PREFIX}" ..

make -j ${CPU_COUNT}
ctest
make install -j ${CPU_COUNT}

# Add the [de]activate scripts for the interpolation tables variable
for CHANGE in "activate" "deactivate"; do
    mkdir -p "${PREFIX}/etc/conda/${CHANGE}.d"
    cp "${RECIPE_DIR}/${CHANGE}.sh" "${PREFIX}/etc/conda/${CHANGE}.d/${CHANGE}-${PKG_NAME}.sh"
done
