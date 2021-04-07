#!/bin/bash

set -e

rm -rf build
mkdir build
cd build

cmake -DCMAKE_INSTALL_PREFIX="${PREFIX}" \
      -DCMAKE_LIBRARY_PATH="${PREFIX}/lib" \
      -DENABLE_GRIBEX_ABORT=OFF \
      -DFFTW_USE_STATIC_LIBS=ON \
      -DFFTW_PATH="${PREFIX}" \
      -G "Unix Makefiles" -Wno-dev ..

make -j ${CPU_COUNT}
ctest
make install -j ${CPU_COUNT}

# Add the [de]activate scripts for the interpolation tables variable
for CHANGE in "activate" "deactivate"; do
    mkdir -p "${PREFIX}/etc/conda/${CHANGE}.d"
    cp "${RECIPE_DIR}/${CHANGE}.sh" "${PREFIX}/etc/conda/${CHANGE}.d/${CHANGE}-${PKG_NAME}.sh"
done
