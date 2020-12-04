#!/bin/bash

set -e

# Use CMake rather than the default file from the repository
rm -rf makefile
rm -rf build
mkdir -vp build
cd build

cmake -DCMAKE_INSTALL_PREFIX="${PREFIX}" \
      -DCMAKE_LIBRARY_PATH="${PREFIX}/lib" \
      -DCMAKE_BUILD_TYPE="Release" \
      -DCMAKE_C_FLAGS="-fPIC -w" \
      -DBUILD_TESTS="yes" \
      -G "Unix Makefiles" -Wno-dev ..

make -j ${CPU_COUNT}
make test
make install -j ${CPU_COUNT}
