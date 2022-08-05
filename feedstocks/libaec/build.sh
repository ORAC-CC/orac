#!/bin/bash

set -e

mkdir build
cd build
cmake -DCMAKE_INSTALL_PREFIX="${PREFIX}" \
      -DCMAKE_LIBRARY_PATH="${PREFIX}/lib" \
      -DCMAKE_BUILD_TYPE="Release" \
      -G "Unix Makefiles" -Wno-dev ..
make install -j ${CPU_COUNT}
