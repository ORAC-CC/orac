#!/bin/bash

set -e

autoreconf -vfi

# Build static, ignoring DAP as it doesn't work
./configure --prefix="${PREFIX}" \
            --enable-netcdf-4 \
            --enable-hdf4 \
            --enable-logging \
            --disable-dap \
            --disable-examples

make -j ${CPU_COUNT}
make check
make install -j ${CPU_COUNT}
