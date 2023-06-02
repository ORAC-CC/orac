#!/bin/bash

set -e

mkdir -vp ${PREFIX}/bin

autoreconf -vfi
./configure --prefix="${PREFIX}"

make -j${CPU_COUNT}
make check
make install -j${CPU_COUNT}
