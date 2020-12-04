#!/bin/sh

set -e

chmod -R 0700 .
# Patch code after fixing permissions
patch -p1 < SWapi.patch
patch -p1 < samples.patch
patch -p1 <  0001-fix-automake-files-for-linux-compatibility.patch

autoreconf -vfi

./configure --prefix="${PREFIX}" \
            --build="${BUILD}" \
            --with-hdf4="${PREFIX}" \
            --with-zlib="${PREFIX}" \
            --with-jpeg="${PREFIX}" \
            --enable-install-include \
            CC="${PREFIX}/bin/h4cc" \
            FC="${PREFIX}/bin/h4fc" \
            CFLAGS="${CFLAGS} -Df2cFortran"

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
