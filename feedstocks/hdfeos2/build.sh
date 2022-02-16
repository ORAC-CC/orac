#!/bin/sh

set -e

autoreconf -vfi

./configure --prefix="${PREFIX}" \
            --with-hdf4="${PREFIX}" \
            --with-zlib="${PREFIX}/lib" \
            --with-szlib="${PREFIX}/lib" \
            --with-jpeg="${PREFIX}/lib" \
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
