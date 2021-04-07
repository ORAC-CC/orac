#!/bin/bash

set -e

export FFLAGS="${FFLAGS} -w -cpp -I. -ffree-line-length-0 -fcheck=bounds"
export F77FLAGS="${FORTRANFLAGS} -w -cpp -I. -ffree-line-length-0 -fcheck=bounds"
export CFLAGS="${CFLAGS} -w -cpp -I."

# Can't use -j ${CPU_COUNT} as dependencies are incomplete
make

mv *.a "${PREFIX}/lib"
mv *.mod "${PREFIX}/include"
mv AHI "${PREFIX}/bin"
