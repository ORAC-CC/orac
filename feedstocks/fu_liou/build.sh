#!/bin/bash

set -e

export F77="${GFORTRAN}"
export F90="${GFORTRAN}"

# They use a non-standard name for the compilation flags
export F90COMP="${FFLAGS} -c -w"
export FCOMP="${F90COMP}"

mkdir "${PREFIX}/include"

cd Ed4_LaRC_FuLiou
make libsrc

mv lib/*.a ${PREFIX}/lib
mv lib/mod/*.mod ${PREFIX}/include
