#!/bin/bash

set -e

export F77="${GFORTRAN}"
export F90="${GFORTRAN}"

# They use a non-standard name for the compilation flags
export F90COMP="${FFLAGS} -c -w"
export FCOMP="${FORTRANFLAGS} -c -w"

# They use implicit casting, which newer gfortran doesn't allow
cat > optiontest.f90 <<EOF
end
EOF
${GFORTRAN} ${F90COMP} -fallow-argument-mismatch -fsyntax-only optiontest.f90 \
            2> /dev/null && FCOMP="${FCOMP} -fallow-argument-mismatch" \
                         && F90COMP="${F90COMP} -fallow-argument-mismatch"
rm optiontest.f90

mkdir "${PREFIX}/include"

cd Ed4_LaRC_FuLiou
make libsrc

mv lib/*.a ${PREFIX}/lib
mv lib/mod/*.mod ${PREFIX}/include
