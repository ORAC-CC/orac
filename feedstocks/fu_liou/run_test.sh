#!/bin/bash
set -e

cd Ed4_LaRC_FuLiou/src/simple
${GFORTRAN} ${FFLAGS} -c -w -I${CONDA_PREFIX}/include simple.f90
${GFORTRAN} ${FFLAGS} -o simple simple.o ${CONDA_PREFIX}/lib/libEd3Fu_201212.a
./simple > test_result
cmp -s test_result ../../../simple.txt
