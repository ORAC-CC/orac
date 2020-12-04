#!/bin/bash

set -e

if [ -n "$(conda list -p $PREFIX | grep openmpi)" ]; then
    ARCH=gfortran-openmp
else
    ARCH=gfortran
fi
REL_PATH=`python -c "import os.path; print(os.path.relpath('$PREFIX', '$PWD'))"`

cd src
../build/Makefile.PL RTTOV_HDF=1 RTTOV_F2PY=1 RTTOV_USER_LAPACK=1
make ARCH=$ARCH INSTALLDIR=$REL_PATH
