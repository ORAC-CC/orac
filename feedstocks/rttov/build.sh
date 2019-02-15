#!/bin/bash

set -e

# Compilers are set within an arch file, which we patch.
# Don't use OpenMP on JASMIN. Do use it everywhere else.
#ARCH=gfortran
ARCH=gfortran-openmp
REL_PATH=`python -c "import os.path; print(os.path.relpath('$PREFIX', '$PWD'))"`

# Point to local coefficients directory
rm -rf rtcoef_rttov12
ln -s $RTTOV_FILES rtcoef_rttov12

# Don't build the Python interface with GNU 8.2.0 as f2py isn't yet functional
cd src
../build/Makefile.PL RTTOV_HDF=1 RTTOV_F2PY=0 RTTOV_USER_LAPACK=1
make ARCH=$ARCH INSTALLDIR=$REL_PATH

cd ../rttov_test
export LD_LIBRARY_PATH="${PREFIX}/lib:${LD_LIBRARY_PATH}"
./test_fwd.sh ARCH=$ARCH BIN=$REL_PATH/bin
./test_rttov12.sh ARCH=$ARCH BIN=$REL_PATH/bin
./test_solar.sh ARCH=$ARCH BIN=$REL_PATH/bin
./test_coef_io.sh ARCH=$ARCH BIN=$REL_PATH/bin
./test_coef_io_hdf.sh ARCH=$ARCH BIN=$REL_PATH/bin
