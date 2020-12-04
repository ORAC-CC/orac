#!/bin/sh
set -e

# Check libraries are present
test -f ${PREFIX}/lib/librttov12_brdf_atlas.a
test -f ${PREFIX}/lib/librttov12_coef_io.a
test -f ${PREFIX}/lib/librttov12_emis_atlas.a
test -f ${PREFIX}/lib/librttov12_main.a

if [ -n "$(conda list -p $PREFIX | grep openmpi)" ]; then
    ARCH=gfortran-openmp
else
    ARCH=gfortran
fi
REL_PATH=`python -c "import os.path; print(os.path.relpath('$PREFIX', '$PWD'))"`

# Point to local coefficients directory
rm -rf rtcoef_rttov12
ln -s $RTTOV_FILES rtcoef_rttov12

export LD_LIBRARY_PATH="${PREFIX}/lib:${LD_LIBRARY_PATH}"
cd rttov_test
./test_fwd.sh ARCH=$ARCH BIN=$REL_PATH/bin
./test_rttov12.sh ARCH=$ARCH BIN=$REL_PATH/bin
./test_solar.sh ARCH=$ARCH BIN=$REL_PATH/bin
#./test_coef_io.sh ARCH=$ARCH BIN=$REL_PATH/bin
#./test_coef_io_hdf.sh ARCH=$ARCH BIN=$REL_PATH/bin
