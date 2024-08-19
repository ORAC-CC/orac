#!/bin/bash

# Work out which version of Python we're linking against
# numpy include folder
NPINC=$(find ${PREFIX} -type d -wholename "*/numpy/core/include/numpy" -print -quit)
# Directory for Python libraries
NPDIR=${NPINC%%/numpy/core/include/numpy}
# Python linking library
PYLIB=$(find ${PREFIX} -type f -name "libpython*.*.so*" -print -quit)
PYLIB_PATH=${PYLIB%.so*}
if [ -z "$PYLIB_PATH" ]; then
    PYLIB=$(find ${PREFIX} -type f -name "libpython*.*.a" -print -quit)
    PYLIB_PATH=${PYLIB%.a}
    if [ -z "$PYLIB_PATH" ]; then
        echo "Cannot locate libpython"
        exit 2
    fi
fi
PYVER=${PYLIB_PATH##*/libpython}
# Include directory for Python
PYINC=${PREFIX}/include/python${PYVER}

make NUMPYINCLUDE=${NPINC} PYINCLUDE=${PYINC}

# Copy over compiled libraries
cp libsevann.a ${PREFIX}/lib
cp seviri_neural_net_m.mod ${PREFIX}/include

# Install Python code
NEW_DIR=${NPDIR}/seviri_ml
mkdir ${NEW_DIR}
mv *py ${NEW_DIR}
mv modis_like_rgb ${NEW_DIR}
mv sba ${NEW_DIR}
mv nn_driver.txt ${NEW_DIR}
mkdir ${NEW_DIR}/data
mv data/v{1,2,3} ${NEW_DIR}/data
#"${PYTHON}" -m pip install seviri_ml --no-deps --ignore-installed --no-cache-dir -vv

# Compile tests
cd test
make PY_VER=${PYVER} NUMPYINCLUDE=${NPINC} PYINCLUDE=${PYINC}
cp ctestexe ${PREFIX}/bin/seviri-ml_ctest
cp fort90testexe ${PREFIX}/bin/seviri-ml_ftest
