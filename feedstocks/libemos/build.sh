#!/bin/bash

set -e

# They use implicit casting, which newer gfortran doesn't allow
cat > optiontest.f90 <<EOF
end
EOF
${GFORTRAN} ${FFLAGS} -fallow-argument-mismatch -fsyntax-only optiontest.f90 \
            2> /dev/null && export FFLAGS="${FFLAGS} -fallow-argument-mismatch"
rm optiontest.f90

rm -rf build
mkdir build
cd build

cmake -DCMAKE_INSTALL_PREFIX="${PREFIX}" \
      -DCMAKE_LIBRARY_PATH="${PREFIX}/lib" \
      -DENABLE_GRIBEX_ABORT=OFF \
      -DFFTW_PATH="${PREFIX}/lib" \
      -G "Unix Makefiles" -Wno-dev ..

make -j ${CPU_COUNT}
ctest
make install -j ${CPU_COUNT}

# Add the [de]activate scripts for the interpolation tables variable
for CHANGE in "activate" "deactivate"; do
    mkdir -p "${PREFIX}/etc/conda/${CHANGE}.d"
    cp "${RECIPE_DIR}/${CHANGE}.sh" "${PREFIX}/etc/conda/${CHANGE}.d/${CHANGE}-${PKG_NAME}.sh"
done
