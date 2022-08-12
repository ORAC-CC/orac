#!/bin/sh

set -e

# They use implicit casting, which newer gfortran doesn't allow
cat > optiontest.f90 <<EOF
end
EOF
${GFORTRAN} ${FFLAGS} -fallow-argument-mismatch -fsyntax-only optiontest.f90 \
            2> /dev/null && export FFLAGS="${FFLAGS} -fallow-argument-mismatch"
rm optiontest.f90

mkdir build
cd build
cmake -DCMAKE_PREFIX_PATH:PATH="${PREFIX}" \
      -DCMAKE_INSTALL_PREFIX:PATH="${PREFIX}" \
      -DBUILD_SHARED_LIBS:BOOL=OFF \
      -DUSE_SHARED_LIBS:BOOL=OFF \
      -DBUILD_TESTING:BOOL=ON \
      -DHDFEOS_PACKAGE_EXT:STRING="" \
      -DHDFEOS_BUILD_SAMPLES:BOOL=ON \
      -DHDFEOS_BUILD_TESTDRIVERS:BOOL=ON \
      -DHDFEOS_ENABLE_JPEG_LIB_SUPPORT:BOOL=ON \
      -DHDFEOS_ENABLE_Z_LIB_SUPPORT:BOOL=ON \
      -DHDFEOS_ENABLE_SZIP_SUPPORT:BOOL=ON \
      -DHDFEOS_ENABLE_SZIP_ENCODING:BOOL=ON \
      -DHDFEOS_DISABLE_COMPILER_WARNINGS:BOOL=OFF \
      -DHDFEOS_ENABLE_PARALLEL:BOOL=OFF \
      -DHDFEOS_BUILD_FORTRAN:BOOL=ON \
      -DHDFEOS_PACKAGE_EXTLIBS:BOOL=OFF \
      -DHDFEOS_ALLOW_EXTERNAL_SUPPORT:BOOL=NO \
      -DHDF4_PACKAGE_NAME:STRING="hdf4" \
      -DZLIB_PACKAGE_NAME:STRING="zlib" \
      -DSZIP_PACKAGE_NAME:STRING="sz" \
      -DJPEG_PACKAGE_NAME:STRING="jpeg" \
      -G "Unix Makefiles" -Wno-dev ${CMAKE_ARGS} ..

make -j ${CPU_COUNT}
make test
make install
