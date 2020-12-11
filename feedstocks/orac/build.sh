#!/bin/bash

set -e

export ORAC_LIB=${SRC_DIR}/config/lib.inc
export ORAC_ARCH=${SRC_DIR}/config/arch.gfortran.inc

if [ ! -d common/obj ]; then
    mkdir common/obj
    mkdir pre_processing/obj
    mkdir src/obj
    mkdir post_processing/obj
    mkdir derived_products/broadband_fluxes/obj
    mkdir derived_products/broadband_fluxes/obj/bugsrad
    mkdir derived_products/broadband_fluxes/obj/fu_liou
fi

make -j ${CPU_COUNT}

# "Install" the files in the PREFIX directories
find . -name "*.a" -exec mv {} ${PREFIX}/lib \;
find . -name "*.mod" -exec mv {} ${PREFIX}/include \;
mv common/rttov_version ${PREFIX}/bin
mv pre_processing/orac_preproc ${PREFIX}/bin
mv src/orac ${PREFIX}/bin
mv post_processing/orac_postproc ${PREFIX}/bin

# Add configuration files, as the Python scripts use them to load libraries
mkdir -p ${PREFIX}/share/orac
cp config/arch.gfortran.inc ${PREFIX}/share/orac
cp config/lib.inc ${PREFIX}/share/orac
sed -i 's/PREFIX/CONDA_PREFIX/g' ${PREFIX}/share/orac/lib.inc

# Add the [de]activate scripts for environment variables
for CHANGE in "activate" "deactivate"; do
    mkdir -p "${PREFIX}/etc/conda/${CHANGE}.d"
    cp "${CHANGE}.sh" "${PREFIX}/etc/conda/${CHANGE}.d/${CHANGE}-${PKG_NAME}.sh"
done
