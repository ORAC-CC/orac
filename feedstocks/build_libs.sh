#!/bin/bash
# Build the ORAC dependencies into a conda environment

set -e

# Directory for conda installation
ROOT_PREFIX="$1"

# Location of ORAC git repository
ORAC_DIR="$2"

# RTTOV_FILES points to where the coefficient files are stored
export RTTOV_FILES="$3"

FEED_DIR="$ORAC_DIR/feedstocks"

# Download, install and start Miniforge
TMPDIR=$(mktemp -d)
pushd $TMPDIR
wget "https://github.com/conda-forge/miniforge/releases/latest/download/Miniforge3-$(uname)-$(uname -m).sh"
chmod 0700 "Miniforge-$(uname)-$(uname -m).sh"
./Miniforge-Linux-x86_64.sh -b -p "$ROOT_PREFIX"
. "$ROOT_PREFIX"/etc/profile.d/conda.sh
popd
rm -rf $TMPDIR
conda activate
conda update -y --all

# Ensure the local directory is searched in the path
export PATH=$PATH:.

# Install the building tools
conda install -y mamba
mamba install -y boa conda-verify

# Packages without dependencies
conda mambabuild --no-anaconda-upload "$FEED_DIR/nr"
#conda mambabuild --no-anaconda-upload "$FEED_DIR/fftw"
#conda mambabuild --no-anaconda-upload "$FEED_DIR/libaec"
conda mambabuild --no-anaconda-upload "$FEED_DIR/epr_api"
conda mambabuild --no-anaconda-upload "$FEED_DIR/fu_liou"

# NOTE: We have to build this because the conda-forge repo contains the
# shared libraries, which can't be built at the same time as the Fortran ones
# we use. I've offered a PR to fix that
conda mambabuild --no-anaconda-upload "$FEED_DIR/hdf4"
# NOTE: We have to build this as the conda-forge version doesn't contain
# Fortran-friendly function names
conda mambabuild --no-anaconda-upload "$FEED_DIR/hdfeos2"

#conda mambabuild --no-anaconda-upload "$FEED_DIR/hdf5"
#conda mambabuild --no-anaconda-upload "$FEED_DIR/libnetcdf"
#conda mambabuild --no-anaconda-upload "$FEED_DIR/netcdf-fortran"
#conda mambabuild --no-anaconda-upload "$FEED_DIR/eccodes"

conda mambabuild --no-anaconda-upload "$FEED_DIR/seviri_util"
conda mambabuild --no-anaconda-upload "$FEED_DIR/hsd_reader"
conda mambabuild --no-anaconda-upload "$FEED_DIR/seviri_ml"

conda mambabuild --no-anaconda-upload "$FEED_DIR/libemos"

# Requires the RTTOV_FILE variable to be exported
conda mambabuild --no-anaconda-upload "$FEED_DIR/rttov"

conda mambabuild --no-anaconda-upload "$FEED_DIR/orac"
conda mambabuild --no-anaconda-upload "$FEED_DIR/pyorac"

# Install the release version of ORAC
conda create -y --override-channels -c local -c conda-forge \
      -n orac_release orac pyorac python=3.8

cp "$ORAC_DIR/tools/pyorac/local_defaults.py" \
   "$ROOT_PREFIX/envs/orac_release/lib/python3.8/site-packages/pyorac/"

# Create an environment suitable for ORAC
conda create -y --override-channels -c local -c conda-forge \
      -n orac_git --file "$FEED_DIR/dependencies.nompi.txt" python=3.8

SCRIPT_DIR="$ROOT_PREFIX/envs/orac_git/etc/conda"
for CHANGE in "activate" "deactivate"; do
    mkdir -p "${SCRIPT_DIR}/${CHANGE}.d"
    cp "${FEED_DIR}/orac/${CHANGE}.sh" "${SCRIPT_DIR}/${CHANGE}.d/${CHANGE}-orac.sh"
done
cat <<EOF >> "$SCRIPT_DIR/activate.d/activate-orac.sh"
# Point to this environment's libraries
if [ -n "\${ORAC_LIB}" ]; then
    export ORAC_LIB_SAVE="\${ORAC_LIB}"
fi
export ORAC_LIB="\${ORACDIR}/config/lib.conda.inc"
if [ -n "\${ORAC_ARCH}" ]; then
    export ORAC_ARCH_SAVE="\${ORAC_ARCH}"
fi
export ORAC_ARCH="\${ORACDIR}/config/arch.conda.inc"

if [ -n "\${PYTHONPATH}" ]; then
    export PYTHONPATH_SAVE="\${PYTHONPATH}"
fi
export PYTHONPATH="\${ORACDIR}/tools:\${PYTHONPATH}"
EOF
cat <<EOF >> "$SCRIPT_DIR/deactivate.d/deactivate-orac.sh"
if [ -n "\${ORAC_LIB_SAVE}" ]; then
    export ORAC_LIB="\${ORAC_LIB_SAVE}"
    unset ORAC_LIB_SAVE
else
    unset ORAC_LIB
fi
if [ -n "\${ORAC_ARCH_SAVE}" ]; then
    export ORAC_ARCH="\${ORAC_ARCH_SAVE}"
    unset ORAC_ARCH_SAVE
else
    unset ORAC_ARCH
fi

if [ -n "\${PYTHONPATH_SAVE}" ]; then
    export PYTHONPATH="\${PYTHONPATH_SAVE}"
    unset PYTHONPATH_SAVE
else
    unset PYTHONPATH
fi
EOF
