#!/bin/bash
# Build the ORAC dependencies into a conda environment

set -e

# Directory for conda installation
PREFIX="$1"

# Location of conda feedstocks
FEED_DIR="$2"

# RTTOV_FILES points to where the coefficient files are stored
export RTTOV_FILES="$3"

# Download, install and start Miniconda
TMPDIR=$(mktemp -d)
pushd $TMPDIR
wget "https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh"
chmod 0700 Miniconda3-latest-Linux-x86_64.sh
./Miniconda3-latest-Linux-x86_64.sh -b -p "$PREFIX"
. "$PREFIX"/etc/profile.d/conda.sh
popd
rm -rf $TMPDIR
conda activate

# Ensure the local directory is searched in the path
export PATH=$PATH:.

# Use conda-forge's GNU v7.3.0
CHANNELS="-c conda-forge -c anaconda --override-channels"

# Install the building tools
conda install -y $CHANNELS conda-build conda-verify

# Packages without dependencies
conda-build $CHANNELS --no-anaconda-upload "$FEED_DIR/nr"
conda-build $CHANNELS --no-anaconda-upload "$FEED_DIR/fftw"
conda-build $CHANNELS --no-anaconda-upload "$FEED_DIR/szip"
conda-build $CHANNELS --no-anaconda-upload "$FEED_DIR/epr_api"
conda-build $CHANNELS --no-anaconda-upload "$FEED_DIR/fu_liou"

# Add our local builds to the channel list
CHANNELS="-c $PREFIX/conda-bld $CHANNELS"

conda-build $CHANNELS --no-anaconda-upload "$FEED_DIR/hdf4"
conda-build $CHANNELS --no-anaconda-upload "$FEED_DIR/hdfeos2"

conda-build $CHANNELS --no-anaconda-upload "$FEED_DIR/hdf5"

conda-build $CHANNELS --no-anaconda-upload "$FEED_DIR/libnetcdf"
conda-build $CHANNELS --no-anaconda-upload "$FEED_DIR/netcdf-fortran"

conda-build $CHANNELS --no-anaconda-upload "$FEED_DIR/eccodes"
conda-build $CHANNELS --no-anaconda-upload "$FEED_DIR/seviri_util"
conda-build $CHANNELS --no-anaconda-upload "$FEED_DIR/hsd_reader"

conda-build $CHANNELS --no-anaconda-upload "$FEED_DIR/libemos"

# Requires the RTTOV_FILE variable to be exported
conda-build $CHANNELS --no-anaconda-upload "$FEED_DIR/rttov"

conda-build $CHANNELS --no-anaconda-upload "$FEED_DIR/orac"

# Create an environment suitable for ORAC
conda create -y $CHANNELS -n orac \
    orac gcc_linux-64 gxx_linux-64 gfortran_linux-64 bison flex

SCRIPT_DIR="$PREFIX/envs/orac/etc/conda"
cat <<EOF > "$SCRIPT_DIR/activate.d/activate-orac.sh"
#!/bin/bash
export ORAC_LIB=\${ORACDIR}/config/lib.inc
export ORAC_ARCH=\${ORACDIR}/config/arch.gfortran.inc
EOF
