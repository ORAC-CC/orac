#!/bin/bash
# Build the ORAC dependencies into a conda environment

set -e

# Directory for conda installation
ROOT_PREFIX="$1"

# Location of conda feedstocks
FEED_DIR="$2"

# RTTOV_FILES points to where the coefficient files are stored
export RTTOV_FILES="$3"

# Download, install and start Miniconda
TMPDIR=$(mktemp -d)
pushd $TMPDIR
wget "https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh"
chmod 0700 Miniconda3-latest-Linux-x86_64.sh
./Miniconda3-latest-Linux-x86_64.sh -b -p "$ROOT_PREFIX"
. "$ROOT_PREFIX"/etc/profile.d/conda.sh
popd
rm -rf $TMPDIR
conda activate
conda update -y --all

# Ensure the local directory is searched in the path
export PATH=$PATH:.

# Install the building tools
conda install -y conda-build conda-verify

# Packages without dependencies
conda-build --no-anaconda-upload "$FEED_DIR/nr"
#conda-build --no-anaconda-upload "$FEED_DIR/fftw"
#conda-build --no-anaconda-upload "$FEED_DIR/szip"
conda-build --no-anaconda-upload "$FEED_DIR/epr_api"
conda-build --no-anaconda-upload "$FEED_DIR/fu_liou"

# NOTE: We have to build these because the conda-forge repo contains the
# shared libraries, which can't be built at the same time as the Fortran ones
# we use. Arguably, we should rewrite our interface to drop the Fortran links
# but that's a lot of work for minimal gain. If bored, one should look into
# how to build both alongside each other
conda-build --no-anaconda-upload "$FEED_DIR/hdf4"
conda-build --no-anaconda-upload "$FEED_DIR/hdfeos2"

#conda-build --no-anaconda-upload "$FEED_DIR/hdf5"

# NOTE: Have to build these as well as they depend on the HDF4 library
conda-build --no-anaconda-upload "$FEED_DIR/libnetcdf"
conda-build --no-anaconda-upload "$FEED_DIR/netcdf-fortran"

#conda-build --no-anaconda-upload "$FEED_DIR/eccodes"
conda-build --no-anaconda-upload "$FEED_DIR/seviri_util"
conda-build --no-anaconda-upload "$FEED_DIR/hsd_reader"

conda-build --no-anaconda-upload "$FEED_DIR/libemos"

# Requires the RTTOV_FILE variable to be exported
conda-build --no-anaconda-upload "$FEED_DIR/rttov"

conda-build --no-anaconda-upload "$FEED_DIR/orac"
conda-build --no-anaconda-upload "$FEED_DIR/pyorac"

# Create an environment suitable for ORAC
conda create -y --override-channels -c local -c conda-forge -c anaconda -n orac \
      gcc_linux-64 gxx_linux-64 gfortran_linux-64 bison flex \
      nr epr_api fu_liou hdf4 hdfeos2 hdf5 libnetcdf netcdf-fortran \
      eccodes seviri_util hsd_reader libemos rttov=12.2 \
      numpy scipy netcdf4 h5py cf-units opencv matplotlib cartopy seaborn

SCRIPT_DIR="$ROOT_PREFIX/envs/orac/etc/conda"
cat <<EOF > "$SCRIPT_DIR/activate.d/activate-orac.sh"
#!/bin/bash
export ORAC_LIB=\${ORACDIR}/config/lib.inc
export ORAC_ARCH=\${ORACDIR}/config/arch.gfortran.inc
EOF
