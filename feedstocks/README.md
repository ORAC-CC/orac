# ORAC Anaconda environment


The files in this folder are used to compile ORAC and it's dependencies using `conda build` for distribution with conda. In short, this is done with the call,
```
cd orac/feedstocks
build_libs.sh $PREFIX $PWD $RTTOV_FILES
```
where `$PREFIX` denotes where you would like to store the building conda environment (ideally on your local machine, as some library tests can be very network intensive), `$PWD` points to this directory of feedstocks, and `$RTTOV_FILES` points to your local repository of coefficient files for RTTOV (which can be obtained [here](https://www.nwpsaf.eu/site/software/rttov/download/coefficients/)).


### The build_libs script
That script first downloads and installs the latest version of Miniconda and adds `conda build` to it. It then calls `conda build` on each folder in this directory to build all of the ORAC dependencies. These are,
- **NR**, the Numerical Recipes C library (an optional dependency for which the user must obtain an appropriate license [here](http://numerical.recipes/));
- **FFTW**, the Fastest Fourier Transform in the West;
- **SZip**, a compression library from the HDF team (for which the user must obtain an appropriate license [here](https://support.hdfgroup.org/doc_resource/SZIP/));
- **EPR**, an API to interact with ENVISAT files (an optional dependency for those wishing to evaluate ATSR-2 or AATSR data);
- **Fu-Liou**, a broadband radiative transfer code (an optional dependency for which the user must obtain an appropriate license [here](https://www-cave.larc.nasa.gov/cgi-bin/lflcode/accesslfl.cgi));
- **HDF4**, a scientific data format;
- **HDF-EOS**, a NASA extension of HDF4;
- **HDF5**, a newer scientific data format;
- **NetCDF**, another scientific data format (which has both C and Fortran libraries, stored in separate packages);
- **ECCodes**, an ECMWF library used to read GRIB files;
- **Seviri Util**, a tool to open data from the SEVIRI geostationary imager (an optional dependency);
- **HSD Reader**, a tool to open data from the Himawari geostationary imager (an optional dependency);
- **EMOS**, an ECMWF library to interpolate meteorological data;
- **RTTOV**, a fast radiative transfer code maintained by the Met Office (for which the user must obtain an appropriate license [here](https://www.nwpsaf.eu/site/software/rttov/download/#Software));

The script then builds the ORAC suite and installs it's Python software (called `pyorac`). Finally, a conda environment called `orac` is created, which contains the ORAC code, all of its dependencies, and the conda-toolchain C and Fortran compilers. That environment may be used to compile ORAC from source.

As saved, these scripts compile all libraries with multi-threading through the OpenMP library. If you wish to compile without that (as is required for some batch queuing systems), please un/comment lines at the top of each `build.sh` script as appropriate.

At the moment, these libraries are only built for Linux systems due to the resources available to the engineer. There is no reason these couldn't be built for Windows or OSX, but additional scripts and debugging would be needed, which are not currently considered a priority. Users are welcome to make a pull request with suitable code.


### The feedstocks
Each dependency has a folder, containing all files necessary to build that library with `conda build`. The `build.sh` script sets appropriate environment variables, compiles the code, tests it, and installs. The `meta.yaml` files defines the library's dependencies and metadata. An optional `run_test.sh` script defines install-level tests for the library that cannot be easily written within the meta-file. Other files in each folder are usually patches for the source code or software used in the testing process. Documentation for the `conda build` package can be found [here](https://docs.conda.io/projects/conda-build/en/latest/index.html).


---
Feb 15 2019
Adam Povey (adam.povey@physics.ox.ac.uk)