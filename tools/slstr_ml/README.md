# Python scripts for NN processing in SLSTR
These scripts are useful for working with NN processing of cloudmasks, primarily for Sentinel-3 but potentially for other sensors too.

# Requires, all available on conda:
 - Satpy: https://github.com/pytroll/satpy/
 - Matplotlib, https://github.com/matplotlib/matplotlib
 - Numpy, https://github.com/numpy/numpy
 - Click, https://github.com/pallets/click
 - Cartopy, https://github.com/SciTools/cartopy
 - pyhdf, https://github.com/fhs/pyhdf
 - keras, https://github.com/keras-team/keras


### `satpy_read_SLSTR.py`:
This loads an SLSTR granule and plots various channels, RGB composites and QA flags.
Can be run from the command line (see below) or as part of another script by calling the `plot` function.
Command line options:

 `--idir`: The input directory that contains SLSTR files.
 
 `--odir`: Where to save images (if you are).
 
 `--save`: A flag indicating whether you wish to save plots to disk (True / False)
 
 `--show`: A flag indicating if you want to show plots on the screen (True / False)
 
 `--verbose`: A flag indicating whether to print info to the command line (True / False)

### `DataLoader.py`:
TBD
### `SLSTR_NN.py`:
TBD
### `Visualisation.py`:
TBD
