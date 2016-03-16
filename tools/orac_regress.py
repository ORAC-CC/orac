# Subroutines used by ORAC python scripts that call netCDF4 library

from glob import glob
import netCDF4
import numpy as np
import re
import subprocess
from termcolor import colored

#----------------------------------------------------------------------------

# Compare two NCDF files
def compare_orac_out(f0, f1):
    # Call nccmp to compare headers, dimensions, and non-global attributes
    subprocess.call('nccmp -m '+f0+' '+f1, shell=True)

    d0 = netCDF4.Dataset(f0, 'r')
    d1 = netCDF4.Dataset(f1, 'r')

    check = True
    for key in d0.variables.keys():
        head = colored(f0[f0.index('.')+1:]+') '+key.upper()+': ', 'cyan')
        try:
            a0 = d0.variables[key]
            a1 = d1.variables[key]
        except KeyError:
            check = False
            print(head+colored('Not present in both files.', 'red'))
            continue

        # Pass equal arrays
        if np.array_equal(a0, a1):
            continue
        # Pass arrays with equal NaNs
        if np.allclose(a0, a1, equal_nan=True, rtol=0., atol=0.):
            continue
        check = False

        # Ignore fields that sodding always change
        if key in ('costjm','costja','niter'):
            print(head+colored('Acceptable variation.', 'green'))

        if a0.size != a1.size:
            print(head+colored('Inconsistent dimensions.', 'red'),
                  a0.shape, a1.shape)
            continue

        if ('scale_factor' in a0.ncattrs() and 'add_offset' in a0.ncattrs()):
            a0.set_auto_scale(a0.getncattr('scale_factor') != 1.0 or
                              a0.getncattr('add_offset') != 0.0)
        else:
            a0.set_auto_scale(False)

        if a0.scale:
            # Accept rounding errors of slightly more than the scale factor
            test = np.allclose(a0, a1, equal_nan=True, rtol=0.,
                               atol=1.01*a0.getncattr('scale_factor'))
        else:
            if a0.dtype.kind == 'f':
                # Allow rounding errors
                test = np.allclose(a0, a1, equal_nan=True, rtol=1e-6, atol=1e-8)
            else:
                # Integer outputs should be free of rounding error
                test = False
        if test:
            print(head+colored('Rounding errors.', 'green'))
        else:
            print(head+colored('Unequal elements.', 'red'))

    d0.close()
    d1.close()
    return check

#-----------------------------------------------------------------------------

# Locate the last version of the file we just made
def orac_regress(fld, fileroot, suffixes):
    regex = re.compile('(.*)_R(\d+)(.*)')
    root = regex.match(fileroot)

    check = True
    for suf in suffixes:
        # Assume all files in the same folder
        cont = glob(fld+'/'+root.group(1)+'*'+suf)
        # Pick files with precisely correct suffix
        files = [f for f in cont if re.match('(.*)_R(\d+)'+suf, f)]
        if len(files) < 2:
            print('WARNING) orac_regress(): Insufficient files for '+fileroot)
            return
        # Assume revision number is everything after the last _ in root
        files.sort(key=lambda x: int(regex.match(x).group(2)), reverse=True)

        i = files.index(fld+'/'+root.group(1)+'_R'+root.group(2)+suf)
        check = compare_orac_out(files[i], files[i+1]) and check

    if check:
        print(colored('----- PASSED -----', 'green'))

