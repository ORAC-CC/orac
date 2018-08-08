"""Routines implementing the regression tests for the ORAC repository."""
import pyorac.local_defaults as defaults

from pyorac.definitions import *
from warnings import warn


# Define the regression tests
REGRESSION_TESTS = {
    # Short tests
    'DAYMYDS'   : ('MYD021KM.A2008172.0405.005.2009317014309.hdf',
                   (700, 1299, 1200, 1204), 'MODIS'),
    'NITMYDS'   : ('MYD021KM.A2008172.1630.005.2009317021545.bscs_'
                   '000500531943.hdf', (500, 1099, 900, 904), 'MODIS'),
    'DAYAATSRS' : ('ATS_TOA_1PRUPA20080620_002337_000065272069_00345_32964_'
                   '0666.N1', (1, 512, 21366, 21370), 'AATSR'),
    'NITAATSRS' : ('ATS_TOA_1PRUPA20080620_002337_000065272069_00345_32964_'
                   '0666.N1', (1, 512, 37450, 37454), 'AATSR'),
    'DAYAVHRRS' : ('noaa18_20080620_0050_99999_satproj_00000_13111_avhrr.h5',
                   (1, 409, 5190, 5194), 'AVHRR'),
    'NITAVHRRS' : ('noaa18_20080620_0050_99999_satproj_00000_13111_avhrr.h5',
                   (1, 409, 10150, 10154), 'AVHRR'),
    # Long tests
    'DAYMYD'    : ('MYD021KM.A2008172.0405.005.2009317014309.hdf',
                   (0, 0, 0, 0), 'MODIS'),
    'NITMYD'    : ('MYD021KM.A2008172.1630.005.2009317021545.bscs_'
                   '000500531943.hdf',   (0, 0, 0, 0), 'MODIS'),
    'AATSR'     : ('ATS_TOA_1PRUPA20080620_002337_000065272069_00345_32964_'
                   '0666.N1', (0, 0, 0, 0), 'AATSR'),
    'AVHRR'     : ('noaa18_20080620_0050_99999_satproj_00000_13111_avhrr.h5',
                   (0, 0, 0, 0), 'AVHRR'),
}


def compare_nc_atts(d0, d1, f, var):
    """Report if the attributes of a NCDF file or variable have changed."""

    # Check if any attributes added/removed
    atts = set(d0.ncattrs()).symmetric_difference(d1.ncattrs())
    if len(atts) > 0:
        warn(FieldMissing(f, ', '.join(atts)), stacklevel=3)

    # Check if any attributes changed
    for key in d0.ncattrs():
        if key in atts:
            continue

        if (d0.__dict__[key] != d1.__dict__[key] and
            key not in defaults.atts_to_ignore):
            warn(Regression(f, var + ', ' + key, 'warning',
                            'Changed attribute ({} vs {})'.format(
                                d0.__dict__[key], d1.__dict__[key]
                            )), stacklevel=3
            )


def compare_orac_out(f0, f1):
    """Compare two NCDF files"""
    import numpy as np
    try:
        from netCDF4 import Dataset

    except ImportError as err:
        warn('Skipping regression tests as netCDF4 unavailable',
             OracWarning, stacklevel=2)
        return

    try:
        # Open files
        d0 = Dataset(f0, 'r')
        d1 = Dataset(f1, 'r')

        # Check if any dimensions added/removed
        dims = set(d0.dimensions.keys()).symmetric_difference(
            d1.dimensions.keys()
        )
        if len(dims) > 0:
            # Not bothering to identify which file contains the errant field
            warn(FieldMissing(f1, ', '.join(dims)), stacklevel=2)

        # Check if any dimensions changed
        for key in d0.dimensions.keys():
            if key in dims:
                continue

            if d0.dimensions[key].size != d1.dimensions[key].size:
                warn(InconsistentDim(f1, key, d0.dimensions[key].size,
                                     d1.dimensions[key].size),
                     stacklevel=2)

            # Check attributes
            compare_nc_atts(d0, d1, f1, key)

        # Check if any variables added/removed
        vars = set(d0.variables.keys()).symmetric_difference(
            d1.variables.keys()
        )
        if len(vars) > 0:
            warn(FieldMissing(f1, ', '.join(vars)), stacklevel=2)

        # Check if any variables changed
        for key in d0.variables.keys():
            if key in vars:
                continue

            a0 = d0.variables[key]
            a1 = d1.variables[key]

            # Turn off masking, so completely NaN fields can be equal
            a0.set_auto_mask(False)
            a1.set_auto_mask(False)

            compare_nc_atts(a0, a1, f1, key)

            if a0.size != a1.size:
                warn(InconsistentDim(f1, key, a0.size, a1.size), stacklevel=2)
                continue

            # Check if there has been any change
            if not np.allclose(a0, a1, equal_nan=True, rtol=0, atol=0):
                test = False

                # For floats, check if variation is acceptable
                if a0.dtype.kind == 'f':
                    test = np.allclose(a0, a1, equal_nan = True,
                                       rtol = defaults.rtol,
                                       atol = defaults.atol)
                else:
                    try:
                        if isinstance(a0.scale_factor, np.floating):
                            # Packed floats consider the scale factor
                            test = np.allclose(a0, a1, equal_nan = True,
                                               rtol = defaults.rtol,
                                               atol = max(a0.scale_factor,
                                                          defaults.atol))
                    except AttributeError:
                        # If there is no scale factor, treat as an integer
                        pass

                if test or key in defaults.vars_to_accept:
                    warn(Acceptable(f1, key), stacklevel=2)
                else:
                    warn(RoundingError(f1, key), stacklevel=2)
    except IOError:
        pass

    finally:
        if ('d0' in locals() or 'd0' in globals()): d0.close()
        if ('d1' in locals() or 'd1' in globals()): d1.close()

