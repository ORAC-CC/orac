"""Routines implementing the regression tests for the ORAC repository."""
import pyorac.local_defaults as defaults

from pyorac.definitions import *
from warnings import warn


# Define the regression tests
REGRESSION_TESTS = {
    # Short tests
    'DAYMYDS': ('MYD021KM.A2008172.0405.061.2018034142316.hdf',
                (700, 1299, 1200, 1204), 'MODIS'),
    'NITMYDS': ('MYD021KM.A2008172.1630.061.2018034143236.hdf',
                (500, 1099, 900, 904), 'MODIS'),
    'NITMODS': ('MOD021KM.A2008172.1330.061.2017256012859.hdf',
                (300, 799, 165, 169), 'MODIS'),
    'DAYMODS': ('MOD021KM.A2008172.0115.061.2017256012659.hdf',
                (150, 749, 300, 304), 'MODIS'),
    'DAYAATSRS': ('ATS_TOA_1PRUPA20080620_002337_000065272069_00345_32964_'
                  '0666.N1', (1, 512, 21366, 21370), 'AATSR'),
    'NITAATSRS': ('ATS_TOA_1PRUPA20080620_002337_000065272069_00345_32964_'
                  '0666.N1', (1, 512, 37450, 37454), 'AATSR'),
    'DAYAVHRRS': ('noaa18_20080620_0050_99999_satproj_00000_13111_avhrr.h5',
                  (1, 409, 5190, 5194), 'AVHRR'),
    'NITAVHRRS': ('noaa18_20080620_0050_99999_satproj_00000_13111_avhrr.h5',
                  (1, 409, 10150, 10154), 'AVHRR'),
    # Long tests
    'DAYMYD': ('MYD021KM.A2008172.0405.061.2018034142316.hdf',
               (0, 0, 0, 0), 'MODIS'),
    'NITMYD': ('MYD021KM.A2008172.1630.061.2018034143236.hdf',
               (0, 0, 0, 0), 'MODIS'),
    'NITMOD': ('MOD021KM.A2008172.1330.061.2017256012859.hdf',
               (0, 0, 0, 0), 'MODIS'),
    'DAYMOD': ('MOD021KM.A2008172.0115.061.2017256012659.hdf',
               (0, 0, 0, 0), 'MODIS'),
    'AATSR': ('ATS_TOA_1PRUPA20080620_002337_000065272069_00345_32964_'
              '0666.N1', (0, 0, 0, 0), 'AATSR'),
    'AVHRR': ('noaa18_20080620_0050_99999_satproj_00000_13111_avhrr.h5',
              (0, 0, 0, 0), 'AVHRR'),
    'DAYSLSTRA': ('S3A_SL_1_RBT____20181221T010953_20181221T011253_'
                  '20181222T091030_0180_039_202_3240_LN2_O_NT_003.SEN3',
                  (0, 0, 0, 0), 'SLSTR'),
    'NITSLSTRA': ('S3A_SL_1_RBT____20181221T133848_20181221T134148_'
                  '20181222T201511_0179_039_209_5760_LN2_O_NT_003.SEN3',
                  (0, 0, 0, 0), 'SLSTR'),
    'DAYSLSTRB': ('S3B_SL_1_RBT____20181221T002724_20181221T003024_'
                  '20181222T080855_0179_020_059_3060_LN2_O_NT_003.SEN3',
                  (0, 0, 0, 0), 'SLSTR'),
    'NITSLSTRB': ('S3B_SL_1_RBT____20181221T125919_20181221T130219_'
                  '20181222T201449_0179_020_066_5760_LN2_O_NT_003.SEN3',
                  (0, 0, 0, 0), 'SLSTR'),
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

