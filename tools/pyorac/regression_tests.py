"""Routines implementing the regression tests for the ORAC repository."""
from warnings import warn

import pyorac.local_defaults as defaults
from pyorac.definitions import (Acceptable, FieldMissing, InconsistentDim,
                                OracWarning, Regression, RoundingError)


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
    'DAYSLSTRAS': ('S3A_SL_1_RBT____20181221T010953_20181221T011253_'
                   '20181222T091030_0180_039_202_3240_LN2_O_NT_003.SEN3',
                   (250, 799, 430, 434), 'SLSTR'),
    'NITSLSTRAS': ('S3A_SL_1_RBT____20181221T133848_20181221T134148_'
                   '20181222T201511_0179_039_209_5760_LN2_O_NT_003.SEN3',
                   (100, 899, 280, 284), 'SLSTR'),
    'DAYSLSTRBS': ('S3B_SL_1_RBT____20181221T002724_20181221T003024_'
                   '20181222T080855_0179_020_059_3060_LN2_O_NT_003.SEN3',
                   (400, 1099, 790, 794), 'SLSTR'),
    'NITSLSTRBS': ('S3B_SL_1_RBT____20181221T125919_20181221T130219_'
                   '20181222T201449_0179_020_066_5760_LN2_O_NT_003.SEN3',
                   (200, 1099, 555, 559), 'SLSTR'),
    'DAYSEVIRIS': ('MSG3-SEVI-MSG15-0100-NA-20170620055740.316000000Z-'
                   '20170620055758-1314114-37.nat',
                   (300, 799, 1196, 1200), 'SEVIRI'),
    'NITSEVIRIS': ('MSG3-SEVI-MSG15-0100-NA-20170620055740.316000000Z-'
                   '20170620055758-1314114-37.nat',
                   (2700, 3199, 1000, 1004), 'SEVIRI'),
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
    'SEVIRI': ('MSG3-SEVI-MSG15-0100-NA-20170620055740.316000000Z-'
               '20170620055758-1314114-37.nat', (0, 0, 0, 0), 'SEVIRI'),
}


def compare_nc_atts(dat0, dat1, filename, var):
    """Report if the attributes of a NCDF file or variable have changed."""

    # Check if any attributes added/removed
    atts = set(dat0.ncattrs()).symmetric_difference(dat1.ncattrs())
    if atts:
        warn(FieldMissing(filename, ', '.join(atts)), stacklevel=3)

    # Check if any attributes changed
    for key in dat0.ncattrs():
        if key in atts:
            continue

        if (dat0.__dict__[key] != dat1.__dict__[key] and
                key not in defaults.ATTS_TO_IGNORE):
            warn(Regression(
                filename, var + ', ' + key, 'warning',
                'Changed attribute ({} vs {})'.format(
                    dat0.__dict__[key], dat1.__dict__[key]
                )
            ), stacklevel=3)


def compare_orac_out(file0, file1):
    """Compare two NCDF files"""
    import numpy as np
    try:
        from netCDF4 import Dataset

    except ImportError:
        warn('Skipping regression tests as netCDF4 unavailable',
             OracWarning, stacklevel=2)
        return

    try:
        # Open files
        dat0 = Dataset(file0, 'r')
        dat1 = Dataset(file1, 'r')

        # Check if any dimensions added/removed
        dims = set(dat0.dimensions.keys()).symmetric_difference(
            dat1.dimensions.keys()
        )
        if dims:
            # Not bothering to identify which file contains the errant field
            warn(FieldMissing(file1, ', '.join(dims)), stacklevel=2)

        # Check if any dimensions changed
        for key in dat0.dimensions.keys():
            if key in dims:
                continue

            if dat0.dimensions[key].size != dat1.dimensions[key].size:
                warn(InconsistentDim(file1, key, dat0.dimensions[key].size,
                                     dat1.dimensions[key].size),
                     stacklevel=2)

            # Check attributes
            compare_nc_atts(dat0, dat1, file1, key)

        # Check if any variables added/removed
        variables = set(dat0.variables.keys()).symmetric_difference(
            dat1.variables.keys()
        )
        if variables:
            warn(FieldMissing(file1, ', '.join(variables)), stacklevel=2)

        # Check if any variables changed
        for key in dat0.variables.keys():
            if key in variables:
                continue

            att0 = dat0.variables[key]
            att1 = dat1.variables[key]

            # Turn off masking, so completely NaN fields can be equal
            att0.set_auto_mask(False)
            att1.set_auto_mask(False)

            compare_nc_atts(att0, att1, file1, key)

            if att0.size != att1.size:
                warn(InconsistentDim(file1, key, att0.size, att1.size), stacklevel=2)
                continue

            # Check if there has been any change
            if not np.allclose(att0, att1, equal_nan=True, rtol=0, atol=0):
                test = False

                # For floats, check if variation is acceptable
                if att0.dtype.kind == 'f':
                    test = np.allclose(
                        att0, att1, equal_nan=True, rtol=defaults.RTOL,
                        atol=defaults.ATOL
                    )
                else:
                    try:
                        if isinstance(att0.scale_factor, np.floating):
                            # Packed floats consider the scale factor
                            test = np.allclose(
                                att0, att1, equal_nan=True, rtol=defaults.RTOL,
                                atol=max(att0.scale_factor, defaults.ATOL)
                            )
                    except AttributeError:
                        # If there is no scale factor, treat as an integer
                        pass

                if test or key in defaults.VARS_TO_ACCEPT:
                    warn(Acceptable(file1, key), stacklevel=2)
                else:
                    warn(RoundingError(file1, key), stacklevel=2)
    except IOError:
        pass

    finally:
        if 'dat0' in locals() or 'dat0' in globals():
            dat0.close()
        if 'dat1' in locals() or 'dat1' in globals():
            dat1.close()
