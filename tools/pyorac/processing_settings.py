"""Settings that all groups should share and not change that often.

REGRESSION_TESTS
    dict of tuples specifying the standard regression tests, where
    the tuple elements are,
    [0] name of the L1B file to open;
    [1] processing limits in the form (x0, x1, y0, y1) where those
        values are in Fortran coordinates;
    [2] name of the sensor passed to ORAC. {TODO: automate that}
    Test names follow the convention that,
    - suffix S indicates a short test of a few hundred pixels;
    - prefix DAY/NIT indicates a full day/night granule or test;

APRIORI_LOOKUP
    dict of dicts providing first guess, apriori value and uncertainty
    as a function of particle type
"""


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


# Things that are used by lots of particles
_standard_aerosol_optical_depth_prior = {'AP': 0.10, 'FG': 0.10, 'SX': 1.5}

# Set priors for all particle types
APRIORI_LOOKUP = {
    None: {},
    "large_clean_aerosol": {
        'ITau': _standard_aerosol_optical_depth_prior,
        'IRe': {'AP': 1.22, 'FG': 1.22, 'SX': 0.15},
    },
    "small_natural_aerosol": {
        'ITau': _standard_aerosol_optical_depth_prior,
        'IRe': {'AP': 0.553, 'FG': 0.553, 'SX': 0.15},
    },
    "manmade_aerosol": {
        'ITau': _standard_aerosol_optical_depth_prior,
        'IRe': {'AP': 0.908, 'FG': 0.908, 'SX': 0.15},
    },
    "biomass_burning": {
        'ITau': _standard_aerosol_optical_depth_prior,
        'IRe': {'AP': 0.142, 'FG': 0.142, 'SX': 0.15},
    },
    "eyja_ash": {
        'ITau': {'AP': 1.51, 'FG': 1.51, 'SX': 1.5},
        'IRe': {'AP': 0.7, 'FG': 0.7, 'SX': 1.5},
    },
}
