# Definitions of default values/paths for your local system

import os


# ===== PATHS =====

# Location of source code trunk from ORAC repository
ORAC_DIR = os.environ['CONDA_PREFIX'] + '/bin'
# Location of data directory from ORAC repository
REGRESS_IN_DIR = '/gws/pw/j07/nceo_aerosolfire/acpovey/orac_test/testinput'
REGRESS_OUT_DIR = ''
# Path to library file used to compile ORAC
ORAC_LIB = os.environ['CONDA_PREFIX'] + '/share/orac/lib.inc'

# Directory of look-up tables
SAD_DIRS = [
    '/gws/nopw/j04/nceo_generic/cloud_ecv/data_in/sad_dir2',
    '/gws/nopw/j04/aerosol_cci/aux/luts/orac_luts/cci_aerosol_A7X_upd',
    '/gws/nopw/j04/aerosol_cci/aux/luts/orac_luts/cci_aerosol_A7X_atsr2',
    '/gws/nopw/j04/aerosol_cci/aux/luts/orac_luts',
    '/gws/nopw/j04/oracluts/public'
]

# Use ECMWF data from the CEDA ERA5 archive
NWP_FLAG = 2

# If any of these directories contain subdirectories named by date, please use
# the syntax of datatime.strftime() to denote the folder structure.
AUXILIARIES = {
    # Directory containing RTTOV emissivity atlas
    'atlas_dir': '/gws/smf/j04/nceo_generic/ORAC/orac_ancillary_data/rttov/emis',
    # Directory of RTTOV instrument coefficient files
    'coef_dir': '/gws/smf/j04/nceo_generic/ORAC/orac_ancillary_data/rttov/coef',
    # Directory of MODIS emisivity retrievals
    'emis_dir': '/gws/nopw/j04/nceo_generic/cloud_ecv/data_in/emissivity',
    'camel_dir': '/gws/nopw/j04/nceo_generic/cloud_ecv/data_in/emissivity/camel',
    # Directories of MODIS surface BRDF retrievals
    'mcd43c1_dir': '/gws/nopw/j04/nceo_generic/cloud_ecv/data_in/modis/'
        'MCD43C1_MODIS_BRDF_neu/V006/%Y',
    'mcd43c3_dir': '/gws/nopw/j04/nceo_generic/cloud_ecv/data_in/modis/'
        'MCD43C3_MODIS_Albedo_neu/V006/%Y',
    # To use ECMWF data from the BADC/CEMS archive (ecmwf_flag == 2), specifiy
    # where each of the three types of file are stored
    #'ggam_dir': '/badc/ecmwf-era-interim/data/gg/am/%Y/%m/%d',
    #'ggas_dir': '/badc/ecmwf-era-interim/data/gg/as/%Y/%m/%d',
    #'spam_dir': '/badc/ecmwf-era-interim/data/sp/am/%Y/%m/%d',
    # To use ERA5 (ecmwf_flag == 0), specify their location
    'ecmwf_dir': '/gws/nopw/j04/cloud_ecv/data_in/era5',
    # Directory to store the EMOS interpolation file (CF_T0255_R0036000, which
    # will be generated if not already present)
    'emos_dir': os.environ['PPDIR'],
    # Directory of NSIDC ice/snow extent maps
    'nise_dir': '/gws/nopw/j04/nceo_generic/cloud_ecv/data_in/ice_snow',
    # Directory of Ocean Colour CCI retrievals
    'occci_dir': '/neodc/esacci/ocean_colour/data/v5.0-release/geographic/'
        'netcdf/iop/monthly/v5.0/%Y',
    # File containing AATSR calibration coefficients
    'calib_file': '/gws/nopw/j04/nceo_generic/cloud_ecv/data_in/'
        'AATSR_VIS_DRIFT_V03-00.DAT',
    # File containing the USGS land-use map
    'usgs_file': '/gws/nopw/j04/cloud_ecv/data_in2/global_lsm.nc',
    # Pre-defined geometry for geostationary imager
    'prelsm_file': '/gws/nopw/j04/cloud_ecv/data_in2/'
        'MSG_000E_LSM.nc',
    'pregeo_file': '/gws/nopw/j04/cloud_ecv/data_in2/'
        'MSG_000E_ANGLES.nc',
    # Climatology of Swansea s and p parameters
    'swansea_dir': '/gws/nopw/j04/cloud_ecv/data_in2/',
    # Strftime-formatted globs for MCD43 files
    'mcd43c3_name': 'MCD43C3.A%Y%j.*.hdf',
    'mcd43c1_name': 'MCD43C1.A%Y%j.*.hdf',
}
# Directory to store the EMOS interpolation file (CF_T0255_R0036000, which
# will be generated if not already present)
try:
    AUXILIARIES['emos_dir'] = os.environ['PPDIR']
except KeyError:
    AUXILIARIES['emos_dir'] = os.environ['CONDA_PREFIX'] + '/share/libemos/tables'

# ===== FILE HEADER INFORMATION =====

GLOBAL_ATTRIBUTES = {
    "cfconvention": "CF-1.4",
    "comments": "Processed on JASMIN.",
    "email": 'first.last@address.com',
    "history": "n/a",
    "institute": "Somewhere",
    "keywords": "aerosol; cloud; optimal estimation",
    "license": "https://github.com/ORAC-CC/orac/blob/master/COPYING",
    "processor": "ORAC",
    "product_name": "L2-CLOUD-AEROSOL",
    "project": "TEST",
    "references": "doi:10.5194/amt-5-1889-2012",
    "summary": "n/a",
    "url": "http://github.com/ORAC-CC/orac",
}


# ===== FOLDER NAME PREFERENCES =====

LOG_DIR = "log"
PRE_DIR = "pre"

EXTRA_LINES = {
    "land_extra": "",
    "sea_extra": "",
    "cld_extra": "",
}


# ===== DEFAULT RETRIEVAL SETTINGS =====

RETRIEVAL_SETTINGS = {}

# Permute a set of standard cloud retrievals over each sensor
_CLD_RETRIEVALS = {
    "wat_cld": "--phase WAT --ret_class ClsCldWat --approach AppCld1L "
               "--sub_dir cld",
    "ice_cld": "--phase ICE --ret_class ClsCldIce --approach AppCld1L "
               "--sub_dir cld",
    "wat_ice": "--phase ICE --ret_class ClsCldIce --approach AppCld2L "
               "--multilayer WAT ClsCldWat --sub_dir cld",
#    "ice_ice": "--phase ICE --ret_class ClsCldIce --approach AppCld2L "
#               "--multilayer ICE ClsCldIce --sub_dir cld"
}
_CLD_CHANNELS = {
    "AATSR": "--use_channels 2 3 4 5 6 7",
    "ATSR2": "--use_channels 2 3 4 5 6 7",
    "AVHRR": "--use_channels 1 2 3 4 5 6",
    "MODIS": "--use_channels 1 2 6 20 31 32",
    "SEVIRI": "--use_channels 1 2 3 4 9 10",
    "SLSTR": "--use_channels 2 3 5 7 8 9",
}
for sensor, channels in _CLD_CHANNELS.items():
    RETRIEVAL_SETTINGS[sensor + "_C"] = [
        channels + " " + ret for ret in _CLD_RETRIEVALS.values()
    ]

# Permute dual-view aerosol retrievals
_AER_PHASES = range(70, 80)
_AER_DUAL_RETRIEVALS = {
    "aer_ox": "--phase A{:02d} --ret_class ClsAerOx --approach AppAerOx "
              "--no_land --sub_dir sea",
    "aer_sw": "--phase A{:02d} --ret_class ClsAerSw --approach AppAerSw "
              "--no_sea --sub_dir land",
}
_AER_DUAL_CHANNELS = {
    "AATSR": "--use_channels 1 2 3 4 8 9 10 11",
    "ATSR2": "--use_channels 1 2 3 4 8 9 10 11",
    "SLSTR": "--use_channels 1 2 3 5 10 11 12 14",
}
for sensor, channels in _AER_DUAL_CHANNELS.items():
    RETRIEVAL_SETTINGS[sensor + "_A"] = [
        channels + " " + ret.format(phs)
        for ret in _AER_DUAL_RETRIEVALS.values() for phs in _AER_PHASES
    ]

# Permute single-view aerosol retrievals
_AER_SINGLE_RETRIEVALS = {
    "aer_o1": "--phase A{:02d} --ret_class ClsAerOx --approach AppAerO1 "
              "--no_land --sub_dir sea",
}
_AER_SINGLE_CHANNELS = {
    "AVHRR": "--use channels 1 2 3",
    "MODIS": "--use_channels 4 1 2 6",
    "SEVIRI": "--use_channels 1 2 3",
    "SLSTR": "--use_channels 1 2 3 5",
}
for sensor, channels in _AER_SINGLE_CHANNELS.items():
    RETRIEVAL_SETTINGS[sensor + "_A"] = [
        channels + " " + ret.format(phs)
        for ret in _AER_SINGLE_RETRIEVALS.values() for phs in _AER_PHASES
    ]

# Joint aerosol-cloud retrievals
for sensor, channels in _CLD_CHANNELS.items():
    RETRIEVAL_SETTINGS[sensor + "_J"] = [
        channels + " " + ret for ret in _CLD_RETRIEVALS.values()
    ]
for sensor, channels in _AER_DUAL_CHANNELS.items():
    RETRIEVAL_SETTINGS[sensor + "_J"].extend([
        channels + " " + ret.format(phs)
        for ret in _AER_DUAL_RETRIEVALS.values() for phs in _AER_PHASES
    ])
for sensor, channels in _AER_SINGLE_CHANNELS.items():
    RETRIEVAL_SETTINGS[sensor + "_J"].extend([
        channels + " " + ret.format(phs)
        for ret in _AER_SINGLE_RETRIEVALS.values() for phs in _AER_PHASES
    ])

# TEMPORARY removal of multilayer where no-ray tables don't exist
del RETRIEVAL_SETTINGS["ATSR2_C"][2]
del RETRIEVAL_SETTINGS["AATSR_C"][2]
del RETRIEVAL_SETTINGS["AVHRR_C"][2]
del RETRIEVAL_SETTINGS["SLSTR_C"][2]
del RETRIEVAL_SETTINGS["ATSR2_J"][2]
del RETRIEVAL_SETTINGS["AATSR_J"][2]
del RETRIEVAL_SETTINGS["AVHRR_J"][2]
del RETRIEVAL_SETTINGS["SLSTR_J"][2]

# Default to cloud retrievals
for sensor in _CLD_CHANNELS.keys():
    RETRIEVAL_SETTINGS[sensor] = RETRIEVAL_SETTINGS[sensor + "_C"]


# ===== DEFAULT CHANNELS FOR EACH SENSOR =====

CHANNELS = {
    "AATSR": (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11),
    "ATSR2": (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11),
    "AVHRR": (1, 2, 3, 4, 5, 6),
    "MODIS": (1, 2, 6, 20, 31, 32),
    "SEVIRI": (1, 2, 3, 4, 9, 10),
    "SLSTR": (1, 2, 3, 5, 7, 8, 9, 10, 11, 12, 14),
}

# ===== REGRESSION TEST SETTINGS =====

# Fields to ignore during regression testing
ATTS_TO_IGNORE = ('L2_Processor_Version', 'Production_Time', 'File_Name')
VARS_TO_ACCEPT = ('costjm', 'costja', 'niter')

# Tollerances in regression testing
RTOL = 1e-7 # Maximum permissable relative difference
ATOL = 1e-7 # Maximum permissable absolute difference

# Filters to apply regression test warnings
# (see https://docs.python.org/2/library/warnings.html#the-warnings-filter)
WARN_FILT = {
    'FieldMissing': 'once',
    'InconsistentDim': 'error',
    'RoundingError': 'once',
    'Acceptable': 'once',
}


# ===== BATCH PROCESSING SETTINGS =====

# The Oxford Yau cluster uses QSUB
from pyorac.batch import SLURM as BATCH

# Arguments that should be included in every call
BATCH_VALUES = {
    'email': GLOBAL_ATTRIBUTES["email"],
    'order': '-r15s:pg',
}

# Initialisation of script file
BATCH_SCRIPT = """#!/bin/bash --noprofile
"""

# Permissions given to any directories created by the script
DIR_PERMISSIONS = 0o774
