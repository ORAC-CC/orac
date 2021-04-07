# Definitions of default values/paths for your local system

from os import environ

# ===== PATHS =====

# Location of source code trunk from ORAC repository
orac_dir = environ['CONDA_PREFIX'] + '/bin'
# Location of data directory from ORAC repository
data_dir = '/group_workspaces/jasmin2/aerosol_cci/orac_testbed/data'
# Path to library file used to compile ORAC
orac_lib = environ['CONDA_PREFIX'] + '/share/orac/lib.inc'

# Directory of look-up tables
sad_dirs = [
    '/gws/nopw/j04/nceo_generic/cloud_ecv/data_in/sad_dir2',
    '/group_workspaces/jasmin2/aerosol_cci/aux/luts/orac_luts',
]

# Use ECMWF data from the BADC/CEMS archive
ecmwf_flag  = 2

# If any of these directories contain subdirectories named by date, please use
# the syntax of datatime.strftime() to denote the folder structure.
auxiliaries = {
    # Directory containing RTTOV emissivity atlas
    'atlas_dir' : '/gws/nopw/j04/nceo_generic/cloud_ecv/data_in/rttov121/'
        'emis_data',
    # Directory of RTTOV instrument coefficient files
    'coef_dir' : '/gws/nopw/j04/nceo_generic/cloud_ecv/data_in/rttov121/coeffs',
    # Directory of MODIS emisivity retrievals
    'emis_dir' : '/gws/nopw/j04/nceo_generic/cloud_ecv/data_in/emissivity',
    # Directories of MODIS surface BRDF retrievals
    'mcd43c1_dir' : '/gws/nopw/j04/nceo_generic/cloud_ecv/data_in/modis/'
        'MCD43C1_MODIS_BRDF_neu/V006/%Y',
    'mcd43c3_dir' : '/gws/nopw/j04/nceo_generic/cloud_ecv/data_in/modis/'
        'MCD43C3_MODIS_Albedo_neu/V006/%Y',
    # To use ECMWF data from the BADC/CEMS archive (ecmwf_flag == 2), specifiy
    # where each of the three types of file are stored
    'ggam_dir' : '/badc/ecmwf-era-interim/data/gg/am/%Y/%m/%d',
    'ggas_dir' : '/badc/ecmwf-era-interim/data/gg/as/%Y/%m/%d',
    'spam_dir' : '/badc/ecmwf-era-interim/data/sp/am/%Y/%m/%d',
    # Directory of high-resolution ECMWF files
    'hr_dir' : '/gws/nopw/j04/nceo_generic/cloud_ecv/data_in/ecmwf/%Y/%m/%d',
    # Directory of NSIDC ice/snow extent maps
    'nise_dir' : '/gws/nopw/j04/nceo_generic/cloud_ecv/data_in/ice_snow',
    # Directory of Ocean Colour CCI retrievals
    'occci_dir' : '/neodc/esacci/ocean_colour/data/v3.1-release/geographic/'
        'netcdf/iop/monthly/v3.1/%Y',
    # File containing AATSR calibration coefficients
    'calib_file' : '/gws/nopw/j04/nceo_generic/cloud_ecv/data_in/'
        'AATSR_VIS_DRIFT_V03-00.DAT',
    # File containing the USGS land-use map
    'usgs_file' : '/group_workspaces/jasmin2/cloud_ecv/data_in2/global_lsm.nc',
    # Pre-defined geometry for geostationary imager
    'prelsm_file' : '/group_workspaces/jasmin2/cloud_ecv/data_in2/'
        'MSG_000E_LSM.nc',
    'pregeo_file' : '/group_workspaces/jasmin2/cloud_ecv/data_in2/'
        'MSG_000E_ANGLES.nc',
    # Climatology of Swansea s and p parameters
    'swansea_dir' : ''
}
# Directory to store the EMOS interpolation file (CF_T0255_R0036000, which
# will be generated if not already present)
try:
    auxiliaries['emos_dir'] = environ['PPDIR']
except KeyError:
    auxiliaries['emos_dir'] = environ['CONDA_PREFIX'] + '/share/libemos/tables'

# ===== FILE HEADER INFORMATION =====

global_attributes = {
    "cfconvention" : "CF-1.4",
    "comments"     : "Processed on JASMIN.",
    "email"        : 'first.last@address.com',
    "history"      : "n/a",
    "institute"    : "Somewhere",
    "keywords"     : "aerosol; cloud; optimal estimation",
    "license"      : "https://github.com/ORAC-CC/orac/blob/master/COPYING",
    "processor"    : "ORAC",
    "product_name" : "L2-CLOUD-AEROSOL",
    "project"      : "TEST",
    "references"   : "doi:10.5194/amt-5-1889-2012",
    "summary"      : "n/a",
    "url"          : "http://github.com/ORAC-CC/orac",
}


# ===== FOLDER NAME PREFERENCES =====

log_dir = "log"
pre_dir = "pre"

extra_lines = {
    "land_extra" : "",
    "sea_extra"  : "",
    "cld_extra"  : "",
}


# ===== DEFAULT RETRIEVAL SETTINGS =====

retrieval_settings = {}

# Permute a set of standard cloud retrievals over each sensor
cld_retrievals = {
    "wat_cld" : "--phase WAT --ret_class ClsCldWat --approach AppCld1L "
                "--sub_dir cld",
    "ice_cld" : "--phase ICE --ret_class ClsCldIce --approach AppCld1L "
                "--sub_dir cld",
    "wat_ice" : "--phase WAT --ret_class ClsCldWat --approach AppCld2L "
                "--multilayer ICE ClsCldIce --sub_dir cld",
#    "ice_ice" : "--phase ICE --ret_class ClsCldIce --approach AppCld2L "
#                "--multilayer ICE ClsCldIce --sub_dir cld"
}
cld_channels = {
    "AATSR" : "--use_channels 2 3 4 5 6 7",
    "ATSR2" : "--use_channels 2 3 4 5 6 7",
    "AVHRR" : "--use_channels 1 2 3 4 5 6",
    "MODIS" : "--use_channels 1 2 6 20 31 32",
    "SEVIRI" : "--use_channels 1 2 3 4 9 10",
    "SLSTR" : "--use_channels 2 3 5 7 8 9",
}
for sensor, channels in cld_channels.items():
    retrieval_settings[sensor + "_C"] = [
        channels + " " + ret for ret in cld_retrievals.values()
    ]

# Permute dual-view aerosol retrievals
aer_phases = range(70, 80)
aer_dual_retrievals = {
    "aer_ox" : "--phase A{:02d} --ret_class ClsAerOx --approach AppAerOx "
               "--no_land --sub_dir sea",
    "aer_sw" : "--phase A{:02d} --ret_class ClsAerSw --approach AppAerSw "
               "--no_sea --sub_dir land",
}
aer_dual_channels = {
    "AATSR" : "--use_channels 1 2 3 4 8 9 10 11",
    "ATSR2" : "--use_channels 1 2 3 4 8 9 10 11",
    "SLSTR" : "--use_channels 1 2 3 5 10 11 12 14",
}
for sensor, channels in aer_dual_channels.items():
    retrieval_settings[sensor + "_A"] = [
        channels + " " + ret.format(phs)
        for ret in aer_dual_retrievals.values() for phs in aer_phases
    ]

# Permute single-view aerosol retrievals
aer_single_retrievals = {
    "aer_o1" : "--phase A{:02d} --ret_class ClsAerOx --approach AppAerO1 "
               "--no_land --sub_dir sea",
}
aer_single_channels = {
    "AVHRR" : "--use channels 1 2 3",
    "MODIS" : "--use_channels 4 1 2 6",
    "SEVIRI" : "--use_channels 1 2 3",
    "SLSTR" : "--use_channels 1 2 3 5",
}
for sensor, channels in aer_single_channels.items():
    retrieval_settings[sensor + "_A"] = [
        channels + " " + ret.format(phs)
        for ret in aer_single_retrievals.values() for phs in aer_phases
    ]

# Joint aerosol-cloud retrievals
for sensor, channels in cld_channels.items():
    retrieval_settings[sensor + "_J"] = [
        channels + " " + ret for ret in cld_retrievals.values()
    ]
for sensor, channels in aer_dual_channels.items():
    retrieval_settings[sensor + "_J"].extend([
        channels + " " + ret.format(phs)
        for ret in aer_dual_retrievals.values() for phs in aer_phases
    ])
for sensor, channels in aer_single_channels.items():
    retrieval_settings[sensor + "_J"].extend([
        channels + " " + ret.format(phs)
        for ret in aer_single_retrievals.values() for phs in aer_phases
    ])

# Default to cloud retrievals
for sensor in cld_channels.keys():
    retrieval_settings[sensor] = retrieval_settings[sensor + "_C"]


# ===== DEFAULT CHANNELS FOR EACH SENSOR =====

channels = {
    "AATSR" : (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11),
    "ATSR2" : (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11),
    "AVHRR" : (1, 2, 3, 4, 5, 6),
    "MODIS" : (1, 2, 6, 20, 31, 32),
    "SEVIRI" : (1, 2, 3, 4, 9, 10),
    "SLSTR" : (1, 2, 3, 5, 7, 8, 9, 10, 11, 12, 14),
}

# ===== REGRESSION TEST SETTINGS =====

# Fields to ignore during regression testing
atts_to_ignore = ('L2_Processor_Version', 'Production_Time', 'File_Name')
vars_to_accept = ('costjm', 'costja', 'niter')

# Tollerances in regression testing
rtol = 1e-7 # Maximum permissable relative difference
atol = 1e-7 # Maximum permissable absolute difference

# Filters to apply regression test warnings
# (see https://docs.python.org/2/library/warnings.html#the-warnings-filter)
warn_filt = {
    'FieldMissing'    : 'once',
    'InconsistentDim' : 'error',
    'RoundingError'   : 'once',
    'Acceptable'      : 'once',
}


# ===== BATCH PROCESSING SETTINGS =====

# The Oxford Yau cluster uses QSUB
from pyorac.batch import bsub as batch

# Arguments that should be included in every call
batch_values = {
    'email'    : global_attributes["email"],
    'order'    : '-r15s:pg',
}

# Initialisation of script file
batch_script = """#!/bin/bash --noprofile
"""
