# Definitions of default values/paths for your local system
import os

# ===== PATHS =====
stack_size = 250000

# Location of source code trunk from ORAC repository
# orac_dir  = '/network/aopp/matin/eodg/shared/orac/orac'
orac_dir = '/home/prataa/orac_fork/orac'

# Location of data directory from ORAC repository
# data_dir  = '/network/aopp/matin/eodg/shared/orac/data'
data_dir = '/home/prataa/data'

# Path to library file used to compile ORAC
try:
    orac_lib  = os.environ["ORAC_LIB"]
except KeyError:
    orac_lib = orac_dir + '/config/lib.inc'

# Directory of look-up tables
sad_dirs = ['/network/aopp/apres/ORAC_LUTS',
            '/network/aopp/matin/eodg/shared/SAD_Files/HIM',]

# Use ECMWF data from the BADC/CEMS archive
ecmwf_flag  = 5

# If any of these directories contain subdirectories named by date, please use
# the syntax of datatime.strftime() to denote the folder structure.
auxiliaries = {
    # Directory containing RTTOV emissivity atlas
    'atlas_dir'   : '/network/aopp/matin/eodg/shared/rtcoef_rttov12/emis_data',
    # Directory of RTTOV instrument coefficient files
    'coef_dir'    : '/network/aopp/matin/eodg/shared/rtcoef_rttov12',
    # Directory of MODIS emisivity retrievals
    'emis_dir'    : '/network/group/aopp/eodg/modis/MYD11',
    'camel_dir'   : '/network/group/aopp/eodg/modis/CAMEL',
    # Directories of MODIS surface BRDF retrievals
    'mcd43c1_dir' : '/network/aopp/matin/eodg/modis/MCD43/C1',
    'mcd43c3_dir' : '/network/aopp/matin/eodg/modis/MCD43/C3',
    # To use ECMWF data from the BADC/CEMS archive (ecmwf_flag == 2), specifiy
    # where each of the three types of file are stored
    'ggam_dir'    : '/network/aopp/matin/eodg/ecmwf/era_interim/%Y/%m/%d',
    'ggas_dir'    : '/network/aopp/matin/eodg/ecmwf/era_interim/%Y/%m/%d',
    'spam_dir'    : '/network/aopp/matin/eodg/ecmwf/era_interim/%Y/%m/%d',
    # To use a single ECMWF file (ecmwf_flag == 0), specify their location
    #'ecmwf_dir'   : '/local/home/povey/eodg/povey/data/ecmwf',
    # To use ERA5 (ecmwf_flag == 0), specify their location
    'ecmwf_dir'   : '/network/aopp/matin/eodg/ecmwf/ERA5/%Y',
    # Directory of high-resolution ECMWF files
    'hr_dir'      : '/network/aopp/matin/eodg/ecmwf/era_hr',
    # Directory to store the EMOS interpolation file (CF_T0255_R0036000, which
    # will be generated if not already present)
    'emos_dir'    : '/network/aopp/matin/eodg/shared/emos_files',
    # Directory of NSIDC ice/snow extent maps
    'nise_dir'    : '/network/aopp/matin/eodg/users/mcgarragh/data/n5eil01u.ecs.'
                    'nsidc.org/SAN/OTHR',
    # Directory of Ocean Colour CCI retrievals
    'occci_dir'   : '/network/aopp/matin/eodg/shared/orac/data/occci/v4.2',
    # File containing AATSR calibration coefficients
    'calib_file'  : '/network/aopp/matin/eodg/shared/AATSR_VIS_DRIFT_V03-00.DAT',
    # File containing the USGS land-use map
    'usgs_file'   : '/network/aopp/matin/eodg/shared/Aux_file_CM_SAF_AVHRR_'
                    'GAC_ori_0.05deg.nc',
    # Pre-defined geometry for geostationary imager
    'prelsm_file' : '/network/aopp/matin/eodg/shared/orac/data/predef_lsm/'
                    #'MSG_000E_LSM.nc',  # SEVIRI
                    'AHI_141E_LSM.nc',  # AHI
    'pregeo_file' : '/network/aopp/matin/eodg/shared/orac/data/predef_lsm/'
                    #'MSG_000E_ANGLES.v01.nc',  # SEVIRI
                    'AHI_141E_ANGLES.nc',  # AHI
    # Climatology of Swansea s and p parameters
    'swansea_dir' : '/network/aopp/matin/eodg/atsr/swansea_cb'
}


# ===== FILE HEADER INFORMATION =====

global_attributes = {
    "cfconvention" : "CF-1.4",
    "comments"     : "n/a",
    "email"        : 'andrew.prata@physics.ox.ac.uk',
    "history"      : "n/a",
    "institute"    : "UoOx",
    "keywords"     : "aerosol; cloud; optimal estimation",
    "license"      : "https://github.com/ORAC-CC/orac/blob/master/COPYING",
    "processor"    : "ORAC",
    "product_name" : "L2-CLOUD-CLD",
    "project"      : "ESACCI",
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
    "wat_ice" : "--phase ICE --ret_class ClsCldIce --approach AppCld2L "
                "--multilayer WAT ClsCldWat --sub_dir cld",
#    "ice_ice" : "--phase ICE --ret_class ClsCldIce --approach AppCld2L "
#                "--multilayer ICE ClsCldIce --sub_dir cld"
}
cld_channels = {
    "AATSR" : "--use_channels 2 3 4 5 6 7",
    "ATSR2" : "--use_channels 2 3 4 5 6 7",
    "AVHRR" : "--use_channels 1 2 3 4 5 6",
    "MODIS" : "--use_channels 1 2 6 20 31 32",
    # Standard ORAC cloud channels
    #"SEVIRI" : "--use_channels 1 2 3 4 9 10",
    # All channels
    # "SEVIRI" : "--use_channels 1 2 3 4 5 6 7 9 10 11",
    # All channels without 3.9 micron
    "SEVIRI" : "--use_channels 1 2 3 5 6 7 9 10 11",
    # Wang et al. (2016) thermal channels incl. 3.9 micron
    #"SEVIRI" : "--use_channels 4 5 6 7 9 10 11",
    "SLSTR" : "--use_channels 2 3 5 7 8 9",
    # Use all AHI channels for cloud retrievals
    #"AHI" : "--use_channels 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16"
    # 1.6 micron run (leave out 2.3 and 3.9 micron channels)
    #"AHI" : "--use_channels 1 2 3 4 5 8 9 10 11 12 13 14 15 16"
    # 2.3 micron run (leave out 1.6 and 3.9 micron channels)
    #"AHI" : "--use_channels 1 2 3 4 6 8 9 10 11 12 13 14 15 16"
    # 3.9 micron run (leave out 1.6 and 2.3 micron channels)
    #"AHI" : "--use_channels 1 2 3 4 7 8 9 10 11 12 13 14 15 16"
    # 1.6 run without 7.3 micron (leave out 2.3, 3.9 and 7.3 micron channels)
    "AHI" : "--use_channels 1 2 3 4 5 8 9 11 12 13 14 15 16"
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
    # Volcanic ash over sea
    # "aer_o1" : "--phase EYJ --ret_class ClsAshEyj --approach AppAerOx "
    #            "--no_land --sub_dir sea",
}
aer_single_channels = {
    "AVHRR" : "--use channels 1 2 3",
    "MODIS" : "--use_channels 4 1 2 6",
    "SEVIRI" : "--use_channels 1 2 3",
#    "AHI" : "--use_channels 1 2 3",
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

# Experimenting with volcanic ash retrievals
ash_retrievals = {
    "ash_cld" : "--phase EYJ --ret_class ClsAshEyj --approach AppCld1L "
                "--sub_dir ash",
    "wat_ash" : "--phase EYJ --ret_class ClsAshEyj --approach AppCld2L "
                "--multilayer WAT ClsCldWat --sub_dir ash",
    "ice_ash" : "--phase EYJ --ret_class ClsAshEyj --approach AppCld2L "
                "--multilayer ICE ClsCldIce --sub_dir ash"
}
ash_channels = {
    # Use all AHI channels
    #"AHI" : "--use_channels 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16"
    # 1.6 micron run (leave out 2.3 and 3.9 micron channels)
    #"AHI" : "--use_channels 1 2 3 4 5 8 9 10 11 12 13 14 15 16"
    # 2.3 micron run (leave out 1.6 and 3.9 micron channels)
    #"AHI" : "--use_channels 1 2 3 4 6 8 9 10 11 12 13 14 15 16"
    # 3.9 micron run (leave out 1.6 and 2.3 micron channels)
    #"AHI" : "--use_channels 1 2 3 4 7 8 9 10 11 12 13 14 15 16"
    # 1.6 run without 7.3 micron (leave out 2.3, 3.9 and 7.3 micron channels)
    #"AHI" : "--use_channels 1 2 3 4 5 8 9 11 12 13 14 15 16"
    # Prata AT channel selections (10.4, 11.2, 12.4 and 13.3 micron)
    #"AHI" : "--use_channels 13 14 15 16"
    # Francis et al. measurement vector (10.8, 12 and 13.4 micron)
    #"AHI" : "--use_channels 14 15 16"
    # Pavolonis et al. measurement vector (11, 11-12 and 11-13.3 micron)
    #"AHI" : "--use_channels ???" TODO: use channel BTDs in measurment vector
    # Thermal channels except 7.3 micron and 8.6 micron
    "AHI" : "--use_channels 7 8 9 12 13 14 15 16"
    }

# Set ash retrievals
for sensor, channels in ash_channels.items():
    retrieval_settings[sensor + "_ASH"] = [
        channels + " " + ret for ret in ash_retrievals.values()
    ]

# Setup ash and cloud joint retrievals
for sensor, channels in cld_channels.items():
    retrieval_settings[sensor + "_ASH_J"] = [
        channels + " " + ret for ret in cld_retrievals.values()
    ]
for sensor, channels in ash_channels.items():
    retrieval_settings[sensor + "_ASH_J"].extend([
        channels + " " + ret for ret in ash_retrievals.values()
    ])

# 'AHI_ASH': ['--use_channels 1 2 3 4 5 8 9 11 12 13 14 15 16 --phase EYJ --ret_class ClsAshEyj --approach AppCld1l --sub_dir ash',
# 						'--use_channels 1 2 3 4 5 8 9 11 12 13 14 15 16 --phase EYJ --ret_class ClsCldEyj --approach AppCld2L --multilayer WAT ClsCldWat --sub_dir ash',]
# 						#'--use_channels 1 2 3 4 5 8 9 11 12 13 14 15 16 --phase EYJ --ret_class ClsCldEyj --approach AppCld2L --multilayer ICE ClsCldIce --sub_dir ash']

# print('AHI_ASH')
# print(retrieval_settings['AHI_ASH'])

print('AHI_ASH_J')
print(retrieval_settings['AHI_ASH_J'])

# ===== DEFAULT CHANNELS FOR EACH SENSOR =====

channels = {
    "AATSR" : (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11),
    "ATSR2" : (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11),
    "AVHRR" : (1, 2, 3, 4, 5, 6),
    "MODIS" : (1, 2, 6, 20, 31, 32),
    "SEVIRI" : (1, 2, 3, 4, 9, 10),
    "SLSTR" : (1, 2, 3, 5, 7, 8, 9, 10, 11, 12, 14),
    "AHI" : (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16),
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
from pyorac.batch import slurm as batch

# Arguments that should be included in every call
batch_values = {
    'email'    : global_attributes["email"],
    'priority' : 10000,
    'queue'    : 'shared',
}

# Initialisation of script file
batch_script = """#!/bin/bash --noprofile
set -e
"""
