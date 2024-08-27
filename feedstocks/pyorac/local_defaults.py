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
    #'emos_dir': os.environ['PPDIR'],
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
    'prelsm_file': '/gws/nopw/j04/cloud_ecv/data_in2/MSG_000E_LSM.nc',
    'pregeo_file': '/gws/nopw/j04/cloud_ecv/data_in2/MSG_000E_ANGLES.nc',
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
    "institute": "JASMIN",
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
    "SEVIRI": (1, 2, 3, 4, 5, 6, 7, 9, 10, 11),
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


# ===== LUT SETTINGS =====

"""LUT_LOOKUP
    dict of functions. Keys are the names used to refer to different
    particle types (e.g. WAT, cci70, biomass-burning) and will be
    used in the ORAC filename as this is passed to Ctrl%LUTclass.
    Functions should take two arguments,
      instrument (pyorac.definition.FileName): sensor description;
      rayleigh (bool): sets if Rayleigh scattering is included in LUT.
    Functions should return a 4-tuple of,
      sad_dir (str): path to the desired LUT;
      sad_file (str): name of the NCDF file to open OR None for text tables;
      particle_type (str): LUT name passed to ORAC;
      prior_type (str): key to pass to APRIORI_LOOKUP which describes
        the a-priori information to pass via driver file OR None.
"""

_AEROSOL = "/gws/nopw/j04/aerosol_cci/aux/luts/orac_luts"
_NCDF_DIR = "/gws/nopw/j04/oracluts/public"

def _ncdf_lut_filename(inst, label, microphysical_model, atmospheric_model=2,
                       rayleigh=True, srf=False):
    """Constructs name of NCDF LUT file following Don's convention

    inst - pyorac.definitions.FileName to label sensor
    label - label for particle classification
    mb  - 'm' for monochromatic calculation or 'b' for band calculation
           weighted by the spectral response function
    atmospheric_model - code denoting the atmospheric model where
                         00: cloud, no Rayleigh scattering
                         01: cloud, includes Rayleigh scattering
                         1X: aerosol, where X denotes the gaseous model where
                                1 = Tropical Atmosphere
                                2 = Midlatitude Summer
                                3 = Midlatitude Winter
                                4 = Subarctic Summer
                                5 = Subarctic Winter
                                6 = 1976 US Standard
    microphysical_model - Three digit code to represent particle microphysics.
                          Backwards compatible with old LUT naming
                          e.g. a70 for aerosol dust model
    version - LUT version
    """

    sensor_name = inst.sensor.lower()
    if sensor_name == 'avhrr':
        sensor_name += '1'

    mb = 'b' if srf else 'm'

    if label == 'aerosol':
        assert rayleigh
        atm_model = atmospheric_model + 10
    else:
        atm_model = 1 if rayleigh else 0

    if inst.sensor == 'EarthCARE':
        version = 21
    elif inst.sensor in ('AATSR', 'SLSTR'):
        version = 12
    elif inst.sensor in ('AVHRR', 'MODIS', 'SEVIRI'):
        version = 11
    elif inst.sensor in ('AHI'):
        version = 9
    else:
        version = 7

    return (f'{inst.ncdf_sad_platform}_{sensor_name}_{mb}_{label}_'
            f'a{atm_model:02d}_p{microphysical_model}_v{version:02d}.nc')

def _text_lut_filename(inst, name):
    """Constructs name of text LUT following traditional convention"""

    if inst.sensor in ('ATSR2', 'AATSR'):
        instrument = inst.sensor
    else:
        instrument = inst.sensor + "-" + inst.text_sad_platform
    return f'{instrument}_{name}_RD_Ch*.sad'

def _jasmin_cloud_text_lut_directory(inst, name, rayleigh=True, srf=False):
    """Constructs path to text LUT on JASMIN Cloud CCI workspace"""

    if inst.sensor == 'AVHRR':
        fdr_name = f"avhrr-{inst.avhrr_type:d}_{name}"
    else:
        fdr_name = inst.sensor.lower() + "_" + name
    if srf:
        fdr_name += '_srf'
    if not rayleigh:
        fdr_name += '_no_ray'

    return _CLOUD + '/sad_dir2/' + fdr_name

def _jasmin_aerosol_text_lut_directory(inst):
    """Constructs path to text LUT on JASMIN Aerosol CCI workspace"""

    if inst.sensor == 'AATSR':
        return _CLOUD + "/sad_dir2/aatsr_CCI_A70-A79"
        #return _AEROSOL + "/cci_aerosol_A7X_upd"
    if inst.sensor == 'ATSR2':
        return _AEROSOL + "/cci_aerosol_A7X_atsr2"
    if inst.sensor == 'SLSTR':
        return _AEROSOL
    if inst.sensor == 'SEVIRI':
        return _CLOUD + "/sad_dir2/seviri_A70-A79"
    raise NotImplementedError(f"Sensor {inst.sensor} not available")

LUT_LOOKUP = {
    # Text cloud tables
    'WAT': lambda inst, rayleigh: (
        _jasmin_cloud_text_lut_directory(inst, 'WAT', rayleigh),
        None, 'WAT', None
    ),
    'ICE': lambda inst, rayleigh: (
        _jasmin_cloud_text_lut_directory(inst, 'ICE_baum', rayleigh),
        None, 'ICE', None
    ),
    'ICE-baran': lambda inst, rayleigh: (
        _jasmin_cloud_text_lut_directory(inst, 'ICE_baran', rayleigh),
        None, 'ICE', None
    ),
    'WAT-srf': lambda inst, rayleigh: (
        _jasmin_cloud_text_lut_directory(inst, 'WAT_srf', rayleigh, True),
        None, 'WAT', None
    ),
    'ICE-srf': lambda inst, rayleigh: (
        _jasmin_cloud_text_lut_directory(inst, 'ICE_baum_srf', rayleigh, True),
        None, 'ICE', None
    ),
    # Text aerosol tables
    'A70': lambda inst, _: (
        _jasmin_aerosol_text_lut_directory(inst), None, 'A70', 'large_clean_aerosol'
    ),
    'A71': lambda inst, _: (
        _jasmin_aerosol_text_lut_directory(inst), None, 'A71', 'small_natural_aerosol'
    ),
    'A72': lambda inst, _: (
        _jasmin_aerosol_text_lut_directory(inst), None, 'A72', 'small_natural_aerosol'
    ),
    'A73': lambda inst, _: (
        _jasmin_aerosol_text_lut_directory(inst), None, 'A73', 'small_natural_aerosol'
    ),
    'A74': lambda inst, _: (
        _jasmin_aerosol_text_lut_directory(inst), None, 'A74', 'small_natural_aerosol'
    ),
    'A75': lambda inst, _: (
        _jasmin_aerosol_text_lut_directory(inst), None, 'A75', 'manmade_aerosol'
    ),
    'A76': lambda inst, _: (
        _jasmin_aerosol_text_lut_directory(inst), None, 'A76', 'large_clean_aerosol'
    ),
    'A77': lambda inst, _: (
        _jasmin_aerosol_text_lut_directory(inst), None, 'A77', 'manmade_aerosol'
    ),
    'A78': lambda inst, _: (
        _jasmin_aerosol_text_lut_directory(inst), None, 'A78', 'small_natural_aerosol'
    ),
    'A79': lambda inst, _: (
        _jasmin_aerosol_text_lut_directory(inst), None, 'A79', 'biomass_burning'
    ),
    'EYJ': lambda inst, _: (
        _jasmin_aerosol_text_lut_directory(inst), None, 'EJY', 'eyja_ash'
    ),
    # NCDF cloud tables
    'liquid-water': lambda inst, ray: (
        _NCDF_DIR,
        _ncdf_lut_filename(inst, 'liquid-water', 'old', rayleigh=ray),
        'liquid-water',
        None
    ),
    'updated-water': lambda inst, ray: (
        _NCDF_DIR,
        _ncdf_lut_filename(inst, 'liquid-water', 'stg', rayleigh=ray),
        'liquid-water-stg',
        None
    ),
    '240k-water': lambda inst, ray: (
        _NCDF_DIR,
        _ncdf_lut_filename(inst, 'liquid-water', '240', rayleigh=ray),
        'liquid-water240',
        None
    ),
    '253k-water': lambda inst, ray: (
        _NCDF_DIR,
        _ncdf_lut_filename(inst, 'liquid-water', '253', rayleigh=ray),
        'liquid-water253',
        None
    ),
    '263k-water': lambda inst, ray: (
        _NCDF_DIR,
        _ncdf_lut_filename(inst, 'liquid-water', '263', rayleigh=ray),
        'liquid-water263',
        None
    ),
    '273k-water': lambda inst, ray: (
        _NCDF_DIR,
        _ncdf_lut_filename(inst, 'liquid-water', '273, rayleigh=ray'),
        'liquid-water273',
        None
    ),
    'water-ice': lambda inst, ray: (
        _NCDF_DIR,
        _ncdf_lut_filename(inst, 'water-ice', 'agg', rayleigh=ray),
        'water-ice-agg',
        None
    ),
    'ghm-ice': lambda inst, ray: (
        _NCDF_DIR,
        _ncdf_lut_filename(inst, 'water-ice', 'ghm', rayleigh=ray),
        'water-ice',
        None
    ),
    'spherical-ice': lambda inst, ray: (
        _NCDF_DIR,
        _ncdf_lut_filename(inst, 'water-ice', 'sph', rayleigh=ray),
        'water-ice-sph',
        None
    ),
    'src-ice': lambda inst, ray: (
        _NCDF_DIR,
        _ncdf_lut_filename(inst, 'water-ice', 'src', rayleigh=ray),
        'water-ice-src',
        None
    ),
    'h2so4': lambda inst, ray: (
        _NCDF_DIR,
        _ncdf_lut_filename(inst, 'sulphuric-acid', 'sa1', rayleigh=ray),
        'sulphuric-acid',
        None
    ),
    'biomass-a': lambda inst, ray: (
        _NCDF_DIR,
        _ncdf_lut_filename(inst, 'biomass', 'bba', rayleigh=ray),
        'biomass-a',
        'biomass_burning'
    ),
    'biomass-b': lambda inst, ray: (
        _NCDF_DIR,
        _ncdf_lut_filename(inst, 'biomass', 'bbb', rayleigh=ray),
        'biomass-b',
        'biomass_burning'
    ),
    'biomass-c': lambda inst, ray: (
        _NCDF_DIR,
        _ncdf_lut_filename(inst, 'biomass', 'bbc', rayleigh=ray),
        'biomass-c',
        'biomass_burning'
    ),
    'biomass-d': lambda inst, ray: (
        _NCDF_DIR,
        _ncdf_lut_filename(inst, 'biomass', 'bbd', rayleigh=ray),
        'biomass-d',
        'biomass_burning'
    ),
    'biomass-i': lambda inst, ray: (
        _NCDF_DIR,
        _ncdf_lut_filename(inst, 'biomass', 'bbi', rayleigh=ray),
        'biomass-i',
        'biomass_burning'
    ),
    'biomass-aus': lambda inst, ray: (
        _NCDF_DIR,
        _ncdf_lut_filename(inst, 'biomass', 'bbaus', rayleigh=ray),
        'biomass-aus'
        'biomass_burning'
    ),
    # NCDF aerosol tables
    'cci70': lambda inst, _: (
        _NCDF_DIR,
        _ncdf_lut_filename(inst, 'aerosol', 'a70'),
        'aerosol70',
        'large_clean_aerosol'
    ),
    'cci75': lambda inst, _: (
        _NCDF_DIR,
        _ncdf_lut_filename(inst, 'aerosol', 'a75'),
        'aerosol75',
        'manmade_aerosol'
    ),
    'cci76': lambda inst, _: (
        _NCDF_DIR,
        _ncdf_lut_filename(inst, 'aerosol', 'a76'),
        'aerosol76',
        'large_clean_aerosol'
    ),
    'cci77': lambda inst, _: (
        _NCDF_DIR,
        _ncdf_lut_filename(inst, 'aerosol', 'a77'),
        'aerosol77',
        'manmade_aerosol'
    ),
    'cci79': lambda inst, _: (
        _NCDF_DIR,
        _ncdf_lut_filename(inst, 'aerosol', 'a79'),
        'aerosol79',
        'biomass_burning'
    ),
}
