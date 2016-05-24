# Subroutines used by ORAC python scripts

import argparse
import datetime
from glob import glob
import os
import re
from termcolor import colored

star = colored('*', 'blue')

#----------------------------------------------------------------------------

# Determine sensor evaluated from filename
def parse_sensor(filename):
    if filename[0:10] == 'ATS_TOA_1P':
        sensor = 'AATSR'
        platform = 'Envisat'
    elif filename[0:8] == 'MOD021KM':
        sensor = 'MODIS'
        platform = 'TERRA'
    elif filename[0:8] == 'MYD021KM':
        sensor = 'MODIS'
        platform = 'AQUA'
    elif filename[0:4] == 'noaa':
        sensor = 'AVHRR'
        platform = 'noaa'+filename[4:6]
    else:
        m = re.search('-L2-CLOUD-CLD-(\w+?)_ORAC_(\w+?)_', filename)
        if m:
            sensor = m.group(1)
            platform = m.group(2)
        else:
            raise ValueError('Unexpected filename format - '+filename)

    return (sensor, platform)

#-----------------------------------------------------------------------------

# Search a folder for the file with timestamp nearest in the past to a given date
def date_back_search(
    fld,      # Folder to be searched
    date    , # Initial date to consider
    pattern): # strftime format string to parse filename

    dt = date
    while True:
        f = glob(fld + dt.strftime(pattern))
        if f:
            if len(f) == 1:
                return f[0]
            else:
                return f[-1]
        else:
            if dt.year < 2006:
                raise NameError('Cannot locate '+fld+' file using '+pattern)
            else:
                dt -= datetime.timedelta(days=1)

#-----------------------------------------------------------------------------

# Return timestamps divisible by some duration that bound a given time
# http://stackoverflow.com/questions/3463930/how-to-round-the-minute-of-a-datetime-object-python/10854034
def boundTime(
    dt=None,                                # Initial time
    dateDelta=datetime.timedelta(hours=6)): # Rounding interval

    roundTo = dateDelta.total_seconds()
    if dt == None : dt = datetime.datetime.now()

    # Remove annoying microseconds
    time = dt + datetime.timedelta(0,0,-dt.microsecond)

    # Floor time to requested delta
    seconds = (dt - dt.min).seconds
    rounding = seconds // roundTo * roundTo
    start = time + datetime.timedelta(0,rounding-seconds)

    # Output floor and ceil of time
    return (start, start + dateDelta)

#-----------------------------------------------------------------------------

# Function called by re.sub to replace variables with their values
#http://stackoverflow.com/questions/7868554/python-re-subs-replace-function-doesnt-accept-extra-arguments-how-to-avoid
def parse_with_lib(lib):
    def replace_var(matchobj):
        if len(matchobj.group(0)) > 3:
            return lib[matchobj.group(0)[2:-1]]
    return replace_var

# Read the ORAC library definitions into a Python dictionary
def read_orac_libraries(file):
    libraries = {}
    if os.environ['LIBBASE']:
        libraries['LIBBASE'] = os.environ['LIBBASE']

    # Open ORAC library file
    with open(file, 'r') as f:
        # Loop over each line
        for line in f:
            # Only process variable definitions
            if '=' in line and '\\' not in line:
                line = line.replace("\n", '')
                parts = line.split('=',2)

                # Replace any variables in this line with those we already know
                fixed = re.sub(r"\$\(.*?\)", parse_with_lib(libraries), parts[1])

                # Add this line to the dictionary
                libraries[parts[0]] = fixed

    return libraries

# Build necessary LD_LIBRARY_PATH variable
def build_orac_library_path(libs):
    ld_path = ':'.join([libs[key] for key in ("SZLIB", "EPR_APILIB", "GRIBLIB",
                                              "HDF5LIB", "HDFLIB",
                                              "NCDF_FORTRAN_LIB", "NCDFLIB")])
    if "LD_LIBRARY_PATH" in os.environ.keys():
        ld_path += ':' + os.environ["LD_LIBRARY_PATH"]
    return ld_path

#-----------------------------------------------------------------------------

# Define driver file for preprocessor
preprocessor_driver = """{sensor}
{l1b}
{geo}
{usgs}
{ggam[0]}
{coef}
{atlas}
{nise}
{alb}
{brdf}
{emis}
{dellon}
{dellat}
{out_dir}
{limit[0]}
{limit[1]}
{limit[2]}
{limit[3]}
{ncdf_version}
{conventions}
{institution}
{l2_processor}
{creator_email}
{creator_url}
{file_version}
{references}
{history}
{summary}
{keywords}
{comment}
{project}
{license}
{uuid}
{production_time}
{atsr_calib}
{ecmwf_flag}
{ggas[0]}
{spam[0]}
{chunk_flag}
{day_flag}
{verbose}
-
{assume_full_paths}
{include_full_brdf}
{rttov_version}
{ecmwf_version}
{svn_version}
ECMWF_TIME_INT_METHOD={ecmwf_int_method}
ECMWF_PATH_2={ggam[1]}
ECMWF_PATH2_2={ggas[1]}
ECMWF_PATH3_2={spam[1]}
USE_HR_ECMWF={use_ecmwf_hr}
ECMWF_PATH_HR={ecmwf_hr[0]}
ECMWF_PATH_HR_2={ecmwf_hr[1]}
USE_ECMWF_SNOW_AND_ICE={ecmwf_nise}
USE_MODIS_EMIS_IN_RTTOV={modis_emis}""".format

main_driver="""{in_dir}
{fileroot}
{out_dir}
{sad_dir}
{sensor}
{nch}
{channels}
{phase}
CTRL%NTYPES_TO_PROCESS={ntypes}
CTRL%TYPES_TO_PROCESS={types}
CTRL%PROCESS_CLOUDY_ONLY={cloudy}
CTRL%VERBOSE={verbose}
CTRL%RS%USE_FULL_BRDF={use_brdf}""".format

postproc_driver="""{wat_pri}
{ice_pri}
{wat_sec}
{ice_sec}
{out_pri}
{out_sec}
{switch}
COST_THRESH={cost_tsh}
NORM_PROB_THRESH={prob_tsh}
OUTPUT_OPTICAL_PROPS_AT_NIGHT={opt_nght}
VERBOSE={verbose}
USE_NETCDF_COMPRESSION={compress}
USE_BAYESIAN_SELECTION={bayesian}""".format

#-----------------------------------------------------------------------------

# Useful local paths for argument defaults
greg  = '/network/aopp/totoro/eodg/mcgarragh/data'
home  = '/network/home/aopp/povey'
matin = '/network/aopp/matin/eodg/shared'

def orac_common_args(parser):
    out = parser.add_argument_group('File paths')
    out.add_argument('-o', '--out_dir', type=str, default = None,
                     help = 'Path for output.')
    out.add_argument('-i','--in_dir', type=str, nargs='+', default = None,
                     help = 'Path for input.')

    script = parser.add_argument_group('Script keywords')
    script.add_argument('--keep_driver', action='store_true',
                        help = 'Retain driver files after processing.')
    script.add_argument('--no_clobber', action='store_true',
                        help = 'Retain existing output files.')
    script.add_argument('--lambertian', action='store_true',
                        help = 'Assume a lambertian surface rather than BRDF.')
    script.add_argument('-p', '--progress', action='store_true',
                        help = 'Print progress through script, not exe.')
    script.add_argument('-v', '--verbose', action='store_true',
                        help = 'Set verbose output from the postprocessor.')

    orac = parser.add_argument_group('ORAC environment paths')
    orac.add_argument('--orac_dir', type=str, nargs='?', metavar='DIR',
                      default = home + '/orac/trunk',
                      help = 'Path to ORAC community code repository.')
    orac.add_argument('--orac_lib', type=str, nargs='?', metavar='FILE',
                      default = os.environ['ORAC_LIB'],
                      help = 'Name and path of ORAC library specification.')
    orac.add_argument('--sh_dir', type=str, nargs='?', metavar='DIR',
                      default = home + '/orac/trunk/tools',
                      help = 'Path to ORAC scripts.')
    return parser

def orac_common_args_check(args):
    # Check of input dir done in routine checker
    if not os.path.isdir(args.orac_dir):
        raise ValueError('Target of --orac_dir does not exist: '+args.orac_dir)
    if not os.path.isfile(args.orac_lib):
        raise ValueError('Target of --orac_lib does not exist: '+args.orac_lib)
    if not os.path.isdir(args.sh_dir):
        raise ValueError('Target of --sh_dir does not exist: '+args.sh_dir)

def orac_preproc_args(parser):
    key = parser.add_argument_group('Preprocessor keywords')
    key.add_argument('--channel_ids', type=int, nargs='+', metavar='#',
                     default = None,
                     help = 'Channels to be considered by the preprocessor.')
    key.add_argument('--geo_dir', type=str, nargs='?', metavar='DIR',
                     default = None,
                     help = 'Path to the geolocation file.')
    key.add_argument('--day_flag', type=int, nargs='?', choices=[0,1,2,3],
                     default = 3,
                     help = '1=Process day only, 2=Night only, 0|3=Both')
    key.add_argument('--dellat', type=float, nargs='?', metavar='VALUE',
                     default = 1.38888889,
                     help = 'Reciprocal of latitude grid resolution.')
    key.add_argument('--dellon', type=float, nargs='?', metavar='VALUE',
                     default = 1.38888889,
                     help = 'Reciprocal of longitude grid resolution.')
    key.add_argument('--single_ecmwf', action='store_const',
                     default = 2, const = 0,
                     help = 'Do not interpolate ECMWF data.')
    key.add_argument('--limit', type=int, nargs=4, default=(0,0,0,0),
                     metavar=('X0','X1','Y0','Y1'),
                     help = 'First/last pixels in across/along-track directions.')
    key.add_argument('--skip_ecmwf_hr', action='store_true',
                     help = 'Ignore high resolution ECMWF files.')
    key.add_argument('--use_ecmwf_snow', action='store_true',
                     help = 'Use ECMWF snow/ice fields rather than NISE.')
    key.add_argument('--use_modis_emis', action='store_true',
                     help = 'Use MODIS surface emissivity rather than RTTOV.')

    att = parser.add_argument_group('Global attribute values')
    att.add_argument('--cfconvention', type=str, nargs='?', metavar='STRING',
                     default = 'CF-1.4',
                     help = 'CF convention used in file definition.')
    att.add_argument('--comments', type=str, nargs='?', metavar='STRING',
                     default = 'n/a',
                     help = 'Any additional comments on file contents or use.')
    att.add_argument('--email', type=str, nargs='?', metavar='STRING',
                     default = 'adam.povey@physics.ox.ac.uk',
                     help = 'Contact email address.')
    att.add_argument('--history', type=str, nargs='?', metavar='STRING',
                     default = 'n/a',
                     help = 'Description of the file processing history.')
    att.add_argument('--institute', type=str, nargs='?', metavar='STRING',
                     default = 'UoOx',
                     help = 'Research institute that produced the file.')
    att.add_argument('--keywords', type=str, nargs='?', metavar='STRING',
                     default = 'aerosol; cloud; optimal estimation',
                     help = 'Keywords describing contents of file.')
    att.add_argument('--license', type=str, nargs='?', metavar='STRING',
                     default = 'http://proj.badc.rl.ac.uk/orac/wiki/License',
                     help = 'Details of the license for use of the data.')
    att.add_argument('--processor', type=str, nargs='?', metavar='STRING',
                     default = 'ORAC',
                     help = 'Name of the L2 processor.')
    att.add_argument('--project', type=str, nargs='?', metavar='STRING',
                     default = 'ESACCI',
                     help = 'Name of the project overseeing this data.')
    att.add_argument('--references', type=str, nargs='?', metavar='STRING',
                     default = 'doi:10.5194/amt-5-1889-2012',
                     help = 'Appropriate citations for product.')
    att.add_argument('--summary', type=str, nargs='?', metavar='STRING',
                     default = 'n/a',
                     help = "Brief description of the file's purpose.")
    att.add_argument('--url', type=str, nargs='?', metavar='STRING',
                     default = 'http://proj.badc.rl.ac.uk/ora',
                     help = 'Reference webpage for product.')
    att.add_argument('--uuid', action='store_true',
                     help = 'Produce a unique identifier number for output.')
    att.add_argument('-r', '--version', type=int, nargs='?', metavar='#',
                     default = None,
                     help = 'Version number for file.')
    att.add_argument('-c', '--clean', action='store_true',
                     help = 'Do not increment the revision number.')

    ecmwf = parser.add_argument_group('Surface property paths')
    ecmwf.add_argument('--emis_dir', type=str, nargs='?', metavar='DIR',
                       default = greg + '/modis_emis/v3',
                       help = 'Path to MODIS emissivity files.')
    ecmwf.add_argument('--mcd43_dir', type=str, nargs='?', metavar='DIR',
                       default = greg + '/modis/mcd43cX',
                       help = 'Path to MCD43C1 and C3 files.')

    rttov = parser.add_argument_group('RTTOV file paths')
    rttov.add_argument('--atlas_dir', type=str, nargs='?', metavar='DIR',
                       default = matin + '/rttov_files/emis_data',
                       help = 'Path to RTTOV emissivity atlas.')
    rttov.add_argument('--coef_dir', type=str, nargs='?', metavar='DIR',
                       default = matin + '/rttov_files',
                       help = 'Path to RTTOV coefficient files.')

    other = parser.add_argument_group('Other paths')
    other.add_argument('--ecmwf_dir', type=str, nargs='?', metavar='DIR',
                       default = '/network/aopp/dave/imager/data/ecmwf',
                       help = 'Path to ECMWF files from the BADC.')
    other.add_argument('--emos_dir', type=str, nargs='?', metavar='DIR',
                       default = matin + '/emos_files',
                       help = 'Path to ECMWF files from the BADC.')
    other.add_argument('--nise_dir', type=str, nargs='?', metavar='DIR',
                       default = '/network/aopp/matin/eodg/nise',
                       help = 'Path to NISE products.')
    other.add_argument('--calib_file', type=str, nargs='?', metavar='FILE',
                       default = matin+'/AATSR_VIS_DRIFT_V03-00.DAT',
                       help = 'Name and path of AATSR calibration file.')
    other.add_argument('--usgs_file', type=str, nargs='?', metavar='FILE',
                       default = matin+
                       '/Aux_file_CM_SAF_AVHRR_GAC_ori_0.05deg.nc',
                       help = 'Name and path of USGS DEM.')

    comp = parser.add_mutually_exclusive_group()
    comp.add_argument('--no_compare', action='store_true',
                      help = 'Do not compare outputs to the previous version.')
    comp.add_argument('--only_compare', action='store_true',
                      help = 'Do not process outputs of the current version.')
    return parser

def orac_preproc_args_check(args):
    if args.in_dir == None:
        args.in_dir = os.path.dirname(args.file)
        args.file   = os.path.basename(args.file)
    if not os.path.isdir(args.in_dir):
        raise ValueError('L1B directory does not exist: '+args.in_dir)
    if not os.path.isfile(args.in_dir+'/'+args.file):
        raise ValueError('L1B file does not exist: '+args.in_dir+'/'+args.file)
    limit_check = args.limit[0] == 0
    for limit_element in args.limit[1:]:
        if (limit_element == 0) ^ limit_check:
            raise UserWarning('All elements of --limit should be non-zero.')
    if not os.path.isdir(args.atlas_dir):
        raise ValueError('Target of --atlas_dir does not exist: '+args.atlas_dir)
    if not os.path.isfile(args.calib_file):
        raise ValueError('AATSR calibration file does not exist: '+
                         args.calib_file)
    if not os.path.isdir(args.coef_dir):
        raise ValueError('Target of --coef_dir does not exist: '+args.coef_dir)
    if not os.path.isdir(args.ecmwf_dir):
        raise ValueError('Target of --ecmwf_dir does not exist: '+args.ecmwf_dir)
    if not os.path.isdir(args.emis_dir):
        raise ValueError('Target of --emis_dir does not exist: '+args.emis_dir)
    if not os.path.isdir(args.mcd43_dir):
        raise ValueError('Target of --mcd43_dir does not exist: '+args.mcd43_dir)
    if not os.path.isdir(args.nise_dir):
        raise ValueError('Target of --nise_dir does not exist: '+args.nise_dir)
    if not os.path.isfile(args.usgs_file):
        raise ValueError('USGS file does not exist: '+args.usgs_file)

def orac_main_args(parser):
    main = parser.add_argument_group('Main processor paths')
    main.add_argument('--approach', type=str, nargs='?', metavar='STRING',
                      default = None,
                      help = 'Retrieval approach to be used.')
    main.add_argument('--cloudy', action='store_true',
                      help = 'Process only cloudy pixels.')
    main.add_argument('--extra_lines', type=str, nargs='?', metavar='FILE',
                      default = None,
                      help = 'Name of file containing additional driver lines.')
    main.add_argument('--sabotage', action='store_true',
                      help = 'Sabotage inputs during processing.')
    main.add_argument('--sad_dir', type=str, nargs='?', metavar='DIR',
                      default = '/local/home/povey/eodg/povey/sad_dir',
                      help = 'Path to SAD and LUT files.')
    main.add_argument('--types', type=int, nargs='+', metavar='#',
                      default = (0,1,2,3,4,5,6,7,8,9),
                      help = 'Pavolonis cloud types to process.')
    main.add_argument('--use_channel', type=int, nargs='+', metavar='T/F',
                      default = (1, 1, 1, 1, 1, 1),
                      help = 'Channels to be evaluated by main processor.')

    ls = main.add_mutually_exclusive_group()
    ls.add_argument('--land', action='store_true',
                    help = 'Process only land pixels.')
    ls.add_argument('--sea', action='store_true',
                    help = 'Process only sea pixels.')
    return parser

def orac_main_args_check(args):
    if len(args.in_dir) > 1:
        print('WARNING: Main processor ignores all but first in_dir.')
    if not os.path.isdir(args.in_dir[0]):
        raise ValueError('Preprocessed directory does not exist: '+
                         args.in_dir[0])
    if not os.path.isdir(args.sad_dir):
        raise ValueError('Target of --sad_dir does not exist: '+args.sad_dir)
    # No error checking yet written for channel arguments

def orac_postproc_args(parser):
    post = parser.add_argument_group('Post-processor paths')
    post.add_argument('--compress', action='store_true',
                      help = 'Use compression in NCDF outputs.')
    post.add_argument('--cost_thresh', type=float, nargs='?',
                      default = 0.0,
                      help = 'Maximum cost to accept a pixel.')
    post.add_argument('--no_night_opt', action='store_true',
                      help = 'Do not output optical properties at night.')
    post.add_argument('--phases', type=str, nargs='+', metavar='PHS',
                      default = ('WAT', 'ICE'),
                      help = 'Phases to be processed.')
    post.add_argument('--prob_thresh', type=float, nargs='?',
                      default = 0.0,
                      help = 'Minimum fractional probability to accept a pixel.')
    post.add_argument('--switch_phase', action='store_true',
                      help = 'With cloud processing, check if CTT is appropriate for the selected type..')
    return parser

def orac_postproc_args_check(args):
    for d in args.in_dir:
        if not os.path.isdir(d):
            raise ValueError('Processed output directory does not exist: '+
                             os.path.isdir(d))
