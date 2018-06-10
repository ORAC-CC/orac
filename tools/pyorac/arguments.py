"""Routines that populate an ArgumentParser for the Orac scripts."""
import pyorac.local_defaults as defaults

from os.path import isdir, isfile
from pyorac.definitions import BadValue, FileMissing, OracWarning, SETTINGS


def args_common(parser):
    """Define arguments common to all ORAC scripts."""
    from pyorac.util import _str2bool

    # Add boolean parsing function to register (type='bool', not type=bool)
    # http://stackoverflow.com/questions/15008758/parsing-boolean-values-with-argparse
    parser.register('type', 'bool', _str2bool)

    out = parser.add_argument_group('Common arguments paths')
    out.add_argument('-i','--in_dir', type=str, action="append", metavar='DIR',
                     help = 'Path for input.')
    out.add_argument('-o', '--out_dir', type=str, metavar='DIR',
                     help = 'Path for output.')
    out.add_argument('--orac_dir', type=str, nargs='?', metavar='DIR',
                     default = defaults.orac_dir,
                     help = 'Path to ORAC community code repository.')
    out.add_argument('--orac_lib', type=str, nargs='?', metavar='PATH',
                     default = defaults.orac_lib,
                     help = 'Name and path of ORAC library specification.')

    key = parser.add_argument_group('Common keyword arguments')
    key.add_argument('--batch', action='store_true',
                     help = 'Use batch processing for this call.')
    key.add_argument('-b', '--batch_settings', type=str, nargs=2, default=[],
                     metavar=('KEY', 'VALUE'),
                     action='append', help = 'Settings to pass to the batch '
                     'processing system. Passed as KEY VALUE pairs, where KEY '
                     'is one of ' + ', '.join(defaults.batch.args.keys()))
    key.add_argument('--procs', type=int, default=1, metavar='#',
                     help = 'Number of processors to use. Default 1.')
    key.add_argument('-d', '--dry_run', action='store_true',
                     help = 'Print the driver file, calling no executables.')
    key.add_argument('-k', '--keep_driver', action='store_true',
                     help = 'Retain driver files after processing.')
    key.add_argument('--lambertian', action='store_true',
                     help = 'Assume a lambertian surface rather than BRDF.')
    key.add_argument('--timing', action='store_true',
                     help = 'Print duration of executable calls.')
    key.add_argument('-D', '--additional', nargs=3, action='append', default=[],
                     metavar=('SECTION', 'KEY', 'VALUE'),
                     help = 'Adds an optional line to any driver file. Passed '
                     'as SECTION KEY VALUE sets, where SECTION is pre, main, or '
                     'post and KEY is an optional argument of that processor.')

    out = key.add_mutually_exclusive_group()
    out.add_argument('-v', '--script_verbose', action='store_true',
                     help = 'Print progress through script, not exe.')
    out.add_argument('-V', '--verbose', action='store_true',
                     help = 'Set verbose output from ORAC.')


def args_preproc(parser):
    """Define arguments for preprocessor script."""

    key = parser.add_argument_group('Preprocessor keywords')
    key.add_argument('-c', '--channel_ids', type=int, nargs='+', metavar='#',
                     default = None,
                     help = 'Channels to be considered by the preprocessor.')
    key.add_argument('--day_flag', type=int, nargs='?', choices=[0,1,2,3],
                     default = 3,
                     help = '1=Process day only, 2=Night only, '
                     '0|3=Both (default)')
    key.add_argument('--dellat', type=float, nargs='?', metavar='VALUE',
                     default = 1.38888889,
                     help = 'Reciprocal of latitude grid resolution. '
                     'Default 1.38888889.')
    key.add_argument('--dellon', type=float, nargs='?', metavar='VALUE',
                     default = 1.38888889,
                     help = 'Reciprocal of longitude grid resolution. '
                     'Default 1.38888889.')
    key.add_argument('-l', '--limit', type=int, nargs=4, default=(0, 0, 0, 0),
                     metavar=('X0', 'X1', 'Y0', 'Y1'),
                     help = 'First/last pixel in across/along-track directions.')
    key.add_argument('--l1_land_mask', action='store_true',
                     help = 'Use the imager landmask rather than the USGS.')
    key.add_argument('--use_modis_emis', action='store_true',
                     help = 'Use MODIS surface emissivity rather than RTTOV.')
    key.add_argument('--use_oc', action='store_true',
                     help = 'Use the Ocean Colour CCI backscatter product.')
    key.add_argument('-x', '--aux', type=str, nargs=2, action='append',
                     default=[], metavar=('KEY', 'VALUE'),
                     help = 'Location of auxilliary files. Passed as KEY VALUE '
                     'pairs, where KEY is one of ' +
                     ', '.join(defaults.auxiliaries.keys()))
    key.add_argument('--no_predef', action='store_true',
                     help = 'For geostationary sensors, calculate geolocation '
                     'online, rather than load a predefined file.')
    key.add_argument('--cloud_emis', action='store_true',
                     help = 'Output cloud emissivity from RTTOV.')
    key.add_argument('--ir_only', action='store_true',
                     help = 'Only load infrared channels.')
    key.add_argument('--skip_cloud_type', action='store_true',
                     help = 'Skip the Pavolonis cloud typing.')
    key.add_argument('--camel_emis', action='store_true',
                     help = 'Use the CAMEL emissivity library instead of RTTOV.')

    att = parser.add_argument_group('Global attribute values')
    att.add_argument('-g', '--global_att', type=str, nargs=2, action='append',
                     default=[], metavar=('KEY', 'VALUE'),
                     help = 'Values for NCDF global attributes. Passed as KEY '
                     'VALUE pairs, where KEY is one of ' +
                     ', '.join(defaults.global_attributes.keys()))
    att.add_argument('--uuid', action='store_true',
                     help = 'Produce a unique identifier number for output.')
    att.add_argument('-r', '--revision', type=int, nargs='?', metavar='#',
                     help = 'Revision (version) number for file.')

    ecmwf = parser.add_argument_group('ECMWF settings')
    ecmwf.add_argument('--ecmwf_flag', type=int, choices = range(5),
                       default = defaults.ecmwf_flag,
                       help = 'Type of ECMWF data to read in.')
    ecmwf.add_argument('--single_ecmwf', action='store_const',
                       default = 2, const = 0,
                       help = 'Do not interpolate ECMWF data.')
    ecmwf.add_argument('--skip_ecmwf_hr', action='store_true',
                       help = 'Ignore high resolution ECMWF files.')
    ecmwf.add_argument('--ecmwf_nlevels', type=int, nargs='?',
                       choices = (60, 91, 137), default = 60,
                       help = 'Number of levels in the ECMWF file used. '
                       'Default 50.')
    ice = ecmwf.add_mutually_exclusive_group()
    ice.add_argument('--use_ecmwf_snow', action='store_true',
                     help = 'Use ECMWF snow/ice fields rather than NISE.')
    ice.add_argument('--no_snow_corr', action='store_true',
                     help = 'Make no snow/uce correction.')


def args_main(parser):
    """Define arguments for main processor script."""
    from pyorac.definitions import ALL_TYPES

    main = parser.add_argument_group('Main processor arguments')
    main.add_argument('-a', '--approach', type=str, nargs='?',
                      choices = ('AppCld1L', 'AppCld2L', 'AppAerOx',
                                 'AppAerSw', 'AppAerO1'),
                      help = 'Retrieval approach to be used.')
    main.add_argument('--ret_class', type = str, nargs='?',
                      choices = ('ClsCldWat', 'ClsCldIce', 'ClsAerOx',
                                 'ClsAerSw', 'ClsAshEyj'),
                      help = 'Retrieval class to be used (for layer 1).')
    main.add_argument('--extra_lines_file', type=str, nargs='?', metavar='PATH',
                      help = 'Name of file containing additional driver lines.')
    main.add_argument('--phase', type=str, default = 'WAT',
                      choices = list(SETTINGS.keys()),
                      help = 'Label of look-up table to use in retrieval. '
                      'Default WAT.')
    main.add_argument('--sabotage', action='store_true',
                      help = 'Sabotage inputs during processing.')
    main.add_argument('--sad_dirs', type=str, nargs='+', metavar='DIR',
                      default = defaults.sad_dirs,
                      help = 'Path to SAD and LUT files.')
    main.add_argument('--types', type=str, nargs='+',
                      choices = ALL_TYPES, default = ALL_TYPES,
                      help = 'Pavolonis cloud types to process.')
    main.add_argument('--use_channel', type='bool', nargs='+', metavar='T/F',
                      default = [True, True, True, True, True, True],
                      help = 'Channels to be evaluated by main processor.')
    main.add_argument('--multilayer', type=str, nargs=2,
                      metavar = ('PHS', 'CLS'),
                      help = 'The phase and class to used for second layer.')

    ls = main.add_mutually_exclusive_group()
    ls.add_argument('--land', action='store_false',
                    help = 'Process only land pixels.')
    ls.add_argument('--sea', action='store_false',
                    help = 'Process only sea pixels.')

    ca = main.add_mutually_exclusive_group()
    ca.add_argument('--cloud_only', action='store_true',
                    help = 'Process only cloudy pixels.')
    ca.add_argument('--aerosol_only', action='store_true',
                    help = 'Process only aerosol pixels.')


def args_postproc(parser):
    """Define arguments for postprocessor script."""

    post = parser.add_argument_group('Post-processor paths')
    post.add_argument('--chunking', action='store_true',
                      help = 'Chunk the reading/writing files to save memory.')
    post.add_argument('--compress', action='store_true',
                      help = 'Use compression in NCDF outputs.')
    post.add_argument('--cost_thresh', type=float, nargs='?',
                      default = 0.0, metavar='VALUE',
                      help = 'Maximum cost to accept a pixel. Default 0.')
    post.add_argument('--no_night_opt', action='store_true',
                      help = 'Do not output optical properties at night.')
    post.add_argument('--suffix', type=str,
                      help = 'Suffix to include in output filename.')
    post.add_argument('--prob_thresh', type=float, nargs='?',
                      default = 0.0, metavar='VALUE',
                      help = 'Minimum fractional probability to accept a pixel. '
                      'Default 0.')
    post.add_argument('--switch_phase', action='store_true',
                      help = 'With cloud processing, check if CTT is '
                      'appropriate for the selected type.')

def args_cc4cl(parser):
    """Define arguments for ORAC suite wrapper script."""

    cccl = parser.add_argument_group('Keywords for CC4CL suite processing')
    cccl.add_argument('-C', '--clobber', type=int, nargs='?', default=3,
                      choices = range(4),
                      help = ('Level of processing to clobber:\n' +
                              '0=None, 1=Post, 2=Main+Post, 3=All (default).'))
    cccl.add_argument('--dur', type=str, nargs=3, metavar='HH:MM',
                      default = ('24:00', '24:00', '24:00'),
                      help = ('Maximal duration (in HH:MM) required by the '
                              'pre, main and post processors. Default 24:00.'))
    cccl.add_argument('--ram', type=int, nargs=3, metavar='Mb',
                      default = (11000, 11000, 11000),
                      help = ('Maximal memory (in Mb) used by the pre, main and '
                              'post processors. Default 11000.'))
    cccl.add_argument('--dir_names', type=str, nargs=2, action='append',
                      metavar=('KEY', 'VALUE'),
                      default=[], help = 'Names for the output directories. '
                      'Passed as KEY VALUE pairs, where KEY is one of ' +
                      ', '.join(defaults.dir_names.keys()))
    cccl.add_argument('-e', '--extra_lines', nargs=2, action='append',
                      metavar=('SECTION', 'VALUE'),
                      default=[], help = 'Path to a file giving extra lines for '
                      'main processing. Passed as SECTION VALUE pairs, where '
                      'KEY is lnd, sea, or cld to specify the particle type.')

    phs = cccl.add_mutually_exclusive_group()
    phs.add_argument('-p', '--phases', type=str, nargs='+',
                     default = ['WAT', 'ICE'],
                     help = 'Phases to be processed. Default WAT + ICE. See '
                     '--phase for choices (two-layer retrievals are pairs of '
                     'phases joined by an underscore).')
    phs.add_argument('-A', '--all_phases', action='store_true',
                     help = 'Sets --phases to run all aerosol and cloud types.')


def args_regress(parser):
    """Define arguments for the regression test script."""
    from pyorac.regression_tests import REGRESSION_TESTS

    reg = parser.add_argument_group('Keywords for ORAC regression tests')
    reg.add_argument('-B', '--benchmark', action='store_true',
                     help = "Produce benchmark output files i.e. "
                     "don't increment the revision number.")
    reg.add_argument('-L', '--long', action='store_true',
                     help = 'Process full orbits rather than short segments.')
    reg.add_argument('-t', '--tests', type=str, nargs='+', metavar='TEST',
                     choices = list(REGRESSION_TESTS.keys()), default = [],
                     help = 'List of tests to run.')

#-----------------------------------------------------------------------------

def check_args_common(args):
    """Ensure common parser arguments are valid."""
    from os import makedirs
    from os.path import dirname, basename

    # If not explicitly given, assume input folder is in target definition
    if args.in_dir is None:
        args.in_dir = [ dirname(args.target) ]
        args.target = basename(args.target)
    else:
        if "/" in args.target:
            raise BadValue("file target", "contains a /")
    if args.out_dir is None:
        args.out_dir = args.in_dir[0]

    for d in args.in_dir:
        if not isdir(d):
            raise FileMissing('in_dir', d)
    if not isdir(args.out_dir):
        makedirs(args.out_dir, 0o774)
    if not isdir(args.orac_dir):
        raise FileMissing('ORAC repository directory', args.orac_dir)
    if not isfile(args.orac_lib):
        raise FileMissing('ORAC library file', args.orac_lib)


def check_args_preproc(args):
    """Ensure preprocessor parser arguments are valid."""
    from pyorac.local_defaults import auxiliaries, global_attributes

    # Add global attributes
    global_attributes.update({key : val for key, val in args.global_att})
    args.__dict__.update(global_attributes)

    # Insert auxilliary locations
    auxiliaries.update({key : val for key, val in args.aux})
    args.__dict__.update(auxiliaries)

    try:
        # When using ecmwf_dir to set a single directory
        args.ggam_dir = args.ecmwf_dir
        args.ggas_dir = args.ecmwf_dir
        args.spam_dir = args.ecmwf_dir
    except:
        pass

    # Limit should either be all zero or all non-zero.
    limit_check = args.limit[0] == 0
    for limit_element in args.limit[1:]:
        if (limit_element == 0) ^ limit_check:
            warnings.warn('All elements of --limit should be non-zero.',
                          OracWarning, stacklevel=2)

    if not isdir(args.atlas_dir):
        raise FileMissing('RTTOV Atlas directory', args.atlas_dir)
    if not isfile(args.calib_file):
        raise FileMissing('AATSR calibration file', args.calib_file)
    if not isdir(args.coef_dir):
        raise FileMissing('RTTOV coefficients directory', args.coef_dir)
    if not isdir(args.emis_dir):
        raise FileMissing('RTTOV emissivity directory', args.emis_dir)
    if not isdir(args.emos_dir):
        raise FileMissing('EMOS temporary directory', args.emos_dir)
    #if not isdir(args.ggam_dir):
    #    raise FileMissing('ECMWF GGAM directory', args.ggam_dir)
    #if not isdir(args.ggas_dir):
    #    raise FileMissing('ECMWF GGAS directory', args.ggas_dir)
    #if not isdir(args.hr_dir) and not args.skip_ecmwf_hr:
    #    raise FileMissing('ECMWF high resolution directory', args.hr_dir)
    #if not isdir(args.mcd43c3_dir):
    #    raise FileMissing('MODIS MCD43C1 directory', args.mcd43c1_dir)
    #if not isdir(args.mcd43c1_dir):
    #    raise FileMissing('MODIS MCD43C3 directory', args.mcd43c3_dir)
    #if not isdir(args.occci_dir):
    #    raise FileMissing('OC CCI directory', args.occci_dir)
    #if not isdir(args.nise_dir):
    #    raise FileMissing('NISE directory', args.nise_dir)
    #if not isdir(args.spam_dir):
    #    raise FileMissing('ECMWF SPAM directory', args.spam_dir)
    if not isfile(args.usgs_file):
        raise FileMissing('USGS file', args.usgs_file)


def check_args_main(args):
    """Ensure main processor parser arguments are valid."""

    if len(args.in_dir) > 1:
        warnings.warn('Main processor ignores all but first in_dir.',
                      OracWarning, stacklevel=2)
    if not isdir(args.in_dir[0]):
        raise FileMissing('Preprocessed directory', args.in_dir[0])
    for d in args.sad_dirs:
        if not isdir(d):
            raise FileMissing('sad_dirs', d)
    if args.multilayer is not None:
        args.approach = "AppCld2L"
    # No error checking yet written for channel arguments


def check_args_postproc(args):
    """Ensure postprocessor parser arguments are valid."""

    for d in args.in_dir:
        if not isdir(d):
            raise FileMissing('Processed output directory', d)


def check_args_cc4cl(args):
    """Ensure ORAC suite wrapper parser arguments are valid."""
    from os import makedirs
    from os.path import isdir, join
    from pyorac.local_defaults import dir_names, extra_lines

    # Add names for output directories
    dir_names.update({key : val for key, val in args.dir_names})
    args.__dict__.update(dir_names)

    # Add extra lines files
    extra_lines.update({key + '_extra' : val for key, val in args.extra_lines})
    args.__dict__.update(extra_lines)

    log_path = join(args.out_dir, args.log_dir)
    if args.batch and not isdir(log_path):
        makedirs(log_path, 0o774)

    if args.all_phases:
        args.phases = list(SETTINGS.keys())

    return log_path


def check_args_regress(args):
    """Ensure regression test arguments are valid."""
    from pyorac.regression_tests import AEROSOL_TESTS

    # Default tests
    if len(args.tests) == 0:
        if args.long:
            args.tests = ['DAYMYD', 'NITMYD', 'AATSR', 'AVHRR']
        else:
            args.tests = ['DAYMYDS', 'NITMYDS', 'DAYAATSRS', 'NITAATSRS',
                          'DAYAVHRRS', 'NITAVHRRS']

    # Remove tests that can't run aerosol retrievals
    if args.all_phases:
        args.tests = filter(lambda t: t in AEROSOL_TESTS, args.tests)
