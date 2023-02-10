"""Routines that populate an ArgumentParser for the Orac scripts."""
import os
import warnings

import pyorac.local_defaults as defaults
from pyorac.definitions import BadValue, FileMissing, OracWarning, SETTINGS


def args_common(parser):
    """Define arguments common to all ORAC scripts."""
    from pyorac.util import str2bool

    # Add boolean parsing function to register (type='bool', not type=bool)
    # http://stackoverflow.com/questions/15008758/parsing-boolean-values-with-argparse
    if 'bool' not in parser._registries['type']:
        parser.register('type', 'bool', str2bool)

    out = parser.add_argument_group('Common arguments paths')
    out.add_argument('-i', '--in_dir', type=str, action="append", metavar='DIR',
                     help='Path for input.')
    out.add_argument('-o', '--out_dir', type=str, metavar='DIR',
                     help='Path for output.')
    out.add_argument('--orac_dir', type=str, nargs='?', metavar='DIR',
                     default=defaults.ORAC_DIR,
                     help='Path to ORAC community code repository.')
    out.add_argument('--orac_lib', type=str, nargs='?', metavar='PATH',
                     default=defaults.ORAC_LIB,
                     help='Name and path of ORAC library specification.')

    key = parser.add_argument_group('Common keyword arguments')
    key.add_argument('-c', '--available_channels', type=int, nargs='+',
                     metavar='#', default=None,
                     help='Channels to be evaluated.')
    key.add_argument('--batch', action='store_true',
                     help='Use batch processing for this call.')
    key.add_argument('--batch_script', default=defaults.BATCH_SCRIPT,
                     help='Execution script to use in batch processing.')
    key.add_argument('-b', '--batch_settings', type=str, nargs=2, default=[],
                     metavar=('KEY', 'VALUE'), action='append',
                     help='Settings to pass to the batch processing system. '
                          'Passed as KEY VALUE pairs, where KEY is one of ' +
                          ', '.join(defaults.BATCH.args.keys()))
    key.add_argument('--procs', type=int, default=1, metavar='#',
                     help='Number of processors to use. Default 1.')
    key.add_argument('-d', '--dry_run', action='store_true',
                     help='Print the driver file, calling no executables.')
    key.add_argument('-k', '--keep_driver', action='store_true',
                     help='Retain driver files after processing.')
    key.add_argument('--lambertian', action='store_true',
                     help='Assume a lambertian surface rather than BRDF.')
    key.add_argument('--timing', action='store_true',
                     help='Print duration of executable calls.')
    key.add_argument('-A', '--additional', nargs=3, action='append', default=[],
                     metavar=('SECTION', 'KEY', 'VALUE'),
                     help='Adds an optional line to any driver file. Passed '
                          'as SECTION KEY VALUE sets, where SECTION is pre, main, or '
                          'post and KEY is an optional argument of that processor.')

    out = key.add_mutually_exclusive_group()
    out.add_argument('-v', '--script_verbose', action='store_true',
                     help='Print progress through script, not exe.')
    out.add_argument('-V', '--verbose', action='store_true',
                     help='Set verbose output from ORAC.')


def args_preproc(parser):
    """Define arguments for preprocessor script."""

    key = parser.add_argument_group('Preprocessor keywords')
    key.add_argument('--day_flag', type=int, nargs='?', choices=(0, 1, 2, 3),
                     default=3, help='1=Process day only, 2=Night only, '
                                     '0|3=Both (default)')
    key.add_argument('--dellat', type=float, nargs='?', metavar='VALUE',
                     default=1.38888889, help='Reciprocal of latitude grid '
                                              'resolution. Default 1.38888889.')
    key.add_argument('--dellon', type=float, nargs='?', metavar='VALUE',
                     default=1.38888889, help='Reciprocal of longitude grid '
                                              'resolution. Default 1.38888889.')
    key.add_argument('-l', '--limit', type=int, nargs=4, default=(0, 0, 0, 0),
                     metavar=('X0', 'X1', 'Y0', 'Y1'),
                     help='First/last pixel in across/along-track directions. '
                          'FORTRAN 1-INDEXING, NOT PYTHON 0-INDEXING!')
    key.add_argument('--l1_land_mask', action='store_true',
                     help='Use the imager landmask rather than the USGS.')
    key.add_argument('--use_oc', action='store_true',
                     help='Use the Ocean Colour CCI backscatter product.')
    key.add_argument('-x', '--aux', type=str, nargs=2, action='append',
                     default=[], metavar=('KEY', 'VALUE'),
                     help='Location of auxilliary files. Passed as KEY VALUE '
                          'pairs, where KEY is one of ' +
                          ', '.join(defaults.AUXILIARIES.keys()))
    key.add_argument('--no_predef', action='store_true',
                     help='For geostationary sensors, calculate geolocation '
                          'online, rather than load a predefined file.')
    key.add_argument('--cloud_emis', action='store_true',
                     help='Output cloud emissivity from RTTOV.')
    key.add_argument('--ir_only', action='store_true',
                     help='Only load infrared channels.')
    key.add_argument('--skip_cloud_type', action='store_true',
                     help='Skip the Pavolonis cloud typing.')
    key.add_argument('--swansea', action='store_true',
                     help='Use the Swansea climatology instead of MODIS BRDF.')
    emis = key.add_mutually_exclusive_group()
    emis.add_argument('--use_modis_emis', action='store_true',
                      help='Use MODIS surface emissivity rather than RTTOV.')
    emis.add_argument('--use_camel_emis', action='store_true',
                      help='Use CAMEL emissivity library rather than RTTOV.')

    att = parser.add_argument_group('Global attribute values')
    att.add_argument('-g', '--global_att', type=str, nargs=2, action='append',
                     default=[], metavar=('KEY', 'VALUE'),
                     help='Values for NCDF global attributes. Passed as KEY '
                          'VALUE pairs, where KEY is one of ' +
                          ', '.join(defaults.GLOBAL_ATTRIBUTES.keys()))
    att.add_argument('--uuid', action='store_true',
                     help='Produce a unique identifier number for output.')
    att.add_argument('-r', '--revision', type=int, nargs='?', metavar='#',
                     help='Revision (version) number for file.')

    ecmwf = parser.add_argument_group('ECMWF settings')
    ecmwf.add_argument('--nwp_flag', type=int, choices=range(5),
                       default=defaults.NWP_FLAG,
                       help='Type of ECMWF data to read in.')
    ecmwf.add_argument('--single_ecmwf', action='store_const',
                       default=2, const=0,
                       help='Do not interpolate ECMWF data.')
    ice = ecmwf.add_mutually_exclusive_group()
    ice.add_argument('--use_ecmwf_snow', action='store_true',
                     help='Use ECMWF snow/ice fields rather than NISE.')
    ice.add_argument('--no_snow_corr', action='store_true',
                     help='Make no snow/uce correction.')


def args_main(parser):
    """Define arguments for main processor script."""
    from pyorac.definitions import ALL_TYPES
    from pyorac.util import str2bool

    if 'bool' not in parser._registries['type']:
        parser.register('type', 'bool', str2bool)

    main = parser.add_argument_group('Main processor arguments')
    main.add_argument('--approach', type=str, nargs='?',
                      choices=('AppCld1L', 'AppCld2L', 'AppAerOx',
                               'AppAerSw', 'AppAerO1'),
                      help='Retrieval approach to be used.')
    main.add_argument('--ret_class', type=str, nargs='?',
                      choices=('ClsCldWat', 'ClsCldIce', 'ClsAerOx',
                               'ClsAerSw', 'ClsAerBR', 'ClsAshEyj'),
                      help='Retrieval class to be used (for layer 1).')
    main.add_argument('--phase', type=str, choices=list(SETTINGS.keys()),
                      help='Label of look-up table to use in retrieval.')
    main.add_argument('--sabotage', action='store_true',
                      help='Sabotage inputs during processing.')
    main.add_argument('--sad_dirs', type=str, nargs='+', metavar='DIR',
                      default=defaults.SAD_DIRS,
                      help='Path to SAD and LUT files.')
    main.add_argument('--types', type=str, nargs='+',
                      choices=ALL_TYPES, default=ALL_TYPES,
                      help='Pavolonis cloud types to process.')
    main.add_argument('-u', '--use_channels', type=int, nargs='+', metavar='#',
                      default=None,
                      help='Channels to be evaluated by main processor.')
    main.add_argument('--multilayer', type=str, nargs=2,
                      metavar=('PHS', 'CLS'),
                      help='Do a two-layer retrieval, where these two args '
                           'specify the phase and class used for near-surface layer.')

    landsea = main.add_mutually_exclusive_group()
    landsea.add_argument('--no_land', action='store_true',
                         help='Ignore land pixels.')
    landsea.add_argument('--no_sea', action='store_true',
                         help='Ignore sea pixels.')

    claer = main.add_mutually_exclusive_group()
    claer.add_argument('--cloud_only', action='store_true',
                       help='Process only cloudy pixels.')
    claer.add_argument('--aerosol_only', action='store_true',
                       help='Process only aerosol pixels.')


def args_postproc(parser):
    """Define arguments for postprocessor script."""

    post = parser.add_argument_group('Post-processor paths')
    post.add_argument('--chunking', action='store_true',
                      help='Chunk the reading/writing files to save memory.')
    post.add_argument('--compress', action='store_true',
                      help='Use compression in NCDF outputs.')
    post.add_argument('--cost_thresh', type=float, nargs='?',
                      default=0.0, metavar='VALUE',
                      help='Maximum cost to accept a pixel. Default 0.')
    post.add_argument('--no_night_opt', action='store_true',
                      help='Do not output optical properties at night.')
    post.add_argument('--suffix', type=str,
                      help='Suffix to include in output filename.')
    post.add_argument('--phases', type=str, nargs='+', default=[],
                      help='Phases to combine. ONLY USED BY SINGLE_PROCESS.PY')
    post.add_argument('--prob_thresh', type=float, nargs='?',
                      default=0.0, metavar='VALUE',
                      help='Minimum fractional probability to accept a pixel. '
                           'Default 0.')
    post.add_argument('--no_switch_phase', action='store_false',
                      help='With cloud processing, do not check if CTT is '
                           'appropriate for the selected type. Only relevant '
                           'when processing exclusively water and ice cloud.')


def args_cc4cl(parser):
    """Define arguments for ORAC suite wrapper script."""

    cccl = parser.add_argument_group('Keywords for CC4CL suite processing')
    cccl.add_argument('-C', '--clobber', type=int, nargs='?', default=3,
                      choices=range(4),
                      help='Level of processing to clobber:\n'
                           '0=None, 1=Post, 2=Main+Post, 3=All (default).')
    cccl.add_argument('--dur', type=str, nargs=3, metavar='HH:MM',
                      default=('24:00', '24:00', '24:00'),
                      help='Maximal duration (in HH:MM) required by the '
                           'pre, main and post processors. Default 24:00.')
    cccl.add_argument('--ram', type=int, nargs=3, metavar='Mb',
                      default=(11000, 11000, 11000),
                      help='Maximal memory (in Mb) used by the pre, main and '
                           'post processors. Default 11000.')
    cccl.add_argument('-e', '--extra_lines', nargs=2, action='append',
                      metavar=('SECTION', 'VALUE'),
                      default=[], help='Path to a file giving extra lines for '
                                       'main processing. Passed as SECTION VALUE pairs, where '
                                       'KEY is lnd, sea, or cld to specify the particle type.')
    cccl.add_argument('--label', type=str, help="Description for job name.",
                      default="")
    cccl.add_argument('--reformat', type=str, default="", metavar='PATH',
                      help='Script used to reformat ORAC output.')
    cccl.add_argument('--sub_dir', type=str, default="",
                      help='Folder in which to store intermediate output.')

    phs = cccl.add_mutually_exclusive_group()
    phs.add_argument('-s',
                     '--settings',
                     type=str,
                     action='append',
                     default=None,
                     help='Parameters for each phase to be processed.'
                          ' Each element is a string listing all the '
                          'arguments to be applied for that phase. This is'
                          ' processed using the same parser so all arguments'
                          ' listed for the main processor are available.')
    phs.add_argument('-S', '--preset_settings', type=str, default=None,
                     choices=defaults.RETRIEVAL_SETTINGS.keys(),
                     help='Use a predefined input for --settings, defined '
                          'in the local_defaults.')
    phs.add_argument('--settings_file', type=str, default=None,
                     help='A file specifying the phases to run, one on '
                          'each line.')


def args_regress(parser):
    """Define arguments for the regression test script."""
    from pyorac.regression_tests import REGRESSION_TESTS

    reg = parser.add_argument_group('Keywords for ORAC regression tests')
    reg.add_argument('-B', '--benchmark', action='store_true',
                     help="Produce benchmark output files i.e. "
                          "don't increment the revision number.")
    reg.add_argument('-L', '--long', action='store_true',
                     help='Process all full orbit tests.')
    reg.add_argument('-t', '--tests', type=str, nargs='+', metavar='TEST',
                     choices=list(REGRESSION_TESTS.keys()), default=[],
                     help='List of tests to run.')
    reg.add_argument('-T', '--test_type', choices=('C', 'A', 'J'),
                     default='C', help='Type of test to run. C=Cloud '
                                       '(default), A=Aerosol, J=Joint.')


# -----------------------------------------------------------------------------

def check_args_common(args):
    """Ensure common parser arguments are valid."""
    from pyorac.definitions import FileName

    # If not explicitly given, assume input folder is in target definition
    if args.in_dir is None:
        args.in_dir = [os.path.dirname(args.target), ]
        args.target = os.path.basename(args.target)
    else:
        if "/" in args.target:
            raise BadValue("file target", "contains a /")
    if args.out_dir is None:
        args.out_dir = args.in_dir[0]

    args.File = FileName(args.in_dir, args.target)

    if args.available_channels is None:
        args.available_channels = defaults.CHANNELS[args.File.sensor]

    for fdr in args.in_dir:
        if not os.path.isdir(fdr):
            raise FileMissing('in_dir', fdr)
    if not os.path.isdir(args.out_dir):
        os.makedirs(args.out_dir, defaults.DIR_PERMISSIONS)
    if not os.path.isdir(args.orac_dir):
        raise FileMissing('ORAC repository directory', args.orac_dir)
    if not os.path.isfile(args.orac_lib):
        raise FileMissing('ORAC library file', args.orac_lib)

    return args


def check_args_preproc(args):
    """Ensure preprocessor parser arguments are valid."""

    # Add global attributes
    defaults.GLOBAL_ATTRIBUTES.update({key: val for key, val in args.global_att})
    args.__dict__.update(defaults.GLOBAL_ATTRIBUTES)

    # Insert auxilliary locations
    defaults.AUXILIARIES.update({key: val for key, val in args.aux})
    args.__dict__.update(defaults.AUXILIARIES)

    try:
        # When using ecmwf_dir to set a single directory
        if os.path.isdir(args.ecmwf_dir):
            args.ggam_dir = args.ecmwf_dir
            args.ggas_dir = args.ecmwf_dir
            args.spam_dir = args.ecmwf_dir
    except AttributeError:
        pass

    # Limit should either be all zero or all non-zero.
    limit_check = args.limit[0] == 0
    for limit_element in args.limit[1:]:
        if (limit_element == 0) ^ limit_check:
            warnings.warn('All elements of --limit should be non-zero.',
                          OracWarning, stacklevel=2)

    # Update FileName class
    if args.revision is not None:
        args.File.revision = args.revision
    if "processor" not in args.File.__dict__:
        args.File.processor = args.processor
    if "project" not in args.File.__dict__:
        args.File.project = args.project
    if "product_name" not in args.File.__dict__:
        args.File.product_name = args.product_name

    if args.File.predef and args.l1_land_mask and not args.no_predef:
        raise ValueError("Do not set --l1_land_mask while using predefined "
                         "geostationary geolocation.")

    if not os.path.isdir(args.atlas_dir):
        raise FileMissing('RTTOV Atlas directory', args.atlas_dir)
    # if not os.path.isfile(args.calib_file):
    #    raise FileMissing('AATSR calibration file', args.calib_file)
    if not os.path.isdir(args.coef_dir):
        raise FileMissing('RTTOV coefficients directory', args.coef_dir)
    if not os.path.isdir(args.emis_dir):
        raise FileMissing('RTTOV emissivity directory', args.emis_dir)
    if not os.path.isdir(args.emos_dir):
        raise FileMissing('EMOS temporary directory', args.emos_dir)
    # if not os.path.isdir(args.ggam_dir):
    #    raise FileMissing('ECMWF GGAM directory', args.ggam_dir)
    # if not os.path.isdir(args.ggas_dir):
    #    raise FileMissing('ECMWF GGAS directory', args.ggas_dir)
    # if not os.path.isdir(args.hr_dir) and not args.skip_ecmwf_hr:
    #    raise FileMissing('ECMWF high resolution directory', args.hr_dir)
    # if not os.path.isdir(args.mcd43c3_dir):
    #    raise FileMissing('MODIS MCD43C1 directory', args.mcd43c1_dir)
    # if not os.path.isdir(args.mcd43c1_dir):
    #    raise FileMissing('MODIS MCD43C3 directory', args.mcd43c3_dir)
    # if not os.path.isdir(args.occci_dir):
    #    raise FileMissing('OC CCI directory', args.occci_dir)
    # if not os.path.isdir(args.nise_dir):
    #    raise FileMissing('NISE directory', args.nise_dir)
    # if not os.path.isdir(args.spam_dir):
    #    raise FileMissing('ECMWF SPAM directory', args.spam_dir)
    # if not os.path.isfile(args.usgs_file):
    #    raise FileMissing('USGS file', args.usgs_file)

    return args


def check_args_main(args):
    """Ensure main processor parser arguments are valid."""

    if len(args.in_dir) > 1:
        warnings.warn('Main processor ignores all but first in_dir.',
                      OracWarning, stacklevel=2)
    if not os.path.isdir(args.in_dir[0]):
        raise FileMissing('Preprocessed directory', args.in_dir[0])
    for fdr in args.sad_dirs:
        if not os.path.isdir(fdr):
            raise FileMissing('sad_dirs', fdr)
    if args.use_channels is None:
        args.use_channels = args.available_channels
    if args.multilayer is not None:
        args.approach = "AppCld2L"
    # No error checking yet written for channel arguments

    return args


def check_args_postproc(args):
    """Ensure postprocessor parser arguments are valid."""

    for fdr in args.in_dir:
        if not os.path.isdir(fdr):
            raise FileMissing('Processed output directory', fdr)

    return args


def check_args_cc4cl(args):
    """Ensure ORAC suite wrapper parser arguments are valid."""

    # Add extra lines files
    defaults.EXTRA_LINES.update({key + '_extra': val for key, val in args.extra_lines})
    args.__dict__.update(defaults.EXTRA_LINES)

    log_path = os.path.join(args.out_dir, defaults.LOG_DIR)
    if args.batch and not os.path.isdir(log_path):
        os.makedirs(log_path, defaults.DIR_PERMISSIONS)

    if args.settings_file is not None:
        # A procedure outlined in a file
        try:
            with open(args.settings_file) as settings_file:
                args.settings = settings_file.read().splitlines()
        except IOError:
            raise FileMissing('Description of settings', args.settings_file)

    elif args.preset_settings is not None:
        # A procedure named in local_defaults
        try:
            args.settings = defaults.RETRIEVAL_SETTINGS[args.preset_settings]
        except KeyError:
            raise BadValue("preset settings", "not defined in local_defaults")

    elif args.settings is None:
        if args.phase is None:
            # Default procedure for this sensor from local_defaults
            args.settings = defaults.RETRIEVAL_SETTINGS[args.File.sensor]
        else:
            # Process a single type
            args.settings = (" ",)

    return args


def check_args_regress(args):
    """Ensure regression test arguments are valid."""

    if args.in_dir is None:
        args.in_dir = [defaults.REGRESS_IN_DIR, ]
    for fdr in args.in_dir:
        if not os.path.isdir(fdr):
            raise FileMissing('Regression L1 input directory', fdr)
    if args.out_dir is None:
        args.out_dir = defaults.REGRESS_OUT_DIR

    # Default tests
    if len(args.tests) == 0:
        if args.long:
            args.tests = ['DAYMYD', 'NITMYD', 'DAYMOD', 'NITMOD',
                          'DAYSLSTRA', 'NITSLSTRA', 'DAYSLSTRB', 'NITSLSTRB',
                          'AATSR', 'AVHRR', 'SEVIRI']
        else:
            args.tests = ['DAYMYDS', 'NITMYDS', 'DAYMODS', 'NITMODS',
                          'DAYAATSRS', 'NITAATSRS', 'DAYAVHRRS', 'NITAVHRRS',
                          'DAYSLSTRAS', 'NITSLSTRAS', 'DAYSLSTRBS', 'NITSLSTRBS',
                          'DAYSEVIRIS', 'NITSEVIRIS']

    return args
