"""Routines that build ORAC driver files."""
import os
import warnings

from copy import copy
from datetime import datetime, timedelta
from dateutil.relativedelta import relativedelta
from glob import glob
from pyorac.definitions import OracError, OracWarning, FileMissing


def build_preproc_driver(args):
    """Prepare a driver file for the preprocessor."""
    from itertools import product
    from re import search
    from subprocess import CalledProcessError, check_output, STDOUT
    from uuid import uuid4
    from pyorac.definitions import BadValue
    from pyorac.util import (build_orac_library_path, extract_orac_libraries,
                             read_orac_library_file)

    l1b = _glob_dirs(args.in_dir, args.File.l1b, 'L1B file')
    geo = _glob_dirs(args.in_dir, args.File.geo, 'geolocation file')

    # Select NISE file
    if args.use_ecmwf_snow or args.no_snow_corr:
        nise = ''
    else:
        # There are usually too many files in this directory to glob quickly.
        # Instead, guess where it is. If your search is failing here, but the
        # appropriate file is present, you need to add a format to one of
        # these loops that finds your file.
        nise_locations = (
            'NISE.005/%Y.%m.%d', 'NISE.004/%Y.%m.%d',
            'NISE.002/%Y.%m.%d', 'NISE.001/%Y.%m.%d',
            '%Y', '%Y.%m.%d', '%Y_%m_d', '%Y-%m-%d', ''
        )
        nise_formats = (
            'NISE_SSMISF18_%Y%m%d.HDFEOS', 'NISE_SSMISF17_%Y%m%d.HDFEOS',
            'NISE_SSMIF13_%Y%m%d.HDFEOS',
        )
        for nise_location, nise_format in product(nise_locations, nise_formats):
            nise = args.File.time.strftime(os.path.join(
                args.nise_dir, nise_location, nise_format
            ))
            if os.path.isfile(nise):
                break
        else:
            raise FileMissing('NISE', args.nise_dir)

    # Select previous surface reflectance and emissivity files
    if args.swansea:
        alb = _date_back_search(args.swansea_dir, args.File.time,
                                'SW_SFC_PRMS_%m.nc', 'years')
        brdf = None
    else:
        for ver in (61, 6, 5):
            try:
                alb = _date_back_search(args.mcd43c3_dir, args.File.time,
                                        f'MCD43C3.A%Y%j.{ver:03d}.*.hdf', 'days')
                brdf = None if args.lambertian else _date_back_search(
                    args.mcd43c1_dir, args.File.time,
                    f'MCD43C1.A%Y%j.{ver:03d}.*.hdf', 'days'
                )
                break
            except FileMissing:
                pass
        else:
            raise FileMissing('MODIS albedo', args.mcd43c3_dir)

    if args.use_modis_emis:
        emis = None
    elif args.use_camel_emis:
        emis = _date_back_search(
            args.camel_dir, args.File.time,
            'CAM5K30EM_emis_%Y%m_V???.nc', 'years'
        )
    else:
        emis = _date_back_search(
            args.emis_dir, args.File.time,
            'global_emis_inf10_monthFilled_MYD11C3.A%Y%j.*nc', 'days'
        )

    # Select ECMWF files
    bounds = _bound_time(args.File.time + args.File.dur // 2)
    if args.nwp_flag == 0:
        ecmwf_nlevels = 91
        raise NotImplementedError('Filename syntax for --nwp_flag 0 unknown')
    elif args.nwp_flag == 4:
        ecmwf_nlevels = 60
        ggam = _form_bound_filenames(bounds, args.ggam_dir, 'ggam%Y%m%d%H%M.grb')
        ggas = _form_bound_filenames(bounds, args.ggas_dir, 'ggas%Y%m%d%H%M.nc')
        spam = _form_bound_filenames(bounds, args.spam_dir, 'spam%Y%m%d%H%M.grb')
    elif args.nwp_flag == 3:
        ecmwf_nlevels = 60
        raise NotImplementedError('Filename syntax for --nwp_flag 3 unknown')
    elif args.nwp_flag == 1:
        ecmwf_nlevels = 137
        for form, ec_hour in (('C3D*%m%d%H*.nc', 3),
                              ('ECMWF_OPER_%Y%m%d_%H+00.nc', 6),
                              ('ECMWF_ERA5_%Y%m%d_%H_0.5.nc', 6),
                              ('ECMWF_ERA_%Y%m%d_%H_0.5.nc', 6),
                              ('ECMWF_ERA_%Y%m%d_%H+00_0.5.nc', 6)):
            try:
                bounds = _bound_time(args.File.time + args.File.dur // 2, ec_hour)
                ggam = _form_bound_filenames(bounds, args.ecmwf_dir, form)
                break
            except FileMissing as tmp_err:
                err = tmp_err
        else:
            raise err

        ggas = ["", ""]
        spam = ["", ""]
    elif args.nwp_flag == 2:
        ecmwf_nlevels = 137
        # Interpolation is done in the code
        ggam = [args.ecmwf_dir, args.ecmwf_dir]
        ggas = ["", ""]
        spam = ["", ""]
    else:
        raise BadValue('nwp_flag', args.nwp_flag)

    if args.use_oc:
        for oc_version in (5.0, 4.2, 4.1, 4.0, 3.1, 3.0, 2.0, 1.0):
            try:
                occci = _date_back_search(
                    args.occci_dir, args.File.time,
                    'ESACCI-OC-L3S-IOP-MERGED-1M_MONTHLY'
                    f'_4km_GEO_PML_OCx_QAA-%Y%m-fv{oc_version:.1f}.nc',
                    'years'
                )
                break
            except FileMissing:
                pass
        else:
            raise FileMissing('Ocean Colour CCI', os.path.join(args.occci_dir,
                              'ESACCI-OC-L3S-IOP-MERGED-1M_MONTHLY'
                              '_4km_GEO_PML_OCx_QAA-%Y%m-fvVV.nc'))
    else:
        occci = ''

    # ------------------------------------------------------------------------

    if args.uuid:
        uid = str(uuid4())
    else:
        uid = 'n/a'

    libs = read_orac_library_file(args.orac_lib)
    lib_list = extract_orac_libraries(libs)
    os.environ["LD_LIBRARY_PATH"] = build_orac_library_path(lib_list=lib_list)

    # Determine current time
    production_time = datetime.now().strftime("%Y%m%d%H%M%S")

    # Determine NCDF version from command line
    for fdr in lib_list:
        ncdf_exe = os.path.join(fdr, "..", "bin", "ncdump")
        try:
            tmp0 = check_output(ncdf_exe, stderr=STDOUT, universal_newlines=True)
        except FileNotFoundError:
            continue
        except CalledProcessError:
            raise OracError('ncdump is non-functional.')

        mat0 = search(r'netcdf library version (.+?) of', tmp0)
        if mat0:
            ncdf_version = mat0.group(1)
        else:
            ncdf_version = 'n/a'
            warnings.warn('Output formatting of ncdump may have changed.',
                          OracWarning, stacklevel=2)
        break
    else:
        raise OracError('NetCDF lib improperly built as ncdump not present. '
                        'LD_LIBRARY_PATH=' + os.environ["LD_LIBRARY_PATH"])

    # Fetch ECMWF version from header of NCDF file
    if 3 <= args.nwp_flag <= 4:
        try:
            ecmwf_check_file = ggam[0] if ggam[0].endswith('nc') else ggas[0]
            tmp1 = check_output([ncdf_exe, "-h", ecmwf_check_file],
                                universal_newlines=True)
        except OSError:
            raise FileMissing('ECMWF ggas file', ggas[0])
        mat1 = search(r':history = "(.+?)" ;', tmp1)
        if mat1:
            ecmwf_version = mat1.group(1)
        else:
            ecmwf_version = 'n/a'
            warnings.warn('Header of ECMWF file may have changed.', OracWarning,
                          stacklevel=2)
    elif args.nwp_flag == 2:
        ecmwf_version = 'ERA5'
    else:
        # TODO: Fetch version information from GFS files
        ecmwf_version = 'n/a'

    # RTTOV version number from small executable
    try:
        rttov_version_exe = os.path.join(args.orac_dir, "common", "rttov_version")
        if not os.path.isfile(rttov_version_exe):
            rttov_version_exe = os.path.join(args.orac_dir, "rttov_version")
        rttov_version = check_output(
            rttov_version_exe, universal_newlines=True
        ).strip()
    except CalledProcessError:
        rttov_version = 'n/a'
        warnings.warn('RTTOV library version number unavailable.',
                      OracWarning, stacklevel=2)

    # Fetch GIT version
    cwd = os.getcwd()
    try:
        os.chdir(os.path.join(args.orac_dir, 'pre_processing'))
        tmp3 = check_output(["git", "--version"], universal_newlines=True)
        mat3 = search('git version (.+?)\n', tmp3)
        git_version = mat3.group(1)
    except (FileNotFoundError, CalledProcessError, AttributeError):
        git_version = 'n/a'
        warnings.warn('Unable to call git.', OracWarning, stacklevel=2)
    finally:
        os.chdir(cwd)

    file_version = f'R{args.File.revision}'

    chunk_flag = False  # File chunking no longer required
    assume_full_paths = True  # We pass absolute paths
    cldtype = not args.skip_cloud_type
    include_full_brdf = not args.lambertian

    # ------------------------------------------------------------------------

    # Write driver file
    driver = f"""{args.File.sensor}
{l1b}
{geo}
{args.usgs_file}
{ggam[0]}
{args.coef_dir}
{args.atlas_dir}
{nise}
{alb}
{brdf}
{emis}
{args.dellon}
{args.dellat}
{args.out_dir}
{args.limit[0]}
{args.limit[1]}
{args.limit[2]}
{args.limit[3]}
{ncdf_version}
{args.cfconvention}
{args.institute}
{args.processor}
{args.email}
{args.url}
{file_version}
{args.references}
{args.history}
{args.summary}
{args.keywords}
{args.comments}
{args.project}
{args.license}
{uid}
{production_time}
{args.calib_file}
{args.nwp_flag}
{ggas[0]}
{spam[0]}
{chunk_flag}
{args.day_flag}
{args.verbose}
-
{assume_full_paths}
{include_full_brdf}
{rttov_version}
{ecmwf_version}
{git_version}
ECMWF_TIME_INT_METHOD={args.single_ecmwf}
ECMWF_PATH_2={ggam[1]}
ECMWF_PATH2_2={ggas[1]}
ECMWF_PATH3_2={spam[1]}
USE_ECMWF_SNOW_AND_ICE={args.use_ecmwf_snow}
USE_MODIS_EMIS_IN_RTTOV={args.use_modis_emis}
ECMWF_NLEVELS={ecmwf_nlevels}
USE_L1_LAND_MASK={args.l1_land_mask}
USE_OCCCI={args.use_oc}
OCCCI_PATH={occci}
DISABLE_SNOW_ICE_CORR={args.no_snow_corr}
DO_CLOUD_EMIS={args.cloud_emis}
DO_IRONLY={args.ir_only}
DO_CLDTYPE={cldtype}
USE_CAMEL_EMIS={args.use_camel_emis}
USE_SWANSEA_CLIMATOLOGY={args.swansea}"""

    if args.available_channels is not None:
        driver += "\nN_CHANNELS={}".format(len(args.available_channels))
        driver += "\nCHANNEL_IDS={}".format(
            ','.join(str(k) for k in args.available_channels)
        )
    for part, filename in args.extra_lines:
        if part == "pre" and filename != "":
            try:
                with open(filename, "r") as extra:
                    driver += "\n" + extra.read().rstrip('\n')
            except IOError:
                raise FileMissing('extra_lines_file', filename)
    for sec, key, val in args.additional:
        if sec == "pre":
            driver += f"\n{key}={val}"

    if args.File.predef and not args.no_predef:
        driver += f"""\nUSE_PREDEF_LSM=False
EXT_LSM_PATH={args.prelsm_file}
USE_PREDEF_GEO=False
EXT_GEO_PATH={args.pregeo_file}"""

    if args.product_name is not None:
        driver += f"\nPRODUCT_NAME={args.product_name}"

    return driver


def build_main_driver(args):
    """Prepare a driver file for the main processor."""
    from pyorac.local_defaults import LUT_LOOKUP
    from pyorac.processing_settings import APRIORI_LOOKUP

    # Evaluate requested LUT path and name for this instrument
    sad_dir, sad_file, particle, prior = LUT_LOOKUP[args.lut_name](args.File, True)
    if not os.path.isdir(sad_dir):
        raise FileMissing('LUT directory', sad_dir)

    # Form mandatory driver file lines
    driver = """# ORAC New Driver File
Ctrl%FID%Data_Dir           = "{in_dir}"
Ctrl%FID%Filename           = "{fileroot}"
Ctrl%FID%Out_Dir            = "{out_dir}"
Ctrl%FID%SAD_Dir            = "{sad_dir}"
Ctrl%InstName               = "{sensor}"
Ctrl%Ind%NAvail             = {nch}
Ctrl%Ind%Channel_Proc_Flag  = {channels}
Ctrl%LUTClass               = "{particle}"
Ctrl%Process_Cloudy_Only    = {cloudy}
Ctrl%Process_Aerosol_Only   = {aerosoly}
Ctrl%Verbose                = {verbose}
Ctrl%RS%Use_Full_BRDF       = {use_brdf}""".format(
        aerosoly=args.aerosol_only,
        channels=','.join('1' if k in args.use_channels else '0'
                          for k in args.available_channels),
        cloudy=args.cloud_only,
        fileroot=args.File.root_name(),
        in_dir=args.in_dir[0],
        nch=len(args.available_channels),
        out_dir=args.out_dir,
        particle=particle,
        sad_dir=sad_dir,
        sensor=args.File.sensor + '-' + args.File.platform,
        use_brdf=not (args.lambertian or args.approach == 'AppAerSw'),
        verbose=args.verbose,
    )
    # If a netcdf LUT is being used then write NCDF LUT filename
    if sad_file is not None:
        if not os.path.isfile(os.path.join(sad_dir, sad_file)):
            raise FileMissing('LUT file', os.path.join(sad_dir, sad_file))
        driver += f"\nCtrl%FID%NCDF_LUT_Filename = \"{sad_file}\""

    for state_index, priors in APRIORI_LOOKUP[prior].items():
        for variable_name, value in priors.items():
            driver += _format_driver_line(variable_name, state_index, value)

    # Optional driver file lines
    if args.multilayer is not None:
        sad_dir2, sad_file2, particle2, prior2 = LUT_LOOKUP[args.multilayer[0]](args.File, False)
        if not os.path.isdir(sad_dir2):
            raise FileMissing('LUT2 directory', sad_dir2)

        driver += f"\nCtrl%LUTClass2              = \"{particle2}\""
        driver += f"\nCtrl%FID%SAD_Dir2           = \"{sad_dir2}\""
        driver += f"\nCtrl%Class2                 = {args.multilayer[1]}"
        if sad_file2 is not None:
            if not os.path.isfile(os.path.join(sad_dir2, sad_file2)):
                raise FileMissing('LUT2 file', os.path.join(sad_dir2, sad_file2))
            driver += f"\nCtrl%FID%NCDF_LUT_Filename2 = \"{sad_file2}\""

        # TODO: Lower level prior preferably set from surrounding obs
        for state_index, priors in APRIORI_LOOKUP[prior].items():
            # Add 2 to text state vector index to hit second layer
            if state_index[0] in 'iI':
                state_index += '2'
            for variable_name, value in priors.items():
                driver += _format_driver_line(variable_name, state_index, value)
    if args.types:
        driver += "\nCtrl%NTypes_To_Process      = {:d}".format(len(args.types))
        driver += ("\nCtrl%Types_To_Process(1:{:d}) = ".format(len(args.types)) +
                   ','.join(k + '_TYPE' for k in args.types))
    if args.sabotage:
        driver += "\nCtrl%Sabotage_Inputs        = true"
    if args.approach:
        driver += "\nCtrl%Approach               = " + args.approach
    if args.ret_class:
        driver += "\nCtrl%Class                  = " + args.ret_class
    if args.no_sea:
        driver += "\nCtrl%Surfaces_To_Skip       = ISea"
    elif args.no_land:
        driver += "\nCtrl%Surfaces_To_Skip       = ILand"
    for part, filename in args.extra_lines:
        if part == "main" and filename != "":
            try:
                with open(filename, "r") as extra:
                    driver += "\n" + extra.read()
            except IOError:
                raise FileMissing('extra_lines_file', filename)
    for sec, key, val in args.additional:
        if sec == "main":
            driver += f"\n{key} = {val}"

    return driver


def build_postproc_driver(args, files):
    """Prepare a driver file for the postprocessor."""

    # Use Bayesian type selection for all but standard Cloud CCI work
    cci_cloud = (len(files) == 2 and "wat" in files[0].lower() and
                 "ice" in files[1].lower())

    try:
        multilayer = args.approach == 'AppCld2L'
    except AttributeError:
        multilayer = any("_" in typ for typ in args.lut_names)

    # Form driver file
    driver = """{multilayer}
{wat_pri}
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
USE_CHUNKING={chunking}
USE_NETCDF_COMPRESSION={compress}
USE_BAYESIAN_SELECTION={bayesian}""".format(
        bayesian=not cci_cloud,
        chunking=args.chunking,
        compress=args.compress,
        cost_tsh=args.cost_thresh,
        ice_pri=files[1],
        ice_sec=files[1].replace('primary', 'secondary'),
        multilayer=multilayer,
        opt_nght=not args.no_night_opt,
        out_pri=args.target,
        out_sec=args.target.replace('primary', 'secondary'),
        prob_tsh=args.prob_thresh,
        switch=not args.no_switch_phase,
        verbose=args.verbose,
        wat_pri=files[0],
        wat_sec=files[0].replace('primary', 'secondary'),
    )

    # Add additional files
    for filename in files[2:]:
        driver += '\n'
        driver += filename
        driver += '\n'
        driver += filename.replace('primary', 'secondary')
    for part, filename in args.extra_lines:
        if part == "post" and filename != "":
            try:
                with open(filename, "r") as extra:
                    driver += "\n" + extra.read()
            except IOError:
                raise FileMissing('extra_lines_file', filename)
    for sec, key, val in args.additional:
        if sec == "post":
            driver += f"\n{key} = {val}"

    return driver


# -----------------------------------------------------------------------------

def _bound_time(date=None, delta_hours=6):
    """Return timestamps divisible by some duration that bound a given time

    http://stackoverflow.com/questions/3463930/how-to-round-the-minute-of-a-datetime-object-python/10854034

    Args:
    :datetime dt: Initial time.
    :timedelta date_delta: Rounding interval. Default 6 hours.
    """

    date_delta = timedelta(hours=delta_hours)
    round_to = date_delta.total_seconds()
    if date is None:
        date = datetime.now()

    # Remove annoying microseconds
    time = date + timedelta(0, 0, -date.microsecond)

    # Floor time to requested delta
    seconds = (date - date.min).seconds
    rounding = seconds // round_to * round_to
    start = time + timedelta(0, rounding - seconds)

    # Output floor and ceil of time
    return start, start + date_delta


def _date_back_search(fdr, date_in, pattern, interval):
    """Search a folder for the file with timestamp closest before a given date.

    Args:
    :str fdr: Folder to be searched.
    :datetime date: Initial date to consider.
    :str pattern: strftime format string used to parse filename.
    :str interval: Keyword of relativedelta indicating interval to step back.
    """

    # Step forward one day, month, or year
    delta = relativedelta(**{interval: 1})

    date = copy(date_in)
    # We only want to look back so far, depending on the interval
    if interval == 'days':
        earliest = date - relativedelta(months=1)
    elif interval == 'months':
        earliest = date - relativedelta(years=1)
    else:
        earliest = datetime(1970, 1, 1)

    while date > earliest:
        # Look for a file with the appropriate date
        files = glob(date.strftime(os.path.join(fdr, pattern)))

        if len(files) >= 1:
            return files[-1]
        else:
            date -= delta

    # If we fail, try to find a climatological file
    if 'XXXX' not in pattern:
        return _date_back_search(fdr.replace('%Y', 'XXXX'), date_in,
                                 pattern.replace('%Y', 'XXXX'), 'days')
    else:
        raise FileMissing(fdr, pattern)


def _form_bound_filenames(bounds, fdr, form):
    """Form 2-element lists of filenames from bounding timestamps.

    Args:
    :list bounds: List of time to use.
    :str fdr: Folder containing BADC files.
    :str form: Formatting string for strftime"""

    out = [time.strftime(os.path.join(fdr, form)) for time in bounds]

    for i, path in enumerate(out):
        filename = glob(path)
        try:
            out[i] = filename[-1]
        except IndexError:
            raise FileMissing('ECMWF file', path)
    return out


def _format_driver_line(variable_name, index, value):
    """Appropriately formats human-readable apriori information"""
    if variable_name == 'AP':
        return f"\nCtrl%XB[{index}] = {value}"
    if variable_name == 'FG':
        return f"\nCtrl%X0[{index}] = {value}"
    if variable_name == 'SX':
        return f"\nCtrl%SX[{index}] = {value}"

    return f"\n{variable_name}[{index}] = {value}"


def _glob_dirs(dirs, path, desc):
    """Search a number of directories for files satisfying some simple regex"""
    from os import stat

    files = [f for d in dirs for f in glob(os.path.join(d, path))]

    if files:
        # Return the most recent file found
        files.sort(key=lambda x: stat(x).st_mtime, reverse=True)
        return files[0]

    # No file found, so throw error
    raise FileMissing(desc, path)
