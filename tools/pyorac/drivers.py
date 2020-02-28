"""Routines that build ORAC driver files."""
import os
import warnings

from datetime import datetime, timedelta
from glob import glob
from pyorac.definitions import OracError, OracWarning, FileMissing


def build_preproc_driver(args):
    """Prepare a driver file for the preprocessor."""
    from pyorac.definitions import FileName, BadValue
    from pyorac.util import (build_orac_library_path, extract_orac_libraries,
                             read_orac_library_file)
    from re import search
    from subprocess import CalledProcessError, check_output, STDOUT
    from uuid import uuid4

    file = _glob_dirs(args.in_dir, args.File.l1b, 'L1B file')
    geo  = _glob_dirs(args.in_dir, args.File.geo, 'geolocation file')

    # Select NISE file
    if args.use_ecmwf_snow or args.no_snow_corr:
        nise = ''
    else:
        for form in ('NISE.004/%Y.%m.%d/NISE_SSMISF17_%Y%m%d.HDFEOS',
                     'NISE.002/%Y.%m.%d/NISE_SSMIF13_%Y%m%d.HDFEOS',
                     '%Y/NISE_SSMIF13_%Y%m%d.HDFEOS',
                     '%Y/NISE_SSMIF17_%Y%m%d.HDFEOS'):
            nise = args.File.time.strftime(os.path.join(args.nise_dir, form))
            if os.path.isfile(nise):
                break
        else:
            raise FileMissing('NISE', nise)

    # Select previous surface reflectance and emissivity files
    if args.swansea:
        alb = _date_back_search(args.swansea_dir, args.File.time,
                                'SW_SFC_PRMS_%m.nc', 'months')
        brdf = None
    else:
        alb = _date_back_search(args.mcd43c3_dir, args.File.time,
                                'MCD43C3.A%Y%j.*.hdf', 'days')
        brdf = None if args.lambertian else _date_back_search(
            args.mcd43c1_dir, args.File.time, 'MCD43C1.A%Y%j.*.hdf', 'days'
        )
    if args.use_modis_emis:
        emis = None
    elif args.use_camel_emis:
        emis = _date_back_search(
            args.camel_dir, args.File.time,
            'CAM5K30EM_emis_%Y%m_V???.nc', 'months'
        )
    else:
        emis = _date_back_search(
            args.emis_dir, args.File.time,
            'global_emis_inf10_monthFilled_MYD11C3.A%Y%j.*nc', 'days'
        )

    # Select ECMWF files
    bounds = _bound_time(args.File.time + args.File.dur//2)
    if args.ecmwf_flag == 0:
        ggam = _form_bound_filenames(bounds, args.ggam_dir,
                                     'ERA_Interim_an_%Y%m%d_%H+00.nc')
    elif args.ecmwf_flag == 1:
        ggam = _form_bound_filenames(bounds, args.ggam_dir, 'ggam%Y%m%d%H%M.nc')
        ggas = _form_bound_filenames(bounds, args.ggas_dir, 'ggas%Y%m%d%H%M.nc')
        spam = _form_bound_filenames(bounds, args.spam_dir, 'gpam%Y%m%d%H%M.nc')
    elif args.ecmwf_flag == 2:
        ggam = _form_bound_filenames(bounds, args.ggam_dir, 'ggam%Y%m%d%H%M.grb')
        ggas = _form_bound_filenames(bounds, args.ggas_dir, 'ggas%Y%m%d%H%M.nc')
        spam = _form_bound_filenames(bounds, args.spam_dir, 'spam%Y%m%d%H%M.grb')
    elif args.ecmwf_flag == 3:
        raise NotImplementedError('Filename syntax for --ecmwf_flag 3 unknown')
    elif args.ecmwf_flag == 4:
        for form, hr in (('C3D*%m%d%H*.nc', 3),
                         ('ECMWF_OPER_%Y%m%d_%H+00.nc', 6),
                         ('ECMWF_ERA_%Y%m%d_%H+00_0.5.nc', 6)):
            try:
                bounds = _bound_time(args.File.time + args.File.dur//2,
                                     timedelta(hours=hr))
                ggam = _form_bound_filenames(bounds, args.ggam_dir, form)
                break
            except FileMissing as e:
                err = e
        else:
            raise err

        ggas = ggam
        spam = ggam
    else:
        raise BadValue('ecmwf_flag', args.ecmwf_flag)

    if not args.skip_ecmwf_hr:
        #hr_ecmwf = _form_bound_filenames(bounds, args.hr_dir,
        #                                 'ERA_Interim_an_%Y%m%d_%H+00_HR.grb')
        # These files don't zero-pad the hour for some reason
        bounds = _bound_time(args.File.time + args.File.dur//2, timedelta(hours=6))
        hr_ecmwf = [time.strftime(os.path.join(
            args.hr_dir, 'ERA_Interim_an_%Y%m%d_') +
            '{:d}+00_HR.grb'.format(time.hour*100))
                    for time in bounds]
        if not os.path.isfile(hr_ecmwf[0]):
            hr_ecmwf = [time.strftime(os.path.join(
                args.hr_dir, 'ERA_Interim_an_%Y%m%d_') +
                '{:d}+00_HR.grb'.format(time.hour*100))
                        for time in bounds]
        for f in hr_ecmwf:
            if not os.path.isfile(f):
                raise FileMissing('HR ECMWF file', f)
    else:
        hr_ecmwf=['', '']

    if args.use_oc:
        for oc_version in (4.2, 4.1, 4.0, 3.1, 3.0, 2.0, 1.0):
            occci = args.File.time.strftime(os.path.join(
                args.occci_dir, 'ESACCI-OC-L3S-IOP-MERGED-1M_MONTHLY'
                '_4km_GEO_PML_OCx_QAA-%Y%m-fv{:.1f}.nc'.format(oc_version)
            ))
            if os.path.isfile(occci):
                break
            else:
                raise FileMissing('Ocean Colour CCI', occci)
    else:
        occci = ''

    #------------------------------------------------------------------------

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
            raise OracError('ncdump does non-functional.')

        m0 = search(r'netcdf library version (.+?) of', tmp0)
        if m0:
            ncdf_version = m0.group(1)
        else:
            ncdf_version = 'n/a'
            warnings.warn('Output formatting of ncdump may have changed.',
                          OracWarning, stacklevel=2)
        break
    else:
        raise OracError('NetCDF lib improperly built as ncdump not present.')

    # Fetch ECMWF version from header of NCDF file
    try:
        ecmwf_check_file = ggam[0] if ggam[0].endswith('nc') else ggas[0]
        tmp1 = check_output([ncdf_exe, "-h", ecmwf_check_file],
                            universal_newlines=True)
    except OSError:
        raise FileMissing('ECMWF ggas file', ggas[0])
    m1 = search(r':history = "(.+?)" ;', tmp1)
    if m1:
        ecmwf_version = m1.group(1)
    else:
        ecmwf_version = 'n/a'
        warnings.warn('Header of ECMWF file may have changed.', OracWarning,
                      stacklevel=2)

    # RTTOV version number from small executable
    try:
        rttov_version = check_output(
            os.path.join(args.orac_dir, "common", "rttov_version"),
            universal_newlines=True
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
        m3 = search('git version (.+?)\n', tmp3)
        git_version = m3.group(1)
    except:
        git_version = 'n/a'
        warnings.warn('Unable to call git.', OracWarning, stacklevel=2)
    finally:
        os.chdir(cwd)

    # Fetch repository commit number
    if not args.revision:
        args.revision = get_repository_revision()

    file_version = 'R{}'.format(args.revision)

    #------------------------------------------------------------------------

    # Write driver file
    driver = """{sensor}
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
{git_version}
ECMWF_TIME_INT_METHOD={ecmwf_int_method}
ECMWF_PATH_2={ggam[1]}
ECMWF_PATH2_2={ggas[1]}
ECMWF_PATH3_2={spam[1]}
USE_HR_ECMWF={use_ecmwf_hr}
ECMWF_PATH_HR={ecmwf_hr[0]}
ECMWF_PATH_HR_2={ecmwf_hr[1]}
USE_ECMWF_SNOW_AND_ICE={ecmwf_nise}
USE_MODIS_EMIS_IN_RTTOV={modis_emis}
ECMWF_NLEVELS={ecmwf_nlevels}
USE_L1_LAND_MASK={l1_land_mask}
USE_OCCCI={use_occci}
OCCCI_PATH={occci_file}
DISABLE_SNOW_ICE_CORR={no_snow}
DO_CLOUD_EMIS={cld_emis}
DO_IRONLY={ir_only}
DO_CLDTYPE={cldtype}
USE_CAMEL_EMIS={camel}
USE_SWANSEA_CLIMATOLOGY={swansea}""".format(
        alb               = alb,
        assume_full_paths = True, # Above file searching returns paths nor dirs
        atlas             = args.atlas_dir,
        atsr_calib        = args.calib_file,
        brdf              = brdf,
        camel             = args.use_camel_emis,
        chunk_flag        = False, # File chunking no longer required
        cldtype           = not args.skip_cloud_type,
        cld_emis          = args.cloud_emis,
        coef              = args.coef_dir,
        comment           = args.comments,
        conventions       = args.cfconvention,
        creator_email     = args.email,
        creator_url       = args.url,
        day_flag          = args.day_flag, # 0=1=Day, 2=Night
        dellat            = args.dellat,
        dellon            = args.dellon,
        ecmwf_flag        = args.ecmwf_flag,
        ecmwf_hr          = hr_ecmwf,
        ecmwf_int_method  = args.single_ecmwf,
        ecmwf_nise        = args.use_ecmwf_snow,
        ecmwf_nlevels     = args.ecmwf_nlevels,
        ecmwf_version     = ecmwf_version,
        emis              = emis,
        file_version      = file_version,
        geo               = geo,
        ggam              = ggam,
        ggas              = ggas,
        history           = args.history,
        include_full_brdf = not args.lambertian,
        institution       = args.institute,
        ir_only           = args.ir_only,
        keywords          = args.keywords,
        l1_land_mask      = args.l1_land_mask,
        l1b               = file,
        l2_processor      = args.processor,
        license           = args.license,
        limit             = args.limit,
        modis_emis        = args.use_modis_emis,
        ncdf_version      = ncdf_version,
        nise              = nise,
        no_snow           = args.no_snow_corr,
        occci_file        = occci,
        out_dir           = args.out_dir,
        usgs              = args.usgs_file,
        production_time   = production_time,
        project           = args.project,
        references        = args.references,
        rttov_version     = rttov_version,
        sensor            = args.File.sensor,
        spam              = spam,
        summary           = args.summary,
        swansea           = args.swansea,
        git_version       = git_version,
        uuid              = uid,
        use_ecmwf_hr      = not args.skip_ecmwf_hr,
        use_occci         = args.use_oc,
        verbose           = args.verbose,
    )

    if args.available_channels is not None:
        driver += "\nN_CHANNELS={}".format(len(args.available_channels))
        driver += "\nCHANNEL_IDS={}".format(
            ','.join(str(k) for k in args.available_channels)
        )
    for part, f in args.extra_lines:
        if part == "pre" and f != "":
            try:
                with open(f, "r") as e:
                    driver += "\n" + e.read()
            except IOError:
                raise FileMissing('extra_lines_file', f)
    for sec, key, val in args.additional:
        if sec == "pre":
            driver += "\n{}={}".format(key, val)

    if args.File.predef and not args.no_predef:
        driver += """
USE_PREDEF_LSM=True
EXT_LSM_PATH={lsm}
USE_PREDEF_GEO=True
EXT_GEO_PATH={geo}""".format(lsm = args.prelsm_file, geo = args.pregeo_file)

    if args.product_name is not None:
        driver += "\nPRODUCT_NAME={}".format(args.product_name)

    return driver


def build_main_driver(args):
    """Prepare a driver file for the main processor."""
    from pyorac.definitions import FileName, SETTINGS

    # Form mandatory driver file lines
    driver = """# ORAC New Driver File
Ctrl%FID%Data_Dir           = "{in_dir}"
Ctrl%FID%Filename           = "{fileroot}"
Ctrl%FID%Out_Dir            = "{out_dir}"
Ctrl%FID%SAD_Dir            = "{sad_dir}"
Ctrl%InstName               = "{sensor}"
Ctrl%Ind%NAvail             = {nch}
Ctrl%Ind%Channel_Proc_Flag  = {channels}
Ctrl%LUTClass               = "{phase}"
Ctrl%Process_Cloudy_Only    = {cloudy}
Ctrl%Process_Aerosol_Only   = {aerosoly}
Ctrl%Verbose                = {verbose}
Ctrl%RS%Use_Full_BRDF       = {use_brdf}""".format(
        aerosoly = args.aerosol_only,
        channels = ','.join('1' if k in args.use_channels else '0'
                            for k in args.available_channels),
        cloudy   = args.cloud_only,
        fileroot = args.File.root_name(),
        in_dir   = args.in_dir[0],
        nch      = len(args.available_channels),
        out_dir  = args.out_dir,
        phase    = args.phase,
        sad_dir  = SETTINGS[args.phase].sad_dir(args.sad_dirs, args.File),
        sensor   = args.File.inst,
        use_brdf = not (args.lambertian or args.approach == 'AppAerSw'),
        verbose  = args.verbose,
    )

    # Optional driver file lines
    if args.multilayer is not None:
        driver += """
Ctrl%LUTClass2              = "{}"
Ctrl%FID%SAD_Dir2           = "{}"
Ctrl%Class2                 = {}""".format(
        args.multilayer[0],
        SETTINGS[args.multilayer[0]].sad_dir(args.sad_dirs, args.File),
        args.multilayer[1],
    )
    if args.types:
        driver += "\nCtrl%NTypes_To_Process      = {:d}".format(len(args.types))
        driver += ("\nCtrl%Types_To_Process(1:{:d}) = ".format(len(args.types)) +
                   ','.join(k+'_TYPE' for k in args.types))
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
    for var in SETTINGS[args.phase].inv:
        driver += var.driver()
    for part, f in args.extra_lines:
        if part == "main" and f != "":
            try:
                with open(f, "r") as e:
                    driver += "\n" + e.read()
            except IOError:
                raise FileMissing('extra_lines_file', f)
    for sec, key, val in args.additional:
        if sec == "main":
            driver += "\n{} = {}".format(key, val)

    return driver


def build_postproc_driver(args, files):
    """Prepare a driver file for the postprocessor.

    If the optional argument files is not specified, this will search args.in_dir
    for primary files with name given by args.target and args.phases."""

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
        bayesian   = args.phases != ['WAT', 'ICE'],
        chunking   = args.chunking,
        compress   = args.compress,
        cost_tsh   = args.cost_thresh,
        ice_pri    = files[1],
        ice_sec    = files[1].replace('primary', 'secondary'),
        multilayer = args.approach == 'AppCld2L',
        opt_nght   = not args.no_night_opt,
        out_pri    = args.target,
        out_sec    = args.target.replace('primary', 'secondary'),
        prob_tsh   = args.prob_thresh,
        switch     = args.switch_phase,
        verbose    = args.verbose,
        wat_pri    = files[0],
        wat_sec    = files[0].replace('primary', 'secondary'),
    )

    # Add additional files
    for f in files[2:]:
        driver += '\n'
        driver += f
        driver += '\n'
        driver += f.replace('primary', 'secondary')
    for part, f in args.extra_lines:
        if part == "post" and f != "":
            try:
                with open(f, "r") as e:
                    driver += "\n" + e.read()
            except IOError:
                raise FileMissing('extra_lines_file', f)
    for sec, key, val in args.additional:
        if sec == "post":
            driver += "\n{} = {}".format(key, val)

    return driver

#-----------------------------------------------------------------------------

def _bound_time(dt=None, dateDelta=timedelta(hours=6)):
    """Return timestamps divisible by some duration that bound a given time

    http://stackoverflow.com/questions/3463930/how-to-round-the-minute-of-a-datetime-object-python/10854034

    Args:
    :datetime dt: Initial time.
    :timedelta dateDelta: Rounding interval. Default 6 hours.
    """

    roundTo = dateDelta.total_seconds()
    if dt is None : dt = datetime.now()

    # Remove annoying microseconds
    time = dt + timedelta(0,0,-dt.microsecond)

    # Floor time to requested delta
    seconds = (dt - dt.min).seconds
    rounding = seconds // roundTo * roundTo
    start = time + timedelta(0,rounding-seconds)

    # Output floor and ceil of time
    return (start, start + dateDelta)


def _date_back_search(fdr, date, pattern, interval):
    """Search a folder for the file with timestamp closest before a given date.

    Args:
    :str fdr: Folder to be searched.
    :datetime date: Initial date to consider.
    :str pattern: strftime format string used to parse filename.
    :str interval: Keyword of relativedelta indicating interval to step back.
    """
    from copy import copy
    from dateutil.relativedelta import relativedelta

    # Step forward one day, month, or year
    delta = relativedelta(**{interval: 1})

    dt = copy(date)
    while dt > datetime(1995, 1, 1):
        # Look for a file with the appropriate date
        files = glob(dt.strftime(os.path.join(fdr, pattern)))

        if len(files) >= 1:
            return files[-1]
        else:
            dt -= delta

    # If we fail, try to find a climatological file
    files = glob(dt.strftime(os.path.join(fdr, pattern.replace('%Y','XXXX'))))
    if len(files) >= 1:
        return files[-1]
    else:
        raise FileMissing(fdr, pattern)


def _form_bound_filenames(bounds, fdr, form):
    """Form 2-element lists of filenames from bounding timestamps.

    Args:
    :list bounds: List of time to use.
    :str fdr: Folder containing BADC files.
    :str form: Formatting string for strftime"""

    out = [time.strftime(os.path.join(fdr, form)) for time in bounds]

    for i in range(len(out)):
        f2 = glob(out[i])
        if len(f2) == 0:
            raise FileMissing('ECMWF file', out[i])
        else:
            out[i] = f2[len(f2)-1]
    return out


def _glob_dirs(dirs, path, desc):
    """Search a number of directories for files satisfying some simple regex"""
    from os import stat

    files = [f for d in dirs for f in glob(os.path.join(d, path))]

    if len(files) == 0:
        # No file found, so throw error
        raise FileMissing(desc, path)
    else:
        # Return the most recent file found
        files.sort(key=lambda x: stat(x).st_mtime, reverse=True)
        return files[0]
