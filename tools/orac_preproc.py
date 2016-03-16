#!/usr/bin/python3
# Run preprocessor for ORAC community code
# 16 Feb 2016, AP: Initial version

import argparse
import datetime
from glob import glob
import os
import re
import subprocess
import tempfile
from termcolor import cprint

import orac_utils

#-----------------------------------------------------------------------------

# Form 2-element lists of BADC filenames from bounding timestamps
def form_badc(
    fld,     # Folder containing BADC files
    pref,    # Four letter prefix for BADC file name (i.e. ggas)
    suff,    # Suffix for file name (i.e. .nc or .grb)
    bounds): # List of times to be

    out = [fld + time.strftime('/%Y/%m/%d/'+pref+'%Y%m%d%H%M'+suff)
           for time in bounds]
    for f in out:
        if not os.path.isfile(f):
            raise NameError('Cannot locate ECMWF file '+f)
    return out

#-----------------------------------------------------------------------------

def orac_preproc(args):
    # Translate arguments
    fileroot = args.file[:args.file.rfind('.')]
    ggam_dir = args.ecmwf_dir + '/era_interim'
    ggas_dir = args.ecmwf_dir + '/era_interim'
    spam_dir = args.ecmwf_dir + '/era_interim'
    hr_ecmwf_dir = args.ecmwf_dir + '/era_hr'

    #------------------------------------------------------------------------

    # Parse filename
    (sensor, platform) = orac_utils.parse_sensor(args.file)
    if sensor == 'AATSR':

        # Start date and time of orbit given in filename
        yr = int(args.file[14:18])
        mn = int(args.file[18:20])
        dy = int(args.file[20:22])
        hr = int(args.file[23:25])
        mi = int(args.file[25:27])
        sc = int(args.file[27:29])
        st_time = datetime.datetime(yr, mn, dy, hr, mi, sc, 0)

        # File duration is given in filename
        dur = datetime.timedelta(seconds=int(args.file[30:38]))

        # Only one file for ATSR
        if args.geo_dir != None:
            print('--geo_dir is ignored with AATSR files.')
            raise Warning()
        geo = args.in_dir+'/'+args.file

    elif sensor == 'MODIS':
        # Start DOY and time of orbit given in filename
        yr = int(args.file[10:14])
        dy = int(args.file[14:17])
        hr = int(args.file[18:20])
        mi = int(args.file[20:22])
        st_time = (datetime.datetime(yr, 1, 1, hr, mi) +
                   datetime.timedelta(days=dy-1))

        # Guess the duration
        dur = datetime.timedelta(minutes=5)

        # Search for geolocation file
        if args.geo_dir == None:
            args.geo_dir = args.in_dir
        geo_search = (args.geo_dir + '/' + args.file[0:3] + '03.A' +
                      args.file[10:26] +'*hdf')
        f = glob(geo_search)
        if f:
            if len(f) == 1:
                geo = f[0]
            else:
                geo = f[-1]
        else:
            raise NameError('Could not find MODIS geolocation file using ' +
                            geo_search)

    elif sensor == 'AVHRR':
        # Start time of orbit given in filename
        yr = int(args.file[7:11])
        mn = int(args.file[11:13])
        dy = int(args.file[13:15])
        hr = int(args.file[16:18])
        mi = int(args.file[18:20])
        st_time = datetime.datetime(yr, mn, dy, hr, mi)

        # Guess the duration
        dur = datetime.timedelta(seconds=6555)

        # Guess the geolocation file
        if args.geo_dir == None:
            args.geo_dir = args.in_dir
        p_ = args.file.rfind('_')
        geo = args.geo_dir + '/' + args.file[0:p_+1] + 'sunsatangles.h5'

    if not os.path.isfile(geo):
        raise NameError('Could not locate expected geolocation file '+geo)

    # Select NISE file
    if not args.use_ecmwf_snow:
        nise = (args.nise_dir + st_time.strftime('/NISE.004/%Y.%m.%d/'+
                                                 'NISE_SSMISF17_%Y%m%d.HDFEOS'))
        if not os.path.isfile(nise):
            nise = (args.nise_dir + st_time.strftime('/NISE.002/%Y.%m.%d/'+
                                                 'NISE_SSMIF13_%Y%m%d.HDFEOS'))
            if not os.path.isfile(nise):
                raise NameError('Cannot locate NISE file '+nise)

    # Select previous surface reflectance and emissivity files
    alb  = orac_utils.date_back_search(args.mcd43_dir, st_time,
                                       '/%Y/MCD43C3.A%Y%j.005.*.hdf')
    if not args.lambertian:
        brdf = orac_utils.date_back_search(args.mcd43_dir, st_time,
                                           '/%Y/MCD43C1.A%Y%j.005.*.hdf')
    if not args.use_modis_emis:
        emis = orac_utils.date_back_search(args.emis_dir, st_time,
               '/global_emis_inf10_monthFilled_MYD11C3.A%Y%j.041.nc')

    # Select ECMWF files
    bounds = orac_utils.boundTime(st_time + 0.5*dur)
    ggam = form_badc(ggam_dir, 'ggam', '.grb', bounds)
    ggas = form_badc(ggas_dir, 'ggas', '.nc',  bounds)
    spam = form_badc(spam_dir, 'spam', '.grb', bounds)
    if not args.skip_ecmwf_hr:
        hr_ecmwf = [hr_ecmwf_dir + time.strftime('/ERA_Interim_an_%Y%m%d_') +
                    '{:d}+00_HR.grb'.format(time.hour*100) for time in bounds]
        for f in hr_ecmwf:
            if not os.path.isfile(f):
                raise NameError('Cannot locate HR ECMWF file '+f)

    #------------------------------------------------------------------------

    if not os.path.isdir(args.out_dir):
        os.makedirs(args.out_dir, 0o774)

    if args.uuid:
        uid = str(uuid.uuid4())
    else:
        uid = 'n/a'

    # Form processing environment
    libs = orac_utils.read_orac_libraries(args.orac_lib)
    os.environ["LD_LIBRARY_PATH"] = orac_utils.build_orac_library_path(libs)
    os.environ["PATH"] = libs["NCDFLIB"][:-4] + '/bin:' + os.environ["PATH"]
    # Above assumes the end of NCDFLIB is /lib

    # Define a directory for EMOS to put it's gridding
    os.environ["PPDIR"] = args.emos_dir

    # Determine current time
    production_time = datetime.datetime.now().strftime("%Y%m%d%H%M%S")

    # Determine NCDF version from command line
    try:
        tmp0 = subprocess.check_output("ncdump", stderr=subprocess.STDOUT,
                                       universal_newlines=True)
    except OSError:
        raise SystemError('NetCDF lib improperly built as ncdump not present.')
    m0 = re.search(r'netcdf library version (.+?) of', tmp0)
    if m0:
        ncdf_version = m0.group(1)
    else:
        ncdf_version = 'n/a'
        print('Output formatting of ncdump may have changed.')

    # Fetch ECMWF version from header of NCDF file
    try:
        tmp1 = subprocess.check_output("ncdump -h "+ggas[0], shell=True,
                                       universal_newlines=True)
    except OSError:
        raise ValueError('Cannot open file ' + ggas[0])
    m1 = re.search(r':history = "(.+?)" ;', tmp1)
    if m1:
        ecmwf_version = m1.group(1)
    else:
        ecmwf_version = 'n/a'
        print('Header of ECMWF file may have changed.')

    # Strip RTTOV version from library definition
    m2 = re.search(r'/rttov(.+?)/lib', libs['RTTOVLIB'])
    if m2:
        rttov_version = m2.group(1)
    else:
        rttov_version = 'n/a'
        print('Naming of RTTOV library directory may have changed.')

    # Fetch SVN version
    os.chdir(args.orac_dir + '/pre_processing')
    try:
        tmp3 = subprocess.check_output("svn --version", shell=True,
                                       universal_newlines=True)
    except OSError:
        raise SystemError('SVN software not present.')
    m3 = re.search('svn, version (.+?)\n', tmp3)
    if m3:
        svn_version = m3.group(1)
    else:
        svn_version = 'n/a'
        print('Format of svn --version may have changed.')

    # Fetch repository commit number
    if args.version:
        file_version = 'R{}'.format(args.version)
    else:
        try:
            tmp4 = subprocess.check_output("svn info", shell=True,
                                           universal_newlines=True)
        except:
            file_version = 'R0'
            print('Preprocessor folder not under version control.')
        m4 = re.search('Revision: (\d+?)\n', tmp4)
        if m4:
            if args.clean:
                file_version = 'R' + m4.group(1)
            else:
                file_version = 'R{}'.format(int(m4.group(1))+1)
        else:
            file_version = 'R0'
            print('Format of svn info may have changed.')

    #------------------------------------------------------------------------

    # Write driver file
    driver = orac_utils.preprocessor_driver(
        alb               = alb,
        assume_full_paths = True, # Above file searching returns paths nor dirs
        atlas             = args.atlas_dir,
        atsr_calib        = args.calib_file,
        brdf              = brdf,
        chunk_flag        = False, # File chunking no longer required
        coef              = args.coef_dir,
        comment           = args.comments,
        conventions       = args.cfconvention,
        creator_email     = args.email,
        creator_url       = args.url,
        day_flag          = 3, # 0=1=Day, 2=Night
        dellat            = args.dellat,
        dellon            = args.dellon,
        ecmwf_flag        = 2, # Use ECMWF files stored at BADC (2 NCDF, 1 GRIB)
        ecmwf_hr          = hr_ecmwf,
        ecmwf_int_method  = args.single_ecmwf,
        ecmwf_nise        = args.use_ecmwf_snow,
        ecmwf_version     = ecmwf_version,
        emis              = emis,
        file_version      = file_version,
        geo               = geo,
        ggam              = ggam,
        ggas              = ggas,
        history           = args.history,
        include_full_brdf = not args.lambertian,
        institution       = args.institute,
        keywords          = args.keywords,
        l1b               = args.in_dir+'/'+args.file,
        l2_processor      = args.processor,
        license           = args.license,
        limit             = args.limit,
        modis_emis        = args.use_modis_emis,
        ncdf_version      = ncdf_version,
        out_dir           = args.out_dir,
        usgs              = args.usgs_file,
        nise              = nise,
        production_time   = production_time,
        project           = args.project,
        references        = args.references,
        rttov_version     = rttov_version,
        sensor            = sensor,
        spam              = spam,
        summary           = args.summary,
        svn_version       = svn_version,
        uuid              = uid,
        use_ecmwf_hr      = not args.skip_ecmwf_hr,
        verbose           = args.verbose
        )

    if args.channel_ids:
        driver += "\nN_CHANNELS={}".format(len(args.channel_ids))
        driver += "\nCHANNEL_IDS={}".format(','.join(str(k)
                                                     for k in args.channel_ids))

    outroot = '-'.join((args.project, 'L2', 'CLOUD', 'CLD',
                        '_'.join((sensor, args.processor, platform,
                               st_time.strftime('%Y%m%d%H%M'), file_version))))
    if (os.path.isfile(args.out_dir+'/'+outroot+'.loc.nc') and
        args.no_clobber):
        # Skip already processed files
        if args.verbose or args.progress:
            print(orac_utils.star * 60)
            print(orac_utils.star * 5 +' '+fileroot+' already preprocessed')
            print(orac_utils.star * 60)
    else:
        # Write driver file
        (fd, driver_file) = tempfile.mkstemp('.driver', 'PREPROC.',
                                             args.out_dir, True)
        try:
            f = os.fdopen(fd, "w")
            f.write(driver)
            f.close()
        except IOError:
            print('Cannot create driver file '+driver_file)

        # Call preprocessor
        if args.verbose or args.progress:
            print(orac_utils.star * 60)
            print(orac_utils.star * 5 +' orac_preproc.x <<')
            print(orac_utils.star * 5 +' '+
                  driver.replace("\n", "\n"+orac_utils.star*5+" "))
            print(orac_utils.star * 60)
        try:
            subprocess.check_call('orac_preproc.x '+driver_file, shell=True)
        except subprocess.CalledProcessError as err:
            cprint('Preprocessing failed with error code {}'.format(err.returncode),'red')
            if err.output:
                print(err.output)
            raise StopIteration
        finally:
            os.remove(driver_file)

    # Return expected output filename
    return outroot

#-----------------------------------------------------------------------------

# Set-up to call this as a script
if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Run the ORAC preprocessor.')
    parser.add_argument('file', type=str, default = None,
                        help = 'Name and path of L1B file to be processed.')
    parser = orac_utils.orac_common_args(parser)
    parser = orac_utils.orac_preproc_args(parser)
    args   = parser.parse_args()
    orac_utils.orac_common_args_check(args)
    orac_utils.orac_preproc_args_check(args)
    print(orac_preproc(args))
