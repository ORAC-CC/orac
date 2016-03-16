#!/usr/bin/python3
# Run main ORAC processor from the community code
# 17 Feb 2016, AP: Initial version

import argparse
import os
import subprocess
import tempfile
from termcolor import cprint

import orac_utils

def orac_main(args):
    if not os.path.isdir(args.out_dir):
        os.makedirs(args.out_dir, 0o774)

    (sensor, platform) = orac_utils.parse_sensor(args.fileroot)
    if platform == 'Envisat':
        platform = ''
    else:
        platform = '-'+platform.upper()

    # Form processing environment
    libs = orac_utils.read_orac_libraries(args.orac_lib)
    os.environ["LD_LIBRARY_PATH"] = orac_utils.build_orac_library_path(libs)

    # Form mandatory driver file lines
    driver = orac_utils.main_driver(
        channels = ','.join('{:d}'.format(k) for k in args.use_channel),
        cloudy   = args.cloudy,
        fileroot = args.fileroot,
        in_dir   = args.in_dir[0],
        nch      = len(args.use_channel),
        ntypes   = len(args.types),
        out_dir  = args.out_dir,
        phase    = args.phase,
        sad_dir  = args.sad_dir,
        sensor   = sensor + platform,
        types    = ','.join(str(k) for k in args.types),
        use_brdf = not args.lambertian,
        verbose  = args.verbose
        )

    # Optional driver file lines
    if args.sabotage:
        driver += "\nCTRL%SABOTAGE_INPUTS=true"
    if args.approach:
        driver += "\nCTRL%APPROACH={}".format(args.approach)
    if args.land:
        driver += "\nCTRL%SURFACES_TO_SKIP=ISEA"
    else:
        if args.sea:
            driver += "\nCTRL%SURFACES_TO_SKIP=ILAND"
    if args.extra_lines != None:
        try:
            e = open(args.extra_lines, "r")
            driver += "\n"
            driver += e.read()
            e.close()
        except IOError:
            print('Cannot open --extra_lines file.')

    if (os.path.isfile(args.out_dir + '/' + args.fileroot + args.phase
                       +'.primary.nc') and args.no_clobber):
        # Skip already processed files
        if args.verbose or args.progress:
            print(orac_utils.star * 60)
            print(orac_utils.star * 5 +' '+args.fileroot+args.phase+' already processed')
            print(orac_utils.star * 60)
    else:
        # Write driver file
        (fd, driver_file) = tempfile.mkstemp('.'+args.phase+'.driver',
                                             'ORAC.', args.out_dir, True)
        try:
            f = os.fdopen(fd, "w")
            f.write(driver)
            f.close()
        except IOError:
            print('Cannot create driver file '+driver_file)

        # Call main processor
        if args.verbose or args.progress:
            print(orac_utils.star * 60)
            print(orac_utils.star * 5 +' orac <<')
            print(orac_utils.star * 5 +' '+
                  driver.replace("\n", "\n"+orac_utils.star*5+" "))
            print(orac_utils.star * 60)
        try:
            os.chdir(args.orac_dir + '/src')
            subprocess.check_call('orac '+driver_file, shell=True)
        except subprocess.CalledProcessError as err:
            cprint('ORAC failed with error code {}'.format(err.returncode),'red')
            if err.output:
                print(err.output)
            raise StopIteration
        finally:
            os.remove(driver_file)

    return

#-----------------------------------------------------------------------------

# Set-up to call this as a script
if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Run the ORAC processor.')
    parser.add_argument('fileroot', type=str, default = None,
                        help = 'Root name of preprocessed files.')
    parser.add_argument('phase', type=str, default = None,
                        help = 'Label of look-up table to use in retrieval.')
    parser = orac_utils.orac_common_args(parser)
    parser = orac_utils.orac_main_args(parser)
    args   = parser.parse_args()
    orac_utils.orac_common_args_check(args)
    orac_utils.orac_main_args_check(args)
    orac_main(args)
