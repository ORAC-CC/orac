#!/usr/bin/env python2.7
# Run ORAC regression tests.
# 29 Feb 2016, AP: Finalised initial version.
# 27 Jun 2016, AP: P2.7 rewrite
# 08 Jul 2016, AP: Debugging against more awkward python environments

from colours import cprint

import argparse
import glob
import local_defaults as defaults
import orac_utils as ou
import os
import subprocess
import tempfile
import warnings


# Calibrate how regression warnings are displayed
for key in defaults.warn_filt.keys():
    warnings.simplefilter(defaults.warn_filt[key], ou.__dict__[key])

#-----------------------------------------------------------------------------

# Define the regression tests
regress = {}
# Short tests
regress['DAYMYDS'] = ('MYD021KM.A2008172.0405.005.2009317014309.hdf',
                      (700, 1299, 1200, 1204))
regress['NITMYDS'] = (
    'MYD021KM.A2008172.1630.005.2009317021545.bscs_000500531943.hdf',
    (500, 1099, 900, 904))
regress['DAYAATSRS'] = (
    'ATS_TOA_1PRUPA20080620_002337_000065272069_00345_32964_0666.N1',
    (1, 512, 21366, 21370))
regress['NITAATSRS'] = (regress['DAYAATSRS'][0], (1, 512, 37450, 37454))
regress['DAYAVHRRS'] = (
    'noaa18_20080620_0050_99999_satproj_00000_13111_avhrr.h5',
    (1, 409, 5190, 5194))
regress['NITAVHRRS'] = (regress['DAYAVHRRS'][0], (1, 409, 10150, 10154))

# Long tests
regress['DAYMYD']    = (regress['DAYMYDS'][0],   (0, 0, 0, 0))
regress['NITMYD']    = (regress['NITMYDS'][0],   (0, 0, 0, 0))
regress['AATSR']     = (regress['DAYAATSRS'][0], (0, 0, 0, 0))
regress['AVHRR']     = (regress['DAYAVHRRS'][0], (0, 0, 0, 0))

#-----------------------------------------------------------------------------

# Define parser
parser = argparse.ArgumentParser(description='Run ORAC regression tests.')
reg = parser.add_argument_group('Regression test parameters')
reg.add_argument('-A', '--all_phases', action='store_true',
                 help = 'Sets phases to run all possible tests.')
reg.add_argument('-B', '--benchmark', action='store_true',
                 help = 'Produce benchmark output files i.e. a clean revision.')
reg.add_argument('-L', '--long', action='store_true',
                 help = 'Process full orbits rather than short segments.')
reg.add_argument('-t', '--tests', type=str, nargs='+', metavar='TEST',
                 choices = regress.keys(),
                 default = ['DAYMYDS', 'NITMYDS', 'DAYAATSRS', 'NITAATSRS',
                            'DAYAVHRRS', 'NITAVHRRS'],
                 help = 'List of tests to run.')

ou.args_common(parser, regression=True)
ou.args_preproc(parser)
ou.args_main(parser)
ou.args_postproc(parser)
ou.args_cc4cl(parser)
args = parser.parse_args()

if args.all_phases:
    args.phases = ou.settings.keys()
    if args.suffix == None:
        args.suffix = 'ALL'
    # For now, only run ATSR through all phases
    if args.long:
        args.tests = ['AATSR']
    else:
        args.tests = ['DAYAATSRS']
elif args.long:
    args.tests = ['DAYMYD', 'NITMYD', 'AATSR', 'AVHRR']

if args.in_dir == None:
    args.in_dir = [ defaults.data_dir + '/testinput' ]
if args.out_dir:
    base_out_dir = args.out_dir
else:
    base_out_dir = defaults.data_dir +'/testoutput'

# Increment version number (as this is usually run on uncommited code)
if not args.revision:
    cwd = os.getcwd()
    os.chdir(args.orac_dir + '/pre_processing')
    args.revision = ou.get_svn_revision()
    os.chdir(cwd)

    if not args.benchmark:
        args.revision += 1

#-----------------------------------------------------------------------------

try:
    for test in args.tests:
        cprint(test, ou.colouring['header'])

        # Set filename to be processed and output folder
        args.target  = regress[test][0]
        args.limit   = regress[test][1]
        args.out_dir = base_out_dir + '/' + test

        # Run ORAC suite
        ou.check_args_cc4cl(args)
        (fileroot, dirs, jid) = ou.cc4cl(args)

        # Check for regressions
        if not args.benchmark:
            if not args.batch:
                for d in dirs:
                    for f in glob.glob(d + '/' + fileroot + '*nc'):
                        g = ou.find_previous_orac_file(f)
                        if g == None:
                            warnings.warn('Could not locate previous file: '+f,
                                          ou.OracWarning)
                            continue

                        ou.compare_orac_out(f, g)
            else:
                (fd, script_file) = tempfile.mkstemp('.sh', 'regression_test.',
                                                     args.out_dir, True)
                f = os.fdopen(fd, "w")
                f.write("#!/bin/bash --noprofile\n")
                f.write("export PATH={}\n".format(os.environ['PATH']))
                f.write("export ORAC_LIB={}\n".format(os.environ['ORAC_LIB']))
                f.write("python <<EOF\n")
                f.write("import os\n")
                f.write("import glob\n")
                f.write("import warnings\n")
                f.write("import sys\n")
                f.write("sys.path.append('{}/tools')\n".format(args.orac_dir))
                f.write("import orac_utils as ou\n")
                f.write("for d in {}:\n".format(dirs))
                f.write("    for f in glob.glob(d + '/{}*nc'):\n".format(
                    fileroot))
                f.write("        g = ou.find_previous_orac_file(f)\n")
                f.write("        if g == None:\n")
                f.write("            warnings.warn('Could not locate previous "
                        "file: '+f, ou.OracWarning)\n")
                f.write("            continue\n")
                f.write("        ou.compare_orac_out(f, g)\n")
                f.write("os.remove('{}')\n".format(script_file))
                f.write("EOF")
                f.close()
                os.chmod(script_file, 0o700)

                values = dict(defaults.batch_values)
                values['log_file'] = '{}/{}/regression_test.R{}.log'.format(
                    args.out_dir, args.log_dir, args.revision)
                values['err_file'] = '{}/{}/regression_test.R{}.err'.format(
                    args.out_dir, args.log_dir, args.revision)
                values['depend']   = jid

                cmd = defaults.batch.PrintBatch(values, exe=script_file)
                if args.verbose:
                    cprint(cmd, ou.colouring['header'])
                _ = subprocess.check_output(cmd.split(' '))

except ou.OracError as err:
    cprint('ERROR) ' + err.message, ou.colouring['error'])
except ou.Regression as err:
    print err.message
except KeyboardInterrupt:
    cprint('Execution halted by user.', ou.colouring['error'])
except subprocess.CalledProcessError as err:
    cprint('{:s} failed with error code {:d}. {:s}'.format(
        ' '.join(err.cmd), err.returncode, err.output))
