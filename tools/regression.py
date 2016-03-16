#!/local/home/povey/eodg/povey/py343ve/bin/python3.4
# Run ORAC regression tests.
# 29 Feb 2016, AP: Finalised initial version.

import argparse
import os
import termcolor
import time

from orac_main     import orac_main
from orac_postproc import orac_postproc
from orac_preproc  import orac_preproc
from orac_regress  import orac_regress
import orac_utils

class RegressTest:
    def __init__(self, l1b=None, geo=None, limit=(0,0,0,0)):
        self.l1b = l1b
        self.geo = geo
        self.lim = limit

repo = '/local/home/povey/eodg/povey/data/testinput'

pre_suf  = ('.alb.nc', '.clf.nc', '.config.nc', '.geo.nc', '.loc.nc',
            '.lsf.nc', '.lwrtm.nc', '.msi.nc', '.prtm.nc', '.swrtm.nc')
post_suf = ('.primary.nc', '.secondary.nc')

proc_took = 'Processing took {:.2f} s'

# Define regression test files and swath
regress = {}
regress['DAYMYDS'] = RegressTest(
    l1b = repo + '/MYD021KM.A2008172.0405.005.2009317014309.hdf',
    geo = repo,
    limit = (700, 1299, 1200, 1204))
regress['NITMYDS'] = RegressTest(
    l1b = repo + '/MYD021KM.A2008172.1630.005.2009317021545.bscs_000500531943.hdf',
    geo = repo,
    limit = (500, 1099, 900, 904))
regress['DAYAATSRS'] = RegressTest(
    l1b = repo + '/ATS_TOA_1PUUPA20080620_002337_000065272069_00345_32964_6203.N1',
    geo = None,
    limit = (1, 512, 21366, 21370))
regress['NITAATSRS'] = RegressTest(
    l1b = regress['DAYAATSRS'].l1b,
    geo = regress['DAYAATSRS'].geo,
    limit = (1, 512, 37450, 37454))
regress['DAYAVHRRS'] = RegressTest(
    l1b = repo + '/noaa18_20080620_0050_99999_satproj_00000_13111_avhrr.h5',
    geo = repo,
    limit = (1, 409, 5190, 5194))
regress['NITAVHRRS'] = RegressTest(
    l1b = regress['DAYAVHRRS'].l1b,
    geo = regress['DAYAVHRRS'].geo,
    limit = (1, 409, 10150, 10154))
regress['DAYMYD'] = RegressTest(
    l1b = regress['DAYMYDS'].l1b,
    geo = regress['DAYMYDS'].geo)
regress['NITMYD'] = RegressTest(
    l1b = regress['NITMYDS'].l1b,
    geo = regress['NITMYDS'].geo)
regress['AATSR'] = RegressTest(
    l1b = regress['DAYAATSRS'].l1b,
    geo = regress['DAYAATSRS'].geo)
regress['AVHRR'] = RegressTest(
    l1b = regress['DAYAVHRRS'].l1b,
    geo = regress['DAYAVHRRS'].geo)

#-----------------------------------------------------------------------------

parser = argparse.ArgumentParser(description='Run ORAC regression tests.')
reg = parser.add_argument_group('Regression test parameters')
reg.add_argument('--clobber_pre', action='store_true',
                 help = 'Overwrite existing preprocessed files. Default behaviour leaves them alone.')
reg.add_argument('-l', '--long', action='store_true',
                 help = 'Process full orbits rather than short segments.')
comp = reg.add_mutually_exclusive_group()
comp.add_argument('--no_compare', action='store_true',
                 help = 'Do not compare outputs to the previous version.')
comp.add_argument('--only_compare', action='store_true',
                  help = 'Do not process outputs of the current version.')
reg.add_argument('--once', action='store_true',
                 help = 'Only run the first phase (probably WAT).')
reg.add_argument('-t', '--tests', type=str, nargs='+', metavar='TEST',
                 choices = ['DAYMYD', 'NITMYD', 'AATSR', 'AVHRR', 'DAYMYDS',
                            'NITMYDS', 'DAYAATSRS', 'NITAATSRS',
                            'DAYAVHRRS', 'NITAVHRRS'],
                 default = None,
                 help = 'List of tests to run.')
parser = orac_utils.orac_common_args(parser)
parser = orac_utils.orac_preproc_args(parser)
parser = orac_utils.orac_main_args(parser)
parser = orac_utils.orac_postproc_args(parser)

args   = parser.parse_args()
orac_utils.orac_common_args_check(args)

if args.in_dir:
    print('WARNING: --in_dir is ignored by this script.')
if args.out_dir:
    base_out_dir = args.out_dir
else:
    base_out_dir = os.environ['TESTOUT']
if args.tests == None:
    if args.long:
        args.tests = ('DAYMYD', 'NITMYD', 'AATSR', 'AVHRR')
    else:
        args.tests = ('DAYMYDS', 'NITMYDS', 'DAYAATSRS', 'NITAATSRS',
                      'DAYAVHRRS', 'NITAVHRRS')
if args.clean:
    args.no_compare = True
no_clobber_copy = args.no_clobber

#-----------------------------------------------------------------------------

for test in args.tests:
    args.file    = regress[test].l1b
    args.geo_dir = regress[test].geo
    args.limit   = regress[test].lim
    args.in_dir  = None
    args.out_dir = base_out_dir+'/'+test

    termcolor.cprint(test+') Preprocessing', 'yellow', attrs=['bold'])
    orac_utils.orac_preproc_args_check(args)
    if not args.only_compare:
        if not args.clobber_pre:
            args.no_clobber = True
        st = time.time()

        # Call preprocessor
        try:
            args.fileroot = orac_preproc(args)
        except StopIteration:
            break

        args.no_clobber = no_clobber_copy
        if args.verbose or args.progress:
            termcolor.cprint(proc_took.format(time.time()-st), 'blue')
    if not args.no_compare:
        orac_regress(args.out_dir, args.fileroot, pre_suf)

    args.in_dir = [args.out_dir]
    if args.once:
        args.phase = args.phases[0]
        termcolor.cprint(test+') Main processing', 'yellow', attrs=['bold'])
        orac_utils.orac_main_args_check(args)
        if not args.only_compare:
            st = time.time()
            try:
                orac_main(args)
            except StopIteration:
                break

            if args.verbose or args.progress:
                termcolor.cprint(proc_took.format(time.time()-st), 'blue')
        if not args.no_compare:
            orac_regress(args.out_dir, args.fileroot,
                         (args.phase + s for s in post_suf))
    else:
        if not args.only_compare:
            st = time.time()
            for phase in args.phases:
                termcolor.cprint(test+') Main processing, '+phase, 'yellow',
                                 attrs=['bold'])
                args.phase = phase
                orac_utils.orac_main_args_check(args)
                try:
                    orac_main(args)
                except StopIteration:
                    exit
            if args.verbose or args.progress:
                termcolor.cprint(proc_took.format(time.time()-st), 'blue')

            termcolor.cprint(test+') Postprocessing', 'yellow', attrs=['bold'])
            orac_utils.orac_postproc_args_check(args)
            try:
                orac_postproc(args)
            except StopIteration:
                break

        if not args.no_compare:
            orac_regress(args.out_dir, args.fileroot, post_suf)
