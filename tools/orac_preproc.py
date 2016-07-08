#!/usr/bin/env python2.7
# Run preprocessor for ORAC community code
# 16 Feb 2016, AP: Initial version
# 24 Jun 2016, AP: P2.7 rewrite
# 08 Jul 2016, AP: Debugging against more awkward python environments

from colours import cprint

import argparse
import orac_utils as ou


# Define parser
parser = argparse.ArgumentParser(
    description='Run the ORAC preprocessor on a given Level 1B file.')
ou.args_common(parser)
ou.args_preproc(parser)
args = parser.parse_args()

try:
    ou.check_args_preproc(args)

    # Run preprocessor
    (driver, outroot) = ou.build_preproc_driver(args)
    ou.call_exe(args, args.orac_dir+'/pre_processing/orac_preproc.x',
                driver)
    if args.verbose or args.script_verbose:
        cprint(outroot, ou.colouring['pass'])
except ou.OracError as err:
    cprint('ERROR) ' + err.message, ou.colouring['error'])

