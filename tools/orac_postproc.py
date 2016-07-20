#!/usr/bin/env python2.7
# Run ORAC postprocessor from the community code
# 18 Feb 2016, AP: Initial version
# 24 Jun 2016, AP: P2.7 rewrite
# 08 Jul 2016, AP: Debugging against more awkward python environments

from colours import cprint

import argparse
import orac_utils as ou


# Define parser
parser = argparse.ArgumentParser(
    description='Run the ORAC postprocessor on all files of given phases.')
ou.args_common(parser)
ou.args_postproc(parser)
args = parser.parse_args()

try:
    ou.check_args_postproc(args)

    # Run postprocessor
    driver = ou.build_postproc_driver(args)
    ou.call_exe(args, args.orac_dir+'/post_processing/post_process_level2',
                driver)
except ou.OracError as err:
    cprint('ERROR) ' + err.message, ou.colouring['error'])
except KeyboardInterrupt:
    cprint('Execution halted by user.', ou.colouring['error'])
