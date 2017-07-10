#!/usr/bin/env python2.7
# Run preprocessor for ORAC community code
# 16 Feb 2016, AP: Initial version
# 24 Jun 2016, AP: P2.7 rewrite
# 08 Jul 2016, AP: Debugging against more awkward python environments
# 09 Mar 2017, GT: Improved support for use with batch queuing systems

from colours import cprint

import argparse
import os
import orac_utils as ou
from subprocess import call

# Define parser
parser = argparse.ArgumentParser(
    description='Run the ORAC preprocessor on a given Level 1B file.')
ou.args_common(parser)
ou.args_preproc(parser)
parser.add_argument("--batch_settings", type=str, nargs='+', help="Settings to "
                    "pass to the batch processing system. Each setting is "
                    "passed as KEY VALUE, where KEY is any key defined in "
                    "orac_batch.py.", default=())
args = parser.parse_args()

# Parse batch processing settings
print len(args.batch_settings)
if len(args.batch_settings) % 2 != 0:
    raise ValueError("--batch must be a list of KEY VALUE pairs.")
values = {}
for key, val in zip(args.batch_settings[0::2], args.batch_settings[1::2]):
    values[key] = val

try:
    ou.check_args_preproc(args)

    # Run preprocessor
    (driver, outroot) = ou.build_preproc_driver(args)
    jid = ou.call_exe(args, args.orac_dir+'/pre_processing/orac_preproc',
                      driver, values)
    if args.verbose or args.script_verbose:
        cprint(outroot, ou.colouring['pass'])
    print 'JOBID ',jid
except ou.OracError as err:
    cprint('ERROR) ' + err.message, ou.colouring['error'])
except KeyboardInterrupt:
    cprint('Execution halted by user.', ou.colouring['error'])
