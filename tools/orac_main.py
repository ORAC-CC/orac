#!/usr/bin/env python2.7
# Run main ORAC processor from the community code
# 17 Feb 2016, AP: Initial version
# 24 Jun 2016, AP: P2.7 rewrite
# 08 Jul 2016, AP: Debugging against more awkward python environments
# 09 Mar 2017, GT: Improved support for use with batch queuing systems

from colours import cprint

import argparse
import orac_utils as ou
import os

# Define parser
parser = argparse.ArgumentParser(
    description='Run the ORAC main processor given a root filename.')
ou.args_common(parser)
ou.args_main(parser)
parser.add_argument("--batch_settings", type=str, nargs='+', help="Settings to "
                    "pass to the batch processing system. Each setting is "
                    "passed as KEY VALUE, where KEY is any key defined in "
                    "orac_batch.py.", default=())
args = parser.parse_args()

# Parse batch processing settings
if len(args.batch_settings) % 2 != 0:
    raise ValueError("--batch must be a list of KEY VALUE pairs.")
values = {}
for key, val in zip(args.batch_settings[0::2], args.batch_settings[1::2]):
    values[key] = val

try:
    ou.check_args_main(args)

    # Run main processor
    driver = ou.build_main_driver(args)
    jid = ou.call_exe(args, args.orac_dir+'/src/orac', driver, values)
    print 'JOBID ',jid
except ou.OracError as err:
    cprint('ERROR) ' + err.message, ou.colouring['error'])
except KeyboardInterrupt:
    cprint('Execution halted by user.', ou.colouring['error'])
