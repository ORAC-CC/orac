#!/usr/bin/env python2.7
# Run ORAC postprocessor from the community code
# 18 Feb 2016, AP: Initial version
# 24 Jun 2016, AP: P2.7 rewrite
# 08 Jul 2016, AP: Debugging against more awkward python environments
# 09 Mar 2017, GT: Improved support for use with batch queuing systems

from colours import cprint

import argparse
import orac_utils as ou


# Define parser
parser = argparse.ArgumentParser(
    description='Run the ORAC postprocessor on all files of given phases.')
ou.args_common(parser)
ou.args_postproc(parser)
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
    ou.check_args_postproc(args)

    # Check if we're running in batch mode. If so, we may not yet have processing
    # output available, so generate the expected primary file names now and pass
    # to build_postproc_driver directly
    # Note that this assumes that we only have one directory for all the processor
    # output files!
    if args.batch:
        pri = []
        for phs in args.phases:
            pri.append(args.in_dir[0] +'/'+ args.target + phs +'.primary.nc')
        print pri
    else:
        pri = None

    # Run postprocessor
    driver = ou.build_postproc_driver(args, pri=pri)
    jid = ou.call_exe(args, args.orac_dir+'/post_processing/post_process_level2',
                      driver, values)
    print 'JOBID ',jid
except ou.OracError as err:
    cprint('ERROR) ' + err.message, ou.colouring['error'])
except KeyboardInterrupt:
    cprint('Execution halted by user.', ou.colouring['error'])
