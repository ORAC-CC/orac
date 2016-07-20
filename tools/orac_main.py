#!/usr/bin/env python2.7
# Run main ORAC processor from the community code
# 17 Feb 2016, AP: Initial version
# 24 Jun 2016, AP: P2.7 rewrite
# 08 Jul 2016, AP: Debugging against more awkward python environments

from colours import cprint

import argparse
import orac_utils as ou


# Define parser
parser = argparse.ArgumentParser(
    description='Run the ORAC main processor given a root filename.')
ou.args_common(parser)
ou.args_main(parser)
args = parser.parse_args()

try:
    ou.check_args_main(args)

    # Run main processor
    driver = ou.build_main_driver(args)
    ou.call_exe(args, args.orac_dir+'/src/orac', driver)
except ou.OracError as err:
    cprint('ERROR) ' + err.message, ou.colouring['error'])
except KeyboardInterrupt:
    cprint('Execution halted by user.', ou.colouring['error'])
