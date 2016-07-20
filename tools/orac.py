#!/usr/bin/env python2.7
# Run pre, main, and post processors for ORAC
# 27 Jun 2016, AP: Initial version
# 08 Jul 2016, AP: Debugging against more awkward python environments

from colours import cprint

import argparse
import orac_utils as ou

import warnings
warnings.filterwarnings('always', category=ou.OracWarning)

# Define parser
parser = argparse.ArgumentParser(
    description='Run the full ORAC suite on a given Level 1B file.')
ou.args_common(parser)
ou.args_preproc(parser)
ou.args_main(parser)
ou.args_postproc(parser)
ou.args_cc4cl(parser)
args = parser.parse_args()

try:
    ou.check_args_cc4cl(args)
    (_,_) = ou.cc4cl(args)
except ou.OracError as err:
    cprint('ERROR) ' + err.message, ou.colouring['error'])
except KeyboardInterrupt:
    cprint('Execution halted by user.', ou.colouring['error'])
