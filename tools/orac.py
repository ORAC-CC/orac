#!/usr/bin/env python
# Run pre, main, and post processors for ORAC
# 27 Jun 2016, AP: Initial version
# 08 Jul 2016, AP: Debugging against more awkward python environments
# 25 Apr 2018. AP: Tidy code more sensibly into a module

import warnings
from argparse import ArgumentParser
from pyorac.arguments import (args_common, args_cc4cl, args_preproc,
                              args_main, args_postproc)
from pyorac.colour_print import colour_print
from pyorac.definitions import COLOURING, OracError, OracWarning
from pyorac.run import process_all
from pyorac.util import warning_format

warnings.formatwarning = warning_format
warnings.filterwarnings('always', category=OracWarning)


# Define parser
pars = ArgumentParser(
    description='Run the full ORAC suite on a given Level 1B file.'
)
pars.add_argument('target', type=str, help='Level 1B file to be processed')
args_common(pars)
args_cc4cl(pars)
args_preproc(pars)
args_main(pars)
args_postproc(pars)
args = pars.parse_args()

try:
    jid, _ = process_all(args)

    if args.script_verbose and args.batch:
        print("Job queued with ID {}".format(jid))

except OracError as err:
    colour_print('ERROR) ' + str(err), COLOURING['error'])
except KeyboardInterrupt:
    colour_print('Execution halted by user.', COLOURING['error'])
