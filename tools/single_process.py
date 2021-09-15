#!/usr/bin/env python
# Run preprocessor for ORAC community code
# 16 Feb 2016, AP: Initial version
# 24 Jun 2016, AP: P2.7 rewrite
# 08 Jul 2016, AP: Debugging against more awkward python environments
# 09 Mar 2017, GT: Improved support for use with batch queuing systems
# 25 Apr 2018. AP: Tidy code more sensibly into a module

import os.path
import warnings
from argparse import ArgumentParser

from pyorac.arguments import (args_common, args_cc4cl, args_preproc,
                              args_main, args_postproc, check_args_common,
                              check_args_cc4cl)
from pyorac.colour_print import colour_print
from pyorac.definitions import COLOURING, FileName, OracError, OracWarning
from pyorac.local_defaults import LOG_DIR
from pyorac.run import process_post, process_pre, process_main
from pyorac.util import warning_format

warnings.formatwarning = warning_format
warnings.filterwarnings('always', category=OracWarning)


# Define parser
pars = ArgumentParser(description='Run one part of ORAC on a given file. Note '
                      'that this *DOES NOT* consider default settings for each '
                      'aerosol phase.')
pars.add_argument('target', type=str, help='File to be processed.')
args_common(pars)
args_cc4cl(pars)
args_preproc(pars)
args_main(pars)
args_postproc(pars)
args = pars.parse_args()


args = check_args_common(args)
args = check_args_cc4cl(args)
log_path = os.path.join(args.out_dir, LOG_DIR)

try:
    inst = FileName(args.in_dir, args.target)

    if inst.oractype in ('primary', 'secondary'):
        jid, _ = process_post(args, log_path)

    elif inst.oractype is None:
        jid, _ = process_pre(args, log_path)

    elif inst.oractype in ('alb', 'clf', 'config', 'geo', 'loc', 'lsf',
                           'lwrtm', 'msi', 'prtm', 'swrtm'):
        jid, _ = process_main(args, log_path)

    else:
        raise OracError("Could not determine processing type. Please pass "
                        "the filename of a valid ORAC input.")

    if args.script_verbose and args.batch:
        print("Job queued with ID {}".format(jid))

except OracError as err:
    colour_print('ERROR) ' + str(err), COLOURING['error'])
except KeyboardInterrupt:
    colour_print('Execution halted by user.', COLOURING['error'])
