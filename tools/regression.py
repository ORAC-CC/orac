#!/usr/bin/env python
# Run ORAC regression tests.
# 29 Feb 2016, AP: Finalised initial version.
# 27 Jun 2016, AP: P2.7 rewrite
# 08 Jul 2016, AP: Debugging against more awkward python environments

import os
import pyorac.local_defaults as defaults
import sys

from argparse import ArgumentParser
from pyorac.arguments import *
from pyorac.colour_print import colour_print
from pyorac.definitions import *
from pyorac.regression_tests import *
from pyorac.run import process_all, run_regression
from pyorac.util import get_repository_revision, warning_format
from subprocess import check_output, CalledProcessError
from tempfile import mkstemp


# Calibrate how regression warnings are displayed
import warnings
warnings.formatwarning = warning_format
for key, item in defaults.warn_filt.items():
    warnings.simplefilter(item, locals()[key])


# Define parser
pars = ArgumentParser(description='Run ORAC regression tests.')
args_common(pars)
args_regress(pars)
args_cc4cl(pars)
args_preproc(pars)
args_main(pars)
args_postproc(pars)
args = pars.parse_args()

check_args_regress(args)

if args.in_dir == None:
    args.in_dir = [ defaults.data_dir + '/testinput' ]
if args.out_dir:
    base_out_dir = args.out_dir
else:
    base_out_dir = defaults.data_dir +'/testoutput'

# Increment version number (as this is usually run on uncommited code)
if not args.revision:
    cwd = os.getcwd()
    os.chdir(args.orac_dir)
    args.revision = get_repository_revision()
    os.chdir(cwd)

    if not args.benchmark:
        args.revision += 1


try:
    for test in args.tests:
        colour_print(test, COLOURING['header'])

        # Set filename to be processed and output folder
        args.out_dir = os.path.join(base_out_dir, test)
        try:
            if any([phs[0] == "A" for phs in args.phases]):
                args.target, args.limit = AEROSOL_TESTS[test]
            else:
                args.target, args.limit  = REGRESSION_TESTS[test]
        except KeyError:
            raise OracError("Invalid regression test for given phases.")

        log_path = check_args_cc4cl(args)
        jid, out_file = process_all(args)

        # Check for regressions
        if not args.benchmark:
            if not args.batch:
                run_regression(out_file, args.out_dir)

            else:
                path = [os.path.join(defaults.orac_dir, "tools"), ]
                path.extend(filter(None, sys.path))

                inst = FileName(out_file)
                job_name = inst.job_name(tag='regression')

                (fd, script_file) = mkstemp(
                    '.sh', 'regression_test.', args.out_dir, True
                )
                f = os.fdopen(fd, "w")
                f.write("#!/bin/bash --noprofile\n")
                f.write("export PYTHONPATH={}\n".format(":".join(path)))
                f.write('{} -c "from pyorac.run import run_regression; '
                        'run_regression(' "'{}', '{}'" ')"\n'.format(
                            sys.executable, out_file, args.out_dir
                        ))
                f.write("rm {}\n".format(script_file))
                f.close()
                os.chmod(script_file, 0o700)

                values = defaults.batch_values.copy()
                values.update({key : val for key, val in args.batch_settings})
                values['job_name'] = job_name
                values['log_file'] = os.path.join(log_path, job_name + '.log')
                values['err_file'] = os.path.join(log_path, job_name + '.log')
                values['depend']   = jid

                cmd = defaults.batch.ListBatch(values, exe=script_file)
                if args.verbose or args.script_verbose:
                    colour_print(' '.join(cmd), COLOURING['header'])
                out = check_output(cmd, universal_newlines=True)

                if args.verbose or args.script_verbose:
                    print("Job queued with ID {}".format(
                        defaults.batch.ParseOut(out, 'ID')
                    ))

except OracError as err:
    colour_print('ERROR) ' + str(err), COLOURING['error'])
except Regression as err:
    print(str(err))
except KeyboardInterrupt:
    colour_print('Execution halted by user.', COLOURING['error'])
except CalledProcessError as err:
    colour_print('{:s} failed with error code {:d}. {:s}'.format(
        ' '.join(err.cmd), err.returncode, err.output
    ))
