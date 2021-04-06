"""Routines to run an ORAC component."""
import os

from collections import OrderedDict
from pyorac.arguments import (check_args_common, check_args_preproc,
                              check_args_main, check_args_postproc,
                              check_args_cc4cl)
from pyorac.util import call_exe

CLOBBER = OrderedDict([
    ('pre', 3),
    ('main', 2),
    ('post', 1),
])


def process_pre(args, log_path, dependency=None, tag='pre'):
    """Call sequence for pre processor"""
    from pyorac.drivers import build_preproc_driver
    from pyorac.local_defaults import DIR_PERMISSIONS

    args = check_args_preproc(args)
    driver = build_preproc_driver(args)

    # This must be called after building the driver as revision is unknown
    job_name = args.File.job_name(args.revision, tag)
    root_name = args.File.root_name(args.revision, args.processor, args.project,
                                    args.product_name)

    if not os.path.isdir(args.out_dir):
        os.makedirs(args.out_dir, DIR_PERMISSIONS)

    out_file = os.path.join(args.out_dir, root_name + '.config.nc')
    if args.clobber >= CLOBBER['pre'] or not os.path.isfile(out_file):
        # Settings for batch processing
        values = {'job_name': job_name,
                  'log_file': os.path.join(log_path, job_name + '.log'),
                  'err_file': os.path.join(log_path, job_name + '.err'),
                  'duration': args.dur[0],
                  'ram': args.ram[0]}
        if dependency is not None:
            values['depend'] = dependency

        exe = os.path.join(args.orac_dir, 'pre_processing', 'orac_preproc')
        if not os.path.isfile(exe):
            exe = os.path.join(args.orac_dir, 'orac_preproc')
        jid = call_exe(args, exe, driver, values)

    else:
        jid = None

    return jid, out_file


def process_main(args, log_path, tag='', dependency=None):
    """Call sequence for main processor"""
    from pyorac.definitions import SETTINGS
    from pyorac.drivers import build_main_driver
    from pyorac.local_defaults import DIR_PERMISSIONS

    args = check_args_main(args)
    if args.multilayer is not None:
        phase = SETTINGS[args.phase].name + "_" + SETTINGS[args.multilayer[0]].name
    else:
        phase = SETTINGS[args.phase].name
    job_name = args.File.job_name(tag=phase + tag)
    root_name = args.File.root_name(args.revision)

    if not os.path.isdir(args.out_dir):
        os.makedirs(args.out_dir, DIR_PERMISSIONS)

    out_file = os.path.join(args.out_dir, root_name + phase + '.primary.nc')
    if args.clobber >= CLOBBER['main'] or not os.path.isfile(out_file):
        # Settings for batch processing
        values = {'job_name': job_name,
                  'log_file': os.path.join(log_path, job_name + '.log'),
                  'err_file': os.path.join(log_path, job_name + '.err'),
                  'duration': args.dur[1],
                  'ram': args.ram[1]}
        if dependency is not None:
            values['depend'] = dependency

        driver = build_main_driver(args)
        exe = os.path.join(args.orac_dir, 'src', 'orac')
        if not os.path.isfile(exe):
            exe = os.path.join(args.orac_dir, 'orac')
        jid = call_exe(args, exe, driver, values)
    else:
        jid = None

    return jid, out_file


def process_post(args, log_path, files=None, dependency=None, tag='post'):
    """Call sequence for post processor"""
    from glob import glob
    from pyorac.drivers import build_postproc_driver
    from pyorac.definitions import FileMissing, SETTINGS
    from pyorac.local_defaults import DIR_PERMISSIONS

    args = check_args_postproc(args)
    job_name = args.File.job_name(args.revision, tag)
    root_name = args.File.root_name(args.revision)

    if not os.path.isdir(args.out_dir):
        os.makedirs(args.out_dir, DIR_PERMISSIONS)

    if files is None:
        # Find all primary files of requested phases in given input folders.
        files = []
        for phs in set(args.phases):
            for fdr in args.in_dir:
                files.extend(glob(os.path.join(
                    fdr, root_name + SETTINGS[phs].name + '.primary.nc'
                )))

    if len(files) < 2:
        raise FileMissing('sufficient processed files', args.target)

    out_file = os.path.join(
        args.out_dir, '.'.join(filter(
            None, (root_name, args.suffix, 'primary', 'nc')
        ))
    )
    if args.clobber >= CLOBBER['post'] or not os.path.isfile(out_file):
        # Settings for batch processing
        values = {'job_name': job_name,
                  'log_file': os.path.join(log_path, job_name + '.log'),
                  'err_file': os.path.join(log_path, job_name + '.err'),
                  'duration': args.dur[2],
                  'ram': args.ram[2]}
        if dependency is not None:
            values['depend'] = dependency

        args.target = out_file
        driver = build_postproc_driver(args, files)
        exe = os.path.join(args.orac_dir, 'post_processing', 'orac_postproc')
        if not os.path.isfile(exe):
            exe = os.path.join(args.orac_dir, 'orac_postproc')
        jid = call_exe(args, exe, driver, values)

    else:
        jid = None

    return jid, out_file


def call_reformat(args, log_path, exe, out_file, dependency=None):
    """Reformat outputs using the script provided."""
    from pyorac.local_defaults import BATCH, BATCH_VALUES

    from pyorac.colour_print import colour_print
    from pyorac.definitions import OracError, COLOURING
    from subprocess import check_call, check_output, CalledProcessError

    # Optionally print command and driver file contents to StdOut
    if args.verbose or args.script_verbose or args.dry_run:
        colour_print('{} {} 1 <<<'.format(exe, out_file), COLOURING['header'])

    if args.dry_run:
        return -1

    job_name = args.File.job_name(args.revision, 'format')

    if not args.batch:
        try:
            check_call([exe, out_file, "1"])
        except CalledProcessError as err:
            raise OracError('{:s} failed with error code {:d}. {}'.format(
                ' '.join(err.cmd), err.returncode, err.output
            ))

    else:
        try:
            # Collect batch settings from defaults, command line, and script
            batch_params = BATCH_VALUES.copy()
            batch_params['job_name'] = job_name
            batch_params['log_file'] = os.path.join(log_path, job_name + '.log')
            batch_params['err_file'] = os.path.join(log_path, job_name + '.err')
            batch_params['duration'] = '01:00'
            batch_params['ram'] = 5000
            batch_params['procs'] = 1
            if dependency is not None:
                batch_params['depend'] = dependency
            batch_params.update({key: val for key, val in args.batch_settings})

            # Form batch queue command and call batch queuing system
            cmd = BATCH.list_batch(batch_params, exe=[exe, out_file, "1"])

            if args.verbose or args.script_verbose:
                colour_print(' '.join(cmd), COLOURING['header'])
            out = check_output(cmd, universal_newlines=True)

            # Parse job ID # and return it to the caller
            jid = BATCH.parse_out(out, 'ID')
            return jid
        except CalledProcessError as err:
            raise OracError('Failed to queue job ' + exe)
        except SyntaxError as err:
            raise OracError(str(err))


def process_all(orig_args):
    """Run the ORAC pre, main, and post processors on a file."""
    from argparse import ArgumentParser
    from copy import deepcopy
    from pyorac.arguments import args_common, args_main
    from pyorac.local_defaults import LOG_DIR, PRE_DIR

    # Generate main-processor-only parser
    pars = ArgumentParser()
    args_common(pars)
    args_main(pars)
    # We need one argument from args_cc4cl()
    pars.add_argument("--sub_dir", default="")
    compare = pars.parse_args("")

    # Copy input arguments as we'll need to fiddle with them
    orig_args = check_args_common(orig_args)
    orig_args = check_args_cc4cl(orig_args)
    log_path = os.path.join(orig_args.out_dir, LOG_DIR)
    args = deepcopy(orig_args)

    written_dirs = set()  # The folders we actually wrote to

    # Work out output filename
    args.out_dir = os.path.join(orig_args.out_dir, PRE_DIR)

    jid_pre, _ = process_pre(args, log_path, tag="pre{}".format(args.label))
    if jid_pre is not None:
        written_dirs.add(args.out_dir)

    # Run main processor -------------------------------------------------------
    root_name = args.File.root_name(args.revision, args.processor, args.project,
                                    args.product_name)
    args.target = root_name + ".config.nc"
    out_files = []  # All files that would be made (facilitates --dry_run)
    jid_main = []  # ID no. for each queued job
    args.in_dir = [args.out_dir]
    for sett in args.settings:
        phs_args = deepcopy(args)
        parsed_settings_arguments = pars.parse_args(sett.split())
        for key, val in compare.__dict__.items():
            if val == parsed_settings_arguments.__dict__[key]:
                parsed_settings_arguments.__dict__.pop(key)
        phs_args.__dict__.update(parsed_settings_arguments.__dict__)
        phs_args.out_dir = os.path.join(orig_args.out_dir, phs_args.sub_dir)

        jid, out = process_main(phs_args, log_path, dependency=jid_pre,
                                tag=args.sub_dir + args.label)
        out_files.append(out)
        if jid is not None:
            jid_main.append(jid)
            written_dirs.add(args.out_dir)

    # Run postprocessor if necessary
    if len(args.settings) > 1:
        args.target = root_name + "NULL.primary.nc"
        args.in_dir = written_dirs
        args.out_dir = orig_args.out_dir
        jid, out_file = process_post(
            args, log_path, out_files, dependency=jid_main,
            tag="post{}".format(args.label)
        )
        if jid is not None:
            written_dirs.add(args.out_dir)
    else:
        out_file = out_files[0]

    # Run CCI formatting
    if args.reformat != "":
        call_reformat(args, log_path, args.reformat, out_file, dependency=jid)

    # Output root filename and output folders for regression tests
    return jid, out_file


def run_regression(in_file):
    """Run the regression test on a set of ORAC files."""
    import re

    from copy import copy
    from glob import iglob
    from pyorac.definitions import OracWarning
    from pyorac.regression_tests import compare_orac_out
    from warnings import warn

    regex = re.compile(r"_R(\d+)")

    this_revision = int(in_file.revision)
    for fdr in in_file.folders:
        for this_file in iglob(os.path.join(
                fdr, "**", in_file.root_name() + "*nc"
        ), recursive=True):
            # Find previous file version
            old_revision = 0
            old_file = None
            for filename in iglob(regex.sub("_R[0-9][0-9][0-9][0-9]", this_file)):
                rev = int(regex.search(filename).group(1))
                if old_revision < rev < this_revision:
                    old_revision = copy(rev)
                    old_file = copy(filename)

            if old_file is None:
                warn("Could not locate previous file: " + this_file, OracWarning)
                continue

            compare_orac_out(this_file, old_file)
