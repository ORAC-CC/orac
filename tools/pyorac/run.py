"""Routines to run an ORAC component."""
import os

from collections import OrderedDict
from pyorac.arguments import *
from pyorac.definitions import FileName
from pyorac.util import call_exe


CLOBBER = OrderedDict([
    ('pre', 3),
    ('main', 2),
    ('post', 1),
])


def process_pre(args, log_path, dependency=None):
    """Call sequence for pre processor"""
    from pyorac.drivers import build_preproc_driver

    check_args_preproc(args)
    driver = build_preproc_driver(args)

    # This must be called after building the driver as revision is unknown
    inst = FileName(args.target)
    job_name = inst.job_name(args.revision, 'pre')
    root_name = inst.root_name(args.revision, args.processor, args.project,
                               args.product_name)

    if not os.path.isdir(args.out_dir):
        os.makedirs(args.out_dir, 0o774)

    out_file = os.path.join(args.out_dir, root_name + '.config.nc')
    if args.clobber >= CLOBBER['pre'] or not os.path.isfile(out_file):
        # Settings for batch processing
        values = {'job_name' : job_name,
                  'log_file' : os.path.join(log_path, job_name + '.log'),
                  'err_file' : os.path.join(log_path, job_name + '.err'),
                  'duration' : args.dur[0],
                  'ram'      : args.ram[0]}
        if dependency is not None:
            values['depend'] = dependency

        jid = call_exe(
            args, os.path.join(args.orac_dir, 'pre_processing', 'orac_preproc'),
            driver, values
        )

    else:
        jid = None

    return jid, out_file


def process_main(args, log_path, phs, tag='', dependency=None):
    """Call sequence for main processor"""
    from pyorac.drivers import build_main_driver

    check_args_main(args)
    inst = FileName(args.target)
    job_name = inst.job_name(tag=phs + tag)
    root_name = inst.root_name()

    if not os.path.isdir(args.out_dir):
        os.makedirs(args.out_dir, 0o774)

    out_file = os.path.join(args.out_dir, root_name + phs + '.primary.nc')
    if args.clobber >= CLOBBER['main'] or not os.path.isfile(out_file):
        # Settings for batch processing
        values = {'job_name' : job_name,
                  'log_file' : os.path.join(log_path, job_name + '.log'),
                  'err_file' : os.path.join(log_path, job_name + '.err'),
                  'duration' : args.dur[1],
                  'ram'      : args.ram[1]}
        if dependency is not None:
            values['depend'] = dependency

        driver = build_main_driver(args)
        jid = call_exe(
            args, os.path.join(args.orac_dir, 'src', 'orac'), driver, values
        )
    else:
        jid = None

    return jid, out_file


def process_post(args, log_path, files=None, dependency=None):
    """Call sequence for post processor"""
    from glob import glob
    from pyorac.drivers import build_postproc_driver

    check_args_postproc(args)
    inst = FileName(args.target)
    job_name = inst.job_name(args.revision, 'post')
    root_name = inst.root_name(args.revision)

    if not os.path.isdir(args.out_dir):
        os.makedirs(args.out_dir, 0o774)

    if files is None:
        # Find all primary files of requested phases in given input folders.
        files = []
        for phs in args.phases:
            for d in args.in_dir:
                files.extend(glob(os.path.join(
                    d, root_name + phs + '.primary.nc'
                )))

    if len(files) < 2:
        raise FileMissing('sufficient processed files', args.target)

    out_file = os.path.join(
        args.out_dir, '.'.join(filter(
            None, (root_name, args.suffix, 'primary', 'nc')
    )))
    if args.clobber >= CLOBBER['post'] or os.path.isfile(out_file):
        # Settings for batch processing
        values = {'job_name' : job_name,
                  'log_file' : os.path.join(log_path, job_name + '.log'),
                  'err_file' : os.path.join(log_path, job_name + '.err'),
                  'duration' : args.dur[2],
                  'ram'      : args.ram[2]}
        if dependency is not None:
            values['depend'] = dependency

        args.target = out_file
        driver = build_postproc_driver(args, files)
        jid = call_exe(
            args,
            os.path.join(args.orac_dir, 'post_processing', 'orac_postproc'),
            driver, values
        )
    else:
        jid = None

    return jid, out_file


def process_all(orig_args):
    """Run the ORAC pre, main, and post processors on a file."""
    from copy import copy
    from pyorac.definitions import MAP_WVL_TO_INST, SETTINGS

    # Copy input arguments as we'll need to fiddle with them
    args = copy(orig_args)
    check_args_common(args)
    log_path = check_args_cc4cl(args)

    inst = FileName(args.target)
    map_wvl = MAP_WVL_TO_INST[inst.sensor]

    # Sort out what channels are required
    if args.channel_ids is None:
        args.channel_ids = sorted(set(
            map_wvl[w] for phs in args.phases for w in SETTINGS[phs].wvl
        ))

    written_dirs = set() # The folders we actually wrote to

    # Work out output filename
    out_dir = args.out_dir
    args.out_dir = os.path.join(out_dir, args.pre_dir)

    jid_pre, _ = process_pre(args, log_path)
    if jid_pre is not None:
        written_dirs.add(args.out_dir)


    # Run main processor -------------------------------------------------------
    root_name = inst.root_name(args.revision, args.processor, args.project,
                               args.product_name)
    args.target = root_name + ".config.nc"
    out_files   = [] # All files that would be made (facilitates --dry_run)
    jid_main    = [] # ID no. for each queued job
    args.in_dir = [args.out_dir]
    for phs in args.phases:
        args.phase   = phs

        # Identify which channels to use
        ids_here = [map_wvl[w] for w in SETTINGS[phs].wvl]
        args.use_channel = [ch in ids_here for ch in args.channel_ids]

        # Process land and sea separately for aerosol
        if SETTINGS[phs].ls:
            if orig_args.land:
                args.out_dir = os.path.join(out_dir, args.land_dir)
                args.approach = 'AppAerSw'
                args.land = True
                args.sea  = False
                args.extra_lines_file = args.land_extra

                jid, out = process_main(args, log_path, phs, tag='L',
                                        dependency=jid_pre)
                out_files.append(out)
                if jid is not None:
                    jid_main.append(jid)
                    written_dirs.add(args.out_dir)

            if orig_args.sea:
                args.out_dir  = os.path.join(out_dir, args.sea_dir)
                args.approach = 'AppAerOx'
                args.land = False
                args.sea  = True
                args.extra_lines_file = args.sea_extra

                jid, out = process_main(args, log_path, phs, tag='S',
                                        dependency=jid_pre)
                out_files.append(out)
                if jid is not None:
                    jid_main.append(jid)
                    written_dirs.add(args.out_dir)

        else:
            args.out_dir  = os.path.join(out_dir, args.main_dir)
            args.approach = orig_args.approach
            args.land = orig_args.land
            args.sea  = orig_args.sea
            args.extra_lines_file = args.cld_extra

            jid, out = process_main(args, log_path, phs, dependency=jid_pre)
            out_files.append(out)
            if jid is not None:
                jid_main.append(jid)
                written_dirs.add(args.out_dir)


    # Run postprocessor
    args.target = root_name + args.phases[0] + ".primary.nc"
    args.in_dir = written_dirs
    args.out_dir = out_dir
    jid, out_file = process_post(args, log_path, out_files, dependency=jid_main)
    if jid is not None:
        written_dirs.add(args.out_dir)

    # Output root filename and output folders for regression tests
    return jid, out_file


def run_regression(target, in_dir):
    """Run the regression test on a set of ORAC files."""
    import re

    from copy import copy
    from glob import iglob
    from pyorac.definitions import OracWarning
    from pyorac.regression_tests import compare_orac_out
    from warnings import warn

    regex = re.compile("_R(\d+)")

    inst = FileName(target)
    this_revision = int(inst.revision)
    for this_file in iglob(os.path.join(in_dir, "**", inst.root_name() + "*nc")):
        # Find previous file version
        old_revision = 0
        old_file = None
        for f in iglob(regex.sub("_R*", this_file)):
            rev = int(regex.search(f).group(1))
            if old_revision < rev < this_revision:
                old_revision = copy(rev)
                old_file = copy(f)

        if old_file is None:
            warn("Could not locate previous file: " + this_file, OracWarning)
            continue

        compare_orac_out(this_file, old_file)
