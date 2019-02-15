"""Utility functions for working with ORAC scripts and outputs."""
import os


def build_orac_library_path(libs=None):
    """Build required LD_LIBRARY_PATH variable"""
    from os import environ
    from pyorac.definitions import OracError
    from pyorac.local_defaults import orac_lib

    if libs is None:
        try:
            libs = read_orac_libraries(environ["ORAC_LIB"])
        except KeyError:
            libs = read_orac_libraries(orac_lib)

    if "CONDA_PREFIX" in libs:
        ld_path = libs["CONDA_PREFIX"] + "/lib"
    else:
        if "GRIBLIB" in libs:
            glib = "GRIBLIB"
        elif "ECCODESLIB" in libs:
            glib = "ECCODESLIB"
        else:
            raise OracError('Neither GRIB_API or ECCODES libraries found')
        ld_path = ':'.join([libs[key] for key in (
            "SZLIB", "EPR_APILIB", glib, "HDF5LIB", "HDFLIB",
            "NCDF_FORTRAN_LIB", "NCDFLIB"
        )])

    if "LD_LIBRARY_PATH" in os.environ.keys():
        ld_path += ':' + os.environ["LD_LIBRARY_PATH"]
    return ld_path


def call_exe(args, exe, driver, values=dict()):
    """Call an ORAC executable, managing the necessary driver file.

    Args:
    :list args: Arguments of the script.
    :str exe: Name of the executable.
    :str driver: Contents of the driver file to pass.
    :dict values: Arguments for the batch queueing system."""
    import pyorac.local_defaults as defaults

    from pyorac.colour_print import colour_print
    from pyorac.definitions import OracError, COLOURING
    from subprocess import check_call, check_output, CalledProcessError
    from tempfile import mkstemp
    from time import time

    # Optionally print command and driver file contents to StdOut
    if args.verbose or args.script_verbose or args.dry_run:
        colour_print(exe + ' <<<', COLOURING['header'])
        colour_print(driver, COLOURING['text'])

    if args.dry_run:
        return

    # Write driver file
    (fd, driver_file) = mkstemp('.driver', os.path.basename(exe) + '.',
                                args.out_dir, True)
    f = os.fdopen(fd, "w")
    f.write(driver)
    f.close()

    if not args.batch:
        # Form processing environment
        os.environ["LD_LIBRARY_PATH"] = build_orac_library_path()

        # Define a directory for EMOS to put it's gridding
        try:
            os.environ["PPDIR"] = args.emos_dir
            os.environ["OPENBLAS_NUM_THREADS"] = "1"
            os.environ["OMP_NUM_THREADS"] = str(args.procs)
        except AttributeError:
            pass

        # Call program
        try:
            st = time()
            check_call([exe, driver_file])
            if args.timing:
                colour_print(exe + ' took {:f}s'.format(time() - st),
                             COLOURING['timing'])
            return True
        except CalledProcessError as err:
            raise OracError('{:s} failed with error code {:d}. {}'.format(
                ' '.join(err.cmd), err.returncode, err.output
            ))
        finally:
            if not args.keep_driver:
                os.remove(driver_file)
            elif args.verbose or args.script_verbose:
                print("Driver file stored at " + driver_file)

    else:
        # Write temporary script to call executable
        (gd, script_file) = mkstemp('.sh', os.path.basename(exe) + '.',
                                    args.out_dir, True)
        g = os.fdopen(gd, "w")
        g.write(defaults.batch_script)

        # Define processing environment
        libs = read_orac_libraries(args.orac_lib)
        g.write("export LD_LIBRARY_PATH=" +
                build_orac_library_path(libs) + "\n")
        g.write("export OPENBLAS_NUM_THREADS=1\n")
        try:
            g.write("export PPDIR=" + args.emos_dir + "\n")
        except AttributeError:
            pass
        defaults.batch.add_openmp_to_script(g)

        # Call executable and give the script permission to execute
        g.write(exe + ' ' + driver_file + "\n")
        if not args.keep_driver:
            g.write("rm -f " + driver_file + "\n")
        g.write("rm -f " + script_file + "\n")
        g.close()
        os.chmod(script_file, 0o700)

        try:
            # Collect batch settings from defaults, command line, and script
            batch_params = defaults.batch_values.copy()
            batch_params.update(values)
            batch_params.update({key : val for key, val in args.batch_settings})

            batch_params['procs'] = args.procs

            # Form batch queue command and call batch queuing system
            cmd = defaults.batch.ListBatch(batch_params, exe=script_file)

            if args.verbose or args.script_verbose:
                colour_print(' '.join(cmd), COLOURING['header'])
            out = check_output(cmd, universal_newlines=True)

            # Parse job ID # and return it to the caller
            jid = defaults.batch.ParseOut(out, 'ID')
            return jid
        except CalledProcessError as err:
            raise OracError('Failed to queue job ' + exe)
        except SyntaxError as err:
            raise OracError(str(err))


def get_repository_revision():
    """Call git to determine repository revision number"""
    from pyorac.local_defaults import orac_dir
    from subprocess import check_output

    fdr = os.getcwd()
    try:
        os.chdir(os.environ["ORACDIR"])
    except KeyError:
        os.chdir(orac_dir)
    try:
        tmp = check_output(["git", "rev-list", "--count", "HEAD"],
                           universal_newlines=True)
        return int(tmp)
    except:
        raise ValueError('Unable to determine revision number. Set it using -r')
    finally:
        os.chdir(fdr)


def read_orac_libraries(filename):
    """Read the ORAC library definitions into a Python dictionary"""
    from re import sub

    def parse_with_lib(lib):
        """Function called by re.sub to replace variables with their values
        http://stackoverflow.com/questions/7868554/python-re-subs-replace-
        function-doesnt-accept-extra-arguments-how-to-avoid"""
        def replace_var(matchobj):
            if len(matchobj.group(0)) > 3:
                return lib[matchobj.group(0)[2:-1]]
        return replace_var

    libraries = {}
    try:
        if os.environ['ORAC_LIBBASE']:
            libraries['ORAC_LIBBASE'] = os.environ['ORAC_LIBBASE']
        if os.environ['ORAC_LIBBASE_FORTRAN']:
            libraries['ORAC_LIBBASE_FORTRAN'] = os.environ['ORAC_LIBBASE_FORTRAN']
        if os.environ['CONDA_PREFIX']:
            libraries['CONDA_PREFIX'] = os.environ['CONDA_PREFIX']
    except KeyError:
        pass

    # Open ORAC library file
    with open(filename, 'r') as f:
        # Loop over each line
        for line in f:
            # Only process variable definitions
            if '=' in line and '\\' not in line and not line.startswith("#"):
                line = line.replace("\n", '')
                parts = [l.strip() for l in line.split('=', 2)]

                # Replace any variables in this line with those we already know
                fixed = sub(r"\$\(.*?\)", parse_with_lib(libraries), parts[1])

                # Add this line to the dictionary
                libraries[parts[0]] = fixed

    return libraries


def _str2bool(value):
    """Parse a string into a boolean value"""
    return value.lower() in ("yes", "y", "true", "t", "1")


def warning_format(message, category, filename, lineno, line=None):
    """Replacement for the default formatting function for warnings."""
    from pyorac.colour_print import colour_format
    from pyorac.definitions import Regression, OracWarning, COLOURING

    if issubclass(category, Regression):
        return colour_format(message.args[0] + "\n")
    elif issubclass(category, OracWarning):
        mess = "{:s})".format(category.__name__)
        for a in message.args:
            mess += " {:s}".format(a)
        return colour_format(mess + "\n", COLOURING['warning'])
    else:
        return colour_format(
            "{:d}: {:s}: {:s}\n".format(lineno, category.__name__, str(message)),
            COLOURING['warning']
        )
