"""Utility functions for working with ORAC scripts and outputs."""
import os
import re
from pyorac import defaults


def build_orac_library_path(lib_dict=None, lib_list=None):
    """Build required LD_LIBRARY_PATH variable."""

    try:
        libs = os.environ["LD_LIBRARY_PATH"].split(':')
    except (KeyError, AttributeError):
        libs = []

    if lib_list is None:
        lib_list = extract_orac_libraries(lib_dict)

    return ':'.join(filter(None, libs + lib_list))


def call_exe(args, exe, driver, values=None):
    """Call an ORAC executable, managing the necessary driver file.

    Args:
    :list args: Arguments of the script.
    :str exe: Name of the executable.
    :str driver: Contents of the driver file to pass.
    :dict values: Arguments for the batch queueing system."""

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
        return -1

    # Write driver file
    (fdes, driver_file) = mkstemp('.driver', os.path.basename(exe) + '.',
                                  args.out_dir, True)
    fhandle = os.fdopen(fdes, "w")
    fhandle.write(driver)
    fhandle.close()

    if not args.batch:
        # Form processing environment
        env = dict(LD_LIBRARY_PATH=build_orac_library_path(),
                   OPENBLAS_NUM_THREADS="1", OMP_NUM_THREADS=str(args.procs))
        env["EMOSLIB_FILES"] = os.environ.get("EMOSLIB_FILES", "")
        env["LOCAL_DEFINITION_TEMPLATES"] = os.environ.get("LOCAL_DEFINITION_TEMPLATES", "")
        env["ECMWF_LOCAL_TABLE_PATH"] = os.environ.get("ECMWF_LOCAL_TABLE_PATH", "")
        env["BUFR_TABLE"] = os.environ.get("BUFR_TABLE", "")
        try:
            # This is only defined for the preprocessor
            env["PPDIR"] = args.emos_dir
        except AttributeError:
            pass

        # Call program
        try:
            start_time = time()
            check_call([exe, driver_file], env=env)
            if args.timing:
                colour_print(exe + ' took {:f}s'.format(time() - start_time),
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
        (gdes, script_file) = mkstemp('.sh', os.path.basename(exe) + '.',
                                      args.out_dir, True)
        ghandle = os.fdopen(gdes, "w")
        ghandle.write(args.batch_script + "\n")

        # Define processing environment
        libs = read_orac_library_file(args.orac_lib)
        ghandle.write("export LD_LIBRARY_PATH=" +
                      build_orac_library_path(libs) + "\n")
        ghandle.write("export OPENBLAS_NUM_THREADS=1\n")
        try:
            ghandle.write("export PPDIR=" + args.emos_dir + "\n")
        except AttributeError:
            pass
        defaults.BATCH.add_openmp_to_script(ghandle)

        # Call executable and give the script permission to execute
        ghandle.write(exe + ' ' + driver_file + "\n")
        if not args.keep_driver:
            ghandle.write("rm -f " + driver_file + "\n")
        ghandle.write("rm -f " + script_file + "\n")
        ghandle.close()
        os.chmod(script_file, 0o700)

        try:
            # Collect batch settings from defaults, command line, and script
            batch_params = defaults.BATCH_VALUES.copy()
            if values:
                batch_params.update(values)
            batch_params.update({key: val for key, val in args.batch_settings})

            batch_params['procs'] = args.procs

            # Form batch queue command and call batch queuing system
            cmd = defaults.BATCH.list_batch(batch_params, exe=script_file)

            if args.verbose or args.script_verbose:
                colour_print(' '.join(cmd), COLOURING['header'])
            out = check_output(cmd, universal_newlines=True)

            # Parse job ID # and return it to the caller
            jid = defaults.BATCH.parse_out(out, 'ID')
            return jid
        except CalledProcessError as err:
            raise OracError('Failed to queue job ' + exe)
        except SyntaxError as err:
            raise OracError(str(err))


def compare_nc_atts(dat0, dat1, filename, var):
    """Report if the attributes of a NCDF file or variable have changed."""
    from warnings import warn
    from pyorac.definitions import FieldMissing, Regression

    # Check if any attributes added/removed
    atts = set(dat0.ncattrs()).symmetric_difference(dat1.ncattrs())
    if atts:
        warn(FieldMissing(filename, ', '.join(atts)), stacklevel=3)

    # Check if any attributes changed
    for key in dat0.ncattrs():
        if key in atts:
            continue

        if (dat0.__dict__[key] != dat1.__dict__[key] and
                key not in defaults.ATTS_TO_IGNORE):
            warn(Regression(
                filename, var + ', ' + key, 'warning',
                'Changed attribute ({} vs {})'.format(
                    dat0.__dict__[key], dat1.__dict__[key]
                )
            ), stacklevel=3)


def compare_orac_out(file0, file1):
    """Compare two NCDF files"""
    import numpy as np
    from warnings import warn
    from pyorac.definitions import (Acceptable, FieldMissing, InconsistentDim,
                                    OracWarning, Regression, RoundingError)

    try:
        from netCDF4 import Dataset
    except ImportError:
        warn('Skipping regression tests as netCDF4 unavailable',
             OracWarning, stacklevel=2)
        return

    try:
        # Open files
        dat0 = Dataset(file0, 'r')
        dat1 = Dataset(file1, 'r')

        # Check if any dimensions added/removed
        dims = set(dat0.dimensions.keys()).symmetric_difference(
            dat1.dimensions.keys()
        )
        if dims:
            # Not bothering to identify which file contains the errant field
            warn(FieldMissing(file1, ', '.join(dims)), stacklevel=2)

        # Check if any dimensions changed
        for key in dat0.dimensions.keys():
            if key in dims:
                continue

            if dat0.dimensions[key].size != dat1.dimensions[key].size:
                warn(InconsistentDim(file1, key, dat0.dimensions[key].size,
                                     dat1.dimensions[key].size),
                     stacklevel=2)

            # Check attributes
            compare_nc_atts(dat0, dat1, file1, key)

        # Check if any variables added/removed
        variables = set(dat0.variables.keys()).symmetric_difference(
            dat1.variables.keys()
        )
        if variables:
            warn(FieldMissing(file1, ', '.join(variables)), stacklevel=2)

        # Check if any variables changed
        for key in dat0.variables.keys():
            if key in variables:
                continue

            att0 = dat0.variables[key]
            att1 = dat1.variables[key]

            # Turn off masking, so completely NaN fields can be equal
            att0.set_auto_mask(False)
            att1.set_auto_mask(False)

            compare_nc_atts(att0, att1, file1, key)

            if att0.size != att1.size:
                warn(InconsistentDim(file1, key, att0.size, att1.size), stacklevel=2)
                continue

            # Check if there has been any change
            if not np.allclose(att0, att1, equal_nan=True, rtol=0, atol=0):
                test = False

                # For floats, check if variation is acceptable
                if att0.dtype.kind == 'f':
                    test = np.allclose(
                        att0, att1, equal_nan=True, rtol=defaults.RTOL,
                        atol=defaults.ATOL
                    )
                else:
                    try:
                        if isinstance(att0.scale_factor, np.floating):
                            # Packed floats consider the scale factor
                            test = np.allclose(
                                att0, att1, equal_nan=True, rtol=defaults.RTOL,
                                atol=max(att0.scale_factor, defaults.ATOL)
                            )
                    except AttributeError:
                        # If there is no scale factor, treat as an integer
                        pass

                if test or key in defaults.VARS_TO_ACCEPT:
                    warn(Acceptable(file1, key), stacklevel=2)
                else:
                    warn(RoundingError(file1, key), stacklevel=2)
    except IOError:
        pass

    finally:
        if 'dat0' in locals() or 'dat0' in globals():
            dat0.close()
        if 'dat1' in locals() or 'dat1' in globals():
            dat1.close()


def extract_orac_libraries(lib_dict=None):
    """Return list of libraries ORAC should link to."""

    if lib_dict is None:
        try:
            lib_dict = read_orac_library_file(os.environ["ORAC_LIB"])
        except KeyError:
            lib_dict = read_orac_library_file(defaults.ORAC_LIB)

    return [m[0] for m in re.findall(r"-L(.+?)(\s|$)", lib_dict["LIBS"])]


def get_repository_revision():
    """Call git to determine repository revision number"""
    from subprocess import check_output

    fdr = os.getcwd()
    try:
        os.chdir(os.environ["ORACDIR"])
    except KeyError:
        os.chdir(defaults.ORAC_DIR)
    try:
        tmp = check_output(["git", "rev-list", "--count", "HEAD"],
                           universal_newlines=True)
        return int(tmp)
    except:
        raise ValueError('Unable to determine revision number. Set it using -r')
    finally:
        os.chdir(fdr)


def read_orac_library_file(filename):
    """Read the ORAC library definitions into a Python dictionary"""

    def fill_in_variables(text, libraries):
        """Replaces all $() with value from a dictionary or environment."""

        def parse_with_dict(dictionary):
            """Function called by re.sub to replace variables with their values
            http://stackoverflow.com/questions/7868554/python-re-subs-replace-
            function-doesnt-accept-extra-arguments-how-to-avoid"""

            def replace_var(matchobj):
                """Fetch name from dictionary."""
                try:
                    name = matchobj.group(1)
                    try:
                        return dictionary[name]
                    except KeyError:
                        return os.environ[name]
                except (IndexError, KeyError):
                    return ""

            return replace_var

        return re.sub(r"\$\((.+?)\)", parse_with_dict(libraries), text)

    # Read the file
    contents = {}
    continuation = False
    with open(filename, 'r') as fhandle:
        for raw_line in fhandle:
            strip_line = raw_line.strip()
            if strip_line.startswith("#") or len(strip_line) < 2:
                # Skip comment and empty lines
                if not strip_line.startswith("#"):
                    continuation = False
                continue

            line = fill_in_variables(raw_line, contents)
            if not continuation:
                key, value = line.split("=", 2)
                variable = key.strip("+\t ")
                if key[-1] == "+":
                    # Append to a previously defined variable
                    contents[variable] += " " + value.strip("\\\n\t ")
                else:
                    # Define new variable
                    contents[variable] = value.strip("\\\n\t ")
            else:
                # Add this to the previous variable
                contents[variable] += " " + line.strip("\\\n\t ")

            continuation = line.endswith("\\\n")

    return contents


def str2bool(value):
    """Parse a string into a boolean value"""
    return value.lower() in ("yes", "y", "true", "t", "1")


def warning_format(message, category, filename, lineno, line=None):
    """Replacement for the default formatting function for warnings."""
    from pyorac.colour_print import colour_format
    from pyorac.definitions import Regression, OracWarning, COLOURING

    if issubclass(category, Regression):
        return colour_format(message.args[0] + "\n")
    if issubclass(category, OracWarning):
        mess = "{:s})".format(category.__name__)
        for arg in message.args:
            mess += " {:s}".format(arg)
        return colour_format(mess + "\n", COLOURING['warning'])

    return colour_format(
        "{:d}: {:s}: {:s}\n".format(lineno, category.__name__, str(message)),
        COLOURING['warning']
    )
