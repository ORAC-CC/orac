"""Utility functions for working with ORAC scripts and outputs."""
import os


def build_orac_library_path(lib_dict=None, lib_list=None):
    """Build required LD_LIBRARY_PATH variable."""

    if lib_list is None:
        lib_list = extract_orac_libraries(lib_dict)

    libs = os.environ["LD_LIBRARY_PATH"].split(':') + lib_list
    return ':'.join(filter(None, libs))


def call_exe(args, exe, driver, values=None):
    """Call an ORAC executable, managing the necessary driver file.

    Args:
    :list args: Arguments of the script.
    :str exe: Name of the executable.
    :str driver: Contents of the driver file to pass.
    :dict values: Arguments for the batch queueing system."""
    from pyorac.local_defaults import BATCH, BATCH_VALUES

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
        BATCH.add_openmp_to_script(ghandle)

        # Call executable and give the script permission to execute
        ghandle.write(exe + ' ' + driver_file + "\n")
        if not args.keep_driver:
            ghandle.write("rm -f " + driver_file + "\n")
        ghandle.write("rm -f " + script_file + "\n")
        ghandle.close()
        os.chmod(script_file, 0o700)

        try:
            # Collect batch settings from defaults, command line, and script
            batch_params = BATCH_VALUES.copy()
            if values:
                batch_params.update(values)
            batch_params.update({key: val for key, val in args.batch_settings})

            batch_params['procs'] = args.procs

            # Form batch queue command and call batch queuing system
            cmd = BATCH.list_batch(batch_params, exe=script_file)

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


def extract_orac_libraries(lib_dict=None):
    """Return list of libraries ORAC should link to."""
    from pyorac.local_defaults import ORAC_LIB
    from re import findall

    if lib_dict is None:
        try:
            lib_dict = read_orac_library_file(os.environ["ORAC_LIB"])
        except KeyError:
            lib_dict = read_orac_library_file(ORAC_LIB)

    return [m[0] for m in findall(r"-L(.+?)(\s|$)", lib_dict["LIBS"])]


def get_repository_revision():
    """Call git to determine repository revision number"""
    from pyorac.local_defaults import ORAC_DIR
    from subprocess import check_output

    fdr = os.getcwd()
    try:
        os.chdir(os.environ["ORACDIR"])
    except KeyError:
        os.chdir(ORAC_DIR)
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
    import re

    def fill_in_variables(text, libraries):
        """Replaces all $() with value from a dictionary or environment."""

        def parse_with_dict(dictionary):
            """Function called by re.sub to replace variables with their values
            http://stackoverflow.com/questions/7868554/python-re-subs-replace-
            function-doesnt-accept-extra-arguments-how-to-avoid"""

            def replace_var(matchobj):
                """Fetch name from dictionary."""
                from os import environ
                try:
                    name = matchobj.group(1)
                    try:
                        return dictionary[name]
                    except KeyError:
                        return environ[name]
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
