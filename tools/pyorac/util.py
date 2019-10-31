"""Utility functions for working with ORAC scripts and outputs."""
import os


def build_orac_library_path(lib_dict=None, lib_list=None):
    """Build required LD_LIBRARY_PATH variable."""

    if lib_list is None:
        lib_list = extract_orac_libraries(lib_dict)

    libs = os.environ["LD_LIBRARY_PATH"].split(':') + lib_list
    return ':'.join(filter(None, libs))


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
        g.write(args.batch_script)

        # Define processing environment
        libs = read_orac_library_file(args.orac_lib)
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


def extract_orac_libraries(lib_dict=None):
    """Return list of libraries ORAC should link to."""
    from pyorac.local_defaults import orac_lib
    from re import findall

    if lib_dict is None:
        try:
            lib_dict = read_orac_library_file(os.environ["ORAC_LIB"])
        except KeyError:
            lib_dict = read_orac_library_file(orac_lib)

    return [m[0] for m in findall(r"-L(.+?)(\s|$)", lib_dict["LIBS"])]


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
    with open(filename, 'r') as f:
        for raw_line in f:
            if raw_line.startswith("#") or len(raw_line) < 2:
                # Skip comment and empty lines
                continue

            line = fill_in_variables(raw_line, contents)
            if continuation:
                # Add this to the previous variable
                contents[variable] += " " + line.strip("\\\n\t ")
            else:
                key, value = line.split("=", 2)
                variable = key.strip("+\t ")
                if key[-1] == "+":
                    # Append to a previously defined variable
                    contents[variable] += " " + value.strip("\\\n\t ")
                else:
                    # Define new variable
                    contents[variable] = value.strip("\\\n\t ")

            continuation = line.endswith("\\\n")

    return contents


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
