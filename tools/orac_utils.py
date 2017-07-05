# Library of Python 2.7 functions used to call and regress ORAC
# 16 Feb 2016, AP: Initial Python 3.2 version
# 22 Jun 2016, AP: Initial Python 2.7 version
# 08 Jul 2016, AP: Debugging against more awkward python environments
# 14 Jul 2016, GT: Debugging on CEMS:
#                  Added support for a simple directory structure of
#                  YYYY subdir for NISE data
#                  Made paths to MCD43C1 and MCD43C2 data separate
#                  arguments
# 20 Jul 2016, AP: Remove shell=True from subprocess calls.
# 3  Aug 2016, SP: Fixed exception handling in the regression test function.
# 09 Mar 2017, GT: Improved support for use with batch queuing systems

import argparse
import colours
import copy
import datetime
import glob as glob
import local_defaults as defaults
import os
import re
import subprocess
import sys
import tempfile
import time
import warnings


#-----------------------------------------------------------------------------
#----- ERROR HANDLING CLASSES ------------------------------------------------
#-----------------------------------------------------------------------------

# Colours used when printing to screen
colouring = {}
colouring['pass']    = 'green'
colouring['warning'] = 'light yellow'
colouring['error']   = 'red'
colouring['text']    = 'cyan'
colouring['header']  = 'light cyan'
colouring['timing']  = 'magenta'

class OracError(Exception):
    """Copy of Exception class to differentiate script errors from system."""
    pass

class FileMissing(OracError):
    def __init__(self, desc, filename):
        OracError.__init__(self, 'Could not locate {:s}: {:s}'.format(desc,
                                                                      filename))
        self.desc = desc
        self.filename = filename

class BadValue(OracError):
    def __init__(self, variable, value):
        OracError.__init__(self, 'Invalid value for {:s}: {:s}'.format(variable,
                                                                       value))
        self.variable = variable
        self.value = value

class OracWarning(UserWarning):
    """Variation on UserWarning class which formats and colours the output."""
    def __init__(self, desc, col='warning'):
        UserWarning.__init__(self, colours.cformat(desc, colouring[col]))

class Regression(OracWarning):
    def __init__(self, filename, variable, col, desc):
        regex = re.search(r'_R(\d+)(.*)\.(.+)\.nc$', filename)
        if sys.stdout.isatty():
            text = '{:s}) {:s}: \C{{{:s}}}{:s}'.format(
                regex.group(3), variable, colouring[col], desc)
        else:
            text = '{:s}) {:s}: {:s}'.format(
                regex.group(3), variable, desc)
        OracWarning.__init__(self, text, 'text')

class InconsistentDim(Regression):
    def __init__(self, filename, variable, dim0, dim1):
        Regression.__init__(self, filename, variable, 'error',
                            'Inconsistent dimensions ({:d} vs {:d})'.format(
                                 dim0, dim1))

class FieldMissing(Regression):
    def __init__(self, filename, variable):
        Regression.__init__(self, filename, variable, 'warning',
                            'Field not present in one file')

class RoundingError(Regression):
    def __init__(self, filename, variable):
        Regression.__init__(self, filename, variable, 'error',
                            'Unequal elements')

class Acceptable(Regression):
    def __init__(self, filename, variable):
        Regression.__init__(self, filename, variable, 'pass',
                            'Acceptable variation')

# Replace default formatting function for warnings
def warning_format(message, category, filename, lineno, line=None):
    if issubclass(category, Regression):
        return colours.cformat("{:s}\n".format(message))
    elif issubclass(category, OracWarning):
        return colours.cformat("{:s}) {:s}\n".format(category.__name__, message),
                               colouring['warning'])
    else:
        return colours.cformat("{:d}: {:s}: {:s}\n".format(lineno,
                                                           category.__name__,
                                                           message),
                               colouring['warning'])

warnings.formatwarning = warning_format


#-----------------------------------------------------------------------------
#----- LIBRARY ROUTINES ------------------------------------------------------
#-----------------------------------------------------------------------------

def bound_time(dt=None,                                # Initial time
               dateDelta=datetime.timedelta(hours=6)): # Rounding interval
    """Return timestamps divisible by some duration that bound a given time"""
    # http://stackoverflow.com/questions/3463930/how-to-round-the-minute-of-a-datetime-object-python/10854034

    roundTo = dateDelta.total_seconds()
    if dt == None : dt = datetime.datetime.now()

    # Remove annoying microseconds
    time = dt + datetime.timedelta(0,0,-dt.microsecond)

    # Floor time to requested delta
    seconds = (dt - dt.min).seconds
    rounding = seconds // roundTo * roundTo
    start = time + datetime.timedelta(0,rounding-seconds)

    # Output floor and ceil of time
    return (start, start + dateDelta)

#-----------------------------------------------------------------------------

def build_orac_library_path(libs):
    """Build required LD_LIBRARY_PATH variable"""

    if "GRIBLIB" in libs:
        glib = "GRIBLIB"
    elif "ECCODESLIB" in libs:
        glib = "ECCODESLIB"
    else:
        raise OracError('Neither GRIB_API or ECCODES libraries found')
    ld_path = ':'.join([libs[key] for key in ("SZLIB", "EPR_APILIB", glib,
                                             "HDF5LIB", "HDFLIB",
                                             "NCDF_FORTRAN_LIB", "NCDFLIB")])
    if "LD_LIBRARY_PATH" in os.environ.keys():
        ld_path += ':' + os.environ["LD_LIBRARY_PATH"]
    return ld_path

#-----------------------------------------------------------------------------

def call_exe(args,           # Arguments of scripts
             exe,            # Name of executable
             driver,         # Contents of driver file for executable
             values=dict()): # Arguments of the batch queuing system
    """Call an ORAC executable, managing the necessary driver file"""

    # Optionally print command and driver file contents to StdOut
    if args.verbose or args.script_verbose or args.just_driver:
        colours.cprint(exe + ' <<<', colouring['header'])
        colours.cprint(driver, colouring['text'])

    if args.just_driver:
        return

    # Write driver file
    (fd, driver_file) = tempfile.mkstemp('.driver', os.path.basename(exe) + '.',
                                         args.out_dir, True)
    f = os.fdopen(fd, "w")
    f.write(driver)
    f.close()

    if not args.batch:
        # Form processing environment
        libs = read_orac_libraries(args.orac_lib)
        os.environ["LD_LIBRARY_PATH"] = build_orac_library_path(libs)

        # Define a directory for EMOS to put it's gridding
        try:
            os.environ["PPDIR"] = args.emos_dir
        except AttributeError:
            pass

        # Call program
        try:
            st = time.time()
            subprocess.check_call([exe, driver_file])
            if args.timing:
                colours.cprint(exe + ' took {:f}s'.format(time.time() - st),
                               colouring['timing'])
            return
        except subprocess.CalledProcessError as err:
            raise OracError('{:s} failed with error code {:d}. {:s}'.format
                            (' '.join(err.cmd), err.returncode, err.output))
        finally:
            if not args.keep_driver:
                os.remove(driver_file)

    else:
        # Write temporary script to call executable
        (gd, script_file) = tempfile.mkstemp('.sh', os.path.basename(exe) + '.',
                                             args.out_dir, True)
        g = os.fdopen(gd, "w")
        g.write(defaults.batch_script)

        # Define processing environment
        libs = read_orac_libraries(args.orac_lib)
        g.write("export LD_LIBRARY_PATH=" + build_orac_library_path(libs) + "\n")
        try:
            g.write("export PPDIR=" + args.emos_dir + "\n")
        except AttributeError:
            pass

        # Call executable and give the script permission to execute
        g.write(exe + ' ' + driver_file + "\n")
        if not args.keep_driver:
            g.write("rm -f " + driver_file + "\n")
        g.write("rm -f " + script_file + "\n")
        g.close()
        os.chmod(script_file, 0o700)

        try:
            # Add default arguments to values
            values.update(defaults.batch_values)

            # Check to see if the depend_arg batch value has been
            # supplied, and extract it from the other batch arguments
            # if it has.
            depend_arg = values.pop('depend_arg', None)

            # If required, add in the depend_arg arguments using the
            # correct format
            if depend_arg:
                depend_arg = depend_arg.split()
                dep_arg = defaults.batch.ParseDepend(depend_arg)
            else:
                dep_arg = None

            # Form batch queue command and call batch queuing system
            cmd = defaults.batch.PrintBatch(values, exe=script_file, depend_arg=dep_arg)

            colours.cprint(cmd, colouring['header'])

            if args.verbose:
                colours.cprint(cmd, colouring['header'])
            out = subprocess.check_output(cmd.split(' '))

            # Parse job ID # and return it to the caller
            jid = defaults.batch.ParseOut(out, 'ID')
            return jid
        except subprocess.CalledProcessError as err:
            raise OracError('Failed to queue job '+exe)
        except SyntaxError as err:
            raise OracError(err.message)

#-----------------------------------------------------------------------------

def compare_nc_atts(d0, d1, f, var):
    """Report if the attributes of a NCDF file or variable have changed."""

    # Check if any attributes added/removed
    atts = set(d0.ncattrs()).symmetric_difference(d1.ncattrs())
    if len(atts) > 0:
        warnings.warn(FieldMissing(f, ', '.join(atts)), stacklevel=3)

    # Check if any attributes changed
    for key in d0.ncattrs():
        if key in atts:
            continue

        if (d0.__dict__[key] != d1.__dict__[key] and
            key not in defaults.atts_to_ignore):
            warnings.warn(Regression(f, var + ',' + key, 'warning',
                'Changed attribute ({} vs {})'.format(d0.__dict__[key],
                                                      d1.__dict__[key])),
                stacklevel=3)

#-----------------------------------------------------------------------------

def compare_orac_out(f0, f1):
    """Compare two NCDF files"""

    try:
        import netCDF4
        import numpy as np

    except ImportError as err:
        warnings.warn('Skipping regression tests as netCDF4/numpy unavailable',
                      OracWarning, stacklevel=2)
        return

    try:
        # Open files
        d0 = netCDF4.Dataset(f0, 'r')
        d1 = netCDF4.Dataset(f1, 'r')

        # Check if any dimensions added/removed
        dims = set(d0.dimensions.keys()).symmetric_difference(
            d1.dimensions.keys())
        if len(dims) > 0:
            # Not bothering to identify which file contains the errant field
            warnings.warn(FieldMissing(f1, ', '.join(dims)), stacklevel=2)

        # Check if any dimensions changed
        for key in d0.dimensions.keys():
            if key in dims:
                continue

            if d0.dimensions[key].size != d1.dimensions[key].size:
                warnings.warn(InconsistentDim(f1, key, d0.dimensions[key].size,
                                              d1.dimensions[key].size),
                              stacklevel=2)

        # Check attributes
        compare_nc_atts(d0, d1, f1, key)

        # Check if any variables added/removed
        vars = set(d0.variables.keys()).symmetric_difference(d1.variables.keys())
        if len(vars) > 0:
            warnings.warn(FieldMissing(f1, ', '.join(vars)), stacklevel=2)

        # Check if any variables changed
        for key in d0.variables.keys():
            if key in vars:
                continue

            a0 = d0.variables[key]
            a1 = d1.variables[key]

            # Turn off masking, so completely NaN fields can be equal
            a0.set_auto_mask(False)
            a1.set_auto_mask(False)

            compare_nc_atts(a0, a1, f1, key)

            if a0.size != a1.size:
                warnings.warn(InconsistentDim(f1, key, a0.size, a1.size),
                              stacklevel=2)
                continue

            # Check if there has been any change
            if not np.allclose(a0, a1, equal_nan=True, rtol=0, atol=0):
                test = False

                # For floats, check if variation is acceptable
                if a0.dtype.kind == 'f':
                    test = np.allclose(a0, a1, equal_nan = True,
                                       rtol = defaults.rtol,
                                       atol = defaults.atol)
                else:
                    try:
                        if isinstance(a0.scale_factor, np.floating):
                            # Packed floats consider the scale factor
                            test = np.allclose(a0, a1, equal_nan = True,
                                               rtol = defaults.rtol,
                                               atol = max(a0.scale_factor,
                                                          defaults.atol))
                    except AttributeError:
                        # If there is no scale factor, treat as an integer
                        pass

                if test or key in defaults.vars_to_accept:
                    warnings.warn(Acceptable(f1, key), stacklevel=2)
                else:
                    warnings.warn(RoundingError(f1, key), stacklevel=2)
    except Exception,e:
        warnings.warn('Problem performing regression tests. '+str(e),
                      OracWarning, stacklevel=2)

    finally:
        if ('d0' in locals() or 'd0' in globals()): d0.close()
        if ('d1' in locals() or 'd1' in globals()): d1.close()

#-----------------------------------------------------------------------------

def date_back_search(fld,      # Folder to be searched
                     date    , # Initial date to consider
                     pattern): # strftime format string to parse filename
    """Search a folder for the file with timestamp nearest in the past to a given date"""

    dt = date
    tried_climat = False
    while True:
        f = glob.glob(fld + dt.strftime(pattern))
        if f:
            if len(f) == 1:
                return f[0]
            else:
                return f[-1]
        else:
            if not tried_climat:
                if not glob.glob(fld +'/*'+ str(dt.year)+'*'):
                    pattern = pattern.replace('%Y','XXXX')
                    tried_climat = True
                else:
                    dt -= datetime.timedelta(days=1)
            else:
                if not glob.glob(fld + '/*XXXX*'):
                    raise FileMissing(fld, pattern)
                else:
                    dt -= datetime.timedelta(days=1)

#-----------------------------------------------------------------------------

def dict_from_list(l):
    return dict(zip(l, range(1,len(l)+1)))

#-----------------------------------------------------------------------------

def find_previous_orac_file(new_file):
    """Given an ORAC filename, find the most recent predecessor in the
    same folder."""

    # Regex for revision number
    reg = re.compile('_R(\d+)')

    new_revision = int(reg.search(new_file).group(1))

    # Check all versions of this file
    diff = sys.maxint
    old_file = None
    for f in glob.glob(reg.sub('_R*', new_file)):
        change = new_revision - int(reg.search(f).group(1))
        if change > 0 and change < diff:
            diff = change
            old_file = f

    return old_file

#-----------------------------------------------------------------------------

def form_bound_filenames(bounds,  # List of times to be
                         fld,     # Folder containing BADC files
                         format): # Formatting string for strftime
    """Form 2-element lists of filenames from bounding timestamps"""

    out = [fld + time.strftime(format) for time in bounds]

    for i in range(len(out)):
        f2 = glob.glob(out[i])
        if len(f2) == 0:
            raise FileMissing('ECMWF file', out[i])
        else:
            out[i] = f2[len(f2)-1]
    return out

#-----------------------------------------------------------------------------

def get_svn_revision():
    """Call SVN to determine repository revision number"""

    try:
        tmp = subprocess.check_output(["svn", "info"],
                                      universal_newlines=True)
        m = re.search('Revision: (\d+?)\n', tmp)
        return int(m.group(1))
    except:
        warnings.warn('Unable to determine revision number.', OracWarning,
                      stacklevel=2)
        return 0

#-----------------------------------------------------------------------------

def glob_dirs(dirs, path, desc):
    """Search a number of directories for files satisfying some simple regex"""

    f = []
    for d in dirs:
        f.extend(glob.glob(d + '/' + path))

    if len(f) == 0:
        # No file found, so throw error
        raise FileMissing(desc, path)
    else:
        # Return the most recent file found
        f.sort(key=lambda x: os.stat(x).st_mtime, reverse=True)
        return f[0]

#-----------------------------------------------------------------------------

# Function called by re.sub to replace variables with their values
#http://stackoverflow.com/questions/7868554/python-re-subs-replace-function-doesnt-accept-extra-arguments-how-to-avoid
def parse_with_lib(lib):
    def replace_var(matchobj):
        if len(matchobj.group(0)) > 3:
            return lib[matchobj.group(0)[2:-1]]
    return replace_var

#----------------------------------------------------------------------------

class FileName:
    """Parses L1B or ORAC filenames to determine the instrument and
    measurement time.
    Member variables:
    l1b      - Name of the level 1B imager file
    geo      - Name of the corresponding geolocation file
    sensor   - Capitalised name of the instrument
    platform - Name of the satellite platform, formatted for the preprocessor
    inst     - Combined sensor/platform, formatted for the main processor
    time     - datetime object giving the start time of the orbit/granule
    dur      - timedelta object giving the expected duration of the file"""

    def __init__(self, filename):
        self.l1b = filename

        # Work out what sensor this file comes from
        m = re.search('ATS_TOA_1P([A-Za-z]{4})'
                      '(?P<year>\d{4})(?P<month>\d{2})(?P<day>\d{2})_'
                      '(?P<hour>\d{2})(?P<min>\d{2})(?P<sec>\d{2})_'
                      '(?P<duration>\d{7})(?P<phase>\d)(?P<cycle>\d{4})_'
                      '(?P<rel_orbit>\d{5})_(?P<abs_orbit>\d{5})_'
                      '(?P<count>\d{4})\.N1', filename)
        if m:
            self.sensor   = 'AATSR'
            self.platform = 'Envisat' # For preprocessor
            self.inst     = 'AATSR'   # For main processor
            self.time = datetime.datetime(
                int(m.group('year')), int(m.group('month')), int(m.group('day')),
                int(m.group('hour')), int(m.group('min')), int(m.group('sec')), 0)
            self.dur = datetime.timedelta(seconds=int(m.group('duration')))
            self.geo = filename
            return

        m = re.search('M(?P<platform>[OY])D021KM\.'
                      'A(?P<year>\d{4})(?P<doy>\d{3})\.'
                      '(?P<hour>\d{2})(?P<min>\d{2})\.(?P<collection>\d{3})\.'
                      '(?P<proc_time>\d{13}).*hdf', filename)
        if m:
            self.sensor = 'MODIS'
            if m.group('platform') == 'O':
                self.platform = 'TERRA'       # For preprocessor
                self.inst     = 'MODIS-TERRA' # For main processor
            else: # == 'Y'
                self.platform = 'AQUA'
                self.inst     = 'MODIS-AQUA'
            self.time = (datetime.datetime(int(m.group('year')), 1, 1,
                                           int(m.group('hour')),
                                           int(m.group('min')), 0, 0) +
                         datetime.timedelta(days=int(m.group('doy'))-1))
            self.dur  = datetime.timedelta(minutes=5) # Approximately
            self.geo  = ('M' + m.group('platform') + 'D03.A' + m.group('year') +
                         m.group('doy') + '.' + m.group('hour') +
                         m.group('min') + '.' + m.group('collection') + '.*hdf')
            return

        m = re.search('noaa(?P<platform>\d{1,2})_'
                      '(?P<year>\d{4})(?P<month>\d{2})(?P<day>\d{2})_'
                      '(?P<hour>\d{2})(?P<min>\d{2})_(\d{5})_satproj_(\d{5})_'
                      '(\d{5})_avhrr.h5', filename)
        if m:
            self.sensor   = 'AVHRR'
            self.platform = 'noaa' + m.group('platform')
            self.inst     = 'AVHRR-NOAA' + m.group('platform')
            self.time = datetime.datetime(
                int(m.group('year')), int(m.group('month')), int(m.group('day')),
                int(m.group('hour')), int(m.group('min')), 0, 0)
            self.dur  = datetime.timedelta(seconds=6555) # Guessing
            # The following may be problematic with interesting dir names
            self.geo  = filename.replace('_avhrr.h5', '_sunsatangles.h5')
            return

        # Default AVHRR filename format produced by pygac (differs from
        # DWD produced L1b data)
        m = re.search('ECC_GAC_avhrr_noaa(?P<platform>\d{1,2})_'
                      '(\d{5})_(?P<year>\d{4})(?P<month>\d{2})(?P<day>\d{2})'
                      'T(?P<hour>\d{2})(?P<min>\d{2})(\d{3})Z_'
                      '(\d{8})T(\d{7})Z.h5', filename)
        if m:
            self.sensor   = 'AVHRR'
            self.platform = 'noaa' + m.group('platform')
            self.inst     = 'AVHRR-NOAA' + m.group('platform')
            # The time specification could be fixed, as the pygac file
            # names include start and end times, to 0.1 seconds
            self.time = datetime.datetime(
                int(m.group('year')), int(m.group('month')), int(m.group('day')),
                int(m.group('hour')), int(m.group('min')), 0, 0)
            self.dur  = datetime.timedelta(seconds=6555) # Guessing
            self.geo  = filename.replace('ECC_GAC_avhrr_',
                                         'ECC_GAC_sunsatangles_')
            return

        m = re.search('H-000-MSG(?P<platform>\d{1})__-MSG(\d{1})'
                      '________-_________-EPI______-'
                      '(?P<year>\d{4})(?P<month>\d{2})(?P<day>\d{2})'
                      '(?P<hour>\d{2})(?P<min>\d{2})-__', filename)
        if m:
            self.sensor   = 'SEVIRI'
            self.platform = 'MSG'+str(int(m.group('platform'))+7)
            self.inst     = 'SEVIRI'+m.group('platform')
            self.time     = datetime.datetime(
                int(m.group('year')), int(m.group('month')), int(m.group('day')),
                int(m.group('hour')), int(m.group('min')), 0, 0)
            self.dur      = datetime.timedelta(seconds=900) # Guessing
            self.geo      = filename
            return

        m = re.search(defaults.project + '-L2-CLOUD-CLD-'
                      '(?P<sensor>\w+)_ORAC_(?P<platform>\w+)_'
                      '(?P<year>\d{4})(?P<month>\d{2})(?P<day>\d{2})'
                      '(?P<hour>\d{2})(?P<min>\d{2})_'
                      'R(?P<revision>\d+).*', filename)
        if m:
            self.sensor   = m.group('sensor')
            self.platform = m.group('platform')
            if m.group('sensor') == 'AATSR':
                # SAD file names don't include platform for ATSR instruments
                self.inst = m.group('sensor')
            else:
                self.inst = m.group('sensor') + '-' + m.group('platform').upper()
            self.time = datetime.datetime(
                int(m.group('year')), int(m.group('month')), int(m.group('day')),
                int(m.group('hour')), int(m.group('min')), 0, 0)
            self.dur = None
            self.geo = None
            return

        raise OracError('Unexpected filename format - ' + filename)

#-----------------------------------------------------------------------------

def read_orac_libraries(filename):
    """Read the ORAC library definitions into a Python dictionary"""

    libraries = {}
    try:
        if os.environ['LIBBASE']:
            libraries['LIBBASE'] = os.environ['LIBBASE']
    except KeyError:
        pass

    # Open ORAC library file
    with open(filename, 'r') as f:
        # Loop over each line
        for line in f:
            # Only process variable definitions
            if '=' in line and '\\' not in line:
                line = line.replace("\n", '')
                parts = line.split('=',2)

                # Replace any variables in this line with those we already know
                fixed = re.sub(r"\$\(.*?\)", parse_with_lib(libraries), parts[1])

                # Add this line to the dictionary
                libraries[parts[0]] = fixed

    return libraries

#-----------------------------------------------------------------------------

def str2bool(value):
    """Parse a string into a boolean value"""
    return value.lower() in ("yes", "y", "true", "t", "1")

#-----------------------------------------------------------------------------
#----- INSTRUMENT/CLASS DEFINITIONS ------------------------------------------
#-----------------------------------------------------------------------------
# Map wavelengths available on each instrument to their channel numbers
map_wvl_to_inst = {
    'AATSR': dict_from_list((0.55, 0.67, 0.87, 1.6, 3.7, 11, 12,
                             -0.55, -0.67, -0.87, -1.6, -3.7, -11, -12)),
    'AVHRR': dict_from_list((0.67, 0.87, 1.6, 3.7, 11, 12)),
    'HIMAWARI': dict_from_list((0.47, 0.51, 0.67, 0.87, 1.6, 2.3, 3.7, 6.2,
                                6.9, 7.3, 8.6, 9.6, 10, 11, 12, 13.3)),
    'MODIS': dict_from_list((0.67, 0.87, 0.47, 0.55, 1.2, 1.6, 2.1, 0.41,
                             0.44, 0.49, 0.53, 0.551, 0.667, 0.678, 0.75, 0.869,
                             0.91, 0.936, 0.94, 3.7, 3.96, 3.959, 4.05, 4.466,
                             4.516, 1.375, 6.715, 7.325, 8.55, 9.73, 11, 12,
                             13.3, 13.6, 13.9, 14.2)),
    'SEVIRI': dict_from_list((0.67, 0.81, 1.64, 3.92, 6.25, 7.35,
                              8.7, 9.7, 11, 12, 13.3)),
    'VIIRS': dict_from_list((0.41, 0.44, 0.49, 0.55, 0.67, 0.75, 0.87, 1.2,
                             1.4, 1.6, 2.3, 3.7, 4.1, 8.6, 11, 12))
}

# Define particle type array and class
settings = {}
class Invpar():
    """Container for settings to pass to an ORAC retrieval
    Member variables:
    var - Name of the element of the state vector
    ap  - A priori value to use
    fg  - First guess of this value
    sx  - A priori uncertainty on this value"""

    def __init__(self,
                 var,
                 ap=None,
                 fg=None,
                 sx=None):
        self.var = var
        if ap:
            self.ap = ap
            if fg == None:
                self.fg = ap
        if fg:
            self.fg = fg
        if sx:
            self.sx = sx

    def driver(self):
        """Output lines for a driver file to specify these settings"""
        driver = ''
        if self.ap != None:
            driver += "\nCtrl%XB[{:s}] = {}".format(self.var, self.ap)
        if self.fg != None:
            driver += "\nCtrl%X0[{:s}] = {}".format(self.var, self.fg)
        if self.sx != None:
            driver += "\nCtrl%Sx[{:s}] = {}".format(self.var, self.sx)
        return driver

class ParticleType():
    """Container for an ORAC particle type
    Member variables:
    inv - Tuple of Invpars giving settings to pass to retrieval
    wvl - Wavelengths used (negative implies the second view)
    sad - SAD file directory to use
    ls  - If true, process land and sea separately"""

    def __init__(self,
                 inv = (),
                 wvl = (0.55, 0.67, 0.87, 1.6, -0.55, -0.67, -0.87, -1.6),
                 sad = defaults.aer_sad_dir,
                 ls = True):
        self.inv = inv
        self.wvl = wvl
        self.sad = sad
        self.ls  = ls

settings['WAT'] = ParticleType(wvl=(0.67, 0.87, 1.6, 3.7, 11, 12),
                           sad=defaults.sad_dir, ls=False)
settings['ICE'] = ParticleType(wvl=(0.67, 0.87, 1.6, 3.7, 11, 12),
                           sad=defaults.sad_dir, ls=False)

tau = Invpar('ITau', ap=-1.0, sx=1.5)
settings['A70'] = ParticleType(inv=(tau, Invpar('IRe', ap=0.0856, sx=0.15)))
settings['A71'] = ParticleType(inv=(tau, Invpar('IRe', ap=-0.257, sx=0.15)))
settings['A72'] = ParticleType(inv=(tau, Invpar('IRe', ap=-0.257, sx=0.15)))
settings['A73'] = ParticleType(inv=(tau, Invpar('IRe', ap=-0.257, sx=0.15)))
settings['A74'] = ParticleType(inv=(tau, Invpar('IRe', ap=-0.257, sx=0.15)))
settings['A75'] = ParticleType(inv=(tau, Invpar('IRe', ap=-0.0419, sx=0.15)))
settings['A76'] = ParticleType(inv=(tau, Invpar('IRe', ap=0.0856, sx=0.15)))
settings['A77'] = ParticleType(inv=(tau, Invpar('IRe', ap=-0.0419, sx=0.15)))
settings['A78'] = ParticleType(inv=(tau, Invpar('IRe', ap=-0.257, sx=0.15)))
settings['A79'] = ParticleType(inv=(tau, Invpar('IRe', ap=-0.848, sx=0.15)))
settings['VSA'] = ParticleType(inv=(tau, Invpar('IRe', ap=-0.848, sx=0.15)))


#-----------------------------------------------------------------------------
#----- PARSER ARGUMENT DEFINITIONS -------------------------------------------
#-----------------------------------------------------------------------------

def args_common(parser, regression=False):
    """Define arguments common to all ORAC scripts."""

    if not regression:
        parser.add_argument('target', type=str,
                            help = 'File to be processed')

    # Add boolean parsing function to register (type='bool', not type=bool)
    # http://stackoverflow.com/questions/15008758/parsing-boolean-values-with-argparse
    parser.register('type', 'bool', str2bool)

    out = parser.add_argument_group('Common arguments paths')
    out.add_argument('-i','--in_dir', type=str, nargs='+',
                     help = 'Path for input.')
    out.add_argument('-o', '--out_dir', type=str,
                     help = 'Path for output.')
    out.add_argument('--orac_dir', type=str, nargs='?', metavar='DIR',
                     default = defaults.orac_trunk,
                     help = 'Path to ORAC community code repository.')
    out.add_argument('--orac_lib', type=str, nargs='?', metavar='FILE',
                     default = defaults.orac_lib,
                     help = 'Name and path of ORAC library specification.')

    key = parser.add_argument_group('Common keyword arguments')
    key.add_argument('--batch', action='store_true',
                      help = 'Use batch processing for this call.')
    key.add_argument('-J', '--just_driver', action='store_true',
                     help = 'Only print the driver file.')
    key.add_argument('-k', '--keep_driver', action='store_true',
                     help = 'Retain driver files after processing.')
    key.add_argument('--no_clobber', action='store_true',
                     help = 'Retain existing output files.')
    key.add_argument('--lambertian', action='store_true',
                     help = 'Assume a lambertian surface rather than BRDF.')
    key.add_argument('--timing', action='store_true',
                     help = 'Print duration of executable calls.')

    out = key.add_mutually_exclusive_group()
    out.add_argument('-v', '--script_verbose', action='store_true',
                     help = 'Print progress through script, not exe.')
    out.add_argument('-V', '--verbose', action='store_true',
                     help = 'Set verbose output from the postprocessor.')

def check_args_common(args):
    """Ensure common parser arguments are valid."""

    # If not explicitly given, assume input folder is in target definition
    if args.in_dir == None:
        args.in_dir = [ os.path.dirname(args.target) ]
        args.target = os.path.basename(args.target)
    if args.out_dir == None:
        args.out_dir = args.in_dir[0]

    for d in args.in_dir:
        if not os.path.isdir(d):
            raise FileMissing('in_dir', d)
    if not os.path.isdir(args.out_dir):
        os.makedirs(args.out_dir, 0o774)
    if not os.path.isdir(args.orac_dir):
        raise FileMissing('ORAC repository directory', args.orac_dir)
    if not os.path.isfile(args.orac_lib):
        raise FileMissing('ORAC library file', args.orac_lib)

#-----------------------------------------------------------------------------

def args_preproc(parser):
    """Define arguments for preprocessor script."""

    key = parser.add_argument_group('Preprocessor keywords')
    key.add_argument('-c', '--channel_ids', type=int, nargs='+', metavar='#',
                     default = None,
                     help = 'Channels to be considered by the preprocessor.')
    key.add_argument('--day_flag', type=int, nargs='?', choices=[0,1,2,3],
                     default = 3,
                     help = '1=Process day only, 2=Night only, 0|3=Both')
    key.add_argument('--dellat', type=float, nargs='?', metavar='VALUE',
                     default = 1.38888889,
                     help = 'Reciprocal of latitude grid resolution.')
    key.add_argument('--dellon', type=float, nargs='?', metavar='VALUE',
                     default = 1.38888889,
                     help = 'Reciprocal of longitude grid resolution.')
    key.add_argument('-l', '--limit', type=int, nargs=4, default=(0, 0, 0, 0),
                     metavar=('X0', 'X1', 'Y0', 'Y1'),
                     help = 'First/last pixel in across/along-track directions.')
    key.add_argument('--l1_land_mask', action='store_true',
                     help = 'Use the imager landmask rather than the USGS.')
    key.add_argument('--use_modis_emis', action='store_true',
                     help = 'Use MODIS surface emissivity rather than RTTOV.')
    key.add_argument('--use_oc', action='store_true',
                     help = 'Use the Ocean Colour CCI backscatter product.')

    att = parser.add_argument_group('Global attribute values')
    att.add_argument('--cfconvention', type=str, nargs='?', metavar='STRING',
                     default = 'CF-1.4',
                     help = 'CF convention used in file definition.')
    att.add_argument('--comments', type=str, nargs='?', metavar='STRING',
                     default = 'n/a',
                     help = 'Any additional comments on file contents or use.')
    att.add_argument('--email', type=str, nargs='?', metavar='STRING',
                     default = defaults.email,
                     help = 'Contact email address.')
    att.add_argument('--history', type=str, nargs='?', metavar='STRING',
                     default = 'n/a',
                     help = 'Description of the file processing history.')
    att.add_argument('--institute', type=str, nargs='?', metavar='STRING',
                     default = defaults.institute,
                     help = 'Research institute that produced the file.')
    att.add_argument('--keywords', type=str, nargs='?', metavar='STRING',
                     default = 'aerosol; cloud; optimal estimation',
                     help = 'Keywords describing contents of file.')
    att.add_argument('--license', type=str, nargs='?', metavar='STRING',
                     default = 'http://proj.badc.rl.ac.uk/orac/wiki/License',
                     help = 'Details of the license for use of the data.')
    att.add_argument('--processor', type=str, nargs='?', metavar='STRING',
                     default = 'ORAC',
                     help = 'Name of the L2 processor.')
    att.add_argument('--project', type=str, nargs='?', metavar='STRING',
                     default = defaults.project,
                     help = 'Name of the project overseeing this data.')
    att.add_argument('--references', type=str, nargs='?', metavar='STRING',
                     default = 'doi:10.5194/amt-5-1889-2012',
                     help = 'Appropriate citations for product.')
    att.add_argument('--summary', type=str, nargs='?', metavar='STRING',
                     default = 'n/a',
                     help = "Brief description of the file's purpose.")
    att.add_argument('--url', type=str, nargs='?', metavar='STRING',
                     default = 'http://proj.badc.rl.ac.uk/orac',
                     help = 'Reference webpage for product.')
    att.add_argument('--uuid', action='store_true',
                     help = 'Produce a unique identifier number for output.')
    att.add_argument('-r', '--revision', type=int, nargs='?', metavar='#',
                     help = 'Revision (version) number for file.')

    surf = parser.add_argument_group('Surface property paths')
    surf.add_argument('--emis_dir', type=str, nargs='?', metavar='DIR',
                      default = defaults.emis_dir,
                      help = 'Path to MODIS emissivity files.')
    surf.add_argument('--mcd43c3_dir', type=str, nargs='?', metavar='DIR',
                      default = defaults.mcd43c3_dir,
                      help = 'Path to MCD43C3 files.')
    surf.add_argument('--mcd43c1_dir', type=str, nargs='?', metavar='DIR',
                      default = defaults.mcd43c1_dir,
                      help = 'Path to MCD43C1 files.')
    surf.add_argument('--occci_dir', type=str, nargs='?', metavar='DIR',
                      default = defaults.occci_dir,
                      help = 'Path to Ocean Colour CCI files.')

    rttov = parser.add_argument_group('RTTOV file paths')
    rttov.add_argument('--atlas_dir', type=str, nargs='?', metavar='DIR',
                       default = defaults.atlas_dir,
                       help = 'Path to RTTOV emissivity atlas.')
    rttov.add_argument('--coef_dir', type=str, nargs='?', metavar='DIR',
                       default = defaults.coef_dir,
                       help = 'Path to RTTOV coefficient files.')

    ecmwf = parser.add_argument_group('ECMWF settings')
    ecmwf.add_argument('--ecmwf_flag', type=int, choices = range(5),
                       default = defaults.ecmwf_flag,
                       help = 'Type of ECMWF data to read in.')
    ecmwf.add_argument('--single_ecmwf', action='store_const',
                       default = 2, const = 0,
                       help = 'Do not interpolate ECMWF data.')
    ecmwf.add_argument('--skip_ecmwf_hr', action='store_true',
                       help = 'Ignore high resolution ECMWF files.')
    ecmwf.add_argument('--use_ecmwf_snow', action='store_true',
                       help = 'Use ECMWF snow/ice fields rather than NISE.')
    ecmwf.add_argument('--ecmwf_dir', type=str, nargs='?', metavar='DIR',
                       help = 'Path to all ECMWF files.')
    ecmwf.add_argument('--ggam_dir', type=str, nargs='?', metavar='DIR',
                       default = defaults.ggam_dir,
                       help = 'Path to ggam ECMWF files.')
    ecmwf.add_argument('--ggas_dir', type=str, nargs='?', metavar='DIR',
                       default = defaults.ggas_dir,
                       help = 'Path to ggas ECMWF files.')
    ecmwf.add_argument('--spam_dir', type=str, nargs='?', metavar='DIR',
                       default = defaults.spam_dir,
                       help = 'Path to spam ECMWF files.')
    ecmwf.add_argument('--hr_dir', type=str, nargs='?', metavar='DIR',
                       default = defaults.hr_dir,
                       help = 'Path to high resolution ECMWF files.')
    ecmwf.add_argument('--ecmwf_nlevels', type=int, nargs='?',
                       choices = (60, 91, 137), default = 60,
                       help = 'Number of levels in the ECMWF file used.')

    other = parser.add_argument_group('Other paths')
    other.add_argument('--emos_dir', type=str, nargs='?', metavar='DIR',
                       default = defaults.emos_dir,
                       help = 'Path for EMOS library temporary files.')
    other.add_argument('--nise_dir', type=str, nargs='?', metavar='DIR',
                       default = defaults.nise_dir,
                       help = 'Path to NISE products.')
    other.add_argument('--calib_file', type=str, nargs='?', metavar='FILE',
                       default = defaults.calib_file,
                       help = 'Name and path of AATSR calibration file.')
    other.add_argument('--usgs_file', type=str, nargs='?', metavar='FILE',
                       default = defaults.usgs_file,
                       help = 'Name and path of USGS DEM.')

def check_args_preproc(args):
    """Ensure preprocessor parser arguments are valid."""

    check_args_common(args)

    if args.ecmwf_dir:
        args.ggam_dir = args.ecmwf_dir
        args.ggas_dir = args.ecmwf_dir
        args.spam_dir = args.ecmwf_dir

    # Limit should either be all zero or all non-zero.
    limit_check = args.limit[0] == 0
    for limit_element in args.limit[1:]:
        if (limit_element == 0) ^ limit_check:
            warnings.warn('All elements of --limit should be non-zero.',
                          OracWarning, stacklevel=2)

    if not os.path.isdir(args.atlas_dir):
        raise FileMissing('RTTOV Atlas directory', args.atlas_dir)
    if not os.path.isfile(args.calib_file):
        raise FileMissing('AATSR calibration file', args.calib_file)
    if not os.path.isdir(args.coef_dir):
        raise FileMissing('RTTOV coefficients directory', args.coef_dir)
    if not os.path.isdir(args.emis_dir):
        raise FileMissing('ETTOV emissivity directory', args.emis_dir)
    if not os.path.isdir(args.emos_dir):
        raise FileMissing('EMOS temporary directory', args.emos_dir)
    if not os.path.isdir(args.ggam_dir):
        raise FileMissing('ECMWF GGAM directory', args.ggam_dir)
    if not os.path.isdir(args.ggas_dir):
        raise FileMissing('ECMWF GGAS directory', args.ggas_dir)
    if not os.path.isdir(args.hr_dir):
        raise FileMissing('ECMWF high resolution directory', args.hr_dir)
    if not os.path.isdir(args.mcd43c3_dir):
        raise FileMissing('MODIS MCD43C1 directory', args.mcd43c1_dir)
    if not os.path.isdir(args.mcd43c1_dir):
        raise FileMissing('MODIS MCD43C3 directory', args.mcd43c3_dir)
    if not os.path.isdir(args.occci_dir):
        raise FileMissing('OC CCI directory', args.occci_dir)
    if not os.path.isdir(args.nise_dir):
        raise FileMissing('NISE directory', args.nise_dir)
    if not os.path.isdir(args.spam_dir):
        raise FileMissing('ECMWF SPAM directory', args.spam_dir)
    if not os.path.isfile(args.usgs_file):
        raise FileMissing('USGS file', args.usgs_file)

#-----------------------------------------------------------------------------

def args_main(parser):
    """Define arguments for main processor script."""

    main = parser.add_argument_group('Main processor arguments')
    main.add_argument('-a', '--approach', type=str, nargs='?',
                      choices = ('AppCld1L', 'AppCld2L', 'AppAerOx',
                                 'AppAerSw', 'AppAerO1'),
                      help = 'Retrieval approach to be used.')
    main.add_argument('--ret_class', type = str, nargs='?',
                      choices = ('ClsCldWat', 'ClsCldIce', 'ClsAerOx',
                                 'ClsAerSw', 'ClsAshEyj'),
                      help = 'Retrieval class to be used (for layer 1).')
    main.add_argument('--extra_lines', type=str, nargs='?', metavar='FILE',
                      help = 'Name of file containing additional driver lines.')
    main.add_argument('--phase', type=str, default = 'WAT',
                      choices = settings.keys(),
                      help = 'Label of look-up table to use in retrieval.')
    main.add_argument('--sabotage', action='store_true',
                      help = 'Sabotage inputs during processing.')
    main.add_argument('--sad_dir', type=str, nargs='?', metavar='DIR',
                      default = defaults.sad_dir,
                      help = 'Path to SAD and LUT files.')
    all_types = ('CLEAR', 'SWITCHED_TO_WATER', 'FOG', 'WATER', 'SUPERCOOLED',
                 'SWITCHED_TO_ICE', 'OPAQUE_ICE', 'CIRRUS', 'OVERLAP',
                 'PROB_OPAQUE_ICE', 'PROB_CLEAR')
    main.add_argument('--types', type=str, nargs='+',
                      choices = all_types, default = all_types,
                      help = 'Pavolonis cloud types to process.')
    main.add_argument('--use_channel', type='bool', nargs='+', metavar='T/F',
                      default = [True, True, True, True, True, True],
                      help = 'Channels to be evaluated by main processor.')

    ls = main.add_mutually_exclusive_group()
    ls.add_argument('--land', action='store_false',
                    help = 'Process only land pixels.')
    ls.add_argument('--sea', action='store_false',
                    help = 'Process only sea pixels.')

    ca = main.add_mutually_exclusive_group()
    ca.add_argument('--cloud_only', action='store_true',
                    help = 'Process only cloudy pixels.')
    ca.add_argument('--aerosol_only', action='store_true',
                    help = 'Process only aerosol pixels.')

def check_args_main(args):
    """Ensure main processor parser arguments are valid."""

    check_args_common(args)

    if len(args.in_dir) > 1:
        warnings.warn('Main processor ignores all but first in_dir.',
                      OracWarning, stacklevel=2)
    if not os.path.isdir(args.in_dir[0]):
        raise FileMissing('Preprocessed directory', args.in_dir[0])
    if not os.path.isdir(args.sad_dir):
        raise FileMissing('sad_dir', args.sad_dir)
    # No error checking yet written for channel arguments

#-----------------------------------------------------------------------------

def args_postproc(parser):
    """Define arguments for postprocessor script."""

    post = parser.add_argument_group('Post-processor paths')
    post.add_argument('--chunking', action='store_true',
                      help = 'Chunk the reading/writing files to save memory.')
    post.add_argument('--compress', action='store_true',
                      help = 'Use compression in NCDF outputs.')
    post.add_argument('--cost_thresh', type=float, nargs='?',
                      default = 0.0,
                      help = 'Maximum cost to accept a pixel.')
    post.add_argument('--no_night_opt', action='store_true',
                      help = 'Do not output optical properties at night.')
    post.add_argument('-p', '--phases', type=str, nargs='+',
                      default = ['WAT', 'ICE'], choices = settings.keys(),
                      help = 'Phases to be processed.')
    post.add_argument('--suffix', type=str,
                      help = 'Suffix to include in output filename.')
    post.add_argument('--prob_thresh', type=float, nargs='?',
                      default = 0.0,
                      help = 'Minimum fractional probability to accept a pixel.')
    post.add_argument('--switch_phase', action='store_true',
                      help = ('With cloud processing, check if CTT is ' +
                              'appropriate for the selected type.'))

def check_args_postproc(args):
    """Ensure postprocessor parser arguments are valid."""

    check_args_common(args)

    for d in args.in_dir:
        if not os.path.isdir(d):
            raise FileMissing('Processed output directory', d)

#-----------------------------------------------------------------------------

def args_cc4cl(parser):
    """Define arguments for ORAC suite wrapper script."""

    cccl = parser.add_argument_group('Keywords for CC4CL suite processing')
    cccl.add_argument('-C', '--clobber', type=int, nargs='?', default=3,
                      choices = range(4),
                      help = ('Level of processing to clobber:\n' +
                              '0=None, 1=Post, 2=Main+Post, 3=All (default).'))
    cccl.add_argument('--dur', type=str, nargs=3,
                      default = ('24:00', '24:00', '24:00'),
                      help = ('Maximal duration (in HH:MM) required by the '
                              'pre, main and post processors.'))
    cccl.add_argument('--ram', type=int, nargs=3,
                      default = (11000, 11000, 11000),
                      help = ('Maximal memory (in Mb) used by the pre, main and '
                              'post processors.'))
    cccl.add_argument('--aer_sad_dir', type=str, nargs='?', metavar='DIR',
                      default = defaults.aer_sad_dir,
                      help = ('Path to aerosol SAD and LUT files, ' +
                              'when different to that for cloud.'))
    cccl.add_argument('--land_dir', type=str, nargs='?',
                      default = defaults.land_dir,
                      help = 'Name of subfolder for land-only aerosol output.')
    cccl.add_argument('--log_dir', type=str, nargs='?',
                      default = defaults.log_dir,
                      help = 'Name of subfolder for log files from batch proc.')
    cccl.add_argument('--main_dir', type=str, nargs='?',
                      default = defaults.main_dir,
                      help = 'Name of subfolder for main processed output.')
    cccl.add_argument('--pre_dir', type=str, nargs='?',
                      default = defaults.pre_dir,
                      help = 'Name of subfolder for preprocessed output.')
    cccl.add_argument('--sea_dir', type=str, nargs='?',
                      default = defaults.sea_dir,
                      help = 'Name of subfolder for sea-only aerosol output.')
    cccl.add_argument('--lnd_extra', type=str, nargs='?',
                      default = None,
                      help = 'Extra lines for land aerosol processing.')
    cccl.add_argument('--sea_extra', type=str, nargs='?',
                      default = None,
                      help = 'Extra lines for sea aerosol processing.')
    cccl.add_argument('--cld_extra', type=str, nargs='?',
                      default = None,
                      help = 'Extra lines for cloud aerosol processing.')

def check_args_cc4cl(args):
    """Ensure ORAC suite wrapper parser arguments are valid."""
    pass

#-----------------------------------------------------------------------------
#----- DRIVER FILE FUNCTIONS -------------------------------------------------
#-----------------------------------------------------------------------------

def build_preproc_driver(args):
    """Prepare a driver file for the preprocessor."""

    inst = FileName(args.target)
    file = glob_dirs(args.in_dir, args.target, 'L1B file')
    geo  = glob_dirs(args.in_dir, inst.geo, 'geolocation file')

    # Select NISE file
    if args.use_ecmwf_snow:
        nise = ''
    else:
        nise = (args.nise_dir + inst.time.strftime('/NISE.004/%Y.%m.%d/'+
                                                 'NISE_SSMISF17_%Y%m%d.HDFEOS'))
        if not os.path.isfile(nise):
            nise = (args.nise_dir + inst.time.strftime('/NISE.002/%Y.%m.%d/'+
                                                     'NISE_SSMIF13_%Y%m%d.HDFEOS'))
            if not os.path.isfile(nise):
                nise = (args.nise_dir + inst.time.strftime('/%Y/'+
                                                         'NISE_SSMIF13_%Y%m%d.HDFEOS'))
                if not os.path.isfile(nise):
                    nise = (args.nise_dir + inst.time.strftime('/%Y/'+
                                                             'NISE_SSMIF17_%Y%m%d.HDFEOS'))
                    if not os.path.isfile(nise):
                        raise FileMissing('NISE', nise)

    # Select previous surface reflectance and emissivity files
    alb  = date_back_search(args.mcd43c3_dir, inst.time,
                            '/%Y/MCD43C3.A%Y%j.005.*.hdf')
    if not args.lambertian:
        brdf = date_back_search(args.mcd43c1_dir, inst.time,
                                '/%Y/MCD43C1.A%Y%j.005.*.hdf')
    if not args.use_modis_emis:
        emis = date_back_search(args.emis_dir, inst.time,
                                '/global_emis_inf10_monthFilled_MYD11C3.A%Y%j.041.nc')

    # Select ECMWF files
    bounds = bound_time(inst.time + inst.dur//2)
    if (args.ecmwf_flag == 0):
        ggam = form_bound_filenames(bounds, args.ggam_dir,
                                    '/%Y/%m/%d/ERA_Interim_an_%Y%m%d_%H+00.nc')
    elif (args.ecmwf_flag == 1):
        ggam = form_bound_filenames(bounds, args.ggam_dir,
                                    '/%Y/%m/%d/ggam%Y%m%d%H%M.nc')
        ggas = form_bound_filenames(bounds, args.ggas_dir,
                                    '/%Y/%m/%d/ggas%Y%m%d%H%M.nc')
        spam = form_bound_filenames(bounds, args.spam_dir,
                                    '/%Y/%m/%d/gpam%Y%m%d%H%M.nc')
    elif (args.ecmwf_flag == 2):
        ggam = form_bound_filenames(bounds, args.ggam_dir,
                                    '/%Y/%m/%d/ggam%Y%m%d%H%M.grb')
        ggas = form_bound_filenames(bounds, args.ggas_dir,
                                    '/%Y/%m/%d/ggas%Y%m%d%H%M.nc')
        spam = form_bound_filenames(bounds, args.spam_dir,
                                    '/%Y/%m/%d/spam%Y%m%d%H%M.grb')
    elif (args.ecmwf_flag == 3):
        raise NotImplementedError('Filename syntax for --ecmwf_flag 3 unknown')
    elif (args.ecmwf_flag == 4):
        bounds = bound_time(inst.time + inst.dur//2,
                            datetime.timedelta(hours=3))
        ggam = form_bound_filenames(bounds, args.ggam_dir,
                                    '/%Y/%m/%d/C3D*%m%d%H*.nc')
        #ggam = form_bound_filenames(bounds, args.ggam_dir,
        #                            '/ECMWF_OPER_%Y%m%d_%H+%M.nc')
        ggas = ggam
        spam = ggam
    else:
        raise BadValue('ecmwf_flag', args.ecmwf_flag)

    if not args.skip_ecmwf_hr:
        #hr_ecmwf = form_bound_filenames(bounds, args.hr_dir,
        #                                '/ERA_Interim_an_%Y%m%d_%H+00_HR.grb')
        # These files don't zero-pad the hour for some reason
        hr_ecmwf = [args.hr_dir +
                    time.strftime('/ERA_Interim_an_%Y%m%d_') +
                    '{:d}+00_HR.grb'.format(time.hour*100)
                    for time in bounds]
        if not os.path.isfile(hr_ecmwf[0]):
            hr_ecmwf = [args.hr_dir +
            time.strftime('/%Y/%m/%d/ERA_Interim_an_%Y%m%d_') +
            '{:d}+00_HR.grb'.format(time.hour*100)
            for time in bounds]
        for f in hr_ecmwf:
            if not os.path.isfile(f):
                raise FileMissing('HR ECMWF file', f)
    else:
        hr_ecmwf=['','']

    occci = args.occci_dir + inst.time.strftime(
            '/ESACCI-OC-L3S-IOP-MERGED-1M_MONTHLY_4km_GEO_PML_OC4v6_QAA-'
            '%Y%m-fv2.0.nc')

    #------------------------------------------------------------------------

    if args.uuid:
        uid = str(uuid.uuid4())
    else:
        uid = 'n/a'

    # Add NetCDF library to path so following calls works
    libs = read_orac_libraries(args.orac_lib)
    os.environ["PATH"] = libs["NCDFLIB"][:-4] + '/bin:' + os.environ["PATH"]

    # Determine current time
    production_time = datetime.datetime.now().strftime("%Y%m%d%H%M%S")

    # Determine NCDF version from command line
    try:
        tmp0 = subprocess.check_output("ncdump", stderr=subprocess.STDOUT,
                                       universal_newlines=True)
    except OSError:
        raise OracError('NetCDF lib improperly built as ncdump not present.')
    m0 = re.search(r'netcdf library version (.+?) of', tmp0)
    if m0:
        ncdf_version = m0.group(1)
    else:
        ncdf_version = 'n/a'
        warnings.warn('Output formatting of ncdump may have changed.',
                      OracWarning, stacklevel=2)

    # Fetch ECMWF version from header of NCDF file
    try:
        if (ggam[0])[len(ggam[0])-2:] == 'nc':
            tmp1 = subprocess.check_output(["ncdump", "-h", ggam[0]],
                                           universal_newlines=True)
        else:
            tmp1 = subprocess.check_output(["ncdump", "-h", ggas[0]],
                                           universal_newlines=True)
    except OSError:
        raise FileMissing('ECMWF ggas file', ggas[0])
    m1 = re.search(r':history = "(.+?)" ;', tmp1)
    if m1:
        ecmwf_version = m1.group(1)
    else:
        ecmwf_version = 'n/a'
        warnings.warn('Header of ECMWF file may have changed.', OracWarning,
                      stacklevel=2)

    # Strip RTTOV version from library definition
    rttov_lib = glob.glob(libs['RTTOVLIB'] + '/librttov?*_main.a')
    m2 = re.search(r'librttov([\d\.]+)_main.a', rttov_lib[0])
    if m2:
        rttov_version = m2.group(1)
    else:
        rttov_version = 'n/a'
        warnings.warn('Naming of RTTOV library directory may have changed.',
                      OracWarning, stacklevel=2)

    # Fetch SVN version
    cwd = os.getcwd()
    os.chdir(args.orac_dir + '/pre_processing')
    try:
        tmp3 = subprocess.check_output(["svn", "--version"],
                                       universal_newlines=True)
        m3 = re.search('svn, version (.+?)\n', tmp3)
        svn_version = m3.group(1)
    except:
        svn_version = 'n/a'
        warnings.warn('Unable to call SVN.', OracWarning, stacklevel=2)

    # Fetch repository commit number
    if not args.revision:
        args.revision = get_svn_revision()

    file_version = 'R{}'.format(args.revision)
    os.chdir(cwd)

    #------------------------------------------------------------------------

    # Write driver file
    driver = """{sensor}
{l1b}
{geo}
{usgs}
{ggam[0]}
{coef}
{atlas}
{nise}
{alb}
{brdf}
{emis}
{dellon}
{dellat}
{out_dir}
{limit[0]}
{limit[1]}
{limit[2]}
{limit[3]}
{ncdf_version}
{conventions}
{institution}
{l2_processor}
{creator_email}
{creator_url}
{file_version}
{references}
{history}
{summary}
{keywords}
{comment}
{project}
{license}
{uuid}
{production_time}
{atsr_calib}
{ecmwf_flag}
{ggas[0]}
{spam[0]}
{chunk_flag}
{day_flag}
{verbose}
-
{assume_full_paths}
{include_full_brdf}
{rttov_version}
{ecmwf_version}
{svn_version}
ECMWF_TIME_INT_METHOD={ecmwf_int_method}
ECMWF_PATH_2={ggam[1]}
ECMWF_PATH2_2={ggas[1]}
ECMWF_PATH3_2={spam[1]}
USE_HR_ECMWF={use_ecmwf_hr}
ECMWF_PATH_HR={ecmwf_hr[0]}
ECMWF_PATH_HR_2={ecmwf_hr[1]}
USE_ECMWF_SNOW_AND_ICE={ecmwf_nise}
USE_MODIS_EMIS_IN_RTTOV={modis_emis}
ECMWF_NLEVELS={ecmwf_nlevels}
USE_L1_LAND_MASK={l1_land_mask}
USE_OCCCI={use_occci}
OCCCI_PATH={occci_file}""".format(
        alb               = alb,
        assume_full_paths = True, # Above file searching returns paths nor dirs
        atlas             = args.atlas_dir,
        atsr_calib        = args.calib_file,
        brdf              = brdf,
        chunk_flag        = False, # File chunking no longer required
        coef              = args.coef_dir,
        comment           = args.comments,
        conventions       = args.cfconvention,
        creator_email     = args.email,
        creator_url       = args.url,
        day_flag          = args.day_flag, # 0=1=Day, 2=Night
        dellat            = args.dellat,
        dellon            = args.dellon,
        ecmwf_flag        = args.ecmwf_flag,
        ecmwf_hr          = hr_ecmwf,
        ecmwf_int_method  = args.single_ecmwf,
        ecmwf_nise        = args.use_ecmwf_snow,
        ecmwf_nlevels     = args.ecmwf_nlevels,
        ecmwf_version     = ecmwf_version,
        emis              = emis,
        file_version      = file_version,
        geo               = geo,
        ggam              = ggam,
        ggas              = ggas,
        history           = args.history,
        include_full_brdf = not args.lambertian,
        institution       = args.institute,
        keywords          = args.keywords,
        l1_land_mask      = args.l1_land_mask,
        l1b               = file,
        l2_processor      = args.processor,
        license           = args.license,
        limit             = args.limit,
        modis_emis        = args.use_modis_emis,
        ncdf_version      = ncdf_version,
        nise              = nise,
        occci_file        = occci,
        out_dir           = args.out_dir,
        usgs              = args.usgs_file,
        production_time   = production_time,
        project           = args.project,
        references        = args.references,
        rttov_version     = rttov_version,
        sensor            = inst.sensor,
        spam              = spam,
        summary           = args.summary,
        svn_version       = svn_version,
        uuid              = uid,
        use_ecmwf_hr      = not args.skip_ecmwf_hr,
        use_occci         = args.use_oc,
        verbose           = args.verbose
    )

    if args.channel_ids:
        driver += "\nN_CHANNELS={}".format(len(args.channel_ids))
        driver += "\nCHANNEL_IDS={}".format(','.join(str(k)
                                            for k in args.channel_ids))

    outroot = '-'.join((args.project, 'L2', 'CLOUD', 'CLD',
                        '_'.join((inst.sensor, args.processor, inst.platform,
                                  inst.time.strftime('%Y%m%d%H%M'),
                                  file_version))))
    return (driver, outroot)

#-----------------------------------------------------------------------------

def build_main_driver(args):
    """Prepare a driver file for the main processor."""

    # Deal with different treatment of platform between pre and main processors
    inst = FileName(args.target)

    # Form mandatory driver file lines
    driver = """# ORAC Driver File
Ctrl%FID%Data_Dir          = "{in_dir}"
Ctrl%FID%Filename          = "{fileroot}"
Ctrl%FID%Out_Dir           = "{out_dir}"
Ctrl%FID%SAD_Dir           = "{sad_dir}"
Ctrl%InstName              = {sensor}
Ctrl%Ind%NAvail            = {nch}
Ctrl%Ind%Channel_Proc_Flag = {channels}
Ctrl%LUTClass              = "{phase}"
Ctrl%Process_Cloudy_Only   = {cloudy}
Ctrl%Process_Aerosol_Only  = {aerosoly}
Ctrl%Verbose               = {verbose}
Ctrl%RS%Use_Full_BRDF      = {use_brdf}""".format(
        aerosoly = args.aerosol_only,
        channels = ','.join('{:d}'.format(k) for k in args.use_channel),
        cloudy   = args.cloud_only,
        fileroot = args.target,
        in_dir   = args.in_dir[0],
        nch      = len(args.use_channel),
        out_dir  = args.out_dir,
        phase    = args.phase,
        sad_dir  = args.sad_dir,
        sensor   = inst.inst,
        use_brdf = not (args.lambertian or args.approach == 'AppAerSw'),
        verbose  = args.verbose
    )

    # Optional driver file lines
    if args.types:
        driver += "\nCtrl%NTypes_To_Process     = {:d}".format(len(args.types))
        driver += ("\nCtrl%Types_To_Process      = " +
                   ','.join(k+'_TYPE' for k in args.types))
    if args.sabotage:
        driver += "\nCtrl%Sabotage_Inputs       = true"
    if args.approach:
        driver += "\nCtrl%Approach              = " + args.approach
    if args.ret_class:
        driver += "\nCtrl%Class                 = " + args.ret_class
    if not args.sea:
        driver += "\nCtrl%Surfaces_To_Skip      = ISea"
    elif not args.land:
        driver += "\nCtrl%Surfaces_To_Skip      = ILand"
    for var in settings[args.phase].inv:
        driver += var.driver()
    if args.extra_lines:
        try:
            e = open(args.extra_lines, "r")
            driver += "\n"
            driver += e.read()
            e.close()
        except IOError:
            raise FileMissing('extra_lines', args.extra_lines)

    return driver

#-----------------------------------------------------------------------------

def build_postproc_driver(args, pri=None):
    """Prepare a driver file for the postprocessor. If the optional argument pri
    is not specified, this will search args.in_dir for primary files with name
    given by args.target and args.phases."""

    if pri == None:
        # Find all primary files of requested phases in given input folders.
        pri = []
        for phs in args.phases:
            for d in args.in_dir:
                pri.extend(glob.glob(d +'/' + args.target + phs + '.primary.nc'))

    if len(pri) < 2:
        raise FileMissing('sufficient processed files', args.target)

    # Assume secondary files are in the same folder as the primary
    files = zip(pri, [re.sub('primary', 'secondary', t) for t in pri])

    # Form driver file
    driver = """{wat_pri}
{ice_pri}
{wat_sec}
{ice_sec}
{out_pri}
{out_sec}
{switch}
COST_THRESH={cost_tsh}
NORM_PROB_THRESH={prob_tsh}
OUTPUT_OPTICAL_PROPS_AT_NIGHT={opt_nght}
VERBOSE={verbose}
USE_CHUNKING={chunking}
USE_NETCDF_COMPRESSION={compress}
USE_BAYESIAN_SELECTION={bayesian}""".format(
        bayesian = args.phases != ['WAT', 'ICE'],
        chunking = args.chunking,
        compress = args.compress,
        cost_tsh = args.cost_thresh,
        ice_pri  = files[1][0],
        ice_sec  = files[1][1],
        opt_nght = not args.no_night_opt,
        out_pri  = args.out_dir + '/' + '.'.join(filter(None, (
            args.target, args.suffix, 'primary', 'nc'))),
        out_sec  = args.out_dir + '/' + '.'.join(filter(None, (
            args.target, args.suffix, 'secondary', 'nc'))),
        prob_tsh = args.prob_thresh,
        switch   = args.switch_phase,
        verbose  = args.verbose,
        wat_pri  = files[0][0],
        wat_sec  = files[0][1])
    # Add additional files
    for f in files[2:]:
        driver += '\n'
        driver += f[0]
        driver += '\n'
        driver += f[1]

    return driver

#-----------------------------------------------------------------------------

class unique_list(list):
    """Extension of list() that allows one to append only new elements"""
    def unique_append(self, element):
        """Append element only if it is not already a member of the list"""
        if element not in self:
            self.append(element)

def do_main_proc(args, main_files, job_name, log_path, jid_pre, jid_main,
                 written_dirs, phs, tag):
    """Call sequence for main processor (that is repeated three times)"""

    check_args_main(args)
    out_file = args.out_dir + '/' + args.target + phs + '.primary.nc'
    main_files.append(out_file)
    if args.clobber >= 2 or not os.path.isfile(out_file):
        # Settings for batch processing
        values = {'job_name' : job_name + phs + tag,
                  'log_file' : log_path + job_name + phs + tag + '.log',
                  'err_file' : log_path + job_name + phs + tag + '.err',
                  'depend'   : jid_pre,
                  'duration' : args.dur[1],
                  'ram'      : args.ram[1]}

        main_driver = build_main_driver(args)
        jd = call_exe(args, args.orac_dir + '/src/orac', main_driver, values)
        if jd != None:
            jid_main.append(jd)
        written_dirs.unique_append(args.out_dir)


def cc4cl(orig):
    """Run the ORAC pre, main, and post processors on a file."""

    written_dirs = unique_list() # Folders we actually write to

    # Copy input arguments as we'll need to fiddle with them
    args = copy.copy(orig)

    # Determine a name for this job from the filename
    inst = FileName(args.target)
    log_path = args.out_dir + '/' + args.log_dir + '/'
    if args.batch and not os.path.isdir(log_path):
        os.makedirs(log_path, 0o774)

    # Sort out what channels are required
    if args.channel_ids == None:
        args.channel_ids = unique_list()
        for phs in args.phases:
            for w in settings[phs].wvl:
                args.channel_ids.unique_append(map_wvl_to_inst[inst.sensor][w])

    args.channel_ids.sort()

    # Work out output filename
    check_args_common(args)
    args.out_dir += '/' + args.pre_dir
    check_args_preproc(args)
    (pre_driver, outroot) = build_preproc_driver(args)
    job_name = inst.time.strftime('{}_%Y-%m-%d-%H-%M_R{}_'.format(inst.inst,
                                                                  args.revision))

    # Run preprocessor (checking if we're clobbering a previous run)
    if args.clobber >= 3 or not os.path.isfile(args.out_dir + '/' +
                                               outroot + '.config.nc'):
        # Settings for batch processing
        pre_values = {'job_name' : job_name + 'pre',
                      'log_file' : log_path + job_name + 'pre.log',
                      'err_file' : log_path + job_name + 'pre.err',
                      'duration' : args.dur[0],
                      'ram'      : args.ram[0]}

        jid_pre = call_exe(args, args.orac_dir+'/pre_processing/orac_preproc.x',
                           pre_driver, pre_values)
        written_dirs.unique_append(args.out_dir)
    else:
        jid_pre = None

    # Run main processor
    jid_main    = [] # ID no. for each queued job
    main_files  = [] # Main files that are generated
    args.target = outroot
    args.in_dir = [args.out_dir]
    for phs in args.phases:
        args.phase   = phs
        args.sad_dir = settings[phs].sad

        # Identify which channels to use
        ids_here = [map_wvl_to_inst[inst.sensor][w] for w in settings[phs].wvl]
        args.use_channel = [ch in ids_here for ch in args.channel_ids]

        # Process land and sea separately for aerosol
        if settings[phs].ls:
            if orig.land:
                args.out_dir  = orig.out_dir + '/' + orig.land_dir
                args.approach = 'AppAerSw'
                args.land = True
                args.sea  = False
                args.extra_lines = orig.lnd_extra

                do_main_proc(args, main_files, job_name, log_path, jid_pre,
                             jid_main, written_dirs, phs, 'L')

            if orig.sea:
                args.out_dir  = orig.out_dir + '/' + args.sea_dir
                args.approach = 'AppAerOx'
                args.land = False
                args.sea  = True
                args.extra_lines = orig.sea_extra

                do_main_proc(args, main_files, job_name, log_path, jid_pre,
                             jid_main, written_dirs, phs, 'S')

        else:
            args.out_dir  = orig.out_dir + '/' + args.main_dir
            args.approach = orig.approach
            args.land = orig.land
            args.sea  = orig.sea
            args.extra_lines = orig.cld_extra

            do_main_proc(args, main_files, job_name, log_path, jid_pre,
                         jid_main, written_dirs, phs, '')

    # Run postprocessor
    args.in_dir = written_dirs
    args.out_dir = orig.out_dir
    check_args_postproc(args)
    final_file = args.out_dir + '/' + (
        '.'.join((el for el in (outroot, args.suffix, 'primary', 'nc') if el)))
    if args.clobber >= 1 or not os.path.isfile(final_file):
        # Settings for batch processing
        post_values = {'job_name' : job_name + 'post',
                       'log_file' : log_path + job_name + 'post.log',
                       'err_file' : log_path + job_name + 'post.err',
                       'depend'   : jid_main,
                       'duration' : args.dur[2],
                       'ram'      : args.ram[2]}

        post_driver = build_postproc_driver(args, main_files)
        jid = call_exe(args,
                       args.orac_dir + '/post_processing/post_process_level2',
                       post_driver, post_values)
        written_dirs.unique_append(args.out_dir)
    else:
        jid = None

    # Output root filename and output folders for regression tests
    return (outroot, written_dirs, jid)

#-----------------------------------------------------------------------------
