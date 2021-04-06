"""Variables and classes used throughout the ORAC scripts."""

# -----------------------------------------------------------------------------
# ----- GLOBAL VARIABLES ------------------------------------------------------
# -----------------------------------------------------------------------------

# Names of the possible Pavolonis cloud classes
ALL_TYPES = ('CLEAR', 'SWITCHED_TO_WATER', 'FOG', 'WATER', 'SUPERCOOLED',
             'SWITCHED_TO_ICE', 'OPAQUE_ICE', 'CIRRUS', 'OVERLAP',
             'PROB_OPAQUE_ICE', 'PROB_CLEAR')

# Colours used when printing to screen
COLOURING = {
    'pass': 'green',
    'warning': 'light yellow',
    'error': 'red',
    'text': 'cyan',
    'header': 'light cyan',
    'timing': 'magenta'
}


# -----------------------------------------------------------------------------
# ----- EXCEPTIONS AND WARNINGS -----------------------------------------------
# -----------------------------------------------------------------------------


class OracError(Exception):
    """Copy of Exception class to differentiate script errors from system."""
    pass


class FileMissing(OracError):
    """Error when a required file could not be found."""

    def __init__(self, desc, filename):
        OracError.__init__(self, 'Could not locate {:s}: {:s}'.format(desc,
                                                                      filename))
        self.desc = desc
        self.filename = filename


class BadValue(OracError):
    """Error when an out-of-range value is provided."""

    def __init__(self, variable, value):
        OracError.__init__(self, 'Invalid value for {}: {}'.format(variable,
                                                                   value))
        self.variable = variable
        self.value = value


class OracWarning(UserWarning):
    """Copy of UserWarning class to regression warnings from system."""
    pass


class Regression(OracWarning):
    """A field has changed during a regression test."""

    def __init__(self, filename, variable, col, desc):
        import re
        import sys

        regex = re.search(r'_R(\d+)(.*)\.(.+)\.nc$', filename)
        if sys.stdout.isatty():
            text = r'{:s}) {:s}: \C{{{:s}}}{:s}'.format(
                regex.group(3), variable, COLOURING[col], desc)
        else:
            text = '{:s}) {:s}: {:s}'.format(
                regex.group(3), variable, desc)
        OracWarning.__init__(self, text, 'text')


class InconsistentDim(Regression):
    """The dimensions of a field have changed."""

    def __init__(self, filename, variable, dim0, dim1):
        Regression.__init__(self, filename, variable, 'error', 'Inconsistent '
                                                               'dimensions ({:d} vs {:d})'.format(dim0, dim1))


class FieldMissing(Regression):
    """A field is missing from one of the files evaluated."""

    def __init__(self, filename, variable):
        Regression.__init__(self, filename, variable, 'warning',
                            'Field not present in one file')


class RoundingError(Regression):
    """A value has changed in a field."""

    def __init__(self, filename, variable):
        Regression.__init__(self, filename, variable, 'error',
                            'Unequal elements')


class Acceptable(Regression):
    """A value has changed by a small increment in a field."""

    def __init__(self, filename, variable):
        Regression.__init__(self, filename, variable, 'pass',
                            'Acceptable variation')


# -----------------------------------------------------------------------------
# ----- CONVIENIENCE CLASSES --------------------------------------------------
# -----------------------------------------------------------------------------
class FileName:
    """Parses L1B or ORAC filenames to determine the instrument and
    measurement time.

    Methods:
    job_name: Generates a unique descriptor of this file.
    root_name: Calculates the filename ORAC will produce for this file.

    Attributes:
    l1b (str): Name of the level 1B imager file.
    geo (str): Name of the corresponding geolocation file.
    sensor (str): Capitalised name of the instrument.
    platform (str): Name of the satellite platform, formatted for the
        preprocessor.
    inst (str): Combined sensor/platform, formatted for the main processor.
    time (datetime): tart time of the orbit/granule.
    dur (timedelta): Expected duration of the file.
    oractype (str): For an ORAC output, describes the type of file.
        Equals None otherwise.
    predef (bool): True if the sensor can accept predefined geolocation.
    noaa (str): The number of this AVHRR sensor (1, 2, or 3).
    """

    def __init__(self, in_dirs, filename=None):
        import datetime
        import os
        import re

        from netCDF4 import Dataset
        from dateutil import parser

        if filename is None:
            in_dirs, filename = os.path.split(in_dirs)

        if isinstance(in_dirs, tuple):
            self.folders = list(in_dirs)
        elif not isinstance(in_dirs, list):
            self.folders = [in_dirs]
        else:
            self.folders = in_dirs
        self.l1b = filename

        # Things that need to be here
        self.oractype = None
        self.predef = False
        self.orbit_num = None

        # Attempt AATSR L1B filename
        mat = re.search(
            r'ATS_TOA_1P([A-Za-z]{4})(?P<year>\d{4})(?P<month>\d{2})'
            r'(?P<day>\d{2})_(?P<hour>\d{2})(?P<min>\d{2})(?P<sec>\d{2})_'
            r'(?P<duration>\d{7})(?P<phase>\d)(?P<cycle>\d{4})_'
            r'(?P<rel_orbit>\d{5})_(?P<abs_orbit>\d{5})_(?P<count>\d{4})\.N1',
            filename
        )
        if mat:
            self.sensor = 'AATSR'
            self.platform = 'Envisat'  # For preprocessor
            self.inst = 'AATSR'  # For main processor
            self.time = datetime.datetime(
                int(mat.group('year')), int(mat.group('month')),
                int(mat.group('day')), int(mat.group('hour')),
                int(mat.group('min')), int(mat.group('sec')), 0
            )
            self.dur = datetime.timedelta(seconds=int(mat.group('duration')))
            self.geo = filename
            return

        # Attempt ATSR2 L1B filename
        mat = re.search(
            r'AT2_TOA_1P([A-Za-z]{4})(?P<year>\d{4})(?P<month>\d{2})'
            r'(?P<day>\d{2})_(?P<hour>\d{2})(?P<min>\d{2})(?P<sec>\d{2})_'
            r'(?P<duration>\d{7})(?P<phase>\d)(?P<cycle>\d{4})_'
            r'(?P<rel_orbit>\d{5})_(?P<abs_orbit>\d{5})_(?P<count>\d{4})\.E2',
            filename
        )
        if mat:
            self.sensor = 'ATSR2'
            self.platform = 'ERS2'  # For preprocessor
            self.inst = 'ATSR2'  # For main processor
            self.time = datetime.datetime(
                int(mat.group('year')), int(mat.group('month')),
                int(mat.group('day')), int(mat.group('hour')),
                int(mat.group('min')), int(mat.group('sec')), 0
            )
            self.dur = datetime.timedelta(seconds=int(mat.group('duration')))
            self.geo = filename
            return

        # Attempt MODIS L1B filename
        mat = re.search(
            r'M(?P<platform>[OY])D021KM\.A(?P<year>\d{4})(?P<doy>\d{3})\.'
            r'(?P<hour>\d{2})(?P<min>\d{2})\.(?P<collection>\d{3})\.'
            r'(?P<proc_time>\d{13}).*hdf', filename
        )
        if mat:
            self.sensor = 'MODIS'
            if mat.group('platform') == 'O':
                self.platform = 'TERRA'  # For preprocessor
                self.inst = 'MODIS-TERRA'  # For main processor
            else:  # == 'Y'
                self.platform = 'AQUA'
                self.inst = 'MODIS-AQUA'
            self.time = (datetime.datetime(
                int(mat.group('year')), 1, 1, int(mat.group('hour')),
                int(mat.group('min')), 0, 0
            ) + datetime.timedelta(days=int(mat.group('doy')) - 1))
            self.dur = datetime.timedelta(minutes=5)  # Approximately
            self.geo = ('M' + mat.group('platform') + 'D03.A' + mat.group('year') +
                        mat.group('doy') + '.' + mat.group('hour') +
                        mat.group('min') + '.' + mat.group('collection') + '.*hdf')
            return

        # Attempt reformatted AVHRR L1B filename
        mat = re.search(
            r'noaa(?P<platform>\d{1,2})_(?P<year>\d{4})(?P<month>\d{2})'
            r'(?P<day>\d{2})_(?P<hour>\d{2})(?P<min>\d{2})_(\d{5})_satproj_'
            r'(\d{5})_(\d{5})_avhrr.h5', filename
        )
        if mat:
            self.sensor = 'AVHRR'
            self.platform = 'noaa' + mat.group('platform')
            self.inst = 'AVHRR-NOAA' + mat.group('platform')
            self.time = datetime.datetime(
                int(mat.group('year')), int(mat.group('month')),
                int(mat.group('day')), int(mat.group('hour')),
                int(mat.group('min')), 0, 0
            )
            self.dur = datetime.timedelta(seconds=6555)  # Approximately
            # The following may be problematic with interesting dir names
            self.geo = filename.replace('_avhrr.h5', '_sunsatangles.h5')
            return

        # Default AVHRR filename format produced by pygac (differs from
        # DWD produced L1b data)
        mat = re.search(
            r'ECC_GAC_avhrr_noaa(?P<platform>\d{1,2})_(\d{5})_(?P<year>\d{4})'
            r'(?P<month>\d{2})(?P<day>\d{2})T(?P<hour>\d{2})(?P<min>\d{2})'
            r'(\d{3})Z_(\d{8})T(\d{7})Z.h5', filename
        )
        if mat:
            self.sensor = 'AVHRR'
            self.platform = 'noaa' + mat.group('platform')
            self.inst = 'AVHRR-NOAA' + mat.group('platform')
            # The time specification could be fixed, as the pygac file
            # names include start and end times, to 0.1 seconds
            self.time = datetime.datetime(
                int(mat.group('year')), int(mat.group('month')),
                int(mat.group('day')), int(mat.group('hour')),
                int(mat.group('min')), 0, 0
            )
            self.dur = datetime.timedelta(seconds=6555)  # Approximately
            self.geo = filename.replace('ECC_GAC_avhrr_',
                                        'ECC_GAC_sunsatangles_')
            return

        # Attempt Himawari L1B filename in HSD format
        mat = re.search(
            r'HS_H(?P<platform>\d{2})_(?P<year>\d{4})'
            r'(?P<month>\d{2})(?P<day>\d{2})_(?P<hour>\d{2})(?P<min>\d{2})'
            r'_(.*).DAT', filename
        )
        if mat:
            self.sensor = 'AHI'
            self.platform = 'Himawari-'+str(int(mat.group('platform')))
            self.inst = 'AHI-'+self.platform
            self.time = datetime.datetime(
                int(mat.group('year')), int(mat.group('month')),
                int(mat.group('day')), int(mat.group('hour')),
                int(mat.group('min')), 0, 0
            )
            self.dur = datetime.timedelta(seconds=600)  # Approximately
            self.geo = filename
            self.predef = True
            return

        # Attempt SEVIRI L1B filename in NAT format
        mat = re.search(
            r'MSG(?P<platform>\d{1})-SEVI-MSG(\d+)-(\d+)-NA-(?P<year>\d{4})'
            r'(?P<month>\d{2})(?P<day>\d{2})(?P<hour>\d{2})(?P<min>\d{2})'
            r'(?P<sec>[\d\.]+)Z-(.*).nat', filename
        )
        if mat:
            self.sensor = 'SEVIRI'
            self.platform = 'MSG' + mat.group('platform')
            self.inst = 'SEVIRI-' + self.platform
            self.time = datetime.datetime(
                int(mat.group('year')), int(mat.group('month')),
                int(mat.group('day')), int(mat.group('hour')),
                int(mat.group('min')), round(float(mat.group('sec'))), 0
            )
            self.dur = datetime.timedelta(seconds=900)  # Approximately
            self.geo = filename
            self.predef = True
            return

        # Attempt SEVIRI L1B filename for segment HRT format
        mat = re.search(
            r'H-000-MSG(?P<platform>\d{1})_+-MSG(\d+)_+-_+-EPI_+-(?P<year>\d{4})'
            r'(?P<month>\d{2})(?P<day>\d{2})(?P<hour>\d{2})(?P<min>\d{2})'
            r'-_+', filename
        )
        if mat:
            self.sensor = 'SEVIRI'
            self.platform = 'MSG' + mat.group('platform')
            self.inst = 'SEVIRI-' + self.platform
            self.time = datetime.datetime(
                int(mat.group('year')), int(mat.group('month')),
                int(mat.group('day')), int(mat.group('hour')),
                int(mat.group('min')), 0, 0
            )
            self.dur = datetime.timedelta(seconds=900)  # Approximately
            self.geo = filename
            self.predef = True
            return

        # Attempt SEVIRI L1B filename for Met Office format
        mat = re.search(
            r'MSG_(?P<year>\d{4})(?P<month>\d{2})(?P<day>\d{2})(?P<hour>\d{2})'
            r'(?P<min>\d{2}).*h5', filename
        )
        if mat:
            self.sensor = 'SEVIRI'
            for fdr in self.folders:
                tmp = os.path.join(fdr, filename)
                if os.path.isfile(tmp):
                    self.platform = _determine_platform_from_metoffice(tmp)
            self.inst = 'SEVIRI-' + self.platform
            self.time = datetime.datetime(
                int(mat.group('year')), int(mat.group('month')),
                int(mat.group('day')), int(mat.group('hour')),
                int(mat.group('min')), 0, 0
            )
            self.dur = datetime.timedelta(seconds=900)  # Approximately
            self.geo = filename
            self.predef = True
            return

        # For SLSTR, we passed a directory name
        mat = re.search(
            r'S3(?P<platform>[AB])_SL_1_RBT____(?P<year>\d{4})(?P<month>\d{2})'
            r'(?P<day>\d{2})T(?P<hour>\d{2})(?P<min>\d{2})(?P<sec>\d{2})_'
            r'\d{8}T\d{6}_\d{8}T\d{6}_(?P<duration>\d{4})_(?P<cycle>\d{3})_'
            r'(?P<orbit>\d{3})_(?P<frame>\d{4})_(?P<centre>[A-Za-z0-9]{3})_'
            r'(?P<class>[OFDR])_(?P<timeliness>[NS][RT])_(?P<version>\d{3}).'
            r'SEN3', filename
        )
        if mat:
            self.l1b = os.path.join(filename, "geodetic_in.nc")
            self.sensor = 'SLSTR'
            self.platform = 'Sentinel3' + mat.group('platform').lower()
            self.inst = 'SLSTR-Sentinel-3' + mat.group('platform').lower()
            self.dur = datetime.timedelta(seconds=int(mat.group('duration')))
            self.geo = os.path.join(filename, "geodetic_in.nc")

            for fdr in self.folders:
                try:
                    with Dataset(os.path.join(fdr, self.l1b)) as slstr_file:
                        # Round time to nearest minute
                        time = parser.parse(slstr_file.start_time).replace(tzinfo=None)
                        sec = (time - time.min).seconds
                        rounding = (sec + 30) // 60 * 60
                        self.time = time + datetime.timedelta(
                            0, rounding - sec, -time.microsecond
                        )

                        self.orbit_num = "{:05d}".format(
                            slstr_file.absolute_orbit_number
                        )
                        break
                except FileNotFoundError:
                    pass

            return

        # Processed ORAC output
        mat = re.search(
            r'(?P<project>\w+)-(?P<product>.+)-(?P<sensor>\w+)_'
            r'(?P<processor>\w+)_(?P<platform>\w+)_(?P<year>\d{4})'
            r'(?P<month>\d{2})(?P<day>\d{2})(?P<hour>\d{2})(?P<min>\d{2})'
            r'(?:_(?P<orbit_num>\d{5}))?_R'
            r'(?P<revision>\d+)(?P<phase>\w*)\.(?P<filetype>\w+)\.nc', filename
        )
        if mat:
            self.sensor = mat.group('sensor')
            self.platform = mat.group('platform')
            if 'ATSR' in mat.group('sensor'):
                # SAD file names don't include platform for ATSR instruments
                self.inst = mat.group('sensor')
            else:
                self.inst = mat.group('sensor') + '-' + mat.group('platform').upper()
            self.time = datetime.datetime(
                int(mat.group('year')), int(mat.group('month')),
                int(mat.group('day')), int(mat.group('hour')),
                int(mat.group('min')), 0, 0
            )
            self.dur = None
            self.geo = None
            self.oractype = mat.group('filetype')
            self.processor = mat.group('processor')
            self.revision = mat.group('revision')
            self.project = mat.group('project')
            self.product_name = mat.group('product')
            self.orbit_num = mat.group('orbit_num')
            return

        raise OracError('Unexpected filename format - ' + filename)

    @property
    def revision(self):
        """Revision number"""
        from pyorac.util import get_repository_revision

        try:
            return self._revision
        except AttributeError:
            return get_repository_revision()

    @revision.setter
    def revision(self, value):
        """Set revision number."""
        self._revision = int(value)

    def job_name(self, revision=None, tag='run'):
        """Returns a formatted description of this orbit."""
        if revision is None:
            revision = self.revision
        return self.time.strftime('{}_%M-%H-%d-%m-%Y_R{}_{}'.format(
            self.inst, revision, tag
        ))

    def root_name(self, revision=None, processor=None, project=None,
                  product_name=None):
        """Returns the ORAC filename for this file."""
        if revision is None:
            revision = self.revision
        try:
            if processor is None:
                processor = self.processor
            if project is None:
                project = self.project
            if product_name is None:
                product_name = self.product_name
        except AttributeError as err:
            terms = err.args[0].split("'")
            raise ValueError("A default root name can only be determined "
                             "for ORAC filenames. Please specify " + terms[-2])

        parts = [
            self.sensor, processor, self.platform,
            self.time.strftime('%Y%m%d%H%M'), "R{}".format(revision)
        ]
        if self.orbit_num:
            parts.insert(4, self.orbit_num)

        return '-'.join((project, product_name, "_".join(parts)))

    @property
    def noaa(self):
        """Returns platform number for NOAA AVHRR sensors."""
        plat = int(self.platform[4:])
        if plat in (6, 8, 10):
            return '1'
        elif plat in (7, 9, 11, 12, 13, 14):
            return '2'
        elif plat in (15, 16, 17, 18, 19):
            return '3'
        else:
            raise ValueError("Unknown AVHRR platform: " + self.platform)


def _determine_platform_from_metoffice(filename):
    from h5py import File

    with File(filename) as data:
        platform = data["MSG/Prologue/GeneralInfo"][0][0]

    if 321 > platform > 324:
        raise ValueError("Unrecognised platform number {}".format(platform))

    return "MSG{}".format(platform - 320)


# -----------------------------------------------------------------------------
# ----- INSTRUMENT/CLASS DEFINITIONS ------------------------------------------
# -----------------------------------------------------------------------------

class Invpar:
    """Container for settings to pass to an ORAC retrieval
    Member variables:
    var - Name of the element of the state vector
    ap  - A priori value to use
    fg  - First guess of this value
    sx  - A priori uncertainty on this value
    """

    def __init__(self, var, ap=None, fg=None, sx=None):
        self.var = var
        if ap:
            self.ap = ap
            if fg is None:
                self.fg = ap
        if fg:
            self.fg = fg
        if sx:
            self.sx = sx

    def driver(self):
        """Output lines for a driver file to specify these settings"""
        driver = ''
        if self.ap is not None:
            driver += "\nCtrl%XB[{:s}] = {}".format(self.var, self.ap)
        if self.fg is not None:
            driver += "\nCtrl%X0[{:s}] = {}".format(self.var, self.fg)
        if self.sx is not None:
            driver += "\nCtrl%Sx[{:s}] = {}".format(self.var, self.sx)
        return driver


class ParticleType:
    """Container for an ORAC particle type
    Member variables:
    inv - Tuple of Invpars giving settings to pass to retrieval
    wvl - Wavelengths used (negative implies the second view)
    sad - SAD file directory to use
    ls  - If true, process land and sea separately
    """

    def __init__(self, name, inv=(), sad="CCI_A70-A79"):
        self.name = name
        self.inv = inv
        self.sad = sad

    def sad_dir(self, sad_dirs, inst):
        """Return the path to the SAD files."""
        from glob import glob
        from os.path import join

        for fdr in sad_dirs:
            if "AVHRR" in inst.sensor:
                fdr_name = join(fdr, inst.sensor.lower() + "-" +
                                inst.noaa + "_" + self.sad)
            if len(self.name) == 3:
                file_name = "_".join((inst.inst, self.name, "RBD", "Ch*.sad"))
            else:
                file_name = "_".join((inst.platform.lower(), inst.sensor.lower(),
                                      self.name + ".nc"))

            # SAD files stored in subdirectories
            if glob(join(fdr_name, file_name)):
                return fdr_name

            # All files in one directory
            if glob(join(fdr, file_name)):
                return fdr

        raise FileMissing("Sad Files", str(sad_dirs))


# Using non-imager LUTs and Baum properties at Greg's recommendation
SETTINGS = {}
SETTINGS['WAT'] = ParticleType("WAT", sad="WAT")
SETTINGS['ICE'] = ParticleType("ICE", sad="ICE_baum")

tau = Invpar('ITau', ap=-1.0, sx=1.5)
SETTINGS['A70'] = ParticleType("A70", inv=(tau, Invpar('IRe', ap=0.0856, sx=0.15)))
SETTINGS['A71'] = ParticleType("A71", inv=(tau, Invpar('IRe', ap=-0.257, sx=0.15)))
SETTINGS['A72'] = ParticleType("A72", inv=(tau, Invpar('IRe', ap=-0.257, sx=0.15)))
SETTINGS['A73'] = ParticleType("A73", inv=(tau, Invpar('IRe', ap=-0.257, sx=0.15)))
SETTINGS['A74'] = ParticleType("A74", inv=(tau, Invpar('IRe', ap=-0.257, sx=0.15)))
SETTINGS['A75'] = ParticleType("A75", inv=(tau, Invpar('IRe', ap=-0.0419, sx=0.15)))
SETTINGS['A76'] = ParticleType("A76", inv=(tau, Invpar('IRe', ap=0.0856, sx=0.15)))
SETTINGS['A77'] = ParticleType("A77", inv=(tau, Invpar('IRe', ap=-0.0419, sx=0.15)))
SETTINGS['A78'] = ParticleType("A78", inv=(tau, Invpar('IRe', ap=-0.257, sx=0.15)))
SETTINGS['A79'] = ParticleType("A79", inv=(tau, Invpar('IRe', ap=-0.848, sx=0.15)))
SETTINGS['EYJ'] = ParticleType("EYJ", inv=(
    Invpar('ITau', ap=0.18, sx=1.5), Invpar('IRe', ap=0.7, sx=0.15)
))
