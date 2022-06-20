"""swath.py) Various routines for reading and using ORAC output data
# 10 Feb 2017, ACP: First version
# 17 Feb 2017, ACP: Adjusted Position.closest() to estimate the coords first.
# 24 Feb 2017, ACP: Completely different Position.closest(). It's still rubbish,
#    but doesn't randomly crash. In future, do something about dateline.
# 24 Mar 2017, ACP: Deal with netCDF returning both ndarray and MaskedArray.
# 06 Apr 2017, ACP: Moved Position() into its own file.
# 17 Apr 2017, ACP: Move management of compressed files into here.
"""

import warnings
import numpy as np

from pyorac.mappable import Mappable

# Values used for features in the aerosol cloud flag
CLDFLAG = {
    "cld": 1,
    "adjacent": 2,
    "stdev": 4,
    "openaot": 8,
    "openaer": 16,
    "openang": 32,
    "snow": 64,
}
QCFLAG = {
    "iter": 1,
    "cost": 2,
    "cld_iter": 1,
    "cld_cost": 2,
    "aer_iter": 4,
    "aer_cost": 8,
}

ORAC_TO_CCI_NAMING = {
    "aot550": "AOD550",
    "aot550_uncertainty": "AOD550_uncertainty",
    "aer": "REFF",
    "aer_uncertainty": "REFF_uncertainty",
    "aot870": "AOD870",
    "aot870_uncertainty": "AOD870_uncertainty",
    "lat": "latitude",
    "lon": "longitude",
    "qcflag": "quality_flag",
    "niter": "iterations",
}


class OracTime(object):
    """Wrapper class that allows arrays of ORAC timestamps to be converted into
    datetime objects using index notation."""

    def __init__(self, time):
        self._time = time

    def __getitem__(self, slic):
        """Converts an ORAC time into a datetime object."""
        from cftime import datetime

        orac_time = self._time[slic]

        # Corrupted data
        if orac_time < 1.:
            return np.nan

        # Some versions of ORAC buggered up the time calculation
        time = orac_time / 2. if orac_time > 4e6 else orac_time

        return datetime.fromordinal(time, "standard")


class Flux(Mappable):
    """Class for Orac radiative flux files. The variables names are quite
    descriptive:
       Xoa_YZ,
    where X = 't or 'b' for top and bottom of atmosphere.

    For photosynthetically active radiation, Y = 'par' and Z = '_dif' or
    'tot' for the diffuse fraction or total.

    For broadband fluxes, Y = AABB, where AA = 'lw' or 'sw' for longwave
    or shortwave radiation and BB = 'up' or 'dn' or upwelling or
    downwelling raditiation. If Z is empty, the all-sky flux is returned.
    Otherwise, Z = '_clr' or '_dif' for the clear-sky flux or the
    difference between all-sky and clear-sky (the radiative effect)."""

    def __init__(self, filename, central_longitude=0.):
        from netCDF4 import Dataset

        # Open file with netCDF interface
        self.bugsrad = filename
        try:
            self._bug = Dataset(filename)
        except OSError as err:
            # Produce more comprehensible error message
            err.args = ("Requested file unavailable: {}\n".format(filename),)
            raise

        # Put an upper limit on all fields
        self.limit = 0.95 * self._bug.variables["toa_swdn"][...]
        # 95% value from Matt Christensen on 15 May 2017

        # Metrics
        self.flag = self["retrflag"]

        # Coordinates
        self.time = OracTime(self["time"])
        super().__init__(self["lat"], self["lon"],
                         central_longitude=central_longitude)

    def close(self):
        """Clean up."""
        try:
            self._bug.close()
        except AttributeError:
            pass

    def __enter__(self):
        return self

    def __exit__(self, exc_type, exc_value, traceback):
        self.close()

    def variables(self):
        """List variables available in all input files."""
        var = set(self._bug.variables.keys())
        return sorted(var)

    def variable(self, name):
        """Return arbitrary data field, ensuring it's a MaskedArray.

        Values outside the range [0, 0.95*TOA_SWDN] are masked."""

        data = self._bug[name][...]
        if not isinstance(data, np.ma.MaskedArray):
            data = np.ma.masked_invalid(data)

        if "_" in name:
            data[data < 0.] = np.ma.masked
            data[data > self.limit] = np.ma.masked
        return data

    def __getitem__(self, name):
        """Returns an arbitrary data field.

        The suffix _del returns the difference between the all and clear-sky
        fields. The phrase alb returns the ratio between the up and downwelling
        versions of that name."""
        try:
            if name.endswith("_del"):
                allsky = name.replace("_del", "")
                clear = name.replace("_del", "_clr")
                dif = self.variable(allsky) - self.variable(clear)
                return dif
            elif "alb" in name:
                down = self.variable(name.replace("alb", "dn"))
                valid = down != 0.
                rat = self.variable(name.replace("alb", "up"))
                rat[valid] /= down[valid]
                rat[~valid] = np.ma.masked
                return rat

            return self.variable(name)
        except (KeyError, IndexError) as err:
            err.args = (name + " not present in input file.\n",)
            raise


class Swath(Mappable):
    """Class for reading and plotting the contents of ORAC files.

    Methods:
    __init__: Open a given file.
    try_open_file: Opens a netCDF file, decompressing first if necessary.
    close: Closes all open files. Called by __exit__.
    variables: List of all available variables.
    flag_map: Generates a dict to translate the values of flag fields.
    get_variable: Fetches a netCDF variable object.
    mask_cloud: Returns a bool array to mask cloud out of an aerosol field.
    mask_clear: Returns a bool array to mask clear sky out of a cloud field.

    Attributes:
    shape (int tuple): Dimensions of the swath.
    size (int): Number of data points.
    time (OracTime): Object that returns a datetime object when subscripted.
    cldmask (bool array): Combined-view cloud mask.
    cldflag (int array): Aerosol CCI cloud flagging.
    ang (float array): 550/870 Angstrom exponent of aerosol fields.
    ang_unc (float array): Uncertainty of Angstrom exponent.
    rs (float array): Surface reflectance.
    qcflag (int array): Flags retrievals with excecessive cost or iterations.
    """

    # -------------------------------------------------------------------
    # Initialisation
    # -------------------------------------------------------------------
    def __init__(self, filename, central_longitude=0., quick_open=False):
        """Open the given filename.

        Args:
        filename: Path to open.
        central_longitude: Centre of projection used to extrapolate the lat-lon
            grid. Ideally outside of the swath. Default 0.
        quick_open: When True, the initialisation only opens the ORAC files, with
            no reading. Many class methods will not function until _init1() is
            called as the lat-lon coords are not defined.
        """
        self.primary = filename
        self.secondary = filename.replace('primary', 'secondary')
        self.nc_files = {}
        self.tmp_files = {}

        try:
            self.try_open_file(self.primary, "pri")
        except OSError as err:
            err.args = ("Requested file unavailable: {}".format(filename),)
            raise
        try:
            self.try_open_file(self.secondary, "sec")
        except OSError:
            # Secondary file may not always exist
            pass

        if not quick_open:
            self._init1(central_longitude)

    def _init1(self, central_longitude):
        """Open retrieval metrics"""
        self.time = OracTime(self["time"])

        # Init out Mappable coordinate system
        super().__init__(self["lat"], self["lon"],
                         central_longitude=central_longitude)

    def _open_file(self, filename, label):
        """Checks if a file has been compressed before opening."""
        import gzip
        from netCDF4 import Dataset
        from tempfile import NamedTemporaryFile

        if filename.endswith(".gz"):
            # Decompress file
            tmp = NamedTemporaryFile()
            try:
                tmp.write(gzip.open(filename).read())
            except:
                tmp.close()
                raise
            ncdf = Dataset(tmp.name)
            self.nc_files[label] = ncdf
            self.tmp_files[label] = tmp
        else:
            ncdf = Dataset(filename)
            self.nc_files[label] = ncdf

    def try_open_file(self, filename, label):
        """If a file doesn't open, check it hasn't been compressed."""

        # If requested file doesn't exist, assume it's been compressed
        try:
            self._open_file(filename, label)
            return
        except OSError as err:
            pass
        try:
            self._open_file(filename, label + ".gz")
        except OSError as err:
            err.args = ("ORAC file unavailable: {}\n".format(filename),)
            raise

    # -------------------------------------------------------------------
    # Values derived from fields (that we only want to calculate once)
    # -------------------------------------------------------------------
    @property
    def cldmask(self):
        """Combined-view cloud mask"""
        if "_cldmask" not in self.__dict__:
            self.set_cldmask()
        return self._cldmask

    def set_cldmask(self):
        """Select appropriate cloud mask."""
        try:
            cldmask_tmp = self["cldmask"]
        except (KeyError, IndexError):
            try:
                cldmask_tmp = self["cloudmask_pre"]
            except (KeyError, IndexError):
                try:
                    cldmask_tmp = self["cldtype"]
                    cldmask_tmp[cldmask_tmp > 1] = 1
                except (KeyError, IndexError):
                    raise KeyError("Cannot find a cloud mask.")

        if len(cldmask_tmp.shape) == 3:
            self._cldmask = np.any(cldmask_tmp, 0)
        else:
            self._cldmask = cldmask_tmp

    @property
    def ang(self):
        """Angstrom exponent"""
        if "_ang" not in self.__dict__:
            self.set_ang()
        return self._ang

    def set_ang(self):
        """Build Angstrom exponent from AOT550 and AOT870."""
        with warnings.catch_warnings():
            warnings.simplefilter("ignore")
            fac = np.log(87. / 55.)
            self._ang = np.ma.log(self["aot550"] / self["aot870"]) / fac

    @property
    def ang_unc(self):
        """Angstrom exponent uncertainty"""
        if "_ang_unc" not in self.__dict__:
            self.set_ang_unc()
        return self._ang_unc

    def set_ang_unc(self):
        """Calculcate uncertainty in the Angstrom exponent."""
        with warnings.catch_warnings():
            warnings.simplefilter("ignore")
            fac = np.log(87. / 55.)
            self._ang_unc = np.ma.sqrt((self["aot550_uncertainty"] /
                                        self["aot550"]) ** 2 +
                                       (self["aot870_uncertainty"] /
                                        self["aot870"]) ** 2) / fac

    @property
    def rs(self):
        """Surface reflectance

        When working with 10km retrievals in a MergedSwath instance, it's
        necessary to use awkward indexing such as orac.rs[ch][1:2, 3:4].
        """
        if "_rs" not in self.__dict__:
            self.set_rs()
        return self._rs

    def set_rs(self):
        """Form channel-indexed array of surface reflectance."""
        from re import search

        # See if surface terms available
        calc_surface = False
        for k in self.variables():
            if k.startswith('swansea_s') or k.startswith('rho_DD'):
                calc_surface = True
                break

        if not calc_surface:
            # Find available surface reflectance channels
            chs = set()
            for k in self.variables():
                mat = search(r"surface_reflectance(\d+)", k)
                if mat:
                    chs.add(int(mat.group(1)))
            if not chs:
                raise TypeError("Surface reflectance not available in this file")

            self.ch = sorted(chs)
            self._rs = np.ma.row_stack([
                self["surface_reflectance{}".format(ch)] for ch in self.ch
            ])
            return

        with warnings.catch_warnings():
            warnings.simplefilter("ignore")

            # Identify available channels
            chs = set((int(k[k.rfind('_') + 1:])
                       for k in self.variables()
                       if 'in_channel_no' in k))
            self.ch = sorted(chs)

            # Make output array
            rs_shape = (len(self.ch),) + self.shape
            self._rs = np.ma.masked_all(rs_shape)

            # Copy over Oxford surface retrievals
            try:
                for out, ch in zip(self._rs, self.ch):
                    rho = self["rho_DD_in_channel_no_{}".format(ch)]
                    out[~np.ma.getmaskarray(rho)] = rho.compressed()
            except BaseException as err:
                if "not present" not in err.args[0]:
                    raise

            # Translate Swansea surface parameters into rho_DD
            try:
                sg = 0.3
                for out, ch in zip(self._rs, self.ch):
                    data_in = self["swansea_s_in_channel_no_{}".format(ch)]
                    ss = data_in.compressed()
                    out[~np.ma.getmaskarray(data_in)] = sg * ss / (1.0 - (1.0 - sg) * ss)
            except BaseException as err:
                if "not present" not in err.args[0]:
                    raise

    @property
    def cldflag(self):
        """Cloud flag for aerosol retrievals"""
        if "_cldflag" not in self.__dict__:
            self.set_cldflag(low_res=len(self.shape) != 2)
        return self._cldflag

    def set_cldflag(self, size=3, low_res=False, cld_limit=3):
        """GET's cloud clearing algorithm from ORAC 3.02."""
        from scipy.signal import convolve2d

        self._cldflag = np.zeros(self.shape, dtype='int')

        # Copy cloud mask
        self._cldflag[self.cldmask == 1] = CLDFLAG["cld"]

        # FMI filters (extends grid)
        with warnings.catch_warnings():
            warnings.simplefilter("ignore")

            # Cloud adjacency filter
            kernel = np.ones((size, size))
            cld_count = convolve2d(self.cldmask, kernel,
                                   mode='same', boundary='symm')
            self._cldflag[cld_count > cld_limit] += CLDFLAG["adjacent"]

            if low_res:
                return

            # AOT StdDev filter (StdDev = Sqrt(<X^2> - <X>^2))
            try:
                aot_tmp = self["aot550"].filled(0.)
                aot_cnt = ~self["aot550"].mask
            except AttributeError:
                # When netCDF decides to return a normal array
                aot_tmp = self["aot550"]
                aot_cnt = np.ones(self.shape)
            num = convolve2d(aot_cnt, kernel,
                             mode='same', boundary='symm')
            e_x = convolve2d(aot_tmp, kernel,
                             mode='same', boundary='symm') / num
            e_x2 = convolve2d(aot_tmp * aot_tmp, kernel,
                              mode='same', boundary='symm') / num
            stdev = np.ma.masked_invalid(np.sqrt(e_x2 - e_x * e_x))
            self._cldflag[stdev > 0.1] += CLDFLAG["stdev"]

        def opening_test(data, kern, limit):
            """Salt-and-pepper noise filters.

            Top Hat is the difference between the input and the opening of the
            image. The opening is erode followed by dilate."""
            from cv2 import morphologyEx, MORPH_TOPHAT

            # tmp = np.floor(255 * data)
            tmp = data.filled(0.)
            opening = morphologyEx(tmp, MORPH_TOPHAT, kern)
            test = opening >= limit
            return test

        kernel = np.ones((5, 5))
        self._cldflag[opening_test(self["aot550"], kernel, 80 / 255)] += CLDFLAG["openaot"]
        self._cldflag[opening_test(self["aer"], kernel, 300 / 255)] += CLDFLAG["openaer"]
        self._cldflag[opening_test(self.ang, kernel, 500 / 255)] += CLDFLAG["openang"]

        # Ice/snow filter
        try:
            snow = (self.rs[0, ...] - self.rs[3, ...]) >= 0.07
            self._cldflag[snow] += CLDFLAG["snow"]
        except IndexError:
            pass

    @property
    def qcflag(self):
        """Quality flag for all retrievals"""
        if "_qcflag" not in self.__dict__:
            self.set_qcflag()
        return self._qcflag

    def set_qcflag(self, iter_limit=25, cost_limit=3.):
        """Build quality control flag from input fields."""
        self._qcflag = np.zeros(self.shape)
        self._qcflag[self["niter"] >= iter_limit] += QCFLAG["iter"]
        self._qcflag[self["costjm"] >= cost_limit] += QCFLAG["cost"]

    def cdnc(self, how="quaas", **kwargs):
        """Cloud droplet number density for a semi-adiabatic cloud

        Kwds:
        how) Equation to use. Options are,
            quaas: Use the constant relationship of Quaas 2006
            gry: Use the temperature-dependent eqn of Gryspeerdt 2016
            acp: Use the temp and pressure-dependent eqn of ACPovey

        If how=acp,
        es_A,B,C,PA,PB: Coefficients of the saturated vapour pressure of
            moist air. Defaults taken from Eq. 21-22 of doi:
            10.1175/1520-0450(1996)035<0601:IMFAOS>2.0.CO;2
        L0,1: Specific enthalpy of vaporisation for water at 0 C and its
            gradient with temperature. Defaults taken from
            https://www.engineeringtoolbox.com/enthalpy-moist-air-d_683.html
        k: Ratio of volumetric and effective radii in cloud. For droplets
            conforming to a modified gamma distribution, this equals
            (1-v)(1-2v) where v is the effective variance of the distribution.
            Default value is for droplets with v=0.111 as used in MODIS
            and ORAC retrievals.
        Q_ext: Extinction coefficient of droplets. Default is for liquid
            droplets much smaller than the observed wavelength.
        f_ab: Adiabatic fraction of the cloud. Default value is from S2.3.3 of
            doi:10.1029/2017RG000593
        """
        if how == "quaas":
            return _cdnc_quaas(self["cot"], self["cer"]*1e-6)
        elif how == "gry" or how == "gryspeerdt":
            return _cdnc_gryspeerdt(self["cot"], self["cer"]*1e-6, self["ctt"])
        elif how == "acp":
            return _cdnc_acp(self["cot"], self["cer"]*1e-6, self["ctt"],
                             self["ctp"]*1e2, **kwargs)
        else:
            raise TypeError("how must be one of quaas, gry, or acp "
                            f"(given {how})")

    def cdnc_qcflag(self, phase="ann_phase", temp_min=268., solzen_max=65.,
                    satzen_max=41.4, cot_min=0., cer_min=None,
                    cer_max=None):
        """Quality flag for cdnc product, seeking homogeneous liquid cloud.

        If the keyword for a test is set to None, that test is skipped.

        Kwargs:
        phase: Name of variable to read for the cloud phase. Should be one
            of phase, ann_phase, or phase_pavolonis. Liquid cloud assumed == 1.
        temp_min: Minimum cloud top temperature of liquid cloud. Default 268.
        solzen_max: Maximum solar zenith angle. Default 65 from Gryspeerdt.
        satzen_max: Maximum satellite zenith angle. Default 41.4 from same.
        cot_min: Minimum cloud optical depth. Default 0 but 4 is good.
        cer_min: Minimum cloud effective radius. Default None but 4 is good.
        cer_max: Maximum cloud effective radius. Default None but 15 is good.
        """

        keep = ~np.ma.getmaskarray(self["cer"])
        # Non-liquid clouds
        keep &= np.all(self["cldtype"] == 3, axis=0)
        try:
            keep &= np.all(self[phase] == 1, axis=0)
        except TypeError:
            pass
        except KeyError:
            if phase:
                raise ValueError(
                    f"{phase} is no a valid variable name. Choose one of "
                    "phase, ann_phase, or phase_pavolonis."
                )
        if temp_min:
            keep &= self["ctt"] > temp_min
        # Multilayer clouds
        try:
            keep &= np.logical_not(self["cot2"] > 0.)
        except KeyError:
            pass
        # Ed's viewing requirements
        if solzen_max:
            keep &= self["solar_zenith_view_no1"] < solzen_max
        if satzen_max:
            keep &= self["satellite_zenith_view_no1"] < satzen_max
        # Cloud requirements
        if cot_min:
            keep &= self["cot"] > cot_min
        if cer_min:
            keep &= self["cer"] > cer_min
        if cer_max:
            keep &= self["cer"] < cer_max

        return np.logical_not(keep)

    def thickness(self, how="quaas", k=0.692, Q_ext=2., **kwargs):
        """Liquid cloud physical thickness

        Writing the CDNC equations is the easiest way to make it clear where
        they came from. This expression needs c_w and, though I could have
        written the CDNC functions in terms of c_w, I'll instead just work
        out H from CDNC here.
        """
        kwargs["how"] = how
        if how == "acp":
            kwargs["k"] = k
            kwargs["Q_ext"] = Q_ext
        cdnc = self.cdnc(**kwargs)
        return cdnc, 5. / (3 * np.pi * k * Q_ext) * self["cot"] / (self["cer"]*1e-6)**2 / cdnc

    # -------------------------------------------------------------------
    # Housekeeping functions
    # -------------------------------------------------------------------
    def __enter__(self):
        return self

    def __exit__(self, exc_type, exc_value, traceback):
        self.close()

    def __len__(self):
        return self.size

    def close(self):
        """Close all open files."""
        for handle in (h for lst in
                       (self.nc_files.values(), self.tmp_files.values())
                       for h in lst):
            try:
                handle.close()
            except (AttributeError, RuntimeError):
                pass

    def variables(self):
        """List variables available in all input files."""
        names = set()
        for handle in self.nc_files.values():
            names.update(handle.variables.keys())
        return sorted(names)

    def flag_map(self, name):
        """Return a dict mapping a flag's values to their meanings."""
        from collections import OrderedDict

        var = self.get_variable(name)
        try:
            vals = var.flag_values.split(" ")
        except AttributeError:
            try:
                # Aerosol CCI files use a different name for some reason
                vals = var.flag_masks.split(" ")
            except AttributeError as err:
                err.args = (name + " does not have flag definitions.",)
                raise

        # Remove "b" from end of value terms
        nums = [int(i[:-1]) for i in vals if i]

        if ":" in var.flag_meanings:
            try:
                keys = (var.flag_meanings + " ").split(": ")
            except AttributeError as err:
                err.args = (name + " does not have flag definitions.",)
                raise
            names = [k[:k.rfind(" ")] for k in keys[1:]]
        else:
            try:
                names = var.flag_meanings.split(" ")
            except AttributeError as err:
                err.args = (name + " does not have flag definitions.",)
                raise

        return OrderedDict(zip(nums, names))

    # -------------------------------------------------------------------
    # Data formatting functions
    # -------------------------------------------------------------------
    def get_variable(self, name):
        """Returns the netCDF variable requested, using CCI names if needed."""
        for handle in self.nc_files.values():
            try:
                return handle[name]
            except (KeyError, IndexError):
                pass

        for handle in self.nc_files.values():
            try:
                return handle[ORAC_TO_CCI_NAMING[name]]
            except (KeyError, IndexError):
                pass

        raise KeyError(name + " not present in inputs.")

    def __getitem__(self, name):
        """Returns an arbitrary data field, ensuring it's a MaskedArray."""
        data = self.get_variable(name)[...]
        if isinstance(data, np.ma.MaskedArray):
            return data

        return np.ma.masked_invalid(data)

    # -------------------------------------------------------------------
    # Masking functions
    # -------------------------------------------------------------------
    def mask_cloud(self, allowing=None):
        """Bool array that is True where the aerosol cloud flag is non-zero.

        The allowing argument can be used to exempt a list of members of the
        orac.CLDFLAG bitmask."""
        if allowing is None:
            allowing = []
        elif type(allowing) is str:
            allowing = [allowing]
        flags = sum((v for k, v in CLDFLAG.items() if k not in allowing))
        cld_mask = np.bitwise_and(self.cldflag, flags) > 0
        qc_mask = self.qcflag != 0
        return np.logical_or(cld_mask, qc_mask)

    def mask_clear(self):
        """Bool array that is True where the cloud mask is not 1."""
        cld_mask = self.cldmask != 1
        qc_mask = self.qcflag != 0
        return np.logical_or(cld_mask, qc_mask)


class MergedSwath(Swath):
    """Emulates the Swath class for a set of ORAC files.

    10km aerosol files can be combined with 1km cloud data using Matt's
    collocation data.
    """

    # -------------------------------------------------------------------
    # Initialisation
    # -------------------------------------------------------------------
    def __init__(self, *files, central_longitude=0.):

        self.nc_files = {}
        self.tmp_files = {}

        self.collocation = False
        for this_file in files:
            if len(this_file) == 2:
                self.try_open_file(this_file[1], this_file[0])

            elif "collocation" in this_file:
                self.collocation = True

                # Low-res aerosol. Use collocation file to map onto 1km
                # Indices to map between 10 and 1km grids
                self.try_open_file(this_file, "col")
                self.indices = self.nc_files["col"]["aerosol_10km_index"][...]

            elif "bugsrad" in this_file or "RAD_PRODUCTS" in this_file:
                self.try_open_file(this_file, "flx")

            elif "AEROSOL-AER" in this_file:
                self.try_open_file(this_file, "aer")

            elif "CLOUD-CLD" in this_file:
                self.try_open_file(this_file, "cld")

            else:
                raise ValueError("File type not recognised: " + this_file)

        # Get coordinates
        self._init1(central_longitude)

        # Check other coordinates match the cloud
        try:
            try:
                assert np.all(self.lat == self.nc_files["flx"]["lat"][...])
            except KeyError:
                pass
            try:
                if not self.collocation:
                    assert np.all(self.lat == self.nc_files["aer"]["lat"][...])
            except KeyError:
                pass
        except AssertionError:
            raise ValueError("Input files have inconsistent grids.")

    # -------------------------------------------------------------------
    # Housekeeping functions
    # -------------------------------------------------------------------
    def get_variable(self, slices, retsource=False):
        # As there are multiple files, allow slice to select one.
        if isinstance(slices, tuple):
            name = slices[0]
            keys = (slices[1],)
        else:
            name = slices
            keys = self.nc_files.keys()

        # Try each file in turn to find this field
        for key in keys:
            if key == "aer" and self.collocation:
                # The ORAC field names aren't CCI standard so some
                # translation may be necessary.
                try:
                    name2 = ORAC_TO_CCI_NAMING[name]
                except KeyError:
                    name2 = name

                try:
                    if retsource:
                        return self.nc_files[key][name2], key

                    return self.nc_files[key][name2]
                except (KeyError, IndexError):
                    pass
            else:
                try:
                    if retsource:
                        return self.nc_files[key][name], key

                    return self.nc_files[key][name]
                except (KeyError, IndexError):
                    pass

        raise KeyError(name + " not present in input files.")

    # -------------------------------------------------------------------
    # Values derived from fields (that we only want to calculate once)
    # -------------------------------------------------------------------
    @property
    def cldflag(self):
        """Cloud flag for aerosol retrievals"""
        if "_cldflag" not in self.__dict__:
            self.set_cldflag(low_res=self.collocation)
        return self._cldflag

    def set_cldmask(self):
        try:
            cldmask_tmp = self["cldmask", "cld"]
        except (KeyError, IndexError):
            try:
                cldmask_tmp = self["cloudmask_pre", "cld"]
            except (KeyError, IndexError):
                try:
                    cldmask_tmp = self["cldtype"]
                    cldmask_tmp[cldmask_tmp > 1] = 1
                except (KeyError, IndexError):
                    raise KeyError("Cannot find a cloud mask.")

        if len(cldmask_tmp.shape) == 3:
            self._cldmask = np.any(cldmask_tmp, 0)
        else:
            self._cldmask = cldmask_tmp

    def set_qcflag(self, iter_limit=25, cld_cost_limit=10., aer_cost_limit=3.):
        self._qcflag = np.zeros(self.shape, dtype=int)
        self._qcflag[self["niter", "cld"] >= iter_limit] += \
            QCFLAG["cld_iter"]
        self._qcflag[self["costjm", "cld"] >= cld_cost_limit] += \
            QCFLAG["cld_cost"]

        if not self.collocation:
            self._qcflag[self["niter", "aer"] >= iter_limit] += \
                QCFLAG["aer_iter"]
            self._qcflag[self["costjm", "aer"] >= aer_cost_limit] += \
                QCFLAG["aer_cost"]

    def expand_field(self, array, slices=(slice(None), slice(None))):
        """Convert a 1D 10km field into a 2D 1km field.

        Exceedingly slow if called without slice."""
        if len(array.shape) > 1:
            raise ValueError("expand_field only acts on 1D arrays.")

        # Define the output array
        xlen = len(range(*slices[0].indices(self.shape[0])))
        ylen = len(range(*slices[1].indices(self.shape[1])))
        data = np.ma.empty((xlen, ylen))

        # Copy initial data one row at a time
        for i, indices in enumerate(self.indices[slices]):
            try:
                tmp = array[indices]
                tmp[indices < 0] = np.ma.masked
            except IndexError:
                tmp = np.ma.masked
            data[i, :] = tmp

        return data


# -------------------------------------------------------------------
# CALCULATIONS OF CLOUD DROPLET NUMBER CONCENTRATION
# -------------------------------------------------------------------
def _cdnc_quaas(tau, r_e):
    """Cloud droplet number density for a semi-adiabatic cloud

    Uses Eq. 1 of doi:10.5194/acp-6-947-2006

    Args:
    tau: Cloud optical thickness.
    r_e: Cloud effective radius, in m.
    """
    return 1.37e-5 * np.sqrt(tau / r_e**5)


def _cdnc_gryspeerdt(tau, r_e, T):
    """Cloud droplet number density for a semi-adiabatic cloud

    Uses Eq. 2 of doi:10.1002/2015JD023744

    Args:
    tau: Cloud optical thickness.
    r_e: Cloud effective radius, in m.
    T: Representative temperature of cloud, in K.
    """
    return 1.37e-5 * (0.0192 * T - 4.293) * np.sqrt(tau / r_e**5)


def _cdnc_acp(tau, r_e, T, p,
              es_A=610.94, es_B=17.625, es_C=243.04, es_PA=1.00071, es_PB=4.5e-8,
              L0=2.501e6, L1=1.86e3, k=0.692, Q_ext=2., f_ab=0.66):
    """Cloud droplet number density for a semi-adiabatic cloud

    Uses A.C. Povey's derivation for variable lapse rate and including the
    moist-air-correction to the saturation vapour pressure.

    Args:
    tau: Cloud optical thickness.
    r_e: Cloud effective radius, in m.
    T: Representative temperature of cloud, in K.
    p: Representative pressure of cloud, in Pa.

    Kwds:
    es_A,B,C,PA,PB: Coefficients of the saturated vapour pressure of
        moist air. Defaults taken from Eq. 21-22 of doi:
        10.1175/1520-0450(1996)035<0601:IMFAOS>2.0.CO;2
    L0,1: Specific enthalpy of vaporisation for water at 0 C and its
        gradient with temperature. Defaults taken from
        https://www.engineeringtoolbox.com/enthalpy-moist-air-d_683.html
    k: Ratio of volumetric and effective radii in cloud. For droplets
        conforming to a modified gamma distribution, this equals
        (1-v)(1-2v) where v is the effective variance of the distribution.
        Default value is for droplets with v=0.111 as used in MODIS
        and ORAC retrievals.
    Q_ext: Extinction coefficient of droplets. Default is for liquid
        droplets much smaller than the observed wavelength.
    f_ab: Adiabatic fraction of the cloud. Default value is from S2.3.3 of
        doi:10.1029/2017RG000593
    """
    # Fundamental constants
    epsilon = 0.622
    R = 8.314
    M_a = 28.96e-3
    g = 9.807
    c_p = 1.004e3
    rho_w = 997.
    R_a = R / M_a

    celsius = T - 273.15
    # Specific latent heat of vaporisation
    L = L0 + L1 * celsius
    # Magnus equation for saturated vapour pressure
    e_s = es_A * np.exp(es_B * celsius / (celsius + es_C))
    # Correction from vapour to moist air
    e_s *= es_PA * np.exp(es_PB * p)
    # Partial pressure of dry air
    p_res = p - e_s

    alpha_fac = 5. * f_ab * g * epsilon
    alpha_fac /= 4. * (np.pi * k)**2 * Q_ext * rho_w * R_a

    denom0 = epsilon * L * p - c_p * p_res * T
    denom1 = R_a * c_p * (p_res * T)**2 + (epsilon * L)**2 * e_s * p
    alpha = alpha_fac * e_s * p_res * denom0 / (T * denom1)

    return np.sqrt(alpha * tau / r_e**5)
