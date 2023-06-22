"""ORAC Forward Model

An attempt to replicate the calculations of the ORAC forward model
within Python code. Arguably, this should be an f2py interface that
calls the actual library routines. Instead, it's a sloppy copy of the
essence of the code. That could have some use for code testing and
validation, particularly if we strap in more accurate methods or inputs.
For the time being, it's just a way to play around with a pixel.

Classes:
PreprocessorFiles
    A simple container for the preprocessor files that allows dict-like
    key referencing for variables without specifying a file suffix.
Spixel
    Container for all of the inputs required by ORAC including
    atmospheric profiles, a priori surface profiles, viewing geometry,
    measured radiances, and RTTOV outputs.
OracForwardModel
    Abstract class for ORAC forward model
SolarForwardModel
    Abstract class for ORAC's solar forward model
SolarBrdfBase
    Abstract class with Calculations for Ctrl%use_full_brdf
SolarBrdfEq1,2,3,4
    The four variations of SolarBrdfBase corresponding to
    Ctrl%i_equation_form
SolarLamberian
    Forward model that ignores the BRDF terms using a Lambertian surface
ThermalForwardModel
    Base class for ORAC's thermal forward model
ThermalUpperLayer
    Model for an upper layer of cloud in the thermal

Notes:

To Do:
- Write f2py interface with compiled Fortran code.
- Add adjoint calculations.
"""
import numpy as np
import os.path

from abc import ABC, abstractmethod
from netCDF4 import Dataset
from scipy.interpolate import RegularGridInterpolator


RHO_NAMES = ("rho_0v", "rho_dv", "rho_0d", "rho_dd")

class PreprocessorFiles(object):
    """Container to fetch fields from a set of preprocessor files."""
    SUFFIXES = ["config", "alb", "clf", "geo", "loc", "lsf", "msi", "prtm",
                "lwrtm", "swrtm"]

    def __init__(self, path, **kwargs):
        """Ensure all 10 preprocessor files are available."""
        from pyorac.definitions import FileName, OracError

        super(PreprocessorFiles, self).__init__()

        fdr, name = os.path.split(path)
        try:
            # See if the path provided conforms to an expected format
            name_ob = FileName(fdr, name)
            root = name_ob.root_name(**kwargs)
        except (OracError, ValueError):
            # Then the path provided should be a preprocessor filename
            if name.endswith(".nc"):
                try:
                    root, _, _ = name.split(".")
                except ValueError:
                    raise ValueError(f"Unknown filename format: {name}")
            else:
                root = name

        for suff in self.SUFFIXES:
            part_name = os.path.join(fdr, ".".join((root, suff, "nc")))
            if not os.path.isfile(part_name):
                raise ValueError(f"Could not locate file {part_name}")

        self.root = os.path.join(fdr, root)

    def __getitem__(self, slices):
        """Read a field from one of the preprocessor files."""

        if isinstance(slices, str):
            key = slices
            sl = Ellipsis
        elif isinstance(slices, (tuple, list)):
            try:
                key = slices[0]
            except IndexError as err:
                raise KeyError("Must provide at least field name") from err
            try:
                sl = slices[1:]
            except IndexError:
                sl = Ellipsis
        else:
            raise KeyError("Key must be string or tuple starting with string")
        try:
            suff, name = key.split("/")
            with self.dataset(suff) as fobj:
                return fobj[name][sl]
        except IndexError:
            pass
        except ValueError:
            # No file given, so try all
            for suff in self.SUFFIXES:
                try:
                    return self[suff + "/" + key, sl]
                except (KeyError, IndexError):
                    continue

        raise KeyError(f"Cannot locate field {key} in files {self.root}*nc")

    def __call__(self, x, y, **kwargs):
        """Returns an SPixel for a specific pixel.

        I used [] indexing for variable access to be consistent with
        netCDF4.Dataset and pyorac.Swath so this gets ()."""
        return SPixel.from_preproc(self, x, y, **kwargs)

    def attribute(self, key):
        with self.dataset(self.SUFFIXES[0]) as fobj:
            return fobj.__dict__[key]

    def dataset(self, suff):
        return Dataset(".".join((self.root, suff, "nc")))

    @property
    def nchannels(self):
        with self.dataset(self.SUFFIXES[0]) as fobj:
            return fobj.dimensions["nc_conf"].size
    @property
    def nsolar(self):
        with self.dataset(self.SUFFIXES[0]) as fobj:
            return fobj.dimensions["nc_alb"].size
    @property
    def nthermal(self):
        with self.dataset(self.SUFFIXES[0]) as fobj:
            return fobj.dimensions["nc_emis"].size
    @property
    def nlat(self):
        with self.dataset(self.SUFFIXES[0]) as fobj:
            return fobj.dimensions["nlat_conf"].size
    @property
    def nlon(self):
        with self.dataset(self.SUFFIXES[0]) as fobj:
            return fobj.dimensions["nlon_conf"].size
    @property
    def nlevels(self):
        with self.dataset(self.SUFFIXES[0]) as fobj:
            return fobj.dimensions["nlevels_conf"].size
    @property
    def nx(self):
        with self.dataset(self.SUFFIXES[0]) as fobj:
            return fobj.dimensions["nx_conf"].size
    @property
    def ny(self):
        with self.dataset(self.SUFFIXES[0]) as fobj:
            return fobj.dimensions["ny_conf"].size
    @property
    def nacross(self):
        return self.nx
    @property
    def nalong(self):
        return self.ny
    @property
    def channel_ids(self):
        with self.dataset(self.SUFFIXES[0]) as fobj:
            return fobj["msi_instr_ch_numbers"][:]
    @property
    def solar(self):
        with self.dataset(self.SUFFIXES[0]) as fobj:
            return fobj["msi_ch_swflag"][:] == 1
    @property
    def thermal(self):
        with self.dataset(self.SUFFIXES[0]) as fobj:
            return fobj["msi_ch_lwflag"][:] == 1
    @property
    def view(self):
        with self.dataset(self.SUFFIXES[0]) as fobj:
            return fobj["msi_ch_view"][:] - 1

    @property
    def date(self):
        from dateutil.parser import parse
        return parse(self.attribute("Date_Created"))

    def spixels(self, **kwargs):
        """Iterate SPixel instances over this field"""
        for x in range(self.nalong):
            for y in range(self.nacross):
                yield SPixel.from_preproc(self, x, y, **kwargs)


class SPixel(object):
    def __init__(self, *args):
        """Fundamental constructor that isn't intended for use"""
        self.time, self.lat, self.lon = args[:3]
        (self.solzen, self.satzen, self.relazi, self.land,
         self.channels, self.solar, self.thermal, self.ym, self.sy, self.rs, self.rho_0v,
         self.rho_0d, self.rho_dv, self.rho_dd, self.pressure, self.temperature,
         self.geopotential, self.emissivity, self.tac_lw, self.tbc_lw, self.rac_up,
         self.rac_down, self.rbc_up, self.tac_sw, self.tbc_sw) = map(np.asarray, args[3:])

    @classmethod
    def from_preproc(cls, preproc, x, y, spixel_y_to_ctrl_y_index=None):
        """Given swath coordinates, extracts that pixel from ORAC preproc

        Largely follows src/get_spixel.F90 and its subroutines, such that
        x and y are the Fortran way around. Skips uncertainty calculations.
        """
        from cftime import datetime
        from pyorac.util import bilinear_coefficients

        time = datetime.fromordinal(preproc["msi/time_data"][y,x], "standard")
        lat = preproc["loc/lat",y,x]
        lon = preproc["loc/lon",y,x]
        solzen = preproc["geo/solzen",preproc.view,y,x]
        satzen = preproc["geo/satzen",preproc.view,y,x]
        relazi = preproc["geo/relazi",preproc.view,y,x]
        land = preproc["lsf/lsflag",y,x] == 1
        ym = preproc["msi/msi_data",:,y,x]
        sy = preproc["msi/sd_data",:,y,x]
        rs = preproc["alb/alb_data",:,y,x]
        rho_0v = preproc["alb/rho_0v_data",:,y,x]
        rho_0d = preproc["alb/rho_0d_data",:,y,x]
        rho_dv = preproc["alb/rho_dv_data",:,y,x]
        rho_dd = preproc["alb/rho_dd_data",:,y,x]

        # Determine interpolation coefficients for RTM fields
        try:
            mask = np.ma.getmaskarray(preproc["lwrtm/tac_lw"])
        except KeyError:
            mask = np.ma.getmaskarray(preproc["swrtm/tac_sw"])
        i0, i1, j0, j1, coef = bilinear_coefficients(
            preproc["prtm/lon_rtm"], lon,
            preproc["prtm/lat_rtm"], lat, mask
        )
        # Do this rather than return an interpolator to save opening the whole field
        def interp(label):
            return (coef[0] * preproc[label,i0,j0] +
                    coef[1] * preproc[label,i0,j1] +
                    coef[2] * preproc[label,i1,j0] +
                    coef[3] * preproc[label,i1,j1])

        pre = interp("prtm/pprofile_rtm")
        temp = interp("prtm/tprofile_rtm")
        geo = interp("prtm/hprofile_rtm")
        try:
            emis = interp("lwrtm/emiss_lw")
            tac_lw = interp("lwrtm/tac_lw")
            tbc_lw = interp("lwrtm/tbc_lw")
            rac_up = interp("lwrtm/rac_up_lw")
            rac_down = interp("lwrtm/rac_down_lw")
            rbc_up = interp("lwrtm/rbc_up_lw")
        except KeyError:
            emis, tac_lw, tbc_lw, rac_up, rac_down, rbc_up = [None] * 6
        try:
            tac_sw = interp("swrtm/tac_sw")
            tbc_sw = interp("swrtm/tbc_sw")
        except KeyError:
            tac_sw, tbc_sw = [None] * 2

        return cls(
            time, lat, lon, solzen, satzen, relazi, land, preproc.channel_ids,
            preproc.solar, preproc.thermal,
            ym, sy, rs, rho_0v, rho_0d, rho_dv, rho_dd, pre, temp, geo,
            emis, tac_lw, tbc_lw, rac_up, rac_down, rbc_up, tac_sw, tbc_sw
        )

    @staticmethod
    def from_rttov(self, rttov_profile, rttov_outputs):
        raise NotImplementedError

    def subset(self, new_channels):
        ch = np.asarray(new_channels)
        if not np.issubdtype(ch.dtype, bool):
            ch = np.array([i in new_channels for i in self.channels])
            if ch.sum() != len(new_channels):
                raise ValueError("Requested unavailable channels: "+
                                 str(set(new_channels) - set(self.channels)))
        elif ch.size != self.channels.size:
            raise ValueError(f"Bool inputs must be length {self.channels.size}")

        vis = ch[self.solar]
        ir = ch[self.thermal]
        return SPixel(
            self.time, self.lat, self.lon, self.solzen[ch], self.satzen[ch], self.relazi[ch],
            self.land, self.channels[ch], self.solar[ch], self.thermal[ch],
            self.ym[ch], self.sy[ch], self.rs[vis], self.rho_0v[vis], self.rho_0d[vis],
            self.rho_dv[vis], self.rho_dd[vis], self.pressure, self.temperature, self.geopotential,
            self.emissivity[ir], self.tac_lw[:,ir], self.tbc_lw[:,ir], self.rac_up[:,ir],
            self.rac_down[:,ir], self.rbc_up[:,ir], self.tac_sw[:,vis], self.tbc_sw[:,vis]
        )

    def no_rtm(self):
        vis = (self.pressure.size, self.solar.sum())
        ir = (self.pressure.size, self.thermal.sum())
        # Fields needed by the thermal model are nan so errors to ensure errors
        # are thrown down the line if you use this in error
        return SPixel(
            self.time, self.lat, self.lon, self.solzen, self.satzen, self.relazi,
            self.land, self.channels, vis, ir, self.ym, self.sy,
            self.rs, self.rho_0v, self.rho_0d,
            self.rho_dv, self.rho_dd, self.pressure, self.temperature, self.geopotential,
            np.nan, np.ones(ir), np.ones(ir), np.nan,
            np.nan, np.nan, np.ones(vis), np.ones(vis)
        )

    def correct_temperature(self, max_tropopause=500., min_tropopause=30., depth=2):
        """Implementation of src/int_ctp.F90

        There is certainly a more Pythonic way of doing this and I
        am in no mood to find it.
        """
        temp = self.temperature.copy()

        # Estimate vertical height with the geopotential divided by gravity
        height = 0.001 / 9.80665 * self.geopotential

        # Skip the surface and start high enough to allow extrapolation
        k = len(temp) - 3
        # Ignore surface inversions (as there isn't an obvious way to remove them)
        while temp[k-1] > temp[k] and k > 0:
            k -= 1

        # Search for inversions within the free troposphere
        while self.pressure[k] > max_tropopause and k > 0:
            # An inversion is a level with temp lower than that above it
            if temp[k-1] > temp[k]:
                # Due to atmospheric transmission, the 11um BT will be an
                # underestimate of the actual CTT. We compensate using the
                # EUMETSAT procedure to extrapolate the lapse rate beneath
                # the inversion through to two levels above the inversion

                # Find top of the inversion
                l = k-2
                while temp[l] < temp[l-1] and l > 0:
                    l -= 1
                l -= depth

                # Extrapolate lapse rate from the two levels beneath
                gradient = ((temp[k+2] - temp[k+1]) /
                            (self.pressure[k+2] - self.pressure[k+1]))
                temp[l:k] = (temp[k+1] + gradient *
                             (self.pressure[l:k] - self.pressure[k+1]))

                # Continue from above that
                k = l-1
            else:
                k -= 1

        # Locate the tropopause
        while self.pressure[k] > min_tropopause and k > 0:
            # Find the lowest level with a lapse rate less than 2 K/km
            if ((temp[k] - temp[k-1]) / (height[k-1] - height[k])) < 2.:
                # Find first level a least 2 km above that
                l = k-1
                while height[l] - height[k] < 2. and l > 0:
                    l -= 1

                # Also require lapse rate remain this low in the 2km above
                if ((temp[k] - temp[l]) / (height[l] - height[k])) < 2.:
                    break
            k -= 1

        # Extrapolate top of troposphere over stratosphere
        gradient = ((temp[k+2] - temp[k+1]) /
                    (self.pressure[k+2] - self.pressure[k+1]))
        temp[:k] = (temp[k+1] + gradient *
                    (self.pressure[:k] - self.pressure[k+1]))

        return temp

    @property
    def nch(self):
        return len(self.channels)

    @property
    def tsf_lw(self):
        """Longwave transmission to surface"""
        return self.tac_lw[-1]

    @property
    def r_clear_lw(self):
        return self.rbc_up[0]

    @property
    def tsf_sw(self):
        """Shortwave transmission to surface"""
        tsf = self.tac_sw[-1]
        # 1 / sec(viewing zenith) for mixed channels
        all_mixed = np.logical_and(self.solar, self.thermal)
        cos_v = np.cos(np.radians(self.satzen[all_mixed]))
        # Thermal tranmission to surface for mixed channels
        ir_mixed = self.solar[self.thermal]
        tsf_mixed = self.tsf_lw[ir_mixed]
        # Override visible transmission for mixed channels
        vis_mixed = self.thermal[self.solar]
        tsf[vis_mixed] = tsf_mixed ** cos_v
        return tsf

    def interp_in_pressure(self, pressure, *args):
        from pyorac.util import bound_grid

        i0, i1, delta = bound_grid(self.pressure, pressure)
        for name in args:
            value = getattr(self, name)
            if not isinstance(value, np.ndarray):
                value = value()
            yield value[i0] * (1.-delta) + value[i1] * delta

    def interp_in_logpressure(self, pressure, *args):
        from pyorac.util import bound_grid

        i0, i1, delta = bound_grid(np.log(self.pressure), np.log(pressure))
        for name in args:
            value = getattr(self, name)
            if not isinstance(value, np.ndarray):
                value = value()
            yield value[i0] * (1.-delta) + value[i1] * delta

    def orac_forward_model(self, lut, **kwargs):
        solar = SolarBrdfEq3(self.subset(self.solar), lut, **kwargs)
        thermal = ThermalForwardModel(self.subset(self.thermal), lut, **kwargs)
        return solar, thermal

    def iter_channels(self):
        """Iterate over wavelength-dependendent quantities"""
        j, k = -1, -1
        for i in range(self.nch):
            out = dict(channel=self.channels[i], index=i, solar=self.solar[i],
                       thermal=self.thermal[i], meas=self.ym[i], sy=self.sy[i])
            if self.solar[i]:
                j += 1
                out["solar_index"] = j
                for key in RHO_NAMES + ("rs", "tac_sw", "tbc_sw"):
                    out[key] = getattr(self, key)[...,j]
            if self.thermal[i]:
                k += 1
                out["thermal_index"] = k
                for key in ("emissivity","tac_lw", "tbc_lw", "rac_up",
                            "rac_down", "rbc_up"):
                    out[key] = getattr(self, key)[...,k]

            yield out


class OracForwardModel(ABC):
    """Abstract class for implementation of the ORAC forward model

    This is intended to be legible, not efficient.
    """
    description = "Abstract"

    def __init__(self, spixel, lut, **kwargs):
        """Setup all of the terms; unspecified state variables are given prior from LUT"""
        self.pixel = spixel
        self.nch = self.pixel.channels.size

        # Set priors
        prior = dict(coverage=1., surface_temperature=spixel.temperature[-1],
                     top_pressure=900., optical_depth=None,
                     effective_radius=None, lut=lut)
        if lut._particle is not None:
            # Fetch priors based on LUT type
            if lut._particle.name in ("WAT", "liquid-water"):
                prior["optical_depth"] = 10.**0.8
                prior["effective_radius"] = 12.
            elif lut._particle.name in ("ICE", "water-ice"):
                prior["top_pressure"] = 400.
                prior["optical_depth"] = 10.**0.8
                prior["effective_radius"] = 30.
            else:
                # Aerosols have type-specific priors
                for parameter in lut._particle.inv:
                    if parameter.var == 'ITau':
                        prior["optical_depth"] = 10.**paramter.ap
                    elif parameter.var == 'IRe':
                        prior["effective_radius"] = parameter.ap

        self.set_state(**(prior | kwargs))

    def set_state(self, **kwargs):
        """Change the value of one or more state vector elements"""

        if upd := {k:v for k,v in kwargs.items() if k in ("lut", "optical_depth", "effective_radius", "t_dv_from_t_0d")}:
            self.__dict__.update(upd)
            self._set_nuclei_terms()

        if upd := {k:v for k,v in kwargs.items() if k in ("coverage")}:
            self.__dict__.update(upd)

    def _set_nuclei_terms(self, use_these):
        """Interpolate all look-up tables to given tau and r_eff"""

        # Work out the SPixel -> LUT indexing
        self.lut_ind = [np.argmax(self.lut.channels[use_these] == ch)
                        for ch in self.pixel.channels]
        assert len(np.unique(self.lut_ind)) == len(self.lut_ind)

        # Only pass this keyword if we actively asked for it
        try:
            kwargs = dict(t_dv_from_t_0d=self.t_dv_from_t_0d)
        except AttributeError:
            kwargs = dict()

        # This will silently allow unavailable channels through
        if self.optical_depth is None and self.effective_radius is None:
            self.__dict__.update(self.lut.state_space(
                self.pixel.channels, self.pixel.satzen, self.pixel.solzen,
                self.pixel.relazi, **kwargs
            ))
        else:
            self.__dict__.update(self.lut(
                self.pixel.channels, self.pixel.satzen, self.pixel.solzen,
                self.pixel.relazi, self.optical_depth, self.effective_radius, **kwargs
            ))

    @abstractmethod
    def _set_top_pressure(self):
        """Interpolate the transmission/reflectance profiles to the layer pressure"""

    def _total_r(self):
        """Modelled R at top-of-atmosphere"""
        return (self.coverage * self._overcast_r() +
                (1. - self.coverage) * self._clear_r())

    @abstractmethod
    def _overcast_r(self):
        """Modelled reflectance due to particulates at top-of-atmosphere"""

    @abstractmethod
    def _clear_r(self):
        """Modelled reflectance at top of a clean atmosphere"""

    def __str__(self):
        return (f"{self.description} forward model\n"
                f"{self.pixel.channels:}")


class SolarForwardModel(OracForwardModel):
    """Abstract class for implementation of the solar ORAC forward model

    This is intended to be legible, not efficient.
    Adjoint not yet implemented as you shouldn't use this in optimisation.
    """
    DIFFUSE_TRANSMISSION_FACTOR = 1.0

    def __init__(self, spixel, lut, **kwargs):
        if np.any(np.logical_not(spixel.solar)):
            raise ValueError("All channels must be solar; use Spixel.subset()")

        # Secant of the solar zenith
        self.sec_0 = 1. / np.cos(np.radians(spixel.solzen))
        # Secant of the satellite zenith
        self.sec_v = 1. / np.cos(np.radians(spixel.satzen))
        # Transmission from the surface
        self.tsf = spixel.tsf_sw
        # Use provided surface as default
        self.rho_0v = spixel.rho_0v
        self.rho_0d = spixel.rho_0d
        self.rho_dv = spixel.rho_dv
        self.rho_dd = spixel.rho_dd

        if "ss" in kwargs and "sg" not in kwargs:
            # Prior Swansea gamma parameter
            kwargs["sg"] = 0.3

        super().__init__(spixel, lut, **kwargs)

    def set_state(self, **kwargs):
        super().set_state(**kwargs)

        if upd := {k:v for k,v in kwargs.items() if k in ("lut", "top_pressure")}:
            self.__dict__.update(upd)
            self._set_top_pressure()

        if upd := {k:v for k,v in kwargs.items() if k in RHO_NAMES}:
            self._set_oxford_surface(upd)

        if upd := {k:v for k,v in kwargs.items() if k in ("ss", "sp", "sg")}:
            if ("ss" in upd) != ("sp" in upd):
                raise ValueError("Must provide both S and P coefficients for Swansea surface")
            if upd["ss"].size != self.nch or upd["sp"].size != self.nch:
                # Identify views from values of sec_v, small being near nadir
                view_values = np.unique(self.sec_v).sort()
                view = [np.argmax(view_values == val) for val in self.sec_v]
                # Guess the wavelength ordering; this can be future me's problem
                ch = np.zeros(self.nch)
                for i in view_values:
                    filt = view == i
                    ch[filt] = np.arange(filt.sum())

                upd["ss"] = upd["ss"][ch]
                upd["sp"] = upd["sp"][view]

            self.__dict__.update(upd)
            self._init_swansea_surface()

    def _set_nuclei_terms(self):
        super()._set_nuclei_terms(self.lut.solar)

    def _set_top_pressure(self):
        """Interpolate the clear-sky radiative transfer"""
        self.tac, self.tbc = self.pixel.interp_in_pressure(
            self.top_pressure, "tac_sw", "tbc_sw"
        )

    def _set_swansea_surface(self):
        """Swansea model of surface reflectance"""
        sa = 1. - (1.-self.sg) * self.ss

        self.rho_0v = self.sp * self.ss
        self.rho_dd = self.sg * self.ss / sa
        self.rho_0d = (1.-self.sg) * self.ss * self.rho_dd
        self.rho_dv = None

        sd = self.t_0d / (self.t_00 + self.t_0d)
        #self.rs = (1.-sd) * (sp*ss + sg*(1.-sg)*ss*ss/sa) + sd*sg*ss/sa
        self.rs = self.ss * (self.sp + sd * (self.sg - self.sp) +
                             self.sg * (1.-self.sg) * self.ss / sa)

    def _set_oxford_surface(self, **kwargs):
        """Maintain ratio between different rho for some wavelengths"""
        for rho_name in RHO_NAMES:
            if rho_name in kwargs:
                # Copy over data provided
                value = kwargs[rho_name]
            else:
                # Set using ratios from spixel
                for ratio_rho_name in RHO_NAMES:
                    if ratio_rho_name in kwargs:
                        value = (getattr(self.pixel, rho_name) /
                                 getattr(self.pixel, ratio_rho_name) *
                                 kwargs[ratio_rho_name])
                        break
                else:
                    raise ValueError("Must provide one of "+", ".join(rho_names))
            assert value.size == self.nch
            setattr(self, rho_name, value)

    def this_below(self, lut, **kwargs):
        """Generate model for a specified layer above this one"""
        # Calculate four-term reflectance of this cloud
        ref_0v = self.reflectance() * self.sec_0[0]
        ref_0d = (self.coverage * self.tac_0 * self.tac_d * e_0d +
                  (1.-self.coverage) * self.rho_0d * self.tsf_0 * self.tsf_d)
        ref_dv = (self.coverage * self.tac_0 * self.tac_d * e_dv +
                  (1.-self.coverage) * self.rho_dv * self.tsf_d * self.tsf_v)
        ref_dd = (self.coverage * self.tac_d * self.tac_d * e_dd +
                  (1.-self.coverage) * self.rho_dd * self.tsf_d * self.tsf_d)
        # Remove atmosphere below lower layer in new case
        upper_pixel = copy(pixel)
        upper_pixel.tbc_sw = upper_pixel.tbc_sw / self.tbc
        upper_pixel.rs = rho_0v * tsf_0v
        # Remove atmosphere above this lower layer
        self.tac = np.ones(self.nch)
        self.description += " lower layer"
        # Make new instance for the upper layer
        upper = self.__class__(upper_pixel, lut, **kwargs)
        upper.description += " upper layer"
        return upper

    def _overcast_r(self):
        return self.tac_0 * self.tac_v * (self.r_0v + self.d())

    def _clear_r(self):
        return self.pixel.rs * self.tsf_0 * self.tsf_v

    def reflectance(self):
        return self._total_r()

    @property
    def tac_0(self):
        """Transmission above cloud in solar direction"""
        return self.tac ** self.sec_0
    @property
    def tac_v(self):
        """Transmission above cloud in view direction"""
        return self.tac ** self.sec_v
    @property
    def tac_d(self):
        """Diffuse transmission above cloud"""
        return self.tac ** self.DIFFUSE_TRANSMISSION_FACTOR
    @property
    def tbc_0(self):
        """Transmission below cloud in solar direction"""
        return self.tbc ** self.sec_0
    @property
    def tbc_v(self):
        """Transmission below cloud in view direction"""
        return self.tbc ** self.sec_v
    @property
    def tbc_d(self):
        """Diffuse transmission below cloud"""
        return self.tbc ** self.DIFFUSE_TRANSMISSION_FACTOR
    @property
    def tsf_0(self):
        """All-sky transmission in solar direction"""
        return self.tsf ** self.sec_0
    @property
    def tsf_v(self):
        """All-sky transmission in view direction"""
        return self.tsf ** self.sec_v

    @abstractmethod
    def d(self):
        """Atmospheric reflection"""

    def e_0d(self):
        raise NotImplementedError

    def e_dv(self):
        raise NotImplementedError

    def e_dd(self):
        raise NotImplementedError

    def solar_constant(self):
        return self.lut.solar_constant(self.pixel.time._dayofyr)[self.lut_ind]

class SolarBrdfBase(SolarForwardModel):
    def a(self):
        return 1. - self.rho_dd * self.r_dd * self.tbc_d * self.tbc_d

    def b(self):
        return (self.t_00 * self.rho_0d * self.tbc_0 +
                self.t_0d * self.rho_dd * self.tbc_d)

    @abstractmethod
    def c(self):
        """Shared term in infinite reflection between surface and cloud"""

class SolarBrdfEq1(SolarBrdfBase):
    description = "Solar BRDF Form 1"
    def c(self):
        return self.t_dv * self.tbc_d

    def d(self):
        return (
            self.t_00 * (self.rho_0v - self.rho_0d) * self.t_vv * self.tbc_0 * self.tbc_v +
            self.b() * self.c() / self.a()
        ) / self.sec_0[0]

    def e_0d(self):
        c_0d = self.t_dd * self.tbc_d
        return self.r_0d + self.b() * c_0d / self.a()

    def e_dv(self):
        c_dv = self.t_dd * self.tbc_d
        d_dv = (self.rho_dv * self.t_vv * self.tbc_v +
                self.rho_dd * self.t_dv * self.tbc_d)
        return self.r_dv + c_dv * d_dv / self.a()

    def e_dd(self):
        return (self.r_dd + self.t_dd**2 * self.rho_dd * self.tbc_d *
                self.tbc_d / self.a())

class SolarBrdfEq2(SolarBrdfBase):
    description = "Solar BRDF Form 2"
    def __init__(self, *args, **kwargs):
        super().__init__(self, *args, **kwargs)
        self.alb /= self.sec_0
        self.rho_0v /= self.sec_0
        self.rho_0d /= self.sec_0
        self.rho_dv /= self.sec_0
        self.rho_dd /= self.sec_0

    def c(self):
        return self.t_dv * self.tbc_d

    def d(self):
        return (
            self.t_00 * (self.rho_0v - self.rho_0d) * self.t_vv * self.tbc_0 * self.tbc_v +
            self.b() * self.c() / self.a()
        )

class SolarBrdfEq3(SolarBrdfBase):
    description = "Solar BRDF Form 3"
    def c(self):
        return (self.t_dv * self.tbc_d +
                self.r_dd * self.rho_dv * self.t_vv * self.tbc_d * self.tbc_d * self.tbc_v)

    def d(self):
        return (self.t_00 * self.rho_0v * self.t_vv * self.tbc_0 * self.tbc_v +
                self.t_0d * self.rho_dv * self.t_vv * self.tbc_d * self.tbc_v +
                self.b() * self.c() / self.a()) / self.sec_0[0]

    def e_0d(self):
        c_0d = self.t_dd * self.tbc_d
        return self.r_0d + self.b() * c_0d / self.a()

    def e_dv(self):
        c_dv = self.t_dd * self.tbc_d
        d_dv = (self.rho_dv * self.t_vv * self.tbc_v +
                self.rho_dd * self.t_dv * self.tbc_d)
        return self.r_dv + c_dv * d_dv / self.a()

    def e_dd(self):
        return (self.r_dd + self.t_dd**2 * self.rho_dd * self.tbc_d *
                self.tbc_d / self.a())

class SolarBrdfEq4(SolarBrdfBase):
    description = "Solar BRDF Form 4"
    def __init__(self, *args):
        super().__init__(self, *args, **kwargs)
        self.alb /= self.sec_0
        self.rho_0v /= self.sec_0
        self.rho_0d /= self.sec_0
        self.rho_dv /= self.sec_0
        self.rho_dd /= self.sec_0

    def c(self):
        return (self.t_dv * self.tbc_d +
                self.r_dd * self.rho_dv * self.t_vv * self.tbc_d * self.tbc_d * self.tbc_v)

    def d(self):
        return (self.t_00 * self.rho_0v * self.t_vv * self.tbc_0 * self.tbc_v +
                self.t_0d * self.rho_dv * self.t_vv * self.tbc_d * self.tbc_v +
                self.b() * self.c() / self.a())

class SolarLambertian(SolarForwardModel):
    description = "Solar Lambertian"
    def a(self):
        return 1. - self.rs * self.r_dd * self.tbc_d * self.tbc_d

    def d(self):
        return (self.t_00 + self.t_0d) * self.rs * self.t_dv * self.tbc_d * self.tbc_d / self.a()


class ThermalForwardModel(OracForwardModel):
    """Python implementation of the thermal ORAC forward model

    This is intended to be legible, not efficient.
    Adjoint not yet implemented as you shouldn't use this in optimisation.
    """
    description = "Thermal"
    def __init__(self, spixel, lut, **kwargs):
        if np.any(np.logical_not(spixel.thermal)):
            raise ValueError("All channels must be thermal; use Spixel.subset()")

        super().__init__(spixel, lut, **kwargs)

    def set_state(self, **kwargs):
        super().set_state(**kwargs)

        if upd := {k:v for k,v in kwargs.items() if k in ("lut", "top_pressure", "surface_temperature")}:
            self.__dict__.update(upd)
            self._set_top_pressure()

    def _set_nuclei_terms(self):
        super()._set_nuclei_terms(self.lut.thermal)

    def _set_top_pressure(self):
        """Interpolate the clear-sky radiative transfer"""
        (self.tac, self.tbc, self.top_temperature, self.rac_up, self.rac_down,
         self.rbc_up) = self.pixel.interp_in_pressure(
            self.top_pressure, "tac_lw", "tbc_lw", "correct_temperature",
            "rac_up", "rac_down", "rbc_up"
        )

        self.b_cloud, _ = self.temp2rad(self.top_temperature)
        _, self.db_dts = self.temp2rad(self.pixel.temperature[-1])
        self.es_db_dts = self.db_dts * self.pixel.emissivity

        self.rbc_up += self.delta_ts * self.es_db_dts * self.tbc

    @property
    def delta_ts(self):
        return self.surface_temperature - self.pixel.temperature[-1]

    def _overcast_r(self):
        return self.rac_up + self.tac * (
            self.rbc_up * self.t_dv + self.b_cloud * self.e_md +
            self.rac_down * self.r_dv
        )

    def _clear_r(self):
        return (self.pixel.rbc_up[0] +
                self.delta_ts * self.es_db_dts * self.pixel.tac_lw[-1])

    def radiance(self):
        return self._total_r()

    def brightness_temperature(self):
        try:
            bt, _ = self.rad2temp(self._total_r())
            return bt
        except ValueError:
            return np.apply_along_axis(
                lambda r: self.rad2temp(r)[0], 2, self._total_r()
            )

    def this_below(self, lut, **kwargs):
        self.description += " lower layer"
        return ThermalUpperLayer(lut, self, **kwargs)

    def temp2rad(self, temperature):
        b, db_dt = self.lut.temp2rad(temperature)
        return b[self.lut_ind], db_dt[self.lut_ind]

    def rad2temp(self, radiance):
        extended_radiance = np.ones(self.lut.thermal.sum())
        extended_radiance[self.lut_ind] = radiance
        t, dt_db = self.lut.rad2temp(extended_radiance)
        return t[self.lut_ind], dt_db[self.lut_ind]

class ThermalUpperLayer(ThermalForwardModel):
    """Python implementation of a two-layer ORAC thermal forward model

    While solar is basically two calls to the same equation, the thermal
    model is sufficiently different to be considered a seperate equation
    """
    description = "Thermal upper layer"
    def __init__(self, lut, lower_layer, **kwargs):
        self.lower = lower_layer
        super().__init__(lut, **kwargs)
        self.tmc = self.lower.tac / self.tac
        self.rmc_up = (self.lower.rac_up - self.rac_up) / self.tac
        self.rmc_down = self.lower.rac_down - self.rac_down * self.tmc

    def overcast_reflectance(self):
        return self.a() + self.b() + self.c()

    def a(self):
        return (((self.lower.rbc_up * self.lower.t_dv +
                  self.lower.b_cloud * self.lower.e_md) * self.tmc + self.rmc_up) *
                self.t_dv +
                self.b_cloud * self.e_md) * self.tac + self.rac_up

    def b(self):
        return self.tmc * self.t_dv * self.tac * self.lower.r_dv * (
            self.rmc_down +
            (self.b_cloud * self.e_md + self.rac_down * self.t_dv) * self.tmc
        )

    def c(self):
        return self.rac_down * self.r_dv * self.tac

    @property
    def coverage(self):
        return self.lower.coverage + self._coverage - self.lower.coverage * self._coverage

    @coverage.setter
    def coverage(self, value):
        self._coverage = value
