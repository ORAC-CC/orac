"""Routines for opening and manipulating ORAC look-up tables

Classes:
RearrangedRegularGridInterpolator
    Hack of scipy.interpolate.RegularGridInterpolator that allows
    us to change the order of dimensions in __call__. This let's
    us use a single call for both text and NCDF tables, which have
    different orders for the dimensions.
OracLut
    Container to open and interpolate the ORAC look-up tables from
    either text or netCDF format.

Functions:
read_orac_chan_file
    Fetches the contents of an ORAC SAD Channel file
stack_orac_chan_file
    Fetches SAD Channel data for a set of channels
read_orac_text_lut
    Fetches the contents of an ORAC SAD LUT file
stack_orac_text_lut
    Fetches SAD LUT data for a set of channels for files with 1 table
stack_orac_text_lut_pair
    Fetches SAD LUT data for a set of channels for files with 2 tables

Notes:
- There is no check that all text LUTs have the same axes, because
  they don't strictly need to with RegularGridInterpolator.
- As done in the main code, we assume the identity of the axes in the
  LUT files. Calls to text tables are manupulated to look like NCDF.

To Do:
- Copy over the Fortran LUT interpolation routines.
"""
import numpy as np
import os.path

from abc import ABC, abstractmethod
from netCDF4 import Dataset
from scipy.interpolate import RegularGridInterpolator


class RearrangedRegularGridInterpolator(RegularGridInterpolator):
    """Hack of scipy.interpolate.RegularGridInterpolator

    Adds the ability to change the order in which grid dimensions
    are referenced by __call__
    """
    def __init__(self, *args, **kwargs):
        self.order = args[-1]
        if len(self.order) != len(args[0]):
            raise ValueError("order must be of same length as grid")
        super().__init__(*args[:-1], **kwargs)

    def __call__(self, *args, **kwargs):
        rearranged_args = tuple(args[0][i] for i in self.order)
        return super().__call__(rearranged_args, *args[1:], **kwargs)


class OracLutBase(ABC):
    """
    Container for reading and interpolating ORAC look-up tables

    Both text and netCDF versions are supported. Each table is opened
    an instance of scipy.interpolate.RegularGridInterpolator but the
    routines src/int_lut* and set_crp_* from ORAC are also implemented
    for exact replication.
    """

    def __init__(self, filename, check_consistency=False):
        """
        Args:
        filename: name of look-up table to open

        Kwargs:
        check_consistency: ensure that the number of channels is consistent
            across read operations (mostly important for text tables)
        """

        self.filename = filename
        self.check = check_consistency
        self._particle = None
        self._inst = None

    @classmethod
    def from_description(cls, sad_dirs, particle, inst, **kwargs):
        """Initialise from pyorac settings objects

        Args:
        sad_dirs: list of directories to search for these LUTs
        particle: instance of definitions.ParticleType for the desired tables
        inst: instance of definitions.FileName for the desired instrument
        """

        fdr = particle.sad_dir(sad_dirs, inst)
        filename = os.path.join(fdr, particle.sad_filename(inst))
        self = cls(filename, **kwargs)
        self._particle = particle
        self._inst = inst
        return self

    def solar_constant(self, doy):
        if self.f1 is not None:
            # Old approximation for F0 variation throughout the year
            return self.f0 + self.f1 * np.cos(2. * np.pi * doy / 365.)

        # New (better) approximation
        return self.f0 / (1. - 0.0167086 * np.cos((2.* np.pi * (doy - 4.)) /
                                                  365.256363))**2

    def rad2temp(self, radiance, sl=slice(None)):
        """Convert radiance into brightness temperature"""

        c = np.log(self.b1[sl] / radiance + 1.)
        t_eff = self.b2[sl] / c
        t = (t_eff - self.t1[sl]) / self.t2[sl]

        dt_dr = self.b1[sl] * self.b2[sl] / (self.t2[sl] * c * c * radiance * (radiance + self.b1[sl]))

        return t, dt_dr

    def temp2rad(self, temperature, sl=slice(None)):
        """Convert brightness temperature into radiance"""

        t_eff = temperature * self.t2[sl] + self.t1[sl]
        bb = self.b2[sl] / t_eff
        c = np.exp(bb)
        denom = c - 1.
        r = self.b1[sl] / denom

        dr_dt = self.b1[sl] * bb * c * self.t2[sl] / (t_eff * denom * denom)

        return r, dr_dt

    def __call__(self, channels, satzen, solzen, relazi, tau, re):
        """Deal with different dimension order/manner in types of LUT"""
        if self.satellite_zenith_spacing.endswith("logarithmic"):
            satzen = np.log10(satzen)
        if self.solar_zenith_spacing.endswith("logarithmic"):
            solzen = np.log10(solzen)
        if self.relative_azimuth_spacing.endswith("logarithmic"):
            relazi = np.log10(relazi)
        if self.optical_depth_spacing.endswith("logarithmic"):
            tau = np.log10(tau)
        if self.effective_radius_spacing.endswith("logarithmic"):
            re = np.log10(re)

        # Only output sets that apply to every requested channel
        solar = True
        thermal = True
        for requested_ch in channels:
            for available_ch, is_solar, is_thermal in zip(self.channels, self.solar, self.thermal):
                if requested_ch == available_ch:
                    solar &= is_solar
                    thermal &= is_thermal
                    break
            else:
                raise ValueError(f"Requested unavailable channel: {requested_ch}")

        out = dict(
            t_dv=self.t_dv((satzen, tau, re, channels)),
            t_dd=self.t_dd((tau, re, channels)),
            r_dv=self.r_dv((satzen, tau, re, channels)),
            r_dd=self.r_dd((tau, re, channels)),
            ext=self.ext((re, channels)),
            ext_ratio=self.ext_ratio((re, channels)),
        )
        if solar:
            out["r_0v"] = self.r_0v((relazi, satzen, solzen, tau, re, channels))
            out["r_0d"] = self.r_0d((solzen, tau, re, channels))
            out["t_0d"] = self.t_0d((solzen, tau, re, channels))
            out["t_00"] = self.t_00((solzen, tau, re, channels))
            # Use satzen on solzen axis (which assumes its in bounds)
            out["t_vd"] = self.t_0d((satzen, tau, re, channels))
            out["t_vv"] = self.t_00((satzen, tau, re, channels))
        if thermal:
            out["e_md"] = self.e_md((satzen, tau, re, channels))

        return out

    @abstractmethod
    def state_space(self, channels, satzen, solzen, relazi):
        """Interpolate angles, returning depth/radius sheets

        This is a linear interpolation as done within the Fortran."""

    @abstractmethod
    def state_mesh(self):
        """Returns the mesh of grid points for the outputs of state_space"""

    @abstractmethod
    def uncertainty(self, pixel):
        """Calculates the ORAC uncertainty model for a given pixel

        Arguments and keywords will vary with table type as they use
        different calculations. See specific documentation."""


class OracTextLut(OracLutBase):
    def __init__(self, filename, method='cubic', check_consistency=False):
        """
        Args:
        filename: name of look-up table to open

        Kwargs:
        method: interpolation method to use. Default cubic.
        check_consistency: ensure that the number of channels is consistent
            across read operations (mostly important for text tables)
        """
        super(OracTextLut, self).__init__(filename, check_consistency)

        try:
            self._open_text_chan_files()
        except UnboundLocalError:
            raise ValueError("Unable to find tables from "+filename)
        self._open_text_tables(method)

    def __call__(self, *args, t_dv_from_t_0d=True):
        """Interpolate table to given values

        t_dv_from_t_0d: ignore the diffuse-to-view transmission table"""
        if t_dv_from_t_0d:
            tmp = self.t_dv
            self.t_dv = self.t_0d
        try:
            return super(OracTextLut, self).__call__(*args)
        finally:
            if t_dv_from_t_0d:
                self.t_dv = tmp

    def _open_text_chan_files(self):
        """Open ORAC channel descriptions for a set of channels"""
        import re
        from glob import iglob

        # Work out what files are available and their channel numbers
        fdr, root = os.path.split(self.filename)
        parts = os.path.basename(root).split("_")
        self.sensor = parts[0]
        ch_filename = os.path.join(fdr, self.sensor+"_"+parts[-1])
        regex = re.compile("(Ch[0-9ab]+)\.sad")
        lut_files = []
        channels = []
        for a_file in iglob(self.filename):
            ch_label = regex.search(a_file).group(1)
            lut_files.append(ch_filename.replace("Ch*", ch_label))
            channels.append(_sensor_ch_num_to_orac_ch_num(self.sensor, ch_label))

        # Sort by channel number
        channels, lut_files = zip(*sorted(zip(channels, lut_files)))

        properties = dict(channels=channels)
        for lut_file in lut_files:
            chan = read_orac_chan_file(lut_file)
            if self.check and chan_properties["name"] != os.path.basename(lut_file):
                raise ValueError("Inconsistent LUT: "+lut_file)

            # Flatten structure
            chan.update(chan.pop("vis", {}))
            chan.update(chan.pop("ir", {}))

            # Form lists
            for key, val in chan.items():
                try:
                    properties[key].append(val)
                except KeyError:
                    properties[key] = [val,]

        if self.check:
            ns = sum(properties["solar"])
            if ns > 0 and ns != len(properties["f01"]):
                raise ValueError("Inconstent solar channels: "+self.filename)
            nt = sum(properties["thermal"])
            if nt > 0 and nt != len(properties["b1"]):
                raise ValueError("Inconsistent thermal channels: "+self.filename)

        # Convert to lists to arrays or delete
        del properties["name"]
        properties = {k: np.asarray(v) for k, v in properties.items()}

        # Reformat the solar constant array
        if (f01 := properties.pop("f01", None)) is not None:
            self.f0 = f01[:,0]
            self.f1 = f01[:,1]
        else:
            self.f0, self.f1 = [None] * 2

        self.__dict__.update(properties)
        self.nch = len(self.channels)

        if not np.any(self.thermal):
            self.b1 = None
            self.b2 = None
            self.t1 = None
            self.t2 = None

    def _stack_orac_text_lut(self, code):
        """Open a set of channels from ORAC text look-up tables"""

        shape = None
        for i, label in enumerate(self.file_id):
            lut_file = self.filename.replace("RD", code).replace("Ch*", label)

            try:
                tables, axes, naxes, _ = read_orac_text_lut(lut_file)
            except FileNotFoundError:
                continue
            if shape is None:
                shape = np.insert(naxes, 0, self.nch)
                result = np.empty(shape)
                base_axes = axes
            elif self.check:
                for ax0, ax1 in zip(base_axes, axes):
                    if not np.allclose(ax0, ax1):
                        raise ValueError("Inconsistent LUT: "+lut_file)

            result[i] = tables[0]

        # Inspect axes spacing
        spacing = []
        for ax in axes:
            spaces = np.unique(np.diff(ax))
            # Check if grid evenly spaced
            label = "uneven_" if spaces.size > 1 else ""
            # Guess axes is log spaced if any values are negative
            label += "logarithmic" if np.any(ax < 0.) else "linear"
            spacing.append(label)

        return result, axes, spacing

    def _stack_orac_text_lut_pair(self, code):
        """Open a pair of ORAC tables for a set of channels"""

        shape0 = None
        for i, label in enumerate(self.file_id):
            lut_file = self.filename.replace("RD", code).replace("Ch*", label)

            try:
                tables, axes, naxes, _ = read_orac_text_lut(lut_file)
            except FileNotFoundError:
                continue
            if shape0 is None:
                shape0 = np.insert(naxes, 0, self.nch)
                result0 = np.empty(shape0)
                shape1 = np.insert(naxes[::2], 0, self.nch)
                result1 = np.empty(shape1)
                base_axes = axes
            elif self.check:
                for ax0, ax1 in zip(base_axes, axes):
                    if not np.allclose(ax0, ax1):
                        raise ValueError("Inconsistent LUT: "+lut_file)

            result0[i] = tables[0]
            result1[i] = tables[1]

        # Inspect axes spacing
        spacing = []
        for ax in axes:
            spaces = np.unique(np.diff(ax))
            label = "uneven_" if spaces.size > 1 else ""
            label += "logarithmic" if np.any(ax < 0.) else "linear"
            spacing.append(label)

        return result0, result1, axes, spacing

    def _open_text_tables(self, method):
        """Open set of ORAC text look-up tables"""

        rd, rfd, axes, spacing = self._stack_orac_text_lut_pair("RD")
        self.r_dv = RearrangedRegularGridInterpolator(
            [self.channels] + axes, rd, method, (3, 2, 0, 1)
        )
        self.r_dd = RearrangedRegularGridInterpolator(
            [self.channels, axes[0], axes[2]], rfd, method, (2, 1, 0)
        )
        self.effective_radius_spacing = spacing[0]
        self.satellite_zenith_spacing = spacing[1]
        self.optical_depth_spacing = spacing[2]

        td, tfd, axes, _ = self._stack_orac_text_lut_pair("TD")
        self.t_dv = RearrangedRegularGridInterpolator(
            [self.channels] + axes, td, method, (3, 2, 0, 1)
        )
        self.t_dd = RearrangedRegularGridInterpolator(
            [self.channels, axes[0], axes[2]], tfd, method, (2, 1, 0)
        )

        # Omit the redundant tau dimension
        bext, axes, _ = self._stack_orac_text_lut("Bext")
        self.ext = RearrangedRegularGridInterpolator(
            [self.channels, axes[0]], bext[:,:,0], method, (1, 0)
        )
        bextrat, axes, _ = self._stack_orac_text_lut("BextRat")
        self.ext_ratio = RearrangedRegularGridInterpolator(
            [self.channels, axes[0]], bextrat[:,:,0], method, (1, 0)
        )

        if np.any(self.solar):
            # Have to invert the relative azimuth axis
            rbd, rfbd, axes, spacing = self._stack_orac_text_lut_pair("RBD")
            axes[1] = 180. - axes[1]
            self.r_0v = RearrangedRegularGridInterpolator(
                [self.channels] + axes, rbd, method, (5, 4, 0, 2, 1, 3)
            )
            self.r_0d = RearrangedRegularGridInterpolator(
                [self.channels, axes[0], axes[2], axes[4]], rfbd, method, (3, 2, 0, 1)
            )
            self.relative_azimuth_spacing = spacing[1]
            self.solar_zenith_spacing = spacing[2]

            tb, axes, _ = self._stack_orac_text_lut("TB")
            self.t_00 = RearrangedRegularGridInterpolator(
                [self.channels] + axes, tb, method, (3, 2, 0, 1)
            )

            tbd, tfbd, axes, _ = self._stack_orac_text_lut_pair("TBD")
            axes[1] = 180. - axes[1]
            self.t_0d = RearrangedRegularGridInterpolator(
                [self.channels, axes[0], axes[2], axes[4]], tfbd, method, (3, 2, 0, 1)
            )
        else:
            self.r_0v = None
            self.r_0d = None
            self.t_0d = None
            self.t_00 = None
            self.relative_azimuth_spacing = "unknown"
            self.solar_zenith_spacing = "unknown"

        if np.any(self.thermal):
            em, axes, _ = self._stack_orac_text_lut("EM")
            self.e_md = RearrangedRegularGridInterpolator(
                [self.channels,] + axes, em, method, (3, 2, 0, 1)
            )
        else:
            self.e_md = None

    def state_space(self, channels, satzen, solzen, relazi, t_dv_from_t_0d=True):
        """Interpolate angles, returning depth/radius sheets

        This is a linear interpolation as done within the Fortran.
        Arguments are either scalars or 1D arrays of the same length

        t_dv_from_t_0d: ignore the diffuse-to-view transmission table"""
        if self.satellite_zenith_spacing.endswith("logarithmic"):
            satzen = np.log10(satzen)
        if self.solar_zenith_spacing.endswith("logarithmic"):
            solzen = np.log10(solzen)
        if self.relative_azimuth_spacing.endswith("logarithmic"):
            relazi = np.log10(relazi)

        isat = np.digitize(satzen, self.r_0v.grid[4])
        dsat = ((self.r_0v.grid[4][isat] - satzen) /
                (self.r_0v.grid[4][isat] - self.r_0v.grid[4][isat-1]))
        isol = np.digitize(solzen, self.r_0v.grid[3])
        dsol = ((self.r_0v.grid[3][isol] - solzen) /
                (self.r_0v.grid[3][isol] - self.r_0v.grid[3][isol-1]))
        irel = np.digitize(relazi, self.r_0v.grid[2])
        drel = ((self.r_0v.grid[2][irel] - relazi) /
                (self.r_0v.grid[2][irel] - self.r_0v.grid[2][irel-1]))
        isatsol = np.digitize(satzen, self.r_0v.grid[3])
        dsatsol = ((self.r_0v.grid[3][isatsol] - satzen) /
                (self.r_0v.grid[3][isatsol] - self.r_0v.grid[3][isatsol-1]))

        ch = [i in channels for i in self.channels]

        return dict(
            t_dv=(((1-dsatsol) * self.t_0d.values[ch,:,isatsol].T +
                   dsatsol * self.t_0d.values[ch,:,isatsol-1].T)
                  if t_dv_from_t_0d else
                  ((1-dsat) * self.t_dv.values[ch,:,isat].T +
                   dsat * self.t_dv.values[ch,:,isat-1].T)),
            t_dd=self.t_dd.values[ch].T,
            r_dv=((1-dsat) * self.r_dv.values[ch,:,isat].T +
                  dsat * self.r_dv.values[ch,:,isat-1].T),
            r_dd=self.r_dd.values[ch].T,
            ext=self.ext.values[ch].T,
            ext_ratio=self.ext_ratio.values[ch].T,
            r_0v=(((1-dsat) * (1-dsol) * (1-drel) * self.r_0v.values[ch,:,irel,isol,isat].T +
                   dsat * (1-dsol) * (1-drel) * self.r_0v.values[ch,:,irel,isol,isat-1].T +
                   (1-dsat) * dsol * (1-drel) * self.r_0v.values[ch,:,irel,isol-1,isat].T +
                   dsat * dsol * (1-drel) * self.r_0v.values[ch,:,irel,isol-1,isat-1].T +
                   (1-dsat) * (1-dsol) * drel * self.r_0v.values[ch,:,irel-1,isol,isat].T +
                   dsat * (1-dsol) * drel * self.r_0v.values[ch,:,irel-1,isol,isat-1].T +
                   (1-dsat) * dsol * drel * self.r_0v.values[ch,:,irel-1,isol-1,isat].T +
                   dsat * dsol * drel * self.r_0v.values[ch,:,irel-1,isol-1,isat-1].T)),
        r_0d=((1-dsol) * self.r_0d.values[ch,:,isol].T +
              dsol * self.r_0d.values[ch,:,isol-1].T),
        t_0d=((1-dsol) * self.t_0d.values[ch,:,isol].T +
              dsol * self.t_0d.values[ch,:,isol-1].T),
        t_00=((1-dsol) * self.t_00.values[ch,:,isol].T +
              dsol * self.t_00.values[ch,:,isol-1].T),
        t_vd=((1-dsatsol) * self.t_0d.values[ch,:,isatsol].T +
              dsatsol * self.t_0d.values[ch,:,isatsol-1].T),
        t_vv=((1-dsatsol) * self.t_00.values[ch,:,isatsol].T +
              dsatsol * self.t_00.values[ch,:,isatsol-1].T),
        e_md=((1-dsat) * self.e_md.values[ch,:,isat].T +
              dsat * self.e_md.values[ch,:,isat-1].T),
    )

    def state_mesh(self):
        return np.meshgrid(_edges_from_points(self.r_dd.grid[1]),
                           _edges_from_points(10.0**self.r_dd.grid[2]))


    def uncertainty(self, pixel, alwaysthermal=False, cloudtype=0, maxsolzen=75.):
        """Calculate measurement variance for a pixel the old way

        Args:
        pixel: SPixel instance to evaluate
        alwaysthermal: Include therm homog/coreg errors during the day
        cloudtype: Int[0-4] specifying cloud structure model to use; None skips
        maxsolzen: Solar zenith beyond which pixel is considered twilight
        """
        from datetime import datetime

        date_into_year = pixel.time - datetime(pixel.time.year-1, 12, 31)
        sol_const = self.solar_constant(date_into_year.days)

        out = []
        for ch in pixel.iter_channels():
            # Map this channel onto the LUT
            i = np.argmax(ch["channel"] == pixel.channels)
            j = np.argmax(ch["channel"] == self.channels[self.solar])
            k = np.argmax(ch["channel"] == self.channels[self.thermal])

            day = pixel.solzen[i] < maxsolzen

            # Measurement uncertainty
            if ch["sy"] > 0.:
                sy = ch["sy"]
            elif ch["thermal"]:
                sy = self.nebt[k]**2
            else:
                sy = self.nedr[j]**2

            if cloudtype is not None:
                # Homog uncertainty
                if ch["thermal"] and (alwaysthermal or ch["solar"] or not day):
                    sy += self.ir_nehomog[k,cloudtype]**2
                if ch["solar"] and day:
                    if ch["thermal"]:
                        _, dr_dtm = self.temp2rad(ch["meas"])
                        radiance = sol_const[j] / dr_dtm[k]
                    else:
                        radiance = ch["meas"]
                    sy += (self.vis_nehomog[j,cloudtype] * radiance)**2

                # Coreg uncertainty
                if ch["thermal"] and (alwaysthermal or ch["solar"] or not day):
                    sy += self.ir_necoreg[k,cloudtype]**2
                if ch["solar"] and day:
                    if ch["thermal"]:
                        # _, dr_dtm = self.temp2rad(ch["meas"])
                        radiance = sol_const[j] / dr_dtm[k]
                    else:
                        radiance = ch["meas"]
                    sy += (self.vis_necoreg[j,cloudtype] * radiance)**2
            out.append(sy)

        return np.array(out)


class OracNcdfLut(OracLutBase):
    def __init__(self, filename, method='cubic', check_consistency=False):
        """
        Args:
        filename: name of look-up table to open

        Kwargs:
        method: interpolation method to use. Default cubic.
        check_consistency: ensure that the number of channels is consistent
            across read operations (mostly important for text tables)
        """
        super(OracNcdfLut, self).__init__(filename, check_consistency)

        self.file_id = None
        with Dataset(self.filename) as lut_file:
            # Identify requested channel subset
            self.channels = lut_file["channel_id"][...]
            self.nch = len(self.channels)

            # Channel information
            self.solar = lut_file["solar_channel_flag"][:] == 1
            self.thermal = lut_file["thermal_channel_flag"][:] == 1

            # Read other axes, noting if they are logarithmic
            def fetch_axes(name):
                var = lut_file[name]
                data = var[...]
                self.__dict__[name + "_spacing"] = var.spacing
                if "logarithmic" in var.spacing:
                    return np.log10(data)
                return data
            tau = fetch_axes("optical_depth")
            re = fetch_axes("effective_radius")
            satzen = fetch_axes("satellite_zenith")
            solzen = fetch_axes("solar_zenith")
            relazi = fetch_axes("relative_azimuth")

            # Read tables
            self.t_dv = RegularGridInterpolator(
                (satzen, tau, re, self.channels), lut_file["T_dv"][...].data, method
            )
            self.t_dd = RegularGridInterpolator(
                (tau, re, self.channels), lut_file["T_dd"][...].data, method
            )
            self.r_dv = RegularGridInterpolator(
                (satzen, tau, re, self.channels), lut_file["R_dv"][...].data, method
            )
            self.r_dd = RegularGridInterpolator(
                (tau, re, self.channels), lut_file["R_dd"][...].data, method
            )
            self.ext = RegularGridInterpolator(
                (re, self.channels),
                lut_file["extinction_coefficient"][...].data, method
            )
            self.ext_ratio = RegularGridInterpolator(
                (re, self.channels),
                lut_file["extinction_coefficient_ratio"][...].data, method
            )

            # Solar channels
            if np.any(self.solar):
                solar_channels = lut_file["solar_channel_id"][:]

                self.r_0v = RegularGridInterpolator(
                    (relazi, satzen, solzen, tau, re, solar_channels),
                    lut_file["R_0v"][...].data, method
                )
                self.r_0d = RegularGridInterpolator(
                    (solzen, tau, re, solar_channels),
                    lut_file["R_0d"][...].data, method
                )
                self.t_0d = RegularGridInterpolator(
                    (solzen, tau, re, solar_channels),
                    lut_file["T_0d"][...].data, method
                )
                self.t_00 = RegularGridInterpolator(
                    (solzen, tau, re, solar_channels),
                    lut_file["T_00"][...].data, method
                )

                # Solar constant
                self.f0 = lut_file["F0"][:]
                self.f1 = None
                # Uncertainty model
                try:
                    self.snr = lut_file["snr"][:]
                    self.ru2 = None
                except (KeyError, IndexError):
                    self.snr = None
                    self.ru2 = np.stack([lut_file["ru"+l][:]**2 for l in "abc"])
            else:
                (self.r_0v, self.r_0d, self.t_0d, self.t_00, self.f0, self.f1,
                 self.snr, self.ru2) = [None] * 8

            # Thermal channels
            if np.any(self.thermal):
                thermal_channels = lut_file["thermal_channel_id"][:]

                self.e_md = RegularGridInterpolator(
                    (satzen, tau, re, thermal_channels),
                    lut_file["E_md"][...].data, method
                )

                # Planck coefficients
                self.b1 = lut_file["B1"][:]
                self.b2 = lut_file["B2"][:]
                self.t1 = lut_file["T1"][:]
                self.t2 = lut_file["T2"][:]
                # Uncertainty model
                self.t0 = lut_file["refbt"][:]
                self.nedt = lut_file["nedt"][:]
            else:
                (self.e_md, self.b1, self.b2, self.t1, self.t2,
                 self.t0, self.nedf) = [None] * 7

    def state_space(self, channels, satzen, solzen, relazi):
        """Interpolate angles, returning depth/radius sheets

        This is a linear interpolation as done within the Fortran.
        All arguments are either scalars or 1D arrays of the same length."""
        # Make all arguments arrays
        if not isinstance(channels, np.ndarray):
            channels = np.asarray([channels]
                                  if np.isscalar(channels) else channels)
        if not isinstance(satzen, np.ndarray):
            satzen = np.asarray([satzen] * len(channels)
                                if np.isscalar(satzen) else satzen)
        if not isinstance(solzen, np.ndarray):
            solzen = np.asarray([solzen] * len(channels)
                                if np.isscalar(solzen) else solzen)
        if not isinstance(relazi, np.ndarray):
            relazi = np.asarray([relazi] * len(channels)
                                if np.isscalar(relazi) else relazi)

        # Ensure those are the same length
        assert channels.shape == satzen.shape
        assert channels.shape == solzen.shape
        assert channels.shape == relazi.shape

        if self.satellite_zenith_spacing.endswith("logarithmic"):
            satzen = np.log10(satzen)
        if self.solar_zenith_spacing.endswith("logarithmic"):
            solzen = np.log10(solzen)
        if self.relative_azimuth_spacing.endswith("logarithmic"):
            relazi = np.log10(relazi)

        isat = np.digitize(satzen, self.r_0v.grid[1])
        dsat = ((self.r_0v.grid[1][isat] - satzen) /
                (self.r_0v.grid[1][isat] - self.r_0v.grid[1][isat-1]))
        isol = np.digitize(solzen, self.r_0v.grid[2])
        dsol = ((self.r_0v.grid[2][isol] - solzen) /
                (self.r_0v.grid[2][isol] - self.r_0v.grid[2][isol-1]))
        irel = np.digitize(relazi, self.r_0v.grid[0])
        drel = ((self.r_0v.grid[0][irel] - relazi) /
                (self.r_0v.grid[0][irel] - self.r_0v.grid[0][irel-1]))
        isatsol = np.digitize(satzen, self.r_0v.grid[2])
        dsatsol = ((self.r_0v.grid[2][isatsol] - satzen) /
                (self.r_0v.grid[2][isatsol] - self.r_0v.grid[2][isatsol-1]))

        ch = [i in channels for i in self.channels]

        out = dict(
            t_dv=(
                (1-dsat) * self.t_dv.values[isat,:,:,ch].T +
                dsat * self.t_dv.values[isat-1,:,:,ch].T),
            t_dd=self.t_dd.values[:,:,ch],
            r_dv=(
                (1-dsat) * self.r_dv.values[isat,:,:,ch].T +
                dsat * self.r_dv.values[isat-1,:,:,ch].T),
            r_dd=self.r_dd.values[:,:,ch],
            ext=self.ext.values[:,ch],
            ext_ratio=self.ext_ratio.values[:,ch],
        )

        # Transposes enable broadcasting with 1D arrays but putting channel dimension last
        vi = np.array([np.argmax(self.channels[self.solar] == i)
                       for i, solar in zip(channels, self.solar[ch]) if solar])
        if vi.size > 0:
            dsat_, dsol_, drel_, dsatsol_, isat_, isol_, irel_, isatsol_ = map(
                lambda arr: arr[self.solar[ch]],
                (dsat, dsol, drel, dsatsol, isat, isol, irel, isatsol))
            out["r_0v"] = (
                (1-dsat_) * (1-dsol_) * (1-drel_) * self.r_0v.values[irel_,isat_,isol_,:,:,vi].T +
                dsat_ * (1-dsol_) * (1-drel_) * self.r_0v.values[irel_,isat_-1,isol_,:,:,vi].T +
                (1-dsat_) * dsol_ * (1-drel_) * self.r_0v.values[irel_,isat_,isol_-1,:,:,vi].T +
                dsat_ * dsol_ * (1-drel_) * self.r_0v.values[irel_,isat_-1,isol_-1,:,:,vi].T +
                (1-dsat_) * (1-dsol_) * drel_ * self.r_0v.values[irel_-1,isat_,isol_,:,:,vi].T +
                dsat_ * (1-dsol_) * drel_ * self.r_0v.values[irel_-1,isat_-1,isol_,:,:,vi].T +
                (1-dsat_) * dsol_ * drel_ * self.r_0v.values[irel_-1,isat_,isol_-1,:,:,vi].T +
                dsat_ * dsol_ * drel_ * self.r_0v.values[irel_-1,isat_-1,isol_-1,:,:,vi].T)
            out["r_0d"] = (
                (1-dsol_) * self.r_0d.values[isol_,:,:,vi].T +
                dsol_ * self.r_0d.values[isol_-1,:,:,vi].T)
            out["t_0d"] = (
                (1-dsol_) * self.t_0d.values[isol_,:,:,vi].T +
                dsol_ * self.t_0d.values[isol_-1,:,:,vi].T)
            out["t_00"] = (
                (1-dsol_) * self.t_00.values[isol_,:,:,vi] .T+
                dsol_ * self.t_00.values[isol_-1,:,:,vi].T)
            out["t_vd"] = (
                (1-dsatsol_) * self.t_0d.values[isatsol_,:,:,vi].T +
                dsatsol_ * self.t_0d.values[isatsol_-1,:,:,vi].T)
            out["t_vv"] = (
                (1-dsatsol_) * self.t_00.values[isatsol_,:,:,vi].T +
                dsatsol_ * self.t_00.values[isatsol_-1,:,:,vi].T)

        ir = np.array([np.argmax(self.channels[self.thermal] == i)
                       for i, thermal in zip(channels, self.thermal[ch])
                       if thermal])
        if ir.size > 0:
            dsat_, isat_ = map(lambda arr: arr[self.thermal[ch]], (dsat, isat))
            out["e_md"] = (
                (1-dsat_) * self.e_md.values[isat_,:,:,ir].T +
                dsat_ * self.e_md.values[isat_-1,:,:,ir].T)

        for key, val in out.items():
            if key not in ("t_dd", "r_dd", "ext", "ext_ratio"):
                # Flip od/ref axis the right way around
                out[key] = np.moveaxis(val, 0, 1)

        return out

    def state_mesh(self):
        return np.meshgrid(_edges_from_points(self.r_dd.grid[1]),
                           _edges_from_points(10.0**self.r_dd.grid[0]))

    def uncertainty(self, pixel, gain, alwaysthermal=False, maxsolzen=75.,
                    thermal_nehomog=0.50, solar_nehomog=0.01,
                    thermal_necoreg=0.15, solar_necoreg=0.02):
        """Calculate measurement variance for a pixel the old way

        Args:
        pixel: SPixel instance to evaluate
        gain: Gain for visible channels (e.g. pre["msi/cal_data"])
        alwaysthermal: Include therm homog/coreg errors during the day
        maxsolzen: Solar zenith beyond which pixel is considered twilight
        thermal_nehomog: homog error for thermal chs; default 0.50 K
        solar_nehomog: homog error for solar chs; default 0.01
        thermal_necoreg: coreg error for thermal chs; default 0.15 K
        solar_necoreg: coreg error for solar chs; default 0.02
        """
        from datetime import datetime

        date_into_year = pixel.time - datetime(pixel.time.year-1, 12, 31)
        sol_const = self.solar_constant(date_into_year.days)
        _, dr_dt0 = self.temp2rad(self.t0)

        out = []
        for ch in pixel.iter_channels():
            i = np.argmax(ch["channel"] == pixel.channels)
            j = np.argmax(ch["channel"] == self.channels[self.solar])
            k = np.argmax(ch["channel"] == self.channels[self.thermal])

            day = pixel.solzen[i] < maxsolzen

            # Measurement uncertainty
            if ch["thermal"]:
                # Scale uncertainty with local gradients of Planck function
                _, dr_dtm = self.temp2rad(ch["meas"])
                sy = (self.nedt[k] * dr_dt0[k] / dr_dtm[k])**2
            else:
                # Convert reflectance to radiance
                fac = np.cos(np.radians(pixel.solzen[ch["index"]])) * sol_const[j] / np.pi
                if self.snr is not None:
                    # Combine SNR and counting error
                    dLx2 = (ch["meas"] * fac / self.snr[j])**2 + gain[j]**2 / 6.
                else:
                    # Polynomial error model
                    dLx2 = 0.
                    for term in self.ru2:
                        dLx2 *= ch["meas"] * fac
                        dLx2 += term[j]
                sy = dLx2 / fac**2

            # Homog uncertainty
            if ch["thermal"] and (alwaysthermal or ch["solar"] or not day):
                sy += thermal_nehomog**2
            if ch["solar"] and day:
                if ch["thermal"]:
                    # _, dr_dtm = self.temp2rad(ch["meas"])
                    radiance = sol_const[j] / dr_dtm[k]
                else:
                    radiance = ch["meas"]
                sy += (solar_nehomog * radiance)**2
            # Coreg uncertainy
            if ch["thermal"] and (alwaysthermal or ch["solar"] or not day):
                sy += thermal_necoreg**2
            if ch["solar"] and day:
                if ch["thermal"]:
                    # _, dr_dtm = self.temp2rad(ch["meas"])
                    radiance = sol_const[j] / dr_dtm[k]
                else:
                    radiance = ch["meas"]
                sy += (solar_necoreg * radiance)**2
            out.append(sy)

        return np.array(out)


def _orac_ch_num_to_sensor_ch_num(sensor, chan_num):
    """Convert ORAC channel number into a sensor channel number"""

    if sensor.startswith("AVHRR") and chan_num > 2:
        # AVHRR has some bespoke channel numbers
        if chan_num == 3:
            return "Ch3a"
        if chan_num == 4:
            return "Ch3b"
        chan_num = chan_num-1

    return "Ch{:0d}".format(chan_num)


def _sensor_ch_num_to_orac_ch_num(sensor, chan_num):
    """Convert sensor channel number into an ORAC channel number"""

    # AVHRR has some bespoke channel numbers
    if chan_num == "Ch3a":
        return 3
    if chan_num == "Ch3b":
        return 4

    number = int(chan_num[2:])
    if sensor.startswith("AVHRR") and number > 2:
        return number+1
    return number


def read_orac_chan_file(filename):
    """Parse an ORAC channel description text file"""
    def clean_line(unit):
        line = next(unit)
        parts = line.split("%")
        return parts[0].strip()

    with open(filename) as lut_file:
        result = dict(
            name=clean_line(lut_file),
            description=clean_line(lut_file),
            file_id=clean_line(lut_file),
            wavenumber=float(clean_line(lut_file)),
            thermal=bool(int(clean_line(lut_file)))
        )
        if result["thermal"]:
            result["ir"] = dict(
                b1=float(clean_line(lut_file)),
                b2=float(clean_line(lut_file)),
                t1=float(clean_line(lut_file)),
                t2=float(clean_line(lut_file)),
                ir_nehomog=np.fromstring(clean_line(lut_file), sep=","),
                ir_necoreg=np.fromstring(clean_line(lut_file), sep=","),
                nebt=float(clean_line(lut_file))
            )

        result["solar"] = bool(int(clean_line(lut_file)))
        if result["solar"]:
            result["vis"] = dict(
                f01=np.fromstring(clean_line(lut_file), sep=","),
                vis_nehomog=np.fromstring(clean_line(lut_file), sep=",") * 1e-2,
                vis_necoreg=np.fromstring(clean_line(lut_file), sep=",") * 1e-2,
                nedr=float(clean_line(lut_file)),
                rs=np.fromstring(clean_line(lut_file), sep=",") * 1e-2
            )
            result["vis"]["nedr"] /= result["vis"]["f01"][0]

    return result


def read_orac_text_lut(filename):
    """Open an ORAC text look-up table as a numpy array

    Reads contents into string, then parses with np.fromstring
    as we don't know if the file contains 1 or 2 tables before
    reading and they don't always end with an even number of
    'columns'"""

    naxes = []
    daxes = []
    axes = []
    contents = None
    # Parse header manually
    with open(filename) as lut_file:
        for i, line in enumerate(lut_file):
             if contents is None:
                parts = line.split()
                if i == 0 and len(parts) == 1:
                    # First line (which BextRat files don't have)
                    wvl = float(line)
                elif len(parts) == 2:
                    # Axis definition
                    naxes.append(int(parts[0]))
                    daxes.append(float(parts[1]))
                    axes.append([])
                elif len(axes[-1]) != naxes[-1]:
                    # Elements of an axis
                    axes[-1].extend(map(float, parts))
                else:
                    # Contents of the file
                    contents = line
             else:
                contents += " "
                contents += line

    naxes = np.fromiter(reversed(naxes), int)
    daxes = np.fromiter(reversed(daxes), float)
    axes = [np.array(s) for s in reversed(axes)]

    # Tables all scalled by factor 100
    lut = np.fromstring(contents, sep=" ") * 0.01
    n = np.prod(naxes).astype(int)
    if lut.size == n:
        tables = (lut.reshape(naxes), )
    else:
        m = np.prod(naxes[::2]).astype(int)
        tables = (lut[:-m].reshape(naxes),
                  lut[-m:].reshape(naxes[::2]))

    return tables, axes, naxes, daxes


def _edges_from_points(z):
    return np.r_[z[0], 0.5*(z[:-1] + z[1:]), z[-1]]
