"""mappable) Routines to plot global data, mainly satellite swaths, on a map.
#
# 18 Jan 2017, ACP: Initial version
# 12 Apr 2017, ACP: Move to object that holds data
"""

import numpy as np


class Mappable(object):
    """A wrapper for data fields that share a lat.lon coordinate system. map()
    is a wrapper for matplotlib.pcolormesh while scatter() wraps
    matplotlib.scatter.

    __init__(lat, lon, central_longitude=0., nvector=False, <name>=None, ...):
        lat (:obj:`ndarray`): Array specifying latitude at cell centre. If 1D,
            a regular grid will be assumed.
        lon (:obj:`ndarray`): Array specifying longitude at cell centre. Must
            have the same shape as lat.
        central_longitude (float): Middle of the coordinate transform. Mostly
            used for orbits at cross the dateline, where 180. is set. Ignored
            if nvector=True and for regular grids (1D inputs).
        nvector (bool): If true, grid corners are deduced using the nvector
            package (very slow). Ignored for regular grids (1D inputs).
        <name> (:obj:`ndarray`): Any remaining keywords are assumed to be data.
            these will be added as properties of the object with the given name.

    Plotting functions:
        transform (cartopy projection): Default PlateCarree(central_longitude).
        mask (bool ndarray): Data to not plot.
        minimise (bool): If True (the default), masks all data outside of the
            extent of the axes to increase plotting speed.
        slices (:obj:`slice`): Slice for all arrays (except data).
    """

    def __init__(self, lat, lon, **kwargs):
        from cartopy.crs import PlateCarree

        # Evaluate arguments
        central_longitude = kwargs.pop('central_longitude', 0.)
        nvector = kwargs.pop("nvector", False)

        self.lat = lat
        self.lon = lon
        for key, val in kwargs.items():
            self.__dict__[key] = val

        if len(lat.shape) == 1:
            self.crnrlon, self.crnrlat = grid_cell_corners(lat, lon)
            self.transform = PlateCarree(central_longitude=0.)
            self._cent_lon = None
            self.nvector = False
        elif nvector:
            assert lat.shape == lon.shape
            self.crnrlat, self.crnrlon = nvector_cell_corners(lat, lon)
            self.transform = PlateCarree(central_longitude=0.)
            self._cent_lon = None
            self.nvector = True
        else:
            assert lat.shape == lon.shape
            self.crnrlat, self.crnrlon = linear_cell_corners(lat, lon,
                                                             central_longitude)
            self.transform = PlateCarree(central_longitude=central_longitude)
            self._cent_lon = central_longitude
            self.nvector = False

    @property
    def shape(self):
        """Shape of arrays"""
        return self.lat.shape

    @property
    def size(self):
        """Number of data points"""
        num = 1
        for dim in self.shape:
            num *= dim
        return num

    @property
    def central_longitude(self):
        """Centre of map projection."""
        try:
            return self._cent_lon
        except AttributeError:
            self._cent_lon = None
            return self._cent_lon

    @central_longitude.setter
    def central_longitude(self, central_longitude):
        from cartopy.crs import PlateCarree

        # Inform user of stupidity
        if len(self.lat.shape) == 1:
            raise TypeError("central_longitude is not compatible with "
                            "a regular lat/lon grid.")
        try:
            if self._cent_lon == central_longitude:
                # No need to change the cell corners
                return
        except AttributeError:
            pass
        try:
            if self.nvector:
                self.nvector = False
                raise UserWarning("Setting central_longitude overwrites nvector")
        except AttributeError:
            self.nvector = False

        # Recalculate cell corners
        self._cent_lon = central_longitude
        self.transform = PlateCarree(central_longitude=central_longitude)
        self.crnrlat, self.crnrlon = linear_cell_corners(self.lat, self.lon,
                                                         central_longitude)

    # Plotting functions
    def _plot_init(self, ax, data, kwargs):
        try:
            data_mask = data.mask
        except AttributeError:
            data_mask = np.full(data.shape, False, dtype=bool)

        kwargs.setdefault('transform', self.transform)
        kwargs.setdefault('rasterized', True)

        # Mask points outside of the defined extent to save time
        sl = kwargs.pop("slices", (slice(None), slice(None)))
        minimise = kwargs.pop('minimise', True)
        if minimise:
            # Mask any data outside of the defined extent
            extent = ax.get_extent()
            try:
                data_mask = np.logical_or(
                    data_mask, np.logical_or(
                        self.lon[sl] < extent[0], np.logical_or(
                            self.lon[sl] > extent[1], np.logical_or(
                                self.lat[sl] < extent[2],
                                self.lat[sl] > extent[3]
                            ))))
            except IndexError:
                # 1D arrays
                filt = np.logical_or(self.lon[sl[0]] < extent[0],
                                     self.lon[sl[0]] > extent[1])
                data_mask[:, filt] = True
                filt = np.logical_or(self.lat[sl[1]] < extent[2],
                                     self.lat[sl[1]] > extent[3])
                data_mask[filt, :] = True

        try:
            initial_mask = kwargs.pop('mask')
            if initial_mask.shape == data.shape:
                mask = np.ma.mask_or(data_mask, initial_mask)
            else:
                mask = np.ma.mask_or(data_mask, initial_mask[sl])
        except KeyError:
            mask = data_mask

        return np.ma.masked_where(mask, data), sl

    def map(self, ax, data, **kwargs):
        """Plot field on map projection using pcolormesh()."""
        masked, sl_orig = self._plot_init(ax, data, kwargs)
        # Increment slices for corners
        sl = tuple(slice(s.start, s.stop + 1)
                   if s.start is not None else slice(None) for s in sl_orig)
        im = ax.pcolormesh(self.crnrlon[sl], self.crnrlat[sl], masked, **kwargs)
        return im

    def scatter(self, ax, data, **kwargs):
        """Plot field on map projection using scatter()."""
        masked, sl = self._plot_init(ax, data, kwargs)
        kwargs['c'] = masked

        kwargs.setdefault('edgecolor', 'none')
        kwargs.setdefault('s', 1)
        im = ax.scatter(self.lon[sl], self.lat[sl], **kwargs)
        return im

    def false(self, ax, data, **kwargs):
        """Plot field on map projection as false-colour pcolormesh()."""
        order = kwargs.pop('order', (0, 1, 2))
        hide_outliers = kwargs.pop('hide_outliers', False)
        vmin = kwargs.pop('vmin', 0.)
        vmax = kwargs.pop('vmax', 1.)

        assert len(order) == 3

        # Add alpha dimension
        order = order + (0,)

        # Applying scaling
        colour = (data[order, ...] - vmin) / (vmax - vmin)
        if hide_outliers:
            colour[colour < 0.] = np.ma.masked
            colour[colour > 1.] = np.ma.masked
        else:
            try:
                colour[(colour < 0.).filled(False)] = 0.
                colour[(colour > 1.).filled(False)] = 1.
            except KeyError:
                colour[colour < 0.] = 0.
                colour[colour > 1.] = 1.

        # Collapsing the color dimension deals with all of their masks
        masked, sl_orig = self._plot_init(ax, colour.sum(axis=0), kwargs)
        if masked.mask.all():
            raise ValueError("No data to plot.")
        # Apply mask as the alpha dimension
        colour[3, ...] = np.where(masked.mask, 0, 1)
        kwargs["color"] = colour.reshape((4, masked.size)).T

        # Increment slices for corners
        sl = tuple(slice(s.start, s.stop + 1)
                   if s.start is not None else slice(None) for s in sl_orig)
        im = ax.pcolormesh(self.crnrlon[sl], self.crnrlat[sl], masked, **kwargs)
        return im


# --- CELL CORNER CALCULATIONS ----------------------------------------------

def grid_cell_corners(lat, lon):
    """
    Given the lat/lon of a regular 2D grid, calculate the coordinates of the
    cell corners.
    """

    def make_crnr(arr):
        """Perform operation."""
        crnr = np.empty(len(arr) + 1)
        crnr[:-2] = 1.5 * arr[:-1] - 0.5 * arr[1:]
        crnr[-2] = 0.5 * arr[-2] + 0.5 * arr[-1]
        crnr[-1] = -0.5 * arr[-2] + 1.5 * arr[-1]
        return crnr

    return np.meshgrid(make_crnr(lon), make_crnr(lat))


def linear_cell_corners(lat, lon, central_longitude=0.):
    """
    Given the central lat/lon of a 2D grid of pixels, calculates the
    lat/lon coordinates of the corners around the cells. (The result
    will be an array one larger than the input in each direction). Uses
    linear interpolation. If the field crosses the dateline, use the
    central_longitude argument to map to a different central point.

    Firstly, the input arrays are extended by one from each side. The
    new points are extrapolated along a geocidic line from the grid. The
    corners are extrapolated from the new edges. Secondly, each set of
    four adjacent points are averaged to find their centre. To
    illustrate,

    u<-a--b  c--d->v
    ^  ^  ^  ^  ^  ^
    |  |  |  |  |  |
    w<-A--B  C--D->x
    |  |  |  |  |  |
    y<-E--F  G--H->z

    A-H are the original data points. Point a is extrapolated from the
    line connecting A and E while point w comes from AB. Similarly, b is
    from BF and y is from ER. Point u is then extrapolated from the
    intersection of lines ab and wy.
    """

    def extend_grid(g):
        """Extend lat/lon grid one pixel in all directions."""
        out = np.empty((g.shape[0] + 2, g.shape[1] + 2))

        out[1:-1, 1:-1] = g
        out[1:-1, 0] = 2. * g[:, 0] - g[:, 1]
        out[1:-1, -1] = 2. * g[:, -1] - g[:, -2]
        out[0, 1:-1] = 2. * g[0, :] - g[1, :]
        out[-1, 1:-1] = 2. * g[-1, :] - g[-2, :]
        out[0, 0] = 4. * g[0, 0] - 2. * (g[0, 1] + g[1, 0]) + g[1, 1]
        out[-1, 0] = 4. * g[-1, 0] - 2. * (g[-1, 1] + g[-2, 0]) + g[-2, 1]
        out[0, -1] = 4. * g[0, -1] - 2. * (g[0, -2] + g[1, -1]) + g[1, -2]
        out[-1, -1] = 4. * g[-1, -1] - 2. * (g[-1, -2] + g[-2, -1]) + g[-2, -2]

        return out

    def grid_corners(g):
        """Find corners from grid cell centres."""
        ex = extend_grid(g)
        crnr = (ex[0:-1, 0:-1] + ex[0:-1, 1:] + ex[1:, 0:-1] +
                ex[1:, 1:]) / 4.
        return crnr

    lon_transform = lon.copy()
    lon_transform -= central_longitude
    lon_transform[lon_transform > 180.] -= 360.
    lon_transform[lon_transform < -180.] += 360.

    return grid_corners(lat), grid_corners(lon_transform) + central_longitude


def nvector_cell_corners(lat, lon):
    """
    Given the central lat/lon of a 2D grid of pixels, calculates the
    lat/lon coordinates of the corners around the cells. (The result
    will be an array one larger than the input in each direction). Uses
    the nvector module to perform interpolation.

    Firstly, the input arrays are extended by one from each side. The
    new points are extrapolated along a geocidic line from the grid. The
    corners are extrapolated from the new edges. Secondly, each set of
    four adjacent points are averaged to find their centre. To
    illustrate,

    u<-a--b  c--d->v
    ^  ^  ^  ^  ^  ^
    |  |  |  |  |  |
    w<-A--B  C--D->x
    |  |  |  |  |  |
    y<-E--F  G--H->z

    A-H are the original data points. Point a is extrapolated from the
    line connecting A and E while point w comes from AB. Similarly, b is
    from BF and y is from ER. Point u is then extrapolated from the
    intersection of lines ab and wy.
    """
    import nvector as nv
    wgs84 = nv.FrameE(name='WGS84')

    op_flags = [['writeonly'], ['readonly'], ['readonly']]

    # Build a larger array and convert all coords to nvectors
    points = np.empty((lon.shape[0] + 2, lon.shape[1] + 2), dtype=nv.Nvector)
    for point, ln, lt in np.nditer([points[1:-1, 1:-1], lon, lat],
                                   flags=['refs_ok'], op_flags=op_flags):
        point[...] = wgs84.GeoPoint(lt, ln, degrees=True).to_nvector()

    def extrap_edge(edge_line, near_line, far_line):
        """Extrapolate points to edges of grid."""
        for edge, near, far in np.nditer([edge_line, near_line, far_line],
                                         flags=['refs_ok'],
                                         op_flags=op_flags):
            path = nv.GeoPath(far[()], near[()])
            edge[()] = path.interpolate(2.)

    extrap_edge(points[1:-1, 0], points[1:-1, 1], points[1:-1, 2])
    extrap_edge(points[1:-1, -1], points[1:-1, -2], points[1:-1, -3])
    extrap_edge(points[0, 1:-1], points[1, 1:-1], points[2, 1:-1])
    extrap_edge(points[-1, 1:-1], points[-2, 1:-1], points[-3, 1:-1])

    def extrap_corner(horz_near, horz_far, vert_near, vert_far):
        """Extrapolate grid corners to the edges of the grid."""
        horz_path = nv.GeoPath(horz_far, horz_near)
        vert_path = nv.GeoPath(vert_far, vert_near)
        return horz_path.intersection(vert_path).to_nvector()

    points[0, 0] = extrap_corner(points[1, 0], points[2, 0],
                                 points[0, 1], points[0, 2])
    points[-1, 0] = extrap_corner(points[-2, 0], points[-3, 0],
                                  points[-1, 1], points[-1, 2])
    points[0, -1] = extrap_corner(points[1, -1], points[2, -1],
                                  points[0, -2], points[0, -3])
    points[-1, -1] = extrap_corner(points[-2, -1], points[-3, -1],
                                   points[-1, -2], points[-1, -3])

    # Form output arrays
    shape = (lon.shape[0] + 1, lon.shape[1] + 1)
    crnrlon = np.empty(shape)
    crnrlat = np.empty(shape)

    # GeoPoints/Nvectors aren't easily merged so make a container
    nvecs = wgs84.Nvector([[np.ones(4)], [np.zeros(4)], [np.zeros(4)]])

    # Corners are the midpoint of each 2x2 box of points
    for (i, j), _ in np.ndenumerate(crnrlon):
        # Copy 2x2 box of points into container (it's a list of arrays)
        for k in range(3):
            nvecs.normal[k] = [pnt.normal[k]
                               for pnt in points[i:i + 2, j:j + 2].flat]

        # Use nvector to find average of each square
        mean = nvecs.mean_horizontal_position().to_geo_point()
        crnrlon[i, j] = mean.longitude_deg
        crnrlat[i, j] = mean.latitude_deg

    return crnrlat, crnrlon


# --- SINUSOIDAL GRID---------- ----------------------------------------------
# Derived from def_sinusoidal.pro
class SinGrid(object):
    r"""Mapping to and from a sinusoidal grid.

    The equations of the mapping are,
    $\begin{align}
        u &= \frac{N}{2} \left( \frac{\lambda}{\pi} \cos \phi + 1 \right) \\
        v &= \frac{N}{2} \left( \frac{\phi}{2} + \frac{1}{2} \right)
    \end{align}$

    Methods:
        __init__: Defines the sinusoidal grid.
        to_sin: Convert lat/lon into sinusoidal coordinates.
        to_rect: Convert sinusoidal coordinates into lat/lon.
        to_1d: Convert int u,v pairs into scalar coordinates.
        to_2d: Convert scalar coordinates in u,v pairs.

    Attributes:
        half_n (float): Half the number of pixels along the equator.
        min_u (int array): When indexed with v, gives the minmal
            possible value of u.
        max_u (int array): When indexed with v, gives the maximal
            possible value of u.
        cumulative (int array): When indexed with v, gives the number
            of sinusoidal grid cells with lesser v. Used to determine
            the 1D coordinate.
    """

    def __init__(
            self, n_equator=4008, resolution=None, radius=6378.137, offset=(0., 0.)
    ):
        """Define the sinusoidal grid.

        Args:
            n_equator (int): Number of pixels along the longest line.
                Default 4008, an approximately 10 km grid.
            resolution (float): Maximal width of a grid cell, in km. If
                set, half_n is ignored.
            radius (float): Radius of the (assumed spherical) Earth in
                km. Used with resolution to determine half_n.
                Default 6,378.137 km.
        """
        from numpy import pi, cos, floor, ceil, arange, cumsum, \
            insert

        if resolution is None:
            self.n_equator = int(n_equator)
            self.half_n = n_equator / 2.
        else:
            self.half_n = ceil(2. * pi * radius / resolution) / 2.
            self.n_equator = int(self.half_n * 2)

        # Set up the 1D coordinate system
        v = arange(self.half_n)
        cos_phi = cos(pi * (v / self.half_n - 0.5))
        self.min_u = floor(self.half_n * (1. - cos_phi)).astype("int32")
        self.max_u = ceil(self.half_n * (1. + cos_phi)).astype("int32")
        self.cumulative = insert(
            cumsum(self.max_u - self.min_u + 1), 0, 0
        )
        self.u0 = offset[0]
        self.v0 = offset[1]

    def to_sin(self, lat, lon, floating=False):
        """Convert lat/lon, in degrees, to sinusoidal coordinates.

        Args:
            lat (float): Latitude, in degrees.
            lon (float): Longitude, in degrees.
            floating (bool): If true, the routine returns the exact
                floating point coordinates rather than rounding down
                to ints, the default behaviour.
        """
        from numpy import pi, radians, cos

        phi = radians(lat)
        lam = radians(lon)
        try:
            # Arrays
            lam[lam >= pi] -= 2. * pi
            lam[lam < -pi] += 2. * pi
        except TypeError:
            # Scalars
            if lam >= pi:
                lam -= 2. * pi
            elif lam < -pi:
                lam += 2. * pi

        u = self.half_n * (lam / pi * cos(phi) + 1.) + self.u0
        v = self.half_n * (phi / pi + 0.5) + self.v0

        if floating:
            return u, v

        try:
            return int(u), int(v)
        except TypeError:
            # If we were passed numpy arrays
            return u.astype(int), v.astype(int)

    def to_rect(self, u, v):
        """Convert sinusoidal coordiantes into lat/lon, in degrees."""
        from numpy import pi, degrees, cos

        phi = ((v - self.v0) / self.half_n - 0.5) * pi
        lam = ((u - self.u0) / self.half_n - 1.) * pi / cos(phi)

        return degrees(phi), degrees(lam)

    def to_1d(self, u_s, v_s):
        """Convert integer u,v pairs into scalar coordinates."""

        def _to_1d(u, v):
            try:
                result = self.cumulative[v] + u - self.min_u[v]
            except IndexError:
                raise ValueError("v out of range ({})".format(v))

            if u < self.min_u[v] or u > self.max_u[v]:
                raise ValueError("u out of range ({})".format(u))

            return result

        try:
            # If something iterable, return same type of iterable
            return type(u_s)([
                _to_1d(u, v) for u, v in zip(u_s, v_s)
            ])
        except TypeError:
            # Scalars
            return _to_1d(u_s, v_s)

    def to_2d(self, coords):
        """Convert an integer scalar coordinate into a u,v pair."""

        def _to_2d(coord):
            v = np.argmax(coord < self.cumulative) - 1
            u = coord - self.cumulative[v] + self.min_u[v]
            return u, v

        try:
            return type(coords)([
                _to_2d(coord) for coord in coords
            ])
        except TypeError:
            return _to_2d(coords)
