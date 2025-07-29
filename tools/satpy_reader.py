"""
*** ONLY SUITABLE FOR USE WITH FCI FOR NOW ***
Uses satpy to read in the satellite data from its native format and convert it into a .nc file that ORAC's preprocessor can handle.
NB// As (for now) there is no ability to subset data that is in the PYTHON format within ORAC's preprocessing, any subsetting must happen here.
     In addition, due to how satpy works, it assumes the directory will only contain a single granule's worth of files.

History:
! 2024/10/04, DR: First version.
! 2024/10/15, DR: Added subsetting in ORAC style and corrected VNIR channels from % reflectance to fractional.
! 2024/10/16, DR: Fixed relazi angle to match SEVIRI
! 2025/07/04. DR: Modified channel support to select subset of channels
"""


import os, sys, click
# os.system('echo "### $(date -u) ### Importing numpy..."')
import numpy as np
# os.system('echo "### $(date -u) ### Importing xarray..."')
import xarray as xr
# os.system('echo "### $(date -u) ### Importing datetime..."')
from datetime import datetime as dt, timedelta
# os.system('echo "### $(date -u) ### Importing pyorbital..."')
from pyorbital.orbital import get_observer_look
from pyorbital.astronomy import get_alt_az
# os.system('echo "### $(date -u) ### Importing satpy..."')
from satpy import Scene, find_files_and_readers
# Satpy wants to tell you everything that's happening by default; raise logging level to ERROR for now
# os.system('echo "### $(date -u) ### Adjusting settings..."')
import logging
logging.getLogger('satpy').setLevel(logging.ERROR)
# Suppress warnings too; this is a bad practice, but I find them very annoying (DJVR)
import warnings
warnings.filterwarnings("ignore")
# # Dask is odd; we have memory overflow when carrying out compute?
import dask
dask.config.set({"array.chunk-size": "1GiB"})
dask.config.set(scheduler='single-threaded')


# Due to some satellites having L1 data spread across multiple files or as a single file,
# we need to define the list of supported imagers and their relavent metadata here.
supported_sensors = { # dict of sensors, whether they're multi-file or single file and extension for top-level dir if multi-file (only a problem for Sentinel), e.g:
    # '<sensor_name_from_satpy>': {'multifile?': <bool_type>, 'extension': <None_or_str_type>}
    'fci': {'multifile?': True, 'extension': None, 'reader': 'fci_l1c_nc'}
} 


def read_sat_data(fname, sensor = 'fci', x0 = None, x1 = None, y0 = None, y1 = None, use_channels = None, high_res = False):
    '''
    Reads in satellite data from the path provided by <fname> using satpy and converts it into netCDF format.
    '''
    try:
        base_dir = os.path.dirname(fname) if '.' in  fname else fname # fname can be a directory of files, e.g. FCI, AHI, SLSTR, or a single file, e.g. SEVIRI
        fnames = find_files_and_readers(
            base_dir = base_dir,
            reader = supported_sensors[sensor]['reader']
        )
    except Exception as e:
        raise Exception(
            '''Files not found for sensor %s in %s
            \tSupported sensors: %s''' % (
                sensor,
                base_dir,
                str(list(supported_sensors.keys()))
            )
        )
    # Assuming we've found the file(s)
    os.system('echo "### $(date -u) ### Loading Scene..."')
    sat_data = Scene(fnames)
    # Load in the channels; there are a lot of datasets usually!
    # print(sat_data.available_dataset_names())
    if sensor == 'fci':
        channels = [
            dst_name
            for dst_name
            in sat_data.available_dataset_names() 
            if len(dst_name.split('_')) == 2
        ]
    else:
        raise Exception('Sensor %s not currently supported' % sensor)
    # Sort the channels by wavelength or channel number
    channels = sorted(zip([int(channel.split('_')[1]) for channel in channels], channels))
    channels = [_[-1] for _ in channels]
    print(channels)
    if use_channels is not None: # This should allow for selection of channels based on channel number
        channels = [channel for n, channel in enumerate(channels) if n+1 in use_channels] # We assume that use_channels is a list of integers using 1-indexing
    print(channels)
    # os.system('echo "### $(date -u) ### Loading channels..."')
    sat_data.load(channels)
    # Sort the channels to be in the correct order by central wavelength
    channels = sorted(zip([sat_data[channel].wavelength.central for channel in channels], channels))
    channels = [_[-1] for _ in channels]
    sat_data.load([channels[-1]+'_time'])
    sat_data_original = sat_data
    # Set everything to the coarsest area for now; in theory, we can use the higher res if we want
    os.system('echo "### $(date -u) ### Resampling..."')
    if not high_res:
        sat_data = sat_data.resample(sat_data.coarsest_area())
    else:
        sat_data = sat_data.resample(sat_data.finest_area())
    os.system('echo "### $(date -u) ### Assigning actual granuale dimensions and subsetting if necessary..."')
    # Define global coords, i.e. x and y
    # DR: I believe this should be done on the basis of Fortran indexing, i.e. from 1, not 0
    # We also need to have this set based on relative position in the whole image, i.e. we
    # can't do this after subsetting the whole image and need to make sure the 1-indexed x 
    # and y are carried forward to the final product
    y, x = sat_data[channels[-1]].data.shape[0], sat_data[channels[-1]].data.shape[1]
    os.system(f'echo "### $(date -u) ### Across track: {x}    Along track: {y}"')
    y, x = [_ + 1 for _ in range(y)], [_ + 1 for _ in range(x)]
    # Attempt subsecting if requested
    bounds = [x0, x1, y0, y1]
    if not bounds.count(None) == 4: # Only carry out the next step if we actually need to subsect
        # Use ORAC convention of bounds starting from 1 and being inclusive for all bounds
        bounds = [_ - 1 if _ is not None else None for _ in bounds]      
        sat_data = sat_data[bounds[-2]:bounds[-1]+1, bounds[0]:bounds[1]+1]
        x = x[bounds[0]:bounds[1]+1]
        y = y[bounds[-2]:bounds[-1]+1]
    # We need to deal with Dask's laziness before it becomes a problem later
    os.system('echo "### $(date -u) ### Carrying out compute..."')
    sat_data = sat_data.compute()
    # Load in the shared(?) lats, lons, solar angles and viewing angles
    os.system('echo "### $(date -u) ### Calculating angles..."')
    # Take this from the last available channel, as this should be a TIR channel at 2km native res
    lons, lats = sat_data[channels[-1]].area.get_lonlats()
    # Retrieve viewing angle data
    sat_azi, sat_zen = get_observer_look( # NB// sat_zen is actually elevation here; var name is for minimising memory waste
        sat_lon = sat_data[channels[-1]].attrs['orbital_parameters']['satellite_actual_longitude'],
        sat_lat = sat_data[channels[-1]].attrs['orbital_parameters']['satellite_actual_latitude'],
        sat_alt = sat_data[channels[-1]].attrs['orbital_parameters']['satellite_actual_altitude'] / 1000.,
        utc_time = sat_data.start_time + ((sat_data.end_time - sat_data.start_time)/2), # Take the mid-time of the scan of the whole scene
        lon = lons,
        lat = lats,
        alt = 0. # This really should be a DEM assigned to the satellite FOV, but not sure the extra effort will make any real difference
    )
    sat_zen = 90. - sat_zen # Convert elevation to zenith that ORAC needs
    # Load in the per-pixel time from the final channel
    per_pixel_time = sat_data[channels[-1]+'_time'].compute().values
    bad_pixel_times = np.isnan(per_pixel_time)
    per_pixel_time[bad_pixel_times] = 0 # Make the date obviously wrong; we'll mask this later
    per_pixel_time = np.datetime64('2000-01-01T00:00:00') + per_pixel_time.astype('timedelta64[s]') - np.timedelta64(86400, 's')
    # print(per_pixel_time.dtype)
    # Use the per-pixel times to get the solar info
    sol_zen, sol_azi = get_alt_az( # NB// sol_zen is actually elevation here; var name is for minimising memory waste
        utc_time = per_pixel_time,
        lon = lons,
        lat = lats
    )
    sol_azi = np.rad2deg(sol_azi)
    # Fix solar azimuth so it's always positive, i.e. between 0 and 360
    # sol_azi = sol_azi - 180.
    # sol_azi[sol_azi < 0] = sol_azi[sol_azi < 0] + 360
    # Convert elevation to zenith that ORAC needs
    sol_zen = 90. - np.rad2deg(sol_zen) 
    # print(np.nanmin(sol_zen), np.nanmax(sol_zen))
    # Now we have the viewing and solar angles, we can calculate relative azimuth from Adam's method:
    # https://eodg.atm.ox.ac.uk/eodg/gray/2020Povey1.pdf#:~:text=This%20notebook%20is%20intended%20to%20plot%20the
    dummy_sol_azi = sol_azi.copy()
    dummy_sol_azi[sol_azi < 0] = sol_azi[sol_azi < 0] + 360
    rel_azi = np.abs(sat_azi - dummy_sol_azi)
    print(np.nanmin(rel_azi), np.nanmax(rel_azi))
    where_too_large = rel_azi > 180
    # Clean it up
    rel_azi[where_too_large] = 360 - rel_azi[where_too_large]
    print(np.nanmin(rel_azi), np.nanmax(rel_azi))
    # print(sat_data[channels[-1]].time_parameters['observation_start_time'])
    obs_start_time = sat_data[channels[-1]].time_parameters['observation_start_time']
    obs_end_time = sat_data[channels[-1]].time_parameters['observation_end_time']
    # Define the global vars for use in generating the final .nc file
    _FillValue = -999.
    # Define the channels dimension based on the central wavelengths
    # os.system('echo "### $(date -u) ### Concatenating channels..."')
    channel_dim = [ # Keep central wavelengths to 3 decimal places, i.e. as it is presented in the FCI data guide
        np.round(sat_data[channel].wavelength.central, 3)
        for channel 
        in channels
    ]
    channel_ids = tuple([_ + 1 for _ in range(len(channels))])
    # Channel data; get all the channel data into a single numpy array that we can hand off to xarray
    # New method requires a bit more memory, but should be much faster
    channel_data = np.zeros(
        (
            len(channels), # Make sure the channels dim is set to the number of channels available
            *sat_data[channels[-1]].shape # Sets the y, x dimensions to the right shape
        ), 
        dtype=np.float32
    )
    
    for n, channel in enumerate(channels):
        # os.system('echo "### $(date -u) ### Appending channel %s..."' % channel)
        # Need to double check we are converting to fractional refelctance, rather than %
        channel_data[n, :, :] = sat_data[channel][:,:] if sat_data[channel].units != '%' else sat_data[channel][:,:]/100
    sat_data = None
    channel_data[channel_data == np.inf] = _FillValue
    channel_data[np.isnan(channel_data)] = _FillValue
    # Longitudes and latitudes
    lons[np.isnan(lons)] = _FillValue
    lats[np.isnan(lats)] = _FillValue
    # Viewing angles
    sat_azi[np.isnan(sat_azi)] = _FillValue
    sat_zen[np.isnan(sat_zen)] = _FillValue
    # Solar angles
    sol_azi[np.isnan(sol_azi)] = _FillValue
    sol_zen[np.isnan(sol_zen)] = _FillValue
    rel_azi[np.isnan(rel_azi)] = _FillValue
    # Assign legacy channels for ORAC to be able to read; hard-coded to FCI for now
    channel_ids_default = (3, 4, 7, 9, 14, 15) # 0.6um, 0.8um, 1.6um, 3.9um, 10.8um, 12um
    # Assign the rest of the attributes that ORAC expects
    all_channel_sw_flag = tuple([1. if _ < 4 else 0. for _ in channel_dim])
    all_channel_lw_flag = tuple([1. if _ >= 3.6 else 0. for _ in channel_dim])
    all_channel_ids_rttov_coef_sw = tuple([n + 1. if _ < 4 else 0 for n, _ in enumerate(channel_dim)])
    all_channel_ids_rttov_coef_lw = tuple([n + 1 + 1 - sum(all_channel_sw_flag) if _ >= 3.6 else 0 for n, _ in enumerate(channel_dim)])
    all_map_ids_view_number = tuple([1. for _ in channels])
    all_channel_fractional_uncertainty = tuple([0. for _ in channels])
    all_channel_minimum_uncertainty = tuple([0. for _ in channels])
    all_channel_numerical_uncertainty = tuple([0. for _ in channels])
    all_channel_lnd_uncertainty = tuple([0. for _ in channels])
    all_channel_sea_uncertainty = tuple([0. for _ in channels])
    # We also have hard-coded mappings of the imager bands to MODIS bands
    # This is extremely easy to automate and should have been in the first place;
    # the process is automated here
    ref_bands = {
        'land' : np.array(( # These are MODIS Land bands with wavelengths in um
            0.645,
            0.8585,
            0.469,
            0.555,
            1.24,
            1.64,
            2.13
        )),
        'sea' : np.array(( # From orac/pre_processing/cox_monk_constants.F90
            4.700e-01, 
            5.500e-01, 
            6.500e-01, 
            8.700e-01, 
            1.240e+00, 
            1.375e+00, 
            1.600e+00, 
            2.130e+00, 
            3.700e+00
        )),
        'snow_and_ice' : np.array(( # These are from orac/pre_processing/correct_for_ice_snow.F90:correct_for_ice_snow_nwp
            0.67,
            0.87,
            1.6,
            3.7
        ))
    }
    # In principle, we want to map every channel we can within reason
    # Do this by setting a tolerance we can accept for mappings
    def map_band(channel_wvl, mapping_type='land', tolerance=0.5):
        '''
        Maps an input channel wavelength in um, <channel_wvl>, to the corresponing band
        in <ref_bands> for the <mapping_type>, which can be 'land', 'sea' or 'snow_and_ice'.
        A check is also carried out that the mappping is within tolerance, i.e. within
        <tolerance> um of the central wavelength the <channel_wvl> best matches with. 
        '''
        compare_bands = np.abs(ref_bands[mapping_type] - channel_wvl)
        min_diff = np.nanmin(compare_bands)
        if min_diff > tolerance:
            return 0
        else:
            return list((compare_bands == min_diff).astype('int')).index(1) + 1 # Fortran needs 1-indexing rather than Python's 0-indexing
    all_map_ids_abs_to_ref_band_land = tuple([map_band(_, mapping_type='land') for _ in channel_dim])
    all_map_ids_abs_to_ref_band_sea = tuple([map_band(_, mapping_type='sea') for _ in channel_dim])
    all_map_ids_abs_to_snow_and_ice = tuple([map_band(_, mapping_type='snow_and_ice') for _ in channel_dim])
    # Finally, convert the gregorian datetime (normal) to Julian datetime
    def greg2jd(arr):
        # arr = arr.item()   
        return arr.toordinal() + (arr.hour / 24.0) + (arr.minute / 1440.0) + (arr.second / 86400.0) + 1721425
    greg2jd = np.vectorize(greg2jd)
    per_pixel_time = greg2jd(per_pixel_time.astype(dt))
    per_pixel_time[bad_pixel_times] = _FillValue
    # Now all the data has been cleaned up, we can write the data to a netcdf file that ORAC can read
    # os.system('echo "### $(date -u) ### Converting into netCDF format..."')
    nc_file = xr.Dataset(
        coords = {
            'channels' : channel_dim,
            'ny' : y,
            'nx' : x,
        },
        data_vars = {
            "channel_data" : (
                ['channels', 'ny', 'nx'],
                channel_data.astype(np.float32)
            ),
            "latitude" : (
                ['ny', 'nx'],
                lats.astype(np.float32)  
            ),
            "longitude" : (
                ['ny', 'nx'],
                lons.astype(np.float32) 
            ),
            "relative_azimuth_angle" : (
                ['ny', 'nx'],
                rel_azi.astype(np.float32)  
            ),
            "solar_zenith_angle" : (
                ['ny', 'nx'],
                sol_zen.astype(np.float32)   
            ),
            "solar_azimuth_angle" : (
                ['ny', 'nx'],
                sol_azi.astype(np.float32)   
            ),
            "satellite_zenith_angle" : (
                ['ny', 'nx'],
                sat_zen.astype(np.float32)  
            ),
            "satellite_azimuth_angle" : (
                ['ny', 'nx'],
                sat_azi.astype(np.float32)   
            ),
            "time_data" : (
                ['ny', 'nx'],
                per_pixel_time
            )
        },
        attrs={
            'platform': 'MTG-I1',
            'sensor' : sensor.upper(),
            'start_time' : obs_start_time.strftime('%Y-%m-%dT%H:%M:%S'),
            'end_time' : obs_end_time.strftime('%Y-%m-%dT%H:%M:%S'),
            'max_chan_count': len(channels),
            'all_channel_wl_abs' : np.array(list(channel_dim), dtype=np.float32),
            'channel_ids': np.array(list(channel_ids), dtype=np.int32),
            'channel_ids_default' : np.array(list(channel_ids_default), dtype=np.int32),
            'all_channel_lw_flag' : np.array(list(all_channel_lw_flag), dtype=np.int32),
            'all_channel_sw_flag' : np.array(list(all_channel_sw_flag), dtype=np.int32),
            'all_channel_ids_rttov_coef_sw' : np.array(list(all_channel_ids_rttov_coef_sw), dtype=np.int32), 
            'all_channel_ids_rttov_coef_lw' : np.array(list(all_channel_ids_rttov_coef_lw), dtype=np.int32),
            'all_map_ids_view_number' : np.array(list(all_map_ids_view_number), dtype=np.int32),
            'all_channel_fractional_uncertainty' : np.array(list(all_channel_fractional_uncertainty), dtype=np.float32),
            'all_channel_minimum_uncertainty' : np.array(list(all_channel_minimum_uncertainty), dtype=np.float32),
            'all_channel_numerical_uncertainty' : np.array(list(all_channel_numerical_uncertainty), dtype=np.float32),
            'all_channel_lnd_uncertainty' : np.array(list(all_channel_lnd_uncertainty), dtype=np.float32),
            'all_channel_sea_uncertainty' : np.array(list(all_channel_sea_uncertainty), dtype=np.float32),
            'all_map_ids_abs_to_ref_band_land' : np.array(list(all_map_ids_abs_to_ref_band_land), dtype=np.int32),
            'all_map_ids_abs_to_ref_band_sea' : np.array(list(all_map_ids_abs_to_ref_band_sea), dtype=np.int32),
            'all_map_ids_abs_to_snow_and_ice' : np.array(list(all_map_ids_abs_to_snow_and_ice), dtype=np.int32),
            '_FillValue' : _FillValue
        },
    )
    # Ensure that all the variables have their _FillValue set to the assigned _FillValue
    for var in nc_file.variables.keys():
        nc_file[var].assign_attrs({'_FillValue' : _FillValue})
    return nc_file, base_dir


def write_orac_compatible_file(base_dir, nc_file):
    '''
    Writes out the ORAC compatible file based on the filename, <fname>, given and the final .nc file, <nc_file>.
    '''
    os.system('echo "### $(date -u) ### Writing out .nc file..."')
    ftime = dt.strptime(
        nc_file.start_time,
        '%Y-%m-%dT%H:%M:%S'
    )
    if nc_file.sensor == 'FCI':
        out_fname = 'W_XX-EUMETSAT-Darmstadt,IMG+SAT,MTI1+FCI-1C-RRAD-FDHSI-FD--CHK-BODY---NC4E_C_EUMT_' + \
            ftime.strftime('%Y%m%d%H%M%S') + '.orac-compatible.nc'
    else:
        raise Exception('sensor not supported yet')
    full_fname = os.path.join(
        base_dir,
        out_fname
    )
    if os.path.exists(full_fname):
        os.remove(full_fname)
    nc_file.to_netcdf(full_fname)

@click.command()
@click.option(
    '--fname', '-f',
    prompt = '/full/path/to/filename',
    help = 'The full path to the filename or directory to be processed.'
)
@click.option(
    '--limit', '-l',
    nargs = 4,
    prompt = 'X0 X1 Y0 Y1',
    default = ['None', 'None', 'None', 'None'],
    help = 'First/last pixel in across/along-track directions. FORTRAN 1-INDEXING, NOT PYTHON 0-INDEXING!'
)
@click.option(
    '--high_res',
    prompt = 'True for highest spatial resolution, False for coarsest spatial resolution.',
    default = 'False',
    help = 'Boolean option to choose whether or not to use the highest spatial resolution available or the coarsest (Default).'
)
@click.option(
    '--use_channels',
    prompt = 'A whitespace separated list of channels numbers to be extracted in the preparation. MUST be submitted as a single string. If left empty, will default to using all channels.',
    default = None,
    help = 'A list of channels numbers to be extracted in the preparation. Defaults to using all channels if none specified.'
)
def main(fname, limit, high_res, use_channels):
    '''
    Process the <fname> satellite product.
    '''
    # Parse high_res option
    if high_res.lower() in ['true', 'yes', 'y', '1']:
        high_res = True
    elif high_res.lower() in ['false', 'no', 'n', '0']:
        high_res = False
    else:
        raise Exception('--high_res option must be logical operator, i.e. True or False')
    # Parse spatial limits
    limits = {}
    for (_coord, _limit) in zip(['x0', 'x1', 'y0', 'y1'], limit):
        try:
            if _limit.lower() == 'none':
                limits[_coord] = None
            else:
                _limit = int(_limit)
                if '0' in _coord: # Need to correct for Fortran indexing
                    _limit += 1
                else: # Also need to account for ORAC's inclusion of the end limits
                    _limit += 2
                limits[_coord] = _limit
        except Exception as e:
            raise Exception('Coord limits must either be valid integers for subsecting or None if the start/end column/row is the limit.')
    # Parse channels
    if use_channels is not None:
        use_channels = [int(n) for n in use_channels.split()]
    print(f'Processing for pixels: {limit}')
    # Now we can pass this to the reader
    nc_file, base_dir = read_sat_data(
        fname,
        x0 = limits['x0'],
        x1 = limits['x1'],
        y0 = limits['y0'],
        y1 = limits['y1'],
        high_res = high_res,
        use_channels = use_channels
    )
    write_orac_compatible_file(
        base_dir,
        nc_file
    )


if __name__ == '__main__':
    # os.system('echo "### $(date -u) ### Carrying out main..."')
    main()
    
