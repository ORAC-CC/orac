!*******************************************************************************
!
! Copyright (C) 2000-2018, RAL Space, Science and Technology Facilities Council
! Copyright (C) 2000-2018, University of Oxford
! Copyright (C) 2011-2018, Deutscher Wetterdienst
!
! This file is part of the Optimal Retrieval of Aerosol and Cloud (ORAC).
!
! ORAC is free software: you can redistribute it and/or modify it under the
! terms of the GNU General Public License as published by the Free Software
! Foundation, either version 3 of the License, or (at your option) any later
! version.
!
! ORAC is distributed in the hope that it will be useful, but WITHOUT ANY
! WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
! A PARTICULAR PURPOSE. See the GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License along with
! ORAC. If not, see <http://www.gnu.org/licenses/>.
!
!*******************************************************************************


!-------------------------------------------------------------------------------
! Name: orac_preproc.F90
!
! Purpose:
! Read in data from the variety of files that contain all the information
! necessary to run the ORAC algorithm, remove any unnecessary details (such as
! restricting the data to a particular area of the swath), and write out the
! remainder to a series of NetCDF files. This operates on individual granules
! and/or orbit segments of MODIS, AVHRR, and AATSR to allow for brute force/
! poor man's parallelization by running multiple instances of ORAC
! simultaneously, each ingesting separate files.
!
! Description and Algorithm details:
! 1) Read input parameters from command line (or driver file)
! 2) Determine dimensions of scene, including breaking of AATSR files into more
!    easily processed 'chunks'
! 3) Read in level 1B radiances, brightness temperatures, and geolocation
! 4) Allocate structures to store data
! 5) Identify ECMWF data required
! 6) Set output file names
! 7) Read ecmwf data
! 8) Define processing grid
! 9) Interpolate ecmwf data onto pre processing grid
! 10) Open netcdf output
! 11) RTTOV calculations
! 12) surface calculations
! 13) deallocate structures
!
! Arguments:
! Name             Type In/Out/Both      Description
! ------------------------------------------------------------------------------
! sensor           string in Name of instrument (only AATSR, MODIS or AVHRR are
!                            valid)
! l1b_path_file    string in Full path to level 1B data
! geo_path_file    string in Full path to geolocation data (identical to above
!                            for AATSR)
! nwp_path       string in Folder containing ECMWF files
! rttov_coef_path  string in Folder containing RTTOV coefficient files
! rttov_emiss_path string in Folder containing RTTOV emissivity files
! nise_ice_snow_path
!                  string in Folder containing NISE ice cover files
! modis_albedo_path
!                  string in Folder containing MODIS MCD43C3 albedo files
! cimss_emiss_path string in Folder containing MODIS monthly average emissivity
! dellon           real   in 1 / longitude increment in degrees
! dellat           real   in 1 / latitude increment in degrees
! output_pathin    string in Folder into which outputs should be saved
! startx           int    in First pixel across swath to consider, counting from
!                            1. This and the next four arguments are only
!                            considered if all four are > 0
! endx             int    in Last pixel across swath to consider
! starty           int    in First pixel along swath to consider
! endy             int    in Last pixel along swath to consider
! ncver            string in Version of NetCDF in use. This and all arguments
!                            until exec_time are descriptive information used to
!                            in the headers of the output files
! con              string in The NetCDF convention used
! inst             string in Processing institute
! l2cproc          string in Name of the L2 processor (generally ORAC)
! l2cprocver       string in Version of that processor
! contact          string in Contact email address for these files
! website          string in Web address related to these files
! file_version     string in Version number for these files
! reference        string in A reference (paper, report) for these files
! history          string in Changelog for these files
! summary          string in Brief description of the data
! keywords         string in Short phrases describing the purpose of these files
! comment          string in Any additional information that may be useful
! project          string in Name of the project for which these files were
!                            generated
! license          string in Any necessary licensing information for this data
! uuid_tag         string in A Universally Unique ID for these files
! exec_time        string in Date/time string for when this file was generated
! aatsr_calib_path_file
!                  string in Full path to the AATSR calibration file
! nwp_flag       int    in 0: GRIB ECMWF files; 1: BADC NetCDF ECMWF files;
!                            2: BADC GRIB files.
! nwp_path2      string in Folder containing ECMWF files (?)
! nwp_path3      string in Folder containing ECMWF files (?)
! chunkproc        logic  in T: split AATSR orbit in 4096 row chunks, F: don't
! day_night        int    in 2: process only night data; 1: day (default)
! verbose          logic  in F: minimise information printed to screen; T: don't
!
! History:
! 2011/12/09, MJ: produces draft code which comprises the main program and the
!    top level subroutines.
! 2011/12/22, MJ: includes read routines for MODIS L1B and geolocation HDF4
!    files.
! 2012/01/13, MJ: includes read routines for ERA Interim GRIB files.
! 2012/02/01, MJ: includes read routines for HDF5 AVHRR L1b and geolocation
!    files.
! 2012/02/14, MJ: implements filenames and attributes for netcdf output.
! 2012/02/21, MJ: adds code to produce preprocessing grid for RTTOV
!    implementation.
! 2012/02/22, MJ: restructures ECMWF reading to prepare RTTOV implementation.
!    Variable naming is corrected as well.
! 2012/02/24, MJ: implements code to acquire ECMWF fields on preprocessing grid,
!    to grid imager data to preprocessing grid.
! 2012/03/13, MJ: fixes AVHRR read bug.
! 2012/03/26, MJ: implements 3D pressure array code.
! 2012/03/26, MJ: fixes bug in nearest neighbour assignment.
! 2012/03/27, MJ: changes all file suffixes from *.f90 to *.F90 for ease of use
!    of the C preprocessor.
! 2012/05/15, MJ: includes code to read AVHRR land/sea mask.
! 2012/05/15, CP: includes path to albedo/snow files/emissivity
! 2012/07/15, CP: add in AATSR reading including calibration file
! 2012/05/15, MJ: removes slight bug in passing of parameters from surrounding
!    shell script.
! 2012/07/23, MJ: adds capability to read from command line OR driver file.
! 2012/07/29, CP: tidied up code and merged in MJ changes and  skips over
!    external emissivity read, improved readability, removed albedo
!    and icepaths from file these are now called in the running
!    script added in top level description
! 2012/07/29, CP: added in year month day for ice file
! 2012/08/02, MJ: implements writing of RTTOV output to netcdf file.
! 2012/08/06, CP: modified code to accept BADC style ecmwf files
! 2012/08/07, CP: modified code write albedo to netcdf file
! 2012/08/08, CP: added in emissivity file selection
! 2012/08/13, CP: modified reading of badc ecmwf files
! 2012/08/14, MJ: some modifications to make code compile at DWD
! 2012/08/15, MJ: initializes badc flag and rearranges reading
! 2012/08/16, GT: Fixed up indentation of command-line argument reading for
!    clarity
! 2012/08/16, GT: Bug-fix: Added "aatsr_calib_file" to the argument list for
!    read_imager
!    Added trim() to write statements of file paths
! 2012/08/20, MJ: fixes several bugs with AATSR dimension read
! 2012/08/20, MJ: changed read_mcd43c3 from function to subroutine in order to
!    iron out bugs
! 2012/08/22, MJ: implements flexible x and y dimensions start and end indices
!    in surface part
! 2012/08/28, CP: small mods
! 2012/09/04, GT: Corrected calls to setup_aatsr and read_aatsr_dimensions
! 2012/09/13, GT: Added write statements for some of the input variables to
!    aid in fixing problems with command arguments
! 2012/11/14, CP: modified how netcdf ecmwf files on pressure levels are read
!    clean up of code
! 2012/11/29, CP: big tidy up of code formatting writes etc
! 2012/12/06, CP: added in option to break aatsr orbit into chunks for faster
!    processing
! 2012/12/06, CP: changed how ecmwf paths are defined because of looping chunks!
! 2012/12/16, CP: added an extra ecmwf path so ecmwf can be read form separate
!    directories
! 2013/02/26, CP: changed how day_night aatsr flag was read in
! 2013/02/25, GT: Added preproc_geoloc to the arguments for
!    get_surface_emissivity, un-commented the call to
!    get_surface_emissivity (which now works).
! 2013/03/06, GT: Tidied up formatting of file header.
! 2013/03/06, GT: Bug fix: default along_track_offset set to 0 rather than 1 and
!    definition of imager_geolocation%endy for whole orbits/granules altered
!    accordingly.
! 2013/03/07, CP: changed badc file read options not all files are netcdf
! 2013/03/19, GT: Bug fix: When reading BADC ECMWF data (NetCDF files), the ggam
!    files were being treated as surface, rather than profile, files
! 2013/04/08, CP/GT: fixed bug where channel_info was not passed through routine
!    to correct for ice and snow.
! 2013/05/21, MJ: merges local DWD version with official version.
! 2013/06/03, CP: changed function find_min_max_preproc
! 2013/07/24, AP: a few bug fixes in the chunking code
! 2013/09/02, AP: removed various redundant variables and if statements. Removed
!    startyi, endye.
! 2013/09/03, GT: Finished changes required to get AATSR night-time data
!    reading working correctly
! 2013/09/06, AP: altering READ routines to take start,n as inputs rather than
!    start,end. Removed channel_flag from and added verbose to command line
!    arguments.
! 2013/10/08, AP: altered read_aatsr_dimensions for new call.
! 2013/10/30, AP: Continued tidying. Altered call for find_min_max_preproc.
! 2013/11/05, GT: Changed type cast of cverbose to verbose variable to use
!    '(I6)', as '(L6)' was causing a buffer overrun.
! 2013/11/06, MJ: adds config file to preprocessing output which holds all
!    relevant dimensional information.
! 2013/11/08, GM: Added missing call to deallocate_surface_structures().
! 2014/01/24, MJ: fixed type mismatch in deallocation of surface structures and
!    of variable nc.
! 2014/01/27, GM: Made '1', 't', 'true', 'T', 'True', '0', 'f', 'false', 'F',
!    and 'False' all valid values for the preprocessor verbose option.
! 2014/02/02, GM: Added NetCDF chunking on/off option.
! 2014/02/03, AP: Ensured all arguments that are logical flags are treated
!    identically
! 2014/02/05, MJ: corrected data type of chunkproc from character to logical
! 2014/02/10, AP: changes to ECMWF routines
! 2014/02/05, MJ: adds verbose to argument list of rttov related routines to
!    mute rttov
! 2014/03/11, MJ: adds writing out of some flags to gain more information.
! 2014/04/02, GM: Get the NetCDF version from the library itself.  Left the
!    obsolete input argument in place for now but it is over written.
! 2014/04/21, GM: Added logical option assume_full_path.
! 2014/05/01, GM: Cleaned up the code.
! 2014/05/01, GM: Move some allocations/deallocations to the proper subroutine.
! 2014/06/04, MJ: introduced "WRAPPER" for c-preprocessor and associated
!    variables
! 2014/06/25, GM: Rewrote along track chunking code fixing a bug where the
!    second segment in AATSR night processing was not being processed when
!    chunking was off.
! 2014/07/01, AP: Update to ECMWF code.
! 2014/08/10, GM: Changes related to new BRDF support.
! 2014/09/11, AP: Remove one level from preproc_prtm grid as it wasn't written
!    to or necessary.
! 2014/10/23, OS: added reading of USGS land use/DEM file; implemented Pavolonis
!    cloud typing algorithm; built in new neural network cloud mask in
!    preprocessing, based on radiances and auxiliary data; implemented CRAY
!    fortran-based alternative for scratch file I/O of ERA-Interim data
! 2014/11/04, OS: ecmwf structure is now passed as argument to Pavolonis/NN
!    cloud mask
! 2014/11/21, GM: Remove the no longer used cgrid_flag from driver file input.
!    Was previously removed from command line input.
! 2014/11/21, GM: Add modis_brdf_path to command line input which was previously
!    added to driver file input.
! 2014/12/01, OS: Platform and DOY now passed as arguments to cloud_type call
! 2014/12/04, OS: Wrapper job ID is new call argument and is passed to SR
!    read_ecmwf_grib
! 2014/12/01, CP: Add new global and source attributes
! 2014/12/16, GM: Fix writing of attributes introduced in the change above by
!    reordering some subroutine calls and putting subroutine
!    netcdf_create_config() back into netcdf_output_create().
! 2014/02/04, OS: Added calls to new SR reading ERA-Interim data from NetCDF
!    (WRAPPER only); added call to snow/ice correction based on ERA-Interim data
! 2015/02/19, GM: Added SEVIRI support.
! 2015/02/24, GM: Improved command line and driver file support using the
!    parsing module in the common library including support for comments and
!    optional arguments/fields and better error handling.
! 2015/02/24, GM: Added command line/driver file options to specify the number
!    of channels and the channel IDs to process.
! 2015/07/02, OS: Added check for output netcdf files (wrapper only) +
!    uncommented parse of L2_Processor_Version
! 2015/07/03, OS: Removed parsing of L2_Processor_Version
! 2015/08/08, CP: Add functionality for ATSR2
! 2015/10/19, GM: Add option use_modis_emis_in_rttov to use the MODIS emissivity
!    product instead of the RTTOV emissivity atlas in the RTTOV computations.
! 2015/11/17, OS: Implemented use of high resolution ERA-Interim data for
!    surface variables only (skint, sea-ice, snow-depth)
! 2015/11/26, GM: Implemented linear interpolation between ECMWF inputs before
!    after the temporal center of the satellite orbit.
! 2015/12/17, OS: ECMWF (de)allocation routines and SR to linearly combine ERA
!    data now use flag indicating whether high or low resolution data are used.
!    High res data do not contain same variables as low res.
! 2016/02/02, OS: Small fix for DWD specific problem with parsing driver file.
! 2016/02/02, OS: High resolution ERA-Interim data now used by default. Snow/ice
!    mask is now built from ERA-Interim data instead of NISE.
! 2016/02/02, CP: Input optional path to high resolution ECMWF files otherwise
!    use nwp_path.
! 2016/02/02, GM: Make use of the high resolution ECMWF input optional.
! 2016/02/02, GM: Make use of the ECMWF ice and snow mask (rather than NISE)
!    optional.
! 2016/02/18, OS: if nargs = -1 (i.e. WRAPPER mode), cut off driver file name
!    at first space; now passing imager_flags to correct_for_ice_snow_ecmwf
! 2016/04/03, SP: Add option to process ECMWF forecast in single NetCDF4 file
!    Note: This should work with either the OPER or FCST streams from ECMWF.
! 2016/04/11, SP: Added support for Himawari-8 AHI
! 2016/05/16, SP: Added support for Suomi-NPP  VIIRS
! 2016/05/30, SP: Fixed bug in multi-view processing
! 2016/06/28, SP: Added initial support for Sentinel-3 SLSTR
! 2016/07/11, SP: Chunk routines now in the common directory
! 2016/05/31, GT: Added use_l1_land_mask optional argument to prevent USGS DEM
!    from overwriting the land/sea mask provided by L1 data (assuming the L1
!    data provides one!).
! 2016/08/11, GT: Added a (fairly nasty) hack which removes flagged cloud over
!    the Sahara region if: (i) the total cldmask_uncertainty in both views is
!    greater then 70, (ii) the Pavolonis cloud type is water in either view.
!    This is an attempt to put mis-flagged dust back into the aerosol product.
! 2017/02/06, SP: Added support for NOAA GFS atmosphere data (ExtWork)
! 2017/02/10, SP: Allow reading LSM, LUM, DEM from external file (ExtWork)
! 2017/02/24, SP: Allow option to disable snow/ice correction
! 2017/03/29, SP: Add new variable for tropopause cloud emissivity (ExtWork)
! 2017/03/29, SP: Add ability to calculate tropospheric cloud emissivity
!    (ExtWork)
! 2017/04/08, SP: New flag to disable VIS processing, saves proc time (ExtWork)
! 2017/04/11, SP: Added nwp_flag=6, for working with GFS analysis files.
! 2017/04/26, SP: Support for loading geoinfo (lat/lon/vza/vaa) from an external
!    file. Supported by AHI, not yet by SEVIRI (ExtWork)
! 2017/06/21, OS: added spectral response correction flag, which defaults to
!    false unless sensor=AATSR/AVHRR/MODIS
! 2017/07/09, SP: Add check for SEVIRI RSS data to ensure correct processing.
! 2017/08/09, SP: Add option to disable the cloud masking (ExtWork)
! 2017/09/14, GT: Added product_name optional argument/driver file line (allows
!    user to replace "L2-CLOUD-CLD" in ouput filenames)
! 2018/01/04, GT: Reimplemented product_name functionality, which had somehow
!    gone missing in the move to github.
! 2018/02/01, GT: source_atts structure is now passed to setup_slstr, which
!     populates the l1b_version and l1b_orbit_number attributes.
! 2018/04/26, SP: Adjustment to the aerosol hack to prevent segfaults.
! 2018/04/29, SP: Add cloud emissivity support for ECMWF profiles (ExtWork)
! 2018/06/03, SP: GSICS calibration is now supported for SEVIRI. The default
!                 setting is ON, meaning that GSICS coefficients will be used
!                 instead of IMPF (as previous). The new driver file option
!                 USE_GSICS enables this to be disabled.
! 2018/10/08, SP: Add support for the GOES-Imager series of sensors (G12-15)
! 2019/05/21, GT: Added code to run ECMWF data reading and RTTOV running
!                 separately for East and West portions of an image that
!                 crosses the date line, rather than populating the entire
!                 lat-lon range.
! 2020/03/02, ATP: Add support for AHI subsetting.
! 2021/03/10, AP: Remove command line arguments.
! 2021/03/14, AP: Move setup selection into a dedicated routine.
! 2023/06/26, GT: Added a 1-hour default for the NWP time factor for BADC
!                 ERA5 data (nwp_flag = 2).
!
! Bugs:
! See http://proj.badc.rl.ac.uk/orac/report/1
!
!
!
! NOTES:
! The nwp_flag option allows the user to set what type of NWP data to use.
! There are five options for this flag:
! 0: NOAA GFS data in a single GRIB file.
! 1: ECMWF Operational or ERA5 data as a single netCDF4 file
! 2: ECMWF ERA5 data in JASMIN format (GRIB, one file per variable)
! 3: DWD format (unknown details)
! 4: ERA-Interim in JASMIN format, three files.
!-------------------------------------------------------------------------------

#ifndef WRAPPER
program orac_preproc
#else
subroutine orac_preproc(mytask, ntasks, lower_bound, upper_bound, driver_path_file, &
     status)
#endif

   use channel_structures_m
   use chunk_utils_m
   use cloud_typing_pavolonis_m, only: cloud_type
   use correct_for_ice_snow_m
   use ecmwf_m
   use global_attributes_m
   use hdf5
   use imager_structures_m
   use orac_ncdf_m
   use netcdf, only: nf90_inq_libvers
   use netcdf_output_m
   use parsing_m
   use preparation_m
   use preproc_constants_m
   use preproc_structures_m
   use read_imager_m
   use rttov_driver_m
   use rttov_driver_gfs_m
   use setup_m
   use source_attributes_m
   use surface_emissivity_m
   use surface_reflectance_m
   use surface_structures_m
   use USGS_physiography_m
   use utils_for_main_m

#ifdef INCLUDE_SATWX
   use cloud_emis_m
#endif

   implicit none

   character(len=path_length)       :: driver_path_file
   character(len=path_length)       :: usgs_path_file
   character(len=path_length)       :: rttov_coef_path
   character(len=path_length)       :: rttov_emiss_path
   character(len=path_length)       :: nise_ice_snow_path
   character(len=path_length)       :: modis_albedo_path
   character(len=path_length)       :: modis_brdf_path
   character(len=path_length)       :: cimss_emiss_path
   character(len=attribute_length)  :: cdellon, cdellat
   character(len=path_length)       :: output_path
   character(len=cmd_arg_length)    :: cstartx, cendx, cstarty, cendy
   character(len=path_length)       :: aatsr_calib_path_file
   character(len=cmd_arg_length)    :: cnwp_flag
   character(len=cmd_arg_length)    :: cchunkproc
   character(len=cmd_arg_length)    :: cday_night
   character(len=cmd_arg_length)    :: cverbose
   character(len=cmd_arg_length)    :: cassume_full_paths
   character(len=cmd_arg_length)    :: cinclude_full_brdf
   character(len=cmd_arg_length)    :: cdummy_arg

   type(global_attributes_t)        :: global_atts
   type(source_attributes_t)        :: source_atts
   integer                          :: nwp_flag
   logical                          :: chunkproc
   logical                          :: verbose
   logical                          :: assume_full_paths
   logical                          :: include_full_brdf

   logical                          :: do_spectral_response_correction

   logical                          :: check
   integer                          :: nargs

   character(len=path_length)       :: label, value

   integer(kind=lint)               :: along_pos

   integer                          :: segment_starts(2)
   integer                          :: segment_ends(2)
   integer                          :: n_segments
   integer                          :: chunksize
   integer                          :: i_chunk
   integer                          :: n_chunks
   integer, allocatable             :: chunk_starts(:)
   integer, allocatable             :: chunk_ends(:)

   type(preproc_paths_t)            :: out_paths
   type(setup_args_t)               :: granule

   type(channel_info_t)             :: channel_info

   type(imager_angles_t)            :: imager_angles
   type(imager_flags_t)             :: imager_flags
   type(imager_geolocation_t)       :: imager_geolocation
   type(imager_measurements_t)      :: imager_measurements
   type(imager_time_t)              :: imager_time
   type(imager_pavolonis_t)         :: imager_pavolonis
   type(imager_cloud_t)             :: imager_cloud

   type(USGS_t)                     :: usgs

   type(ecmwf_t)                    :: ecmwf, ecmwf1, ecmwf2

   type(surface_t)                  :: surface

   type(preproc_dims_t)             :: preproc_dims
   type(preproc_geo_t)              :: preproc_geo
   type(preproc_geoloc_t)           :: preproc_geoloc
   type(preproc_opts_t)             :: preproc_opts
   type(preproc_prtm_t)             :: preproc_prtm, preproc_prtm1, preproc_prtm2
   type(preproc_surf_t)             :: preproc_surf
   type(preproc_cld_t)              :: preproc_cld

   type(netcdf_output_info_t)       :: netcdf_info

   integer                          :: index_space
   real                             :: ecmwf_time_int_fac
   integer                          :: date, ind

   ! Temporary variables for the aerosol_cci dust mask hack
   !real(kind=sreal), allocatable    :: tot_cldmask_uncertainty(:,:)

!  integer, dimension(8)            :: values

   ! this is for the wrapper
#ifdef WRAPPER
   logical                              :: corrupt
   integer                              :: check_output
   integer                              :: mytask, ntasks, lower_bound, upper_bound
   integer, intent(out)                 :: status
   logical, allocatable, dimension(:,:) :: mask
   nargs = -1
#else
   ! get number of arguments
   nargs = command_argument_count()
#endif

   ! Minimal output to notify start and end of programme
   write(*,*) 'Beginning orac_preproc'

   ! Set defaults for optional arguments/fields
   nullify(preproc_opts%channel_ids)
   preproc_opts%n_channels               = 0
   preproc_opts%ecmwf_time_int_method    = 2
   preproc_opts%use_ecmwf_snow_and_ice   = .true.
   preproc_opts%use_modis_emis_in_rttov  = .false.
   preproc_opts%nwp_fnames%nwp_path(2)   = ' '
   preproc_opts%nwp_fnames%nwp_path2(2)  = ' '
   preproc_opts%nwp_fnames%nwp_path3(2)  = ' '
   preproc_opts%nwp_nlevels              = 0
   preproc_opts%use_l1_land_mask         = .false.
   preproc_opts%use_occci                = .false.
   preproc_opts%occci_path               = ' '
   preproc_opts%use_predef_lsm           = .false.
   preproc_opts%ext_lsm_path             = ' '
   preproc_opts%use_predef_geo           = .false.
   preproc_opts%ext_geo_path             = ' '
   preproc_opts%disable_snow_ice_corr    = .false.
   preproc_opts%do_cloud_emis            = .false.
   preproc_opts%do_ironly                = .false.
   preproc_opts%do_cloud_type            = .true.
   preproc_opts%product_name             = 'L2-CLOUD-CLD'
   do_spectral_response_correction       = .false.
   preproc_opts%use_camel_emis           = .false.
   preproc_opts%do_gsics                 = .true.
   preproc_opts%do_nasa                  = .false.
   preproc_opts%do_co2                   = .true.
   preproc_opts%use_swansea_climatology  = .false.
   preproc_opts%swansea_gamma            = 0.3
   preproc_opts%use_seviri_ann_cma_cph   = .false.
   preproc_opts%use_seviri_ann_ctp_fg    = .false.
   preproc_opts%use_seviri_ann_mlay      = .false.
   preproc_opts%mcd43_max_qaflag         = 5
   preproc_opts%do_dust_correction       = .true.
   preproc_opts%use_ecmwf_preproc_grid   = .false.

   ! When true, the offset between the nadir and oblique views is read from
   ! the track_offset global attribute. Otherwise, the two longitude fields are
   ! read and compared to determine an appropriate offset.
   preproc_opts%calculate_slstr_alignment = .false.

   ! Initialise satellite position string
   global_atts%Satpos_Metadata = 'null'

   if (nargs .eq. 1) then
      ! if just one argument => this is a driver file
      call get_command_argument(1, driver_path_file)
   else if (nargs .eq. -1) then
      index_space = index(driver_path_file, " ")
      driver_path_file = driver_path_file(1:(index_space-1))
      write(*,*) 'inside preproc: ', trim(adjustl(driver_path_file))
   else
      write(*,*) 'ERROR: Only expected argument is the driver file path'
      stop error_stop_code
   end if

   open(11, file=trim(adjustl(driver_path_file)), status='old', &
        form='formatted')

   call parse_required(11, granule%sensor,                       'sensor')
   call parse_required(11, granule%l1b_file,                     'l1b_path_file')
   call parse_required(11, granule%geo_file,                     'geo_path_file')
   call parse_required(11, usgs_path_file,                       'usgs_path_file')
   call parse_required(11, preproc_opts%nwp_fnames%nwp_path(1),  'nwp_path')
   call parse_required(11, rttov_coef_path,                      'rttov_coef_path')
   call parse_required(11, rttov_emiss_path,                     'rttov_emiss_path')
   call parse_required(11, nise_ice_snow_path,                   'nise_ice_snow_path')
   call parse_required(11, modis_albedo_path,                    'modis_albedo_path')
   call parse_required(11, modis_brdf_path,                      'modis_brdf_path')
   call parse_required(11, cimss_emiss_path,                     'cimss_emiss_path')
   call parse_required(11, cdellon,                              'cdellon')
   call parse_required(11, cdellat,                              'cdellat')
   call parse_required(11, output_path,                          'output_path')
   call parse_required(11, cstartx,                              'cstartx')
   call parse_required(11, cendx,                                'cendx')
   call parse_required(11, cstarty,                              'cstarty')
   call parse_required(11, cendy,                                'cendy')
   call parse_required(11, global_atts%NetCDF_Version,           'NetCDF_Version')
   call parse_required(11, global_atts%Conventions,              'Conventions')
   call parse_required(11, global_atts%Institution,              'Institution')
   call parse_required(11, global_atts%L2_Processor,             'L2_Processor')
   call parse_required(11, global_atts%Creator_Email,            'Creator_Email')
   call parse_required(11, global_atts%Creator_URL,              'Creator_URL')
   call parse_required(11, global_atts%file_version,             'file_version')
   call parse_required(11, global_atts%References,               'References')
   call parse_required(11, global_atts%History,                  'History')
   call parse_required(11, global_atts%Summary,                  'Summary')
   call parse_required(11, global_atts%Keywords,                 'Keywords')
   call parse_required(11, global_atts%Comment,                  'Comment')
   call parse_required(11, global_atts%Project,                  'Project')
   call parse_required(11, global_atts%License,                  'License')
   call parse_required(11, global_atts%UUID,                     'UUID')
   call parse_required(11, global_atts%Production_Time,          'Production_Time')
   call parse_required(11, aatsr_calib_path_file,                'aatsr_calib_path_file')
   call parse_required(11, cnwp_flag,                            'cnwp_flag')
   call parse_required(11, preproc_opts%nwp_fnames%nwp_path2(1), 'nwp_path2')
   call parse_required(11, preproc_opts%nwp_fnames%nwp_path3(1), 'nwp_path3')
   call parse_required(11, cchunkproc,                           'cchunkproc')
   call parse_required(11, cday_night,                           'cday_night')
   call parse_required(11, cverbose,                             'cverbose')
   call parse_required(11, cdummy_arg,                           'cdummy_arg')
   call parse_required(11, cassume_full_paths,                   'cassume_full_paths')
   call parse_required(11, cinclude_full_brdf,                   'cinclude_full_brdf')
   call parse_required(11, global_atts%RTTOV_Version,            'RTTOV_Version')
   call parse_required(11, global_atts%ECMWF_Version,            'ECMWF_Version')
   call parse_required(11, global_atts%SVN_Version,              'SVN_Version')

   do while (parse_driver(11, value, label) == 0)
      call clean_driver_label(label)
      call parse_optional(label, value, preproc_opts)
   end do

   ! Ensure sensor name and reader name match
   granule%sensor_rdr = granule%sensor

   close(11)

   ! Set this since it was removed from the command line but not removed from
   ! the global attributes.
   global_atts%L2_Processor_Version = '1.0'

   ! Parse appropriate type from input strings
   if (parse_string(cdellon, preproc_dims%dellon) /= 0) &
      call handle_parse_error('dellon')
   if (parse_string(cdellat, preproc_dims%dellat) /= 0) &
      call handle_parse_error('dellat')
   if (parse_string(cstartx, granule%startx) /= 0) &
      call handle_parse_error('startx')
   if (parse_string(cendx, granule%endx) /= 0) &
      call handle_parse_error('endx')
   if (parse_string(cstarty, granule%starty) /= 0) &
      call handle_parse_error('starty')
   if (parse_string(cendy, granule%endy) /= 0) &
      call handle_parse_error('endy')
   if (parse_string(cday_night, granule%day_night)   /= 0) &
      call handle_parse_error('day_night')
   if (parse_string(cnwp_flag, nwp_flag) /= 0) &
      call handle_parse_error('nwp_flag')

   ! Still use the old parse_logical() here at supports 0 and 1
   if (parse_logical(cchunkproc, chunkproc) /= 0) &
      call handle_parse_error('chunkproc')
   if (parse_logical(cverbose, verbose)     /= 0) &
      call handle_parse_error('verbose')
   if (parse_logical(cassume_full_paths, assume_full_paths) /= 0) &
      call handle_parse_error('assume_full_paths')
   if (parse_logical(cinclude_full_brdf, include_full_brdf) /= 0) &
      call handle_parse_error('include_full_brdf')

   ! Check argument/fields
   if (preproc_opts%n_channels .ne. 0 .and. .not. associated(preproc_opts%channel_ids)) then
      write(*,*) 'ERROR: options n_channels and channel_ids must be used together'
      stop error_stop_code
   end if

   ! If we're reading BADC ERA5 data, we default to one file every hour,
   ! otherwise assume it's one every six hours.
   if (nwp_flag .eq. 2) then
      preproc_opts%nwp_time_factor       = 1.
   else
      preproc_opts%nwp_time_factor       = 6.
   end if

   ! Check if SatWx is available, if not then don't do cloud emissivity stuff
#ifndef INCLUDE_SATWX
   preproc_opts%do_cloud_emis = .false.
#endif

   ! If we're using an external land-sea file, place that into USGS filename var
   if (preproc_opts%use_predef_lsm) usgs_path_file = preproc_opts%ext_lsm_path

   if (preproc_opts%nwp_fnames%nwp_path(2) .eq. '') &
      preproc_opts%nwp_fnames%nwp_path(2) =  preproc_opts%nwp_fnames%nwp_path(1)
   if (preproc_opts%nwp_fnames%nwp_path2(2) .eq. '') &
      preproc_opts%nwp_fnames%nwp_path2(2) = preproc_opts%nwp_fnames%nwp_path2(1)
   if (preproc_opts%nwp_fnames%nwp_path3(2) .eq. '') &
      preproc_opts%nwp_fnames%nwp_path3(2) = preproc_opts%nwp_fnames%nwp_path3(1)

   ! get the NetCDF version
   global_atts%netcdf_version = nf90_inq_libvers()

   ! get the execution time in UTC
!  call date_and_time(VALUES=values)
!  write(global_atts%exec_time, '(i4,i2.2,i2.2,i2.2,i2.2,i2.2)'), &
!        values(1), values(2), values(3), values(5), values(6), values(7)
!  write(*,*) global_atts%exec_time
!  stop

   ! initialise some counts, offset variables...
   granule%along_track_offset = 0
   granule%along_track_offset2 = 0
   granule%n_along_track2 = 0
   imager_angles%nviews = 1

   ! determine platform, day, time, check if l1b and geo match
   if (verbose) &
      write(*,*) 'Determine platform, day, time, check if l1b and geo match'
   inquire(file=granule%l1b_file, exist=check)
   if (.not. check) then
      write(*,*) 'ERROR: L1B file does not exist: ', trim(granule%l1b_file)
      stop error_stop_code
   end if
   inquire(file=granule%geo_file, exist=check)
   if (.not. check) then
      write(*,*) 'ERROR: GEO file does not exist: ', trim(granule%geo_file)
      stop error_stop_code
   end if

   source_atts%level1b_file         = granule%l1b_file
   source_atts%geo_file             = granule%geo_file
   ! Set default values for fields that some instruments setups do not set yet
   source_atts%level1b_version      = 'null'
   source_atts%level1b_orbit_number = 'null'
   ! Set default values for other attributes, just in case
   source_atts%albedo_file          = 'null'
   source_atts%brdf_file            = 'null'
   source_atts%emissivity_file      = 'null'
   source_atts%usgs_file            = 'null'
   source_atts%snow_file            = 'null'
   source_atts%sea_ice_file         = 'null'

   call setup_imager(granule, preproc_opts, source_atts, channel_info, verbose)

   ! We now have the number of viewing geometries. Put this in imager_angles
   imager_angles%nviews = channel_info%nviews

   if (verbose) then
      write(*,*) 'WE ARE PROCESSING ', trim(granule%platform), ' FOR ORBIT', &
           granule%year, granule%month, granule%day, granule%hour, granule%minute
      write(*,*) 'File dimensions determined:'
      write(*,*) 'n_across_track:      ', granule%n_across_track
      write(*,*) 'along_track_offset:  ', granule%along_track_offset
      write(*,*) 'n_along_track:       ', granule%n_along_track
      if ((trim(adjustl(granule%sensor)) .eq. 'AATSR' .or. &
           trim(adjustl(granule%sensor)) .eq. 'ATSR2') .and. &
           granule%day_night .eq. 2) then
         write(*,*) 'along_track_offset2: ', granule%along_track_offset2
         write(*,*) 'n_along_track2:      ', granule%n_along_track2
      end if
   end if

   ! determine processing chunks and their dimensions
   if (verbose) write(*,*) 'Determine processing chunks and their dimensions'

   if (granule%startx.ge.1 .and. granule%endx.ge.1 .and. &
        granule%starty.ge.1 .and. granule%endy.ge.1) then
      if ( trim(adjustl(granule%sensor_rdr)) .eq. 'VIIRSI' .or. &
           trim(adjustl(granule%sensor_rdr)) .eq. 'VIIRSM' .or. &
           trim(adjustl(granule%sensor_rdr)) .eq. 'PYTHON') then
         write(*,*) 'ERROR: subsetting not supported for ', trim(granule%sensor)
         stop error_stop_code
      end if
      if (granule%startx.gt.granule%n_across_track) then
         write(*,*) 'ERROR: invalid startx (across track dimensions)'
         write(*,*) '       Should be < ', granule%n_across_track
         stop error_stop_code
      end if
      if (granule%startx.gt.granule%endx) then
         write(*,*) 'ERROR: invalid startx (across track dimensions)'
         write(*,*) '       Should be < endx', granule%endx
         stop error_stop_code
      end if
      if (granule%endx.gt.granule%n_across_track) then
         write(*,*) 'ERROR: invalid endx (across track dimensions)'
         write(*,*) '       Should be < ', granule%n_across_track
         stop error_stop_code
      end if
      if ((trim(adjustl(granule%sensor)) .eq. 'AATSR' .or. &
           trim(adjustl(granule%sensor)) .eq. 'ATSR2') .and. &
           granule%day_night .eq. 2) then
         along_pos = granule%along_track_offset2 + granule%n_along_track2
      else
         along_pos = granule%along_track_offset + granule%n_along_track
      end if
      if (granule%starty.gt.along_pos) then
         write(*,*) 'ERROR: invalid starty (along track dimensions)'
         write(*,*) '       Should be < ', along_pos
         stop error_stop_code
      end if
       if (granule%starty.gt.granule%endy) then
         write(*,*) 'ERROR: invalid starty (along track dimensions)'
         write(*,*) '       Should be < endy ', granule%endy
         stop error_stop_code
      end if
     if (granule%endy.gt.along_pos) then
         write(*,*) 'ERROR: invalid endy (along track dimensions)'
         write(*,*) '       Should be < ', along_pos
         stop error_stop_code
      end if

      ! use specified values
      imager_geolocation%startx = granule%startx
      imager_geolocation%endx   = granule%endx

      n_chunks = 1

      allocate(chunk_starts(n_chunks))
      allocate(chunk_ends(n_chunks))

      chunk_starts(1) = granule%starty
      chunk_ends(1)   = granule%endy
   else
      imager_geolocation%startx = 1
      imager_geolocation%endx = granule%n_across_track

      if (chunkproc) then
         chunksize = 4096
      else
         chunksize = granule%n_along_track + granule%n_along_track2
      end if

      n_segments = 1
      segment_starts(1) = granule%along_track_offset + 1
      segment_ends(1)   = granule%along_track_offset + granule%n_along_track

      if ((trim(adjustl(granule%sensor)) .eq. 'AATSR' .or. &
           trim(adjustl(granule%sensor)) .eq. 'ATSR2') .and. &
           granule%day_night .eq. 2) then
         n_segments = n_segments + 1

         segment_starts(n_segments) = granule%along_track_offset2 + 1
         segment_ends(n_segments)   = granule%along_track_offset2 + granule%n_along_track2
      end if

      n_chunks = calc_n_chunks(n_segments, segment_starts, segment_ends, &
                               chunksize)

      allocate(chunk_starts(n_chunks))
      allocate(chunk_ends(n_chunks))

      call chunkify(n_segments, segment_starts, segment_ends, chunksize, &
                    n_chunks, chunk_starts, chunk_ends)

   end if ! end of startx and starty selection

   if (verbose) then
      write(*,*) 'The number of chunks to be processed: ', n_chunks
      write(*,*) 'The chunks to be processed are ', &
           '(i_chunk, chunk_start, chunk_end):'
      do i_chunk = 1, n_chunks
         write(*,*) i_chunk, chunk_starts(i_chunk), chunk_ends(i_chunk)
      end do
   end if

   imager_geolocation%nx = imager_geolocation%endx-imager_geolocation%startx+1

   if (verbose) write(*,*) 'Begin looping over chunks'
   do i_chunk = 1, n_chunks
      if (verbose) write(*,*) 'i_chunk: ', i_chunk

      imager_geolocation%starty = chunk_starts(i_chunk)
      imager_geolocation%endy   = chunk_ends(i_chunk)
      imager_geolocation%ny     = chunk_ends(i_chunk) - chunk_starts(i_chunk) + 1

      if (verbose) then
         write(*,*) 'startx: ', imager_geolocation%startx, ', endx: ', &
              imager_geolocation%endx
         write(*,*) 'starty: ', imager_geolocation%starty, ', endy: ', &
              imager_geolocation%endy
      end if

      ! allocate the structures for the imager: geolocation, angles and
      ! measurements, and surface data
      if (verbose) write(*,*) 'Allocate imager and surface structures'
      call allocate_imager_structures(imager_geolocation, imager_angles, &
           imager_flags, imager_time, imager_measurements, imager_pavolonis, &
           imager_cloud, channel_info, preproc_opts%use_seviri_ann_ctp_fg, &
           preproc_opts%use_seviri_ann_mlay)
      call allocate_surface_structures(surface, imager_geolocation, channel_info, &
           include_full_brdf)

      ! read imager data:
      if (verbose) write(*,*) 'Read imager data'
      call read_imager(granule, preproc_opts, aatsr_calib_path_file, &
           imager_geolocation, imager_angles, imager_flags, imager_time, &
           imager_measurements, channel_info, global_atts, verbose)
#ifdef WRAPPER
      ! do not process this orbit if no valid lat/lon data available
      mask = imager_geolocation%latitude.gt.sreal_fill_value .and. &
             imager_geolocation%longitude.gt.sreal_fill_value
      if (.not. any(mask)) then
         write(*,*) "any mask: ", any(mask)
         write(*,*) "maxval lat/lon:", maxval(imager_geolocation%latitude), &
              maxval(imager_geolocation%longitude)
         status = 1
         return
      end if
#endif

      ! carry out any preparatory steps: identify required ECMWF and MODIS L3
      ! information, set paths and filenames to those required auxiliary /
      ! ancillary input...
      if (verbose) write(*,*) 'Carry out any preparatory steps'
      call preparation(out_paths, granule, preproc_opts, global_atts, &
           source_atts%level1b_orbit_number, nwp_flag, imager_geolocation, &
           imager_time, i_chunk, ecmwf_time_int_fac, assume_full_paths, verbose)

      ! read ECMWF fields and grid information
      if (verbose) then
         write(*,*) 'Start reading meteorological data file'
         write(*,*) 'nwp_flag: ', nwp_flag
         write(*,*) 'nwp_path_file: ', trim(preproc_opts%nwp_fnames%nwp_path_file(1))
         if (nwp_flag.gt.0.and.nwp_flag.lt.4) then
            write(*,*) 'nwp_path_file2: ', trim(preproc_opts%nwp_fnames%nwp_path_file2(1))
            write(*,*) 'nwp_path_file3: ', trim(preproc_opts%nwp_fnames%nwp_path_file3(1))
         end if
      end if
      ! NOAA GFS has limited (pressure) levels and no HR, so set these.
      if (nwp_flag .eq. 0) preproc_opts%nwp_nlevels = 31

      ! read surface wind fields and ECMWF dimensions
      if (preproc_opts%ecmwf_time_int_method .ne. 2) then
         call read_ecmwf_wind(nwp_flag, preproc_opts%nwp_fnames, 1, ecmwf, preproc_opts%nwp_nlevels, date, ind, verbose)
      else
         call read_ecmwf_wind(nwp_flag, preproc_opts%nwp_fnames, 1, ecmwf1, preproc_opts%nwp_nlevels, date, ind, verbose)
         call read_ecmwf_wind(nwp_flag, preproc_opts%nwp_fnames, 2, ecmwf2, preproc_opts%nwp_nlevels, date, ind, verbose)

         call dup_ecmwf_allocation(ecmwf1, ecmwf)

         call linearly_combine_ecmwfs(1.-ecmwf_time_int_fac, &
              ecmwf_time_int_fac, ecmwf1, ecmwf2, ecmwf)

         call deallocate_ecmwf_structures(ecmwf1)
         call deallocate_ecmwf_structures(ecmwf2)
      end if
      
      ! define preprocessing grid from user grid spacing and satellite limits
      if (verbose) write(*,*) 'Define preprocessing grid'
      if (preproc_opts%use_ecmwf_preproc_grid) then
         call define_preproc_grid_ecmwf(imager_geolocation, preproc_dims, ecmwf, verbose)
      else
         preproc_dims%kdim = ecmwf%kdim
         call define_preprop_grid(imager_geolocation, preproc_dims, verbose)

      end if 

      ! allocate preprocessing structures
      if (verbose) write(*,*) 'Allocate preprocessing structures'
      call allocate_preproc_structures(imager_angles, preproc_dims, &
           preproc_geoloc, preproc_geo, preproc_prtm, preproc_surf, preproc_cld, &
           channel_info)
      ! now read the actual data and interpolate it to the preprocessing grid
      if (verbose) write(*,*) 'Build preprocessing grid'
      call build_preproc_fields(preproc_dims, preproc_geoloc, preproc_geo, &
           imager_geolocation, imager_angles, ecmwf, preproc_opts%use_ecmwf_preproc_grid)

      ! read ecmwf era interim file
      if (verbose) write(*,*) 'Read and interpolate NWP / Reanalysis data.'

      if (preproc_opts%use_ecmwf_preproc_grid.and.nwp_flag.gt.0.and.nwp_flag.lt.4) then
         if (verbose) write(*,*) 'Using ECMWF as preproc grid'
         call ecmwf_for_preproc_structures(preproc_opts, ecmwf, preproc_geoloc, &
              preproc_prtm, preproc_dims, verbose, ecmwf_time_int_fac, date, ind, &
              nwp_flag)
      else
         if (preproc_opts%ecmwf_time_int_method .ne. 2) then
            call read_ecmwf(nwp_flag, preproc_opts%nwp_fnames, 1, ecmwf, preproc_dims, &
                 preproc_geoloc, preproc_prtm, preproc_opts, date, ind, verbose)
         else
            call allocate_preproc_prtm(preproc_dims, preproc_prtm1)
            call read_ecmwf(nwp_flag, preproc_opts%nwp_fnames, 1, ecmwf, preproc_dims,  &
                 preproc_geoloc, preproc_prtm1, preproc_opts, date, ind, verbose)

            call allocate_preproc_prtm(preproc_dims, preproc_prtm2)
            call read_ecmwf(nwp_flag, preproc_opts%nwp_fnames, 2, ecmwf, preproc_dims,  &
                 preproc_geoloc, preproc_prtm2, preproc_opts, date, ind, verbose)

            call linearly_combine_prtms(1.-ecmwf_time_int_fac, ecmwf_time_int_fac, &
                 preproc_prtm1, preproc_prtm2, preproc_prtm)

            call deallocate_preproc_prtm(preproc_prtm1)
            call deallocate_preproc_prtm(preproc_prtm2)
         end if

      end if

      if (verbose) write(*,*) 'Compute geopotential vertical coords'
      ! compute geopotential vertical coordinate from pressure coordinate
      ! First check that we're not processing a GFS file
      if (nwp_flag .le. 5 .or. nwp_flag .gt. 8) call &
         compute_geopot_coordinate(preproc_prtm, preproc_dims, ecmwf)

      ! read USGS physiography file, including land use and DEM data
      ! NOTE: variable imager_flags%lsflag is overwritten by USGS data !!!

      if (verbose) write(*,*) 'Reading USGS path: ', trim(usgs_path_file)
      call get_USGS_data(usgs_path_file, imager_flags, imager_geolocation, &
           usgs, assume_full_paths, preproc_opts%use_l1_land_mask, source_atts, &
           preproc_opts%use_predef_lsm, granule%sensor, verbose)

      ! select correct emissivity file and calculate the emissivity over land
      if (verbose) write(*,*) 'Get surface emissivity'
      if (.not. preproc_opts%use_camel_emis) then
         call get_surface_emissivity(granule%cyear, granule%cdoy, cimss_emiss_path, &
              imager_flags, imager_geolocation, channel_info, preproc_dims, preproc_geoloc, &
              assume_full_paths, verbose, surface, preproc_surf, source_atts)
      else
         call get_camel_emissivity(granule%cyear, granule%cmonth, cimss_emiss_path, &
              imager_flags, imager_geolocation, channel_info, preproc_dims, preproc_geoloc, &
              assume_full_paths, verbose, surface, preproc_surf, source_atts)
      end if

      if (.not. preproc_opts%do_ironly) then
         ! select correct reflectance files and calculate surface reflectance
         ! over land and ocean
         if (verbose) write(*,*) 'Get surface reflectance'
         call get_surface_reflectance(granule%cyear, granule%cdoy, granule%cmonth, &
              modis_albedo_path, modis_brdf_path, preproc_opts%occci_path, imager_flags, &
              imager_geolocation, imager_angles, channel_info, ecmwf, &
              assume_full_paths, include_full_brdf, preproc_opts%use_occci, &
              preproc_opts%use_swansea_climatology, preproc_opts%swansea_gamma, verbose, &
              preproc_opts%mcd43_max_qaflag, surface, source_atts)

         ! Use the Near-real-time Ice and Snow Extent (NISE) data from the National
         ! Snow and Ice Data Center to detect ice and snow pixels, and correct the
         ! surface albedo.
         if (verbose) write(*,*) 'Correct for ice and snow'
         if (.not. preproc_opts%disable_snow_ice_corr) then
            if (.not. preproc_opts%use_ecmwf_snow_and_ice) then
               call correct_for_ice_snow(nise_ice_snow_path, &
                    imager_geolocation, surface, granule%cyear, granule%cmonth, granule%cday, &
                    channel_info, assume_full_paths, include_full_brdf, &
                    source_atts, verbose)
            else
               call correct_for_ice_snow_nwp(preproc_opts%nwp_fnames%nwp_path_file(1), &
                    imager_geolocation, channel_info, imager_flags, preproc_dims, &
                    preproc_prtm, preproc_geoloc, surface, include_full_brdf, source_atts, &
                    verbose)
            end if
         end if
      else
         if (channel_info%nchannels_sw .gt. 0) then
            surface%albedo(:,:,:) = sreal_fill_value
            if (include_full_brdf) then
               surface%rho_0v(:,:,:) = sreal_fill_value
               surface%rho_0d(:,:,:) = sreal_fill_value
               surface%rho_dv(:,:,:) = sreal_fill_value
               surface%rho_dd(:,:,:) = sreal_fill_value
            end if
         end if
      end if
      if (.not. preproc_opts%do_cloud_emis) then
         if (verbose) write(*,*) 'Calculate Pavolonis cloud phase with high '// &
              'resolution ERA surface data'
         if (preproc_opts%do_cloud_type) then
           call cloud_type(channel_info, granule%sensor, surface, imager_flags, &
                imager_angles, imager_geolocation, imager_measurements, &
                imager_pavolonis, ecmwf, granule%platform, granule%doy, preproc_opts%do_ironly, &
                do_spectral_response_correction, preproc_opts%use_seviri_ann_cma_cph, &
                preproc_opts%use_seviri_ann_ctp_fg, preproc_opts%use_seviri_ann_mlay, &
                preproc_opts%do_nasa, verbose)
         end if
      end if
      
      if (preproc_opts%do_dust_correction) then
         if (verbose) write(*,*) 'Apply dust-detection correction to cloud mask'
         call correct_for_dust(channel_info, imager_measurements, imager_angles, &
              imager_geolocation, imager_pavolonis)
      end if
      
!!$      if (imager_angles%nviews .gt. 1) then
!!$         ! A temporary hack for Aerosol_cci:
!!$         ! Due to the cloud masking being very effective at detecting dust,
!!$         ! we'll try and re-introduce it
!!$         if (trim(adjustl(granule%sensor)) .eq. 'AATSR' .or. &
!!$            trim(adjustl(granule%sensor)) .eq. 'ATSR2' .or. &
!!$            trim(adjustl(granule%sensor)) .eq. 'SLSTR') then
!!$            if (1 .eq. 1 .and. &
!!$                 minval(imager_geolocation%latitude)  .lt.  40.0 .and. &
!!$                 maxval(imager_geolocation%latitude)  .gt.   0.0 .and. &
!!$                 minval(imager_geolocation%longitude) .lt.  75.0 .and. &
!!$                 maxval(imager_geolocation%longitude) .gt. -40.0) then
!!$               if (verbose) write(*,*) 'Aerosol_cci dust correction hack is underway'
!!$               allocate(tot_cldmask_uncertainty( &
!!$                    imager_geolocation%startx:imager_geolocation%endx, &
!!$                    1:imager_geolocation%ny) )
!!$               ! product a smoothed version of the cldmask uncertainty
!!$               if (verbose) then
!!$                  write(*,*) minval(imager_pavolonis%cldmask_uncertainty(:,:,1)), &
!!$                       maxval(imager_pavolonis%cldmask_uncertainty(:,:,1))
!!$                  write(*,*) minval(imager_pavolonis%cldmask_uncertainty(:,:,2)), &
!!$                       maxval(imager_pavolonis%cldmask_uncertainty(:,:,2))
!!$               end if
!!$
!!$               tot_cldmask_uncertainty(:,:) = &
!!$                    imager_pavolonis%cldmask_uncertainty(:,:,1) + &
!!$                    imager_pavolonis%cldmask_uncertainty(:,:,2)
!!$
!!$               if (verbose) write(*,*) 'Total cldmask uncertainty: min-max', &
!!$                    minval(tot_cldmask_uncertainty), maxval(tot_cldmask_uncertainty)
!!$               ! Now use this smoothed mask, and the pavolonis cloud type
!!$               ! to "de-mask" possibly dust-filled pixels
!!$               ! Note that we leave the cldtype  alone, so we can still tell
!!$               ! that the pixels were originally flagged as cloud
!!$               if (verbose) write(*,*) 'Total clouds before correction: ', &
!!$                    count(imager_pavolonis%cldmask(:,:,1) .gt. 0), &
!!$                    count(imager_pavolonis%cldmask(:,:,2) .gt. 0)
!!$               where(tot_cldmask_uncertainty .gt. 70          .and. &
!!$                    (imager_pavolonis%cldtype(:,:,1) .eq. 3  .or. &
!!$                    imager_pavolonis%cldtype(:,:,2) .eq. 3) .and. &
!!$                    imager_geolocation%latitude .gt.    0.0  .and. &
!!$                    imager_geolocation%latitude .lt.   40.0  .and. &
!!$                    imager_geolocation%longitude .gt. -40.0  .and. &
!!$                    imager_geolocation%longitude .lt.  75.0)
!!$                  imager_pavolonis%cldmask(:,:,1) = 0
!!$                  imager_pavolonis%cldmask(:,:,2) = 0
!!$               end where
!!$               if (verbose) write(*,*) 'Total clouds after correction: ', &
!!$                    count(imager_pavolonis%cldmask(:,:,1) .gt. 0), &
!!$                    count(imager_pavolonis%cldmask(:,:,2) .gt. 0)
!!$               deallocate(tot_cldmask_uncertainty)
!!$            end if
!!$         end if
!!$      end if

      ! create output netcdf files.
      if (verbose) write(*,*) 'Create output netcdf files'
      if (verbose) write(*,*) 'output_path: ', trim(output_path)

      call netcdf_output_create(output_path, out_paths, granule, global_atts, &
           source_atts, preproc_dims, imager_angles, imager_geolocation, &
           netcdf_info, channel_info, include_full_brdf, nwp_flag, &
           preproc_opts%do_cloud_emis, preproc_opts%use_seviri_ann_ctp_fg, &
           preproc_opts%use_seviri_ann_mlay, verbose)

      ! perform RTTOV calculations
      if (verbose) write(*,*) 'Perform RTTOV calculations'
      if (nwp_flag .gt. 5 .and. nwp_flag .le. 8) then
         call rttov_driver_gfs(rttov_coef_path, rttov_emiss_path, granule, &
              preproc_dims, preproc_geoloc, preproc_geo, preproc_prtm, &
              preproc_surf, preproc_cld, netcdf_info, channel_info, &
              preproc_opts, verbose)
         ! Call cloud emissivity function
#ifdef INCLUDE_SATWX
         if (preproc_opts%do_cloud_emis) then
            call get_cloud_emis(channel_info, imager_measurements, &
                  imager_geolocation, preproc_dims, preproc_geoloc, &
                  preproc_cld, preproc_prtm, imager_cloud, &
                  granule%sensor, verbose)
         end if
#endif
      else
#ifdef INCLUDE_SATWX
         if (preproc_opts%do_cloud_emis) call get_trop_tp(preproc_prtm, preproc_dims)
#endif
         call rttov_driver(rttov_coef_path, rttov_emiss_path, granule, &
              preproc_dims, preproc_geoloc, preproc_geo, preproc_prtm, &
              preproc_surf, preproc_cld, netcdf_info, channel_info, &
              preproc_opts, verbose)
         ! Call cloud emissivity function
         if (preproc_opts%do_cloud_emis) then
#ifdef INCLUDE_SATWX
            call get_cloud_emis(channel_info, imager_measurements, &
                  imager_geolocation, preproc_dims, preproc_geoloc, &
                  preproc_cld, preproc_prtm, imager_cloud, &
                  granule%sensor, verbose)
            call do_cb_detect(channel_info, imager_measurements, &
                 imager_geolocation, imager_cloud, imager_pavolonis, &
                 granule%sensor, verbose)
#else
            write(*,*) "ERROR: Cannot compute cloud emissivity and CB locations without SatWx."
#endif
         end if
      end if

#ifdef WRAPPER

      corrupt = .false.

      ! repeat write attempt in case output files are corrupt implementation
      ! necessary for DWD processing chain running on ECMWF, where writing to
      ! network file system seems to cause random errors in netcdf output files
      ! that are not captured during creation
      do check_output = 1, 100

         ! write netcdf output files
         if (verbose) write(*,*) 'Write netcdf output files'
         call netcdf_output_write_swath(imager_flags, imager_angles, &
              imager_geolocation, imager_measurements, imager_cloud, imager_time, &
              imager_pavolonis, netcdf_info, channel_info, surface, &
              include_full_brdf, preproc_opts%do_cloud_emis, &
              preproc_opts%use_seviri_ann_ctp_fg, preproc_opts%use_seviri_ann_mlay)

         ! close output netcdf files
         if (verbose) write(*,*)'Close netcdf output files'
         call netcdf_output_close(netcdf_info)

         ! check whether output files are corrupt
         if (verbose) write(*,*)'Check whether output files are corrupt'
         call netcdf_output_check(output_path, out_paths, corrupt, verbose)

         ! exit loop if output files are not corrupt, else try writing again
         if (.not. corrupt) then
            write(*,*) 'No output file is corrupt at attempt ', check_output
            exit
         else
            write(*,*) 'A preprocessing output file is corrupt - ', &
                 'rewriting attempt no. ', check_output
            ! recreate output files if previous attempt produced corrupt files
            call netcdf_output_create(output_path, out_paths, granule, &
                 global_atts, source_atts, preproc_dims, imager_angles, &
                 imager_geolocation, netcdf_info, channel_info, &
                 include_full_brdf, nwp_flag, preproc_opts%do_cloud_emis, &
                 verbose)

         end if

      end do

#else

      ! write netcdf output files
      if (verbose) write(*,*) 'Write netcdf output files'
      call netcdf_output_write_swath(imager_flags, imager_angles, &
           imager_geolocation, imager_measurements, imager_cloud, imager_time, &
           imager_pavolonis, netcdf_info, channel_info, surface, include_full_brdf, &
           preproc_opts%do_cloud_emis, preproc_opts%use_seviri_ann_ctp_fg, &
           preproc_opts%use_seviri_ann_mlay)

      ! close output netcdf files
      if (verbose) write(*,*)'Close netcdf output files'
      call netcdf_output_close(netcdf_info, preproc_opts%use_seviri_ann_ctp_fg)

#endif

      ! deallocate the array parts of the structures
      if (verbose) write(*,*) 'Deallocate chunk specific structures'
      call deallocate_ecmwf_structures(ecmwf)
      call deallocate_preproc_structures(preproc_dims, preproc_geoloc, &
           preproc_geo, preproc_prtm, preproc_surf, preproc_cld)
      call deallocate_imager_structures(imager_geolocation, imager_angles, &
           imager_flags, imager_time, imager_measurements, imager_pavolonis, &
           imager_cloud)
      call deallocate_surface_structures(surface, channel_info, &
           include_full_brdf)

   end do ! end looping over chunks

   if (verbose) write(*,*)'Deallocate remaining memory'
   deallocate(chunk_starts)
   deallocate(chunk_ends)

   ! deallocate the array parts of the structures
   call deallocate_channel_info(channel_info)

   ! deallocate optional arguments
   if (associated(preproc_opts%channel_ids)) deallocate(preproc_opts%channel_ids)

   ! Notify of sucessful completion
   write(*,*) 'orac_preproc is complete. Output written to:'
   Write(*,*) trim(output_path)

#ifdef WRAPPER
end subroutine orac_preproc
#else
end program orac_preproc
#endif
