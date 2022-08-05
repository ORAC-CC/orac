!-------------------------------------------------------------------------------
! Name: rttov_driver.F90
!
! Purpose:
! Initialise and run RTTOV on the profiles contained in preproc_prtm and output
! the results into three NetCDF files (LWRTM, SWRTM, PRTM).
!
! Description and Algorithm details:
! 1)  Select the appropriate coefficient file for the sensor.
! 2)  Initialise options structure.
! 3)  Write out details of long and shortwave channels.
! 4)  Build the profiles structure from preproc_prtm. This currently neglects
!     surfacetype and the details required for the addsolar option.
! 5)  Write the profiles structure to the PRTM output file.
! 6)  Loop of long and shortwave.
! 7)     Set up coefficients.
! 8)     Allocate channel and emissivity arrays.
! 9)     Read RTTOV emissivity atlas. (Consider inputting emissivity.)
! 10)    Allocate radiance and transmission structures.
! 11)    Perform RTTOV direct calculation
! 12)    Reformat RTTOV output into a multidimensional array and perform
!        calculations to determine required above/below cloud fields.
!        If shortwave, apply airmass correction.
! 13)    Write results to appropriate output file.
! 14)    Deallocate arrays and structures.
!
! Arguments:
! Name           Type   In/Out/Both Description
! ------------------------------------------------------------------------------
! coef_path      string in   Folder containing RTTOV coefficient files
! emiss_path     string in   Folder containing MODIS monthly average emissivity
! granule        struct in   Parameters of the swath file
! preproc_dims   struct in   Summary of preprocessing grid definitions
! preproc_geoloc struct in   Summary of preprocessing lat/lon
! preproc_geo    struct in   Summary of preprocessing geometry
! preproc_prtm   struct both Summary of profiles
! preproc_surf   struct in   Summary of surface fields
! preproc_cld    struct both Summary of cloud fields
! netcdf_info    struct both Summary of NCDF file properties.
! channel_info   struct in   Structure summarising the channels to be processed
! pre_opts       struct in   Processing options
! verbose        logic  in   T: print status information; F: don't
!
! History:
! 2012/03/27, MJ: provides initial implementation based on the
!    example program example_fw of  Annex X of the RTTOV V10.2 user guide V1.3.
! 2012/05/23, MJ: fixes bug with AVHRR setup.
! 2012/06/20, CP: removed emissivity, implemented new calls to
!    call_rtm_ir and call_rtm_solar
! 2012/07/29, CP: improved readability added in algorithm description, added in
!    month variable required for emissivity
! 2012/08/02, MJ: implements writing of RTTOV output to netcdf file.
! 2012/08/06, CP: added in solazi angle
! 2012/08/10, CP: modified how RTTOV_direct is called
! 2012/08/24, MJ: rearranged filtering and calling of rtm
! 2012/08/24, MJ: implemented writing of fill values in RTTOV output if not a
!    good preproc pixel
! 2012/08/28, MJ: initialized emissivity_out properly
! 2012/08/28, CP: general tidy up of code formatting.
!    fixed bug where prtm lat and longitude were switched around.
! 2012/08/29, CP: fix bug that gave unrealistic values of profile longitudes
!    readded lw and sw to filter_array, commented out printouts
!    used profile error status to skip RTTOV calculations if appropriate
! 2012/08/31, CP: changed position of emissivity calculation to speed up code
! 2012/09/13, CP: changed AATSR coefficient file naming
! 2012/09/13, CP: changed criteria for processing of coefficients and tidied up
!    file
! 2012/10/04, CP: fixed bug and wrote skin temp and surface pressure to output
!    prtm file
! 2012/10/24, CP: added extra level to profile in define_preproc_grid so had to
!    change code to read surface info into profile
! 2012/11/14, CP: converted output to levels from layers
! 2012/11/14, CP: read in surface pressure from badc netcdf files for AATSR
!    instrument cleaned up code
! 2012/11/14, CP: atsr ir coefficients are in reverse order a temporary code
!    fudge has been introduced to read the coefficient files correctly
! 2013/03/08, GT: Changed the (dummy) values of surface/2m Q, O3, as well as
!    CTP to profiles(1)%nlayers profile value as profiles(1)%nlevels is past the
!    end program the arrays. Added check on pressure profile against the RTTOV
!    "pmax" parameter to prevent RTTOV throwing an error. Pressures greater than
!    pmax are set to pmax in the RTTOV profile structure.
! 2013/03/14, CP: added in land sea mask
! 2013/03/19, GT: Tidied up some debugging write statements
! 2013/07/25, MJ: tidies up some preprocessor statements
! 2013/10/14, CP: bug fix changed number of lay/level in rttov_alloc_rad and in
!    the assignment of profiles and preproc_lwrtm%plevels to
!    preproc_lwrtm%players
! 2013/10/29, CP: removed redundant variables nlevs and nlayers
! 2013/10/31, MJ: added another bugfix for levels/layers
! 2013/11/08, GM: added missing call to rttov_alloc_auxrad() in deallocate mode
! 2013/12/11, GM: Significant code clean up.
! 2014/01/15, GM: Removed some unnecessary RTTOV error handling calls.
! 2014/02/04, MJ: implements if to set verbosity of RTTOV based on "verbose"
!    variable
! 2014/02/10, AP: variable renaming
! 2014/06/13, GM: Mass-path removal from solar wavelength RTTOV calculated
!    transmittances should should not include the solar beam path as the RTTOV
!    calculated transmittances are only from a particular level to TOA in the
!    satellite viewing path. As such, the sza input to effective_2way_za() has
!    been set to zero.
! 2014/07/01, AP: More variable renaming. Altered treatment of surface pressure.
! 2014/08/01, AP: Remove unused counter fields.
! 2014/09/02, GM: start_Xd, counter_Xd, and stride_Xd from the netcdf_info
!    structure to be local variables here. There was no reason for them to be in
!    that structure.
! 2014/09/02, GM: Use the nc_write_array interface from the orac_ncdf module
!    in the common library.
! 2014/09/10, AP: Upgrade to RTTOV11.2, involving complete overhaul of code.
!    Angles removed from output files and output fields now use two dimensions
!    for the spatial dimension (previously one). Turned off solar component.
!    Solar transmission now relative to TOA rather than top level.
! 2014/09/28, GM: Fixed a significant performance regression by rearranging the
!    rtm variable dimensions.
! 2014/10/23, OS: set do_checkinput config variable to .false.; this avoids
!    extrapolation to negative profile values, causing a fatal RTTOV error
! 2015/01/15, AP: Eliminate channel_ids_abs.
! 2015/01/30, AP: Eliminate skint, sp, and lsf field for PRTM.
! 2015/02/19, GM: Added SEVIRI support.
! 2015/04/30, MSt: Added correct setting of coef_file name for NOAAs before
!    NOAA-10
! 2015/07/02, GM: Added code to remove the Rayleigh component from the RTTOV 11
!    computed transmittances.
! 2015/07/23, GM: Added specific humidity and ozone profile output.
! 2015/08/08, CP: Added ATSR2 functionality
! 2015/09/04, GM: Fix support for SEVIRI on MSG1, MSG3 and MSG4.
! 2015/10/19, GM: Add the option to use the MODIS emissivity product instead of
!    the RTTOV emissivity atlas.
! 2016/01/27, SP: Added support for RTTOV v11.3 via the NEW_RTTOV definition
! 2016/03/31, GM: Changes to support processing only SW or only LW channels.
! 2016/04/09, SP: Added multiple views
! 2016/04/11, SP: Added Himawari processing capability.
! 2016/05/19, SP: Added VIIRS/Suomi-NPP processing capability.
! 2016/05/27, SP: Updates to enable RTTOV to work correctly with multi-views
! 2016/07/05, SP: Added SLSTR/Sentinel-3 processing capability.
! 2017/02/10, GT: Added a check for the existence of valid ozone profile data.
! 2017/02/25, SP: Update to RTTOV v12.1 (ExtWork)
! 2017/03/24, SP: Tidying, improved method for finding snow fraction (ExtWork)
! 2017/03/29, SP: Switch to parallel RTTOV for performance improvement
! 2017/04/12, SP: Allow switch to parallel RTTOV only if OPENMP is enabled.
! 2017/06/21, OS: string name adaptations for METOPA/B
! 2017/11/15, SP: Add feature to give access to sensor azimuth angle
! 2017/12/12, GT: Changed Sentinel-3 platform name to Sentinel3a
! 2018/04/29, SP: Add cloud emissivity support for ECMWF profiles (ExtWork)
! 2018/08/30, SP: Allow variable CO2 in RTTOV, linear scaling from 2006 value
! 2019/08/14, SP: Add Fengyun4A support.
! 2019/08/15, SP: Add check for good pixels, meaning we don't run RTTOV on
!                 those we don't care about. This gives a big speedup to
!                 processing instruments that cross the dateline.
!
! Bugs:
! - BRDF not yet implemented here, so RTTOV internal calculation used.
! - Possible issue with conversion from layers to levels.
!-------------------------------------------------------------------------------

module rttov_driver_m

implicit none

contains

subroutine rttov_driver(coef_path, emiss_path, granule, preproc_dims, &
     preproc_geoloc, preproc_geo, preproc_prtm, preproc_surf, preproc_cld, &
     netcdf_info, channel_info, pre_opts, verbose)

   use channel_structures_m
   use netcdf_output_m
   use orac_ncdf_m
   use preproc_constants_m
   use preproc_structures_m
   use remove_rayleigh_m

   ! rttov_const contains useful RTTOV constants
   use rttov_const, only: &
        errorstatus_success, errorstatus_fatal, &
        zenmax, zenmaxv9

   ! rttov_types contains definitions of all RTTOV data types
   use rttov_types, only:  &
       rttov_options,      &
       rttov_coefs,        &
       rttov_chanprof,     &
       rttov_profile,      &
       rttov_emissivity,   &
       rttov_reflectance,  &
       rttov_transmission, &
       rttov_radiance,     &
       rttov_radiance2,    &
       rttov_traj

   use mod_rttov_emis_atlas, only : &
        rttov_emis_atlas_data, &
        atlas_type_ir, atlas_type_mw

   ! jpim, jprb and jplm are the RTTOV integer, real and logical KINDs
   use parkind1, only: jpim, jprb, jplm

   implicit none

#include "rttov_alloc_prof.interface"
#include "rttov_read_coefs.interface"
#include "rttov_setup_emis_atlas.interface"
#include "rttov_get_emis.interface"
#include "rttov_alloc_rad.interface"
#include "rttov_alloc_transmission.interface"
#include "rttov_alloc_traj.interface"
#ifdef INCLUDE_RTTOV_OPENMP
#include "rttov_parallel_direct.interface"
#else
#include "rttov_direct.interface"
#endif
#include "rttov_deallocate_emis_atlas.interface"
#include "rttov_dealloc_coefs.interface"

   ! Arguments
   character(len=*),           intent(in)    :: coef_path
   character(len=*),           intent(in)    :: emiss_path
   type(setup_args_t),         intent(in)    :: granule
   type(preproc_dims_t),       intent(in)    :: preproc_dims
   type(preproc_geoloc_t),     intent(in)    :: preproc_geoloc
   type(preproc_geo_t),        intent(in)    :: preproc_geo
   type(preproc_prtm_t),       intent(inout) :: preproc_prtm
   type(preproc_surf_t),       intent(in)    :: preproc_surf
   type(preproc_cld_t),        intent(inout) :: preproc_cld
   type(netcdf_output_info_t), intent(inout) :: netcdf_info
   type(channel_info_t),       intent(in)    :: channel_info
   type(preproc_opts_t),       intent(in)    :: pre_opts
   logical,                    intent(in)    :: verbose

   ! RTTOV in/outputs
   type(rttov_options)                  :: opts
   type(rttov_coefs)                    :: coefs
   type(rttov_emis_atlas_data)          :: emis_atlas
   type(rttov_chanprof),    allocatable :: chanprof(:)
   type(rttov_profile),     allocatable :: profiles(:)
   logical(kind=jplm),      allocatable :: calcemis(:)
   type(rttov_emissivity),  allocatable :: emissivity(:)
   real(kind=jprb),         allocatable :: emis_data(:)
   type(rttov_transmission)             :: transmission
   type(rttov_radiance)                 :: radiance
   type(rttov_radiance2)                :: radiance2
   type(rttov_traj)                     :: traj

   ! RTTOV variables
   integer(kind=jpim)                   :: stat
   integer(kind=jpim)                   :: nprof, nevals, imonth
   integer(kind=jpim)                   :: nlevels, nlayers
   integer(kind=jpim),      allocatable :: input_chan(:)
   logical                              :: write_rttov

   ! Loop variables
   integer(kind=lint)                   :: i, j
   integer(kind=lint)                   :: i_, j_
   integer(kind=lint)                   :: i_coef
   integer(kind=lint)                   :: count, nchan
   integer(kind=lint)                   :: idim, jdim

   ! Coefficient file selection
   character(len=file_length)           :: coef_file_vis, coef_file_ir
   character(len=path_length)           :: coef_full_path

   ! Scratch variables
   integer(kind=lint),      allocatable :: dummy_lint_1dveca(:)
   integer(kind=lint),      allocatable :: dummy_lint_1dvecb(:)
   real(kind=sreal),        allocatable :: dummy_sreal_1dveca(:)

   ! Useful aliases
   integer,                 parameter   :: ALLOC=1, DEALLOC=0

   ! View variables
   integer(kind=sint)                   :: cview
   integer,                 allocatable :: chan_pos(:)

   ! CO2 calculation variables
   real(kind=sreal)                     :: co2_val
   real(kind=sreal)                     :: yrfrac

   if (verbose) write(*,*) '<<<<<<<<<<<<<<< Entering rttov_driver()'

   if (verbose) write(*,*) 'coef_path: ', trim(coef_path)
   if (verbose) write(*,*) 'emiss_path: ', trim(emiss_path)
   if (verbose) write(*,*) 'sensor: ', trim(granule%sensor)
   if (verbose) write(*,*) 'platform: ', trim(granule%platform)
   if (verbose) write(*,*) 'Date: ', granule%year, granule%month, granule%day

   ! Determine coefficient filename (Vis/IR distinction made later)
   select case (trim(granule%sensor))
   case('ATSR2')
      coef_file_vis = 'rtcoef_ers_2_atsr_o3co2.dat'
      coef_file_ir = 'rtcoef_ers_2_atsr_o3co2_ironly.dat'
   case('AATSR')
      coef_file_vis = 'rtcoef_envisat_1_atsr-shifted_o3co2.dat'
      coef_file_ir = 'rtcoef_envisat_1_atsr-shifted_o3co2_ironly.dat'
   case('ABI')
      if (trim(granule%platform) == 'GOES-16') then
          coef_file_vis = 'rtcoef_goes_16_abi_o3co2.dat'
          coef_file_ir = 'rtcoef_goes_16_abi_o3co2_ironly.dat'
      else if (trim(granule%platform) == 'GOES-17') then
          coef_file_vis = 'rtcoef_goes_17_abi_o3co2.dat'
          coef_file_ir = 'rtcoef_goes_17_abi_o3co2_ironly.dat'
      else
         write(*,*) 'ERROR: rttov_driver(): Invalid GOES platform: ', &
                    trim(granule%platform)
         stop error_stop_code
      end if
   case('AGRI')
      if (trim(granule%platform) == 'FY-4A') then
         coef_file_vis = 'rtcoef_fy4_1_agri_o3co2.dat'
         coef_file_ir = 'rtcoef_fy4_1_agri_o3co2_ironly.dat'
      else if (trim(granule%platform) == 'FY-4B') then
         coef_file_vis = 'rtcoef_fy4_2_agri_o3co2.dat'
         coef_file_ir = 'rtcoef_fy4_2_agri_o3co2_ironly.dat'
      else
         write(*,*) 'ERROR: rttov_driver(): Invalid Fengyun-4 platform: ', &
                    trim(granule%platform)
         stop error_stop_code
      end if
   case('AHI')
      if (trim(granule%platform) == 'Himawari-8') then
        coef_file_vis = 'rtcoef_himawari_8_ahi_o3co2.dat'
        coef_file_ir = 'rtcoef_himawari_8_ahi_o3co2_ironly.dat'
      else if (trim(granule%platform) == 'Himawari-9') then
        coef_file_vis = 'rtcoef_himawari_9_ahi_o3co2.dat'
        coef_file_ir = 'rtcoef_himawari_9_ahi_o3co2_ironly.dat'
      else
         write(*,*) 'ERROR: rttov_driver(): Invalid HIMAWARI platform: ', &
                    trim(granule%platform)
         stop error_stop_code
      end if
   case('AVHRR')
      if (index(granule%platform, 'noaa') >= 1) then
         if(granule%platform(5:5) == '1') then
            coef_file_vis = 'rtcoef_noaa_'//granule%platform(5:6)//'_avhrr_o3co2.dat'
            coef_file_ir = 'rtcoef_noaa_'//granule%platform(5:6)//'_avhrr_o3co2_ironly.dat'
          else
            coef_file_vis = 'rtcoef_noaa_'//granule%platform(5:5)//'_avhrr_o3co2.dat'
            coef_file_ir = 'rtcoef_noaa_'//granule%platform(5:5)//'_avhrr_o3co2_ironly.dat'
          end if
       else if (index(granule%platform, 'metop') >= 1) then
          if (granule%platform(6:6) == "a") then
             coef_file_vis = 'rtcoef_metop_2_avhrr_o3co2.dat'
             coef_file_ir = 'rtcoef_metop_2_avhrr_o3co2_ironly.dat'
          else if (granule%platform(6:6) == "b") then
             coef_file_vis = 'rtcoef_metop_1_avhrr_o3co2.dat'
             coef_file_ir = 'rtcoef_metop_1_avhrr_o3co2_ironly.dat'
          else
             write(*,*) 'ERROR: rttov_driver(): Invalid Metop platform: ', &
                  trim(granule%platform)
             stop error_stop_code
          end if
      else
         write(*,*) 'ERROR: rttov_driver(): Invalid AVHRR platform: ', &
                    trim(granule%platform)
         stop error_stop_code
      end if
   case('MODIS')
      if (trim(granule%platform) == 'TERRA') then
         coef_file_vis = 'rtcoef_eos_1_modis-shifted_o3co2.dat'
         coef_file_ir = 'rtcoef_eos_1_modis-shifted_o3co2_ironly.dat'
      else if (trim(granule%platform) == 'AQUA') then
         coef_file_vis = 'rtcoef_eos_2_modis-shifted_o3co2.dat'
         coef_file_ir = 'rtcoef_eos_2_modis-shifted_o3co2_ironly.dat'
      else
         write(*,*) 'ERROR: rttov_driver(): Invalid MODIS platform: ', &
                    trim(granule%platform)
         stop error_stop_code
      end if
   case('SEVIRI')
      if (trim(granule%platform) == 'MSG1') then
         coef_file_vis = 'rtcoef_msg_1_seviri_o3co2.dat'
         coef_file_ir = 'rtcoef_msg_1_seviri_o3co2_ironly.dat'
      else if (trim(granule%platform) == 'MSG2') then
         coef_file_vis = 'rtcoef_msg_2_seviri_o3co2.dat'
         coef_file_ir = 'rtcoef_msg_2_seviri_o3co2_ironly.dat'
      else if (trim(granule%platform) == 'MSG3') then
         coef_file_vis = 'rtcoef_msg_3_seviri_o3co2.dat'
         coef_file_ir = 'rtcoef_msg_3_seviri_o3co2_ironly.dat'
      else if (trim(granule%platform) == 'MSG4') then
         coef_file_vis = 'rtcoef_msg_4_seviri_o3co2.dat'
         coef_file_ir = 'rtcoef_msg_4_seviri_o3co2_ironly.dat'
      else
         write(*,*) 'ERROR: rttov_driver(): Invalid SEVIRI platform: ', &
                    trim(granule%platform)
         stop error_stop_code
      end if
   case('SLSTR')
      if (trim(granule%platform) == 'Sentinel3a') then
         coef_file_vis = 'rtcoef_sentinel3_1_slstr_o3co2.dat'
         coef_file_ir = 'rtcoef_sentinel3_1_slstr_o3co2_ironly.dat'
      else if (trim(granule%platform) == 'Sentinel3b') then
         coef_file_vis = 'rtcoef_sentinel3_2_slstr_o3co2.dat'
         coef_file_ir = 'rtcoef_sentinel3_2_slstr_o3co2_ironly.dat'
      else
         write(*,*) 'ERROR: rttov_driver(): Invalid SLSTR platform: ', &
                    trim(granule%platform)
         stop error_stop_code
      end if
   case('VIIRSI')
      if (trim(granule%platform) == 'SuomiNPP') then
         coef_file_vis = 'rtcoef_jpss_0_viirs_o3co2.dat'
         coef_file_ir = 'rtcoef_jpss_0_viirs_o3co2_ironly.dat'
      else if (trim(granule%platform) == 'NOAA20') then
         coef_file_vis = 'rtcoef_noaa_20_viirs_o3co2.dat'
         coef_file_ir = 'rtcoef_noaa_20_viirs_o3co2_ironly.dat'
      else
         write(*,*) 'ERROR: rttov_driver(): Invalid VIIRS platform: ', &
                    trim(granule%platform)
         stop error_stop_code
      end if
   case('VIIRSM')
      if (trim(granule%platform) == 'SuomiNPP') then
         coef_file_vis = 'rtcoef_jpss_0_viirs_o3co2.dat'
         coef_file_ir = 'rtcoef_jpss_0_viirs_o3co2_ironly.dat'
      else if (trim(granule%platform) == 'NOAA20') then
         coef_file_vis = 'rtcoef_noaa_20_viirs_o3co2.dat'
         coef_file_ir = 'rtcoef_noaa_20_viirs_o3co2_ironly.dat'
      else
         write(*,*) 'ERROR: rttov_driver(): Invalid VIIRS platform: ', &
                    trim(granule%platform)
         stop error_stop_code
      end if
   case default
      write(*,*) 'ERROR: rttov_driver(): Invalid sensor: ', trim(granule%sensor)
      stop error_stop_code
   end select

   if (verbose) write(*,*) 'RTTOV VIS coef file: ', trim(coef_file_vis)
   if (verbose) write(*,*) 'RTTOV IR coef file: ', trim(coef_file_ir)

   ! Initialise options structure (leaving default settings be)
   opts % interpolation % addinterp = .true. ! Interpolate input profile
   ! Removed as occassionally returns negative ozone at 0.005 hPa
   ! opts % interpolation % reg_limit_extrap = .true. ! Extrapolate to 0.5 Pa
   opts % config % do_checkinput = .false. ! necessary due to negative
   ! extrapolated values; from RTTOV 11 homepage: turns off RTTOV's internal
   ! checking for unphysical profile values and values outside the
   ! regression limits (NB by doing this the extrapolated values outside
   ! the regression limits will be reset to the limits: it will not result
   ! in unphysical extrapolated profile values being used)
   opts % rt_all % use_q2m   = .false. ! Do not use surface humidity
   opts % rt_all % addrefrac = .true.  ! Include refraction in path calc
   opts % rt_ir % addsolar   = .true.  ! Do not include reflected solar
   opts % rt_all % ozone_data = .true.  ! Include ozone profile
   if (pre_opts%do_co2) then
      opts % rt_all % co2_data   = .true.  ! Include CO2 profile
   else
      opts % rt_all % co2_data   = .false.  ! Include CO2 profile
   end if
   opts % config % verbose   = .false. ! Display only fatal error messages

   if (verbose) write(*,*) 'Write static information to the output files'

   ! Write LW channel information
   if (channel_info%nchannels_lw /= 0) then
      allocate(dummy_lint_1dveca(channel_info%nchannels_lw))
      allocate(dummy_lint_1dvecb(channel_info%nchannels_lw))
      allocate(dummy_sreal_1dveca(channel_info%nchannels_lw))
      count = 0
      do i = 1, channel_info%nchannels_total
         if (channel_info%channel_lw_flag(i) == 1) then
            count = count + 1
            dummy_lint_1dveca(count)  = i
            dummy_lint_1dvecb(count)  = channel_info%channel_ids_instr(i)
            dummy_sreal_1dveca(count) = channel_info%channel_wl_abs(i)
         end if
      end do

      call ncdf_write_array(netcdf_info%ncid_lwrtm, 'lw_channel_abs_ids', &
              netcdf_info%vid_lw_channel_abs_ids, dummy_lint_1dveca, &
              1, 1, channel_info%nchannels_lw)
      call ncdf_write_array(netcdf_info%ncid_lwrtm, 'lw_channel_instr_ids', &
              netcdf_info%vid_lw_channel_instr_ids, dummy_lint_1dvecb, &
              1, 1, channel_info%nchannels_lw)
      call ncdf_write_array(netcdf_info%ncid_lwrtm, 'lw_channel_wvl', &
              netcdf_info%vid_lw_channel_wvl, dummy_sreal_1dveca, &
              1, 1, channel_info%nchannels_lw)

      deallocate(dummy_lint_1dveca)
      deallocate(dummy_lint_1dvecb)
      deallocate(dummy_sreal_1dveca)
   end if


   ! Write SW channel information
   if (channel_info%nchannels_sw /= 0) then
      allocate(dummy_lint_1dveca(channel_info%nchannels_sw))
      allocate(dummy_lint_1dvecb(channel_info%nchannels_sw))
      allocate(dummy_sreal_1dveca(channel_info%nchannels_sw))
      count = 0
      do i = 1, channel_info%nchannels_total
         if (channel_info%channel_sw_flag(i) == 1) then
            count = count + 1
            dummy_lint_1dveca(count)  = i
            dummy_lint_1dvecb(count)  = channel_info%channel_ids_instr(i)
            dummy_sreal_1dveca(count) = channel_info%channel_wl_abs(i)
         end if
      end do

      call ncdf_write_array(netcdf_info%ncid_swrtm, 'sw_channel_abs_ids', &
              netcdf_info%vid_sw_channel_abs_ids, dummy_lint_1dveca, &
              1, 1, channel_info%nchannels_sw)
      call ncdf_write_array(netcdf_info%ncid_swrtm, 'sw_channel_instr_ids', &
              netcdf_info%vid_sw_channel_instr_ids, dummy_lint_1dvecb, &
              1, 1, channel_info%nchannels_sw)
      call ncdf_write_array(netcdf_info%ncid_swrtm, 'sw_channel_wvl', &
              netcdf_info%vid_sw_channel_wvl, dummy_sreal_1dveca, &
              1, 1, channel_info%nchannels_sw)

      deallocate(dummy_lint_1dveca)
      deallocate(dummy_lint_1dvecb)
!     deallocate(dummy_sreal_1dveca)
   end if


   ! Allocate input profile structures (coefs struct not required as addclouds
   ! and addaerosl not set)
   if (verbose) write(*,*) 'Allocate profile structure'

   nprof   = preproc_dims%xdim * preproc_dims%ydim
   nlayers = preproc_dims%kdim
   nlevels = preproc_dims%kdim + 1
   allocate(profiles(nprof))
   call rttov_alloc_prof(stat, nprof, profiles, nlevels, opts, &
        ALLOC, init=.true._jplm)
   if (stat /= errorstatus_success)  then
      write(*,*) 'ERROR: rttov_alloc_prof(), errorstatus = ', stat
      stop error_stop_code
   end if

   profiles%id = 'standard'

   ! Compute the appropriate CO2 value for this scene
   ! This is taken from Martin's addition to the driver_for_bugsrad.f90 file
   ! Note: CO2 profile is assumed constant
   yrfrac  = granule%year + (granule%month / 12.0) + ((granule%day/30.)/12.0)
   co2_val = 380.0+(yrfrac-2006.)*1.7
   co2_val = co2_val * 1e-6 * 44.0095 / 28.9644

   ! Copy preprocessing grid data into RTTOV profile structure
   ! Create a lowest layer from the surface properties
   count = 0
   do jdim = preproc_dims%min_lat, preproc_dims%max_lat
      do idim = preproc_dims%min_lon, preproc_dims%max_lon
         count = count + 1

         ! Check to see if the ECMWF data read in includes ozone
         ! profiles: forecast data does not
         if (maxval(preproc_prtm%ozone(idim,jdim,:)) == 0.0) then
            opts % rt_all % ozone_data = .false. ! No valid ozone profiles!
         else
            opts % rt_all % ozone_data = .true.
         end if

         ! set gas units to 1, specifying gas input in kg/kg
         profiles(count)%gas_units = 1

         ! Profile information
         profiles(count)%p(:nlayers) = preproc_prtm%pressure(idim,jdim,:) * &
              pa2hpa ! convert from Pa to hPa
         profiles(count)%t(:nlayers) = preproc_prtm%temperature(idim,jdim,:)
         profiles(count)%q(:nlayers) = preproc_prtm%spec_hum(idim,jdim,:)
         profiles(count)%o3(:nlayers) = preproc_prtm%ozone(idim,jdim,:)

         ! Add CO2 in kg/kg for each level
         if (pre_opts%do_co2) profiles(count)%co2(:) = co2_val

         ! Surface information
         profiles(count)%s2m%p = exp(preproc_prtm%lnsp(idim,jdim)) * pa2hpa
         profiles(count)%s2m%t = preproc_prtm%temp2(idim,jdim)
         profiles(count)%s2m%u = preproc_prtm%u10(idim,jdim)
         profiles(count)%s2m%v = preproc_prtm%v10(idim,jdim)
         profiles(count)%s2m%wfetc = 100000.0
         profiles(count)%p(nlevels) = profiles(count)%s2m%p
         profiles(count)%t(nlevels) = preproc_prtm%skin_temp(idim,jdim)
         ! For lack of a better idea, use lowest level we actually have
         profiles(count)%q(nlevels) = profiles(count)%q(nlayers)
         profiles(count)%o3(nlevels) = profiles(count)%o3(nlayers)

         ! These features currently disabled and so do not need to be input
         profiles(count)%cfraction = 0.
!        profiles(count)%ctp   = profiles(count)%p(profiles(count)%nlayers)
!        profiles(count)%s2m%q = profiles(count)%q(profiles(count)%nlayers)
!        profiles(count)%s2m%o = profiles(count)%o3(profiles(count)%nlayers)

         profiles(count)%skin%t = preproc_prtm%skin_temp(idim,jdim)
         ! Force land emissivity from the atlas. !!CONSIDER REVISION!!
         profiles(count)%skin%surftype  = 0
         profiles(count)%skin%watertype = 1

         profiles(count)%date(1) = granule%year
         profiles(count)%date(2) = granule%month
         profiles(count)%date(3) = granule%day
         profiles(count)%elevation = 0. ! One day, we will do something here
         profiles(count)%latitude  = preproc_geoloc%latitude(jdim)
         ! Manual may say this is 0-360, but src/emsi_atlas/mod_iratlas.F90
         ! line 790 disagrees
         profiles(count)%longitude = preproc_geoloc%longitude(idim)
         ! Use poor man's approach to snow fraction
         if (preproc_prtm%snow_depth(idim,jdim) > 0.05) then
            profiles(count)%skin%snow_fraction = 1.
         else if (preproc_prtm%snow_depth(idim,jdim) > 0.00) then
            profiles(count)%skin%snow_fraction = &
                 preproc_prtm%snow_depth(idim,jdim) / 0.05
         else
            profiles(count)%skin%snow_fraction = 0.
         end if

         ! Write profiles structure to PRTM file (array operations needed to
         ! recast structure in form ncdf_write_array recognises)
         i_ = idim - preproc_dims%min_lon + 1
         j_ = jdim - preproc_dims%min_lat + 1
         call ncdf_write_array(netcdf_info%ncid_prtm, 'lon_rtm', &
              netcdf_info%vid_lon_pw, &
              (/profiles(count)%longitude/), &
              1, i_, 1)
         call ncdf_write_array(netcdf_info%ncid_prtm, 'lat_rtm', &
              netcdf_info%vid_lat_pw, &
              (/profiles(count)%latitude/), &
              1, j_, 1)

         call ncdf_write_array(netcdf_info%ncid_prtm, 'pprofile_rtm', &
              netcdf_info%vid_pprofile_lev_pw, &
              reshape(profiles(count)%p, (/nlevels, 1, 1/)), &
              1, 1, nlevels, 1, i_, 1, 1, j_, 1)
         call ncdf_write_array(netcdf_info%ncid_prtm, 'tprofile_rtm', &
              netcdf_info%vid_tprofile_lev_pw, &
              reshape(profiles(count)%t, (/nlevels, 1, 1/)), &
              1, 1, nlevels, 1, i_, 1, 1, j_, 1)
         call ncdf_write_array(netcdf_info%ncid_prtm, 'hprofile_rtm', &
              netcdf_info%vid_hprofile_lev_pw, &
              reshape(preproc_prtm%phi_lev(idim, jdim,:), &
              (/nlevels, 1, 1/)), 1, 1, nlevels, 1, i_, 1, 1, j_, 1)
         call ncdf_write_array(netcdf_info%ncid_prtm, 'qprofile_rtm', &
              netcdf_info%vid_qprofile_lev_pw, &
              reshape(profiles(count)%q, (/nlevels, 1, 1/)), &
              1, 1, nlevels, 1, i_, 1, 1, j_, 1)
         call ncdf_write_array(netcdf_info%ncid_prtm, 'o3profile_rtm', &
              netcdf_info%vid_o3profile_lev_pw, &
              reshape(profiles(count)%o3, (/nlevels, 1, 1/)), &
              1, 1, nlevels, 1, i_, 1, 1, j_, 1)
      end do
   end do

   ! Write fields not in profiles structure
!  call ncdf_write_array(netcdf_info%ncid_prtm, 'lsf_rtm', &
!       netcdf_info%vid_lsf_pw, &
!       preproc_prtm%land_sea_mask, &
!       1, 1, preproc_dims%xdim, 1, 1, preproc_dims%ydim)

   ! Do RTTOV calculations for long and shortwave in turn
   if (verbose) write(*,*) 'Do RTTOV calculations'

   ! Loop over view geometries
   do cview = 1, channel_info%nviews
      if (verbose) write(*,*) ' - Calculating for viewing geometry number', cview

      count = 0
      do jdim = preproc_dims%min_lat, preproc_dims%max_lat
         do idim = preproc_dims%min_lon, preproc_dims%max_lon

            count = count + 1
            profiles(count)%zenangle = preproc_geo%satza(idim,jdim,cview)
            profiles(count)%azangle = preproc_geo%satazi(idim,jdim,cview)
            profiles(count)%sunzenangle = preproc_geo%solza(idim,jdim,cview)
            profiles(count)%sunazangle = preproc_geo%solazi(idim,jdim,cview)
         end do
      end do

      do i_coef = 1, 2
         ! Set factors that differ between long and shortwave
         if (i_coef == 1) then
            ! Longwave
            nchan = 0

            ! Loop to determine how many LW channels exist with a given view
            do i_ = 1, channel_info%nchannels_lw
               if (channel_info%lw_view_ids(i_) == cview) nchan = nchan + 1
            end do

            if (nchan == 0) cycle

            allocate(input_chan(nchan))
            allocate(chan_pos(nchan))

            j_ = 1
            do i_ = 1, channel_info%nchannels_lw
               if (channel_info%lw_view_ids(i_) == cview) then
                  chan_pos(j_) = i_
                  input_chan(j_) = channel_info%channel_ids_rttov_coef_lw(i_)
                  j_ = j_ + 1
               end if
            end do

            ! This assumes the recommended structure of the RTTOV coef library
            coef_full_path = trim(adjustl(coef_path))//'/rttov13pred54L/'// &
                 trim(adjustl(coef_file_ir))
         else
            ! Shortwave
            nchan = 0

            ! Loop to determine how many SW channels exist with a given view
            do i_ = 1, channel_info%nchannels_sw
               if (channel_info%sw_view_ids(i_) == cview) nchan = nchan + 1
            end do

            if (nchan == 0) cycle

            allocate(input_chan(nchan))
            allocate(chan_pos(nchan))

            j_ = 1
            do i_ = 1, channel_info%nchannels_sw
               if (channel_info%sw_view_ids(i_) == cview) then
                  chan_pos(j_) = i_
                  input_chan(j_) = channel_info%channel_ids_rttov_coef_sw(i_)
                  j_ = j_ + 1
               end if
            end do

            coef_full_path = trim(adjustl(coef_path))//'/rttov13pred54L/'// &
                 trim(adjustl(coef_file_vis))
         end if

         if (verbose) write(*,*) 'Read coefficients'
         call rttov_read_coefs(stat, coefs, opts, form_coef='formatted', &
              channels=input_chan, file_coef=coef_full_path)
         if (stat /= errorstatus_success) then
            write(*,*) 'ERROR: rttov_read_coefs(), errorstatus = ', stat
            stop error_stop_code
         end if

         ! Force all SW channels to be processed
         if (i_coef == 2) coefs%coef%ss_val_chn = 1

         if (verbose) write(*,*) 'Allocate channel and emissivity arrays'
         allocate(chanprof(nchan))
         allocate(emissivity(nchan))
         allocate(emis_data(nchan))
         allocate(calcemis(nchan))

         chanprof%prof = 1
         do j = 1, nchan
            chanprof(j)%chan = j
         end do

         if (verbose) write(*,*) 'Allocate RTTOV structures'
         call rttov_alloc_rad(stat, nchan, radiance, nlevels, ALLOC, radiance2, &
              init=.true._jplm)
         if (stat /= errorstatus_success) then
            write(*,*) 'ERROR: rttov_alloc_rad(), errorstatus = ', stat
            stop error_stop_code
         end if
         call rttov_alloc_transmission(stat, transmission, nlevels, nchan, &
              ALLOC, init=.true._jplm)
         if (stat /= errorstatus_success) then
            write(*,*) 'ERROR: rttov_alloc_transmission(), errorstatus = ', stat
            stop error_stop_code
         end if
         call rttov_alloc_traj(stat, 1, nchan, opts, nlevels, coefs, &
              ALLOC, traj=traj)
         if (stat /= errorstatus_success) then
            write(*,*) 'ERROR: rttov_alloc_traj(), errorstatus = ', stat
            stop error_stop_code
         end if

         if (verbose) write(*,*) 'Fetch emissivity atlas'
         imonth = granule%month
         call rttov_setup_emis_atlas(stat, opts, imonth, atlas_type_ir, &
              emis_atlas, coefs=coefs, path=emiss_path)
         if (stat /= errorstatus_success) then
            write(*,*) 'ERROR: rttov_setup_emis_atlas(), errorstatus = ', stat
            stop error_stop_code
         end if

         ! Loop over profiles (as the conditions for processing LW and SW are
         ! different, we can't just pass the whole array)
         count = 0
#ifdef INCLUDE_RTTOV_OPENMP
         if (verbose) write(*,*) 'Run RTTOV_Parallel'
#else
         if (verbose) write(*,*) 'Run RTTOV'
#endif
         do jdim = preproc_dims%min_lat, preproc_dims%max_lat
            do idim = preproc_dims%min_lon, preproc_dims%max_lon

               count = count + 1

               ! Process points that contain information and satisfy the zenith
               ! angle restrictions of the coefficient file
               if ((i_coef == 1 .and. &
                    preproc_dims%counter_lw(idim,jdim,cview) > 0 .and. &
                    profiles(count)%zenangle <= zenmax) .or. &
                   (i_coef == 2 .and. &
                    preproc_dims%counter_sw(idim,jdim,cview) > 0 .and. &
                    profiles(count)%zenangle <= zenmaxv9)) then

                  ! Fetch emissivity from atlas
                  call rttov_get_emis(stat, opts, chanprof, &
                       profiles(count:count), coefs, emis_atlas, emis_data)
                  if (stat /= errorstatus_success) then
                     write(*,*) 'ERROR: rttov_get_emis(), errorstatus = ', &
                          stat
                     stop error_stop_code
                  end if
                  emissivity%emis_in = emis_data

                  ! Fetch emissivity from the MODIS CIMSS emissivity product
                  if (i_coef == 1 .and. pre_opts%use_modis_emis_in_rttov) then
                     where (preproc_surf%emissivity(idim,jdim,:) /= &
                          sreal_fill_value)
                        emissivity%emis_in = &
                             preproc_surf%emissivity(idim,jdim,chan_pos)
                     end where
                  end if

                  calcemis = emissivity%emis_in <= dither

                  if (preproc_dims%counter_lw(idim,jdim,cview) .gt. 0) then
                     ! Call RTTOV for this profile
#ifdef INCLUDE_RTTOV_OPENMP
                     call rttov_parallel_direct(stat, chanprof, opts, &
                          profiles(count:count), coefs, transmission, radiance, &
                          radiance2, calcemis, emissivity, traj=traj)
#else
                     call rttov_direct(stat, chanprof, opts, &
                          profiles(count:count), coefs, transmission, radiance, &
                          radiance2, calcemis, emissivity, traj=traj)
#endif

                     if (stat /= errorstatus_success) then
                        write(*,*) 'ERROR: rttov_direct(), errorstatus = ', stat
                        stop error_stop_code
                     end if

                     ! Remove the Rayleigh component from the RTTOV tranmittances.
                     if (i_coef == 2) then
                        call remove_rayleigh(nchan, nlevels, dummy_sreal_1dveca, &
                             profiles(count)%zenangle, profiles(count)%p, &
                             transmission%tau_levels, transmission%tau_total)
                     end if

                  end if
                  write_rttov = .true.
               else
                  write_rttov = .false.
               end if
               ! Reformat and write output to NCDF files
               if (i_coef == 1) then
                  do i_ = 1, nchan
                     call write_ir_rttov(netcdf_info, &
                          idim-preproc_dims%min_lon+1, &
                          jdim-preproc_dims%min_lat+1, &
                          profiles(count)%nlevels, emissivity, transmission, &
                          radiance, radiance2, write_rttov, chan_pos(i_), i_)
                  end do
               else
                  do i_ = 1, nchan
                     call write_solar_rttov(netcdf_info, coefs, &
                          idim-preproc_dims%min_lon+1, &
                          jdim-preproc_dims%min_lat+1, &
                          profiles(count)%nlevels, profiles(count)%zenangle, &
                          transmission, write_rttov, chan_pos(i_), i_)
                  end do
               end if
            end do
         end do

         ! Loop again for cloud radiance
         if (pre_opts%do_cloud_emis) then
            count = 0
            if (i_coef .eq. 1) then
#ifdef INCLUDE_RTTOV_OPENMP
            if (verbose) write(*,*) 'Run RTTOV_Parallel for cloud'
#else
            if (verbose) write(*,*) 'Run RTTOV for cloud'
#endif
               do jdim = preproc_dims%min_lat, preproc_dims%max_lat
                  do idim = preproc_dims%min_lon, preproc_dims%max_lon
                     count = count + 1
                     profiles(count)%cfraction = 1.
                     profiles(count)%ctp = preproc_prtm%trop_p(idim,jdim)
                    if (any(preproc_dims%counter_lw(idim,jdim,:) .gt. 0)) then
                     ! Call RTTOV for this profile
#ifdef INCLUDE_RTTOV_OPENMP
                     call rttov_parallel_direct(stat, chanprof, opts, &
                          profiles(count:count), coefs, transmission, radiance, &
                          radiance2, calcemis, emissivity, traj=traj)
#else
                     call rttov_direct(stat, chanprof, opts, &
                          profiles(count:count), coefs, transmission, radiance, &
                          radiance2, calcemis, emissivity, traj=traj)
#endif

                     ! Save into the appropriate arrays
                     preproc_cld%cloud_bt(idim,jdim,:) = radiance%bt
                     preproc_cld%clear_bt(idim,jdim,:) = radiance%bt_clear
                   end if
                  end do
               end do
            end if
         end if


         if (verbose) write(*,*) 'Deallocate structures'

         call rttov_deallocate_emis_atlas(emis_atlas)
         call rttov_alloc_traj(stat, 1, nchan, opts, nlevels, coefs, DEALLOC, &
              traj)
         call rttov_alloc_transmission(stat, transmission, nlevels, nevals, &
              DEALLOC)
         call rttov_alloc_rad(stat, nevals, radiance, nlevels, DEALLOC, &
              radiance2)
         call rttov_dealloc_coefs(stat, coefs)

         deallocate(input_chan)
         deallocate(chanprof)
         deallocate(emissivity)
         deallocate(emis_data)
         deallocate(calcemis)
         deallocate(chan_pos)
      end do !coef loop
   end do  !view loop

   if (channel_info%nchannels_sw /= 0) then
      deallocate(dummy_sreal_1dveca)
   end if
   call rttov_alloc_prof(stat, nprof, profiles, nlevels, opts, DEALLOC)
   deallocate(profiles)

   if (verbose) write(*,*) '>>>>>>>>>>>>>>> Leaving rttov_driver()'

end subroutine rttov_driver


#include "write_ir_rttov.F90"
#include "write_solar_rttov.F90"


end module rttov_driver_m
