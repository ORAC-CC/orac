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
! sensor         string in   Name of sensor.
! platform       string in   Name of satellite platform.
! preproc_dims   struct both Summary of preprocessing grid definitions
! preproc_geoloc struct in   Summary of preprocessing lat/lon
! preproc_geo    struct in   Summary of preprocessing geometry
! preproc_prtm   struct both Summary of profiles and surface fields
! netcdf_info    struct both Summary of NCDF file properties.
! channel_info   struct in   Structure summarising the channels to be processed
! year           sint   in   Year of observation (4 digit)
! month          sint   in   Month of year (1-12)
! day            siny   in   Day of month (1-31)
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
!
! $Id$
!
! Bugs:
! - Emissivity is read in elsewhere in the code but not utilised here.
! - BRDF not yet implimented here, so RTTOV internal calculation used.
! - Possible issue with conversion from layers to levels.
!-------------------------------------------------------------------------------

module rttov_driver_m

implicit none

contains

subroutine rttov_driver(coef_path,emiss_path,sensor,platform,preproc_dims, &
     preproc_geoloc,preproc_geo,preproc_prtm,preproc_surf,netcdf_info, &
     channel_info,year,month,day,use_modis_emis,verbose)

   use channel_structures_m
   use netcdf_output_m
   use orac_ncdf_m
   use preproc_constants_m
   use preproc_structures_m

   ! rttov_const contains useful RTTOV constants
   use rttov_const, only: &
        errorstatus_success, errorstatus_fatal, &
        q_mixratio_to_ppmv, o3_mixratio_to_ppmv, &
        zenmax, zenmaxv9

   ! rttov_types contains definitions of all RTTOV data types
   use rttov_types, only:    &
        rttov_options,       &
        rttov_coefs,         &
        rttov_chanprof,      &
        profile_type,        &
        rttov_emissivity,    &
        rttov_reflectance,   &
        transmission_type,   &
        radiance_type,       &
        radiance2_type,      &
        rttov_traj

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
#include "rttov_direct.interface"
#include "rttov_deallocate_emis_atlas.interface"
#include "rttov_dealloc_coefs.interface"

   ! Arguments
   character(len=path_length),     intent(in)    :: coef_path
   character(len=path_length),     intent(in)    :: emiss_path
   character(len=sensor_length),   intent(in)    :: sensor
   character(len=platform_length), intent(in)    :: platform
   type(preproc_dims_t),           intent(in)    :: preproc_dims
   type(preproc_geoloc_t),         intent(in)    :: preproc_geoloc
   type(preproc_geo_t),            intent(in)    :: preproc_geo
   type(preproc_prtm_t),           intent(inout) :: preproc_prtm
   type(preproc_surf_t),           intent(in)    :: preproc_surf
   type(netcdf_output_info_t),     intent(inout) :: netcdf_info
   type(channel_info_t),           intent(in)    :: channel_info
   integer(kind=sint),             intent(in)    :: year, month, day
   logical,                        intent(in)    :: use_modis_emis
   logical,                        intent(in)    :: verbose

   ! RTTOV in/outputs
   type(rttov_options)                  :: opts
   type(rttov_coefs)                    :: coefs
   type(rttov_chanprof),    allocatable :: chanprof(:)
   type(profile_type),      allocatable :: profiles(:)
   logical(kind=jplm),      allocatable :: calcemis(:)
   type(rttov_emissivity),  allocatable :: emissivity(:)
   real(kind=jprb),         allocatable :: emis_atlas(:)
   type(transmission_type)              :: transmission
   type(radiance_type)                  :: radiance
   type(radiance2_type)                 :: radiance2
   type(rttov_traj)                     :: traj

   ! RTTOV variables
   integer(kind=jpim)                   :: stat
   integer(kind=jpim)                   :: nprof, nevals, imonth
   integer(kind=jpim)                   :: nlevels, nlayers
   integer(kind=jpim),      allocatable :: input_chan(:)
   logical                              :: write_rttov

   ! Loop variables
   integer(kind=lint)                   :: i_coef, j
   integer(kind=lint)                   :: i_, j_
   integer(kind=lint)                   :: count, nchan
   integer(kind=lint)                   :: idim, jdim

   ! Coefficient file selection
   character(len=file_length)           :: coef_file
   character(len=path_length)           :: coef_full_path

   ! Scratch variables
   integer(kind=lint),      allocatable :: dummy_lint_1dveca(:)
   integer(kind=lint),      allocatable :: dummy_lint_1dvecb(:)
   real(kind=sreal),        allocatable :: dummy_sreal_1dveca(:)

   ! Useful aliases
   integer,                 parameter   :: ALLOC=1, DEALLOC=0

   real                                 :: p_0, sec_vza, lambda, tau_ray_0, &
                                           tau_ray_p

   if (verbose) write(*,*) '<<<<<<<<<<<<<<< Entering rttov_driver()'

   if (verbose) write(*,*) 'coef_path: ', trim(coef_path)
   if (verbose) write(*,*) 'emiss_path: ', trim(emiss_path)
   if (verbose) write(*,*) 'sensor: ', trim(sensor)
   if (verbose) write(*,*) 'platform: ', trim(platform)
   if (verbose) write(*,*) 'Date: ', year, month, day
#ifdef NEW_RTTOV
   if (verbose) write(*,*) 'Using new RTTOV version (>11.2)'
#endif


   ! Determine coefficient filename (vis/IR distinction made later)
   select case (trim(sensor))
   case('AATSR')
      coef_file = 'rtcoef_envisat_1_atsr.dat'
   case('ATSR2')
      coef_file = 'rtcoef_ers_2_atsr.dat'
   case('AVHRR')
      if (index(platform,'noaa') >= 1) then
         if(platform(5:5) == '1') then
            coef_file = 'rtcoef_noaa_'//platform(5:6)//'_avhrr.dat'
          else
            coef_file = 'rtcoef_noaa_'//platform(5:5)//'_avhrr.dat'
          end if
      else if (index(platform,'metop') >= 1) then
         coef_file = 'rtcoef_metop_'//platform(6:7)//'_avhrr.dat'
      else
         write(*,*) 'ERROR: rttov_driver(): Invalid AVHRR platform: ', &
                    trim(platform)
         stop error_stop_code
      end if
   case('MODIS')
      if (trim(platform) == 'TERRA') then
         coef_file = 'rtcoef_eos_1_modis.dat'
      else if (trim(platform) == 'AQUA') then
         coef_file = 'rtcoef_eos_2_modis.dat'
      else
         write(*,*) 'ERROR: rttov_driver(): Invalid MODIS platform: ', &
                    trim(platform)
         stop error_stop_code
      end if
   case('SEVIRI')
      if (trim(platform) == 'MSG1') then
         coef_file = 'rtcoef_msg_1_seviri.dat'
      else if (trim(platform) == 'MSG2') then
         coef_file = 'rtcoef_msg_2_seviri.dat'
      else if (trim(platform) == 'MSG3') then
         coef_file = 'rtcoef_msg_3_seviri.dat'
      else if (trim(platform) == 'MSG4') then
         coef_file = 'rtcoef_msg_4_seviri.dat'
      else
         write(*,*) 'ERROR: rttov_driver(): Invalid SEVIRI platform: ', &
                    trim(platform)
         stop error_stop_code
      end if
   case default
         write(*,*) 'ERROR: rttov_driver(): Invalid sensor: ', trim(sensor)
         stop error_stop_code
   end select

   if (verbose) write(*,*) 'RTTOV coef file: ', trim(coef_file)


   ! Initialise options structure (leaving default settings be)
   opts % interpolation % addinterp        = .true.  ! Interpolate input profile
   ! Removed as occassionally returns negative ozone at 0.005 hPa
   ! opts % interpolation % reg_limit_extrap = .true.  ! Extrapolate to 0.5 Pa
   opts % config % do_checkinput = .false. ! necessary due to negative
     ! extrapolated values; from RTTOV 11 homepage: turns off RTTOV's internal
     ! checking for unphysical profile values and values outside the
     ! regression limits (NB by doing this the extrapolated values outside
     ! the regression limits will be reset to the limits: it will not result
     ! in unphysical extrapolated profile values being used)
   opts % rt_all % use_q2m   = .false. ! Do not use surface humidity
   opts % rt_all % addrefrac = .true.  ! Include refraction in path calc
   opts % rt_ir % addsolar   = .false. ! Do not include reflected solar
   opts % rt_ir % ozone_data = .true.  ! Include ozone profile
   opts % config % verbose   = .false. ! Display only fatal error messages

   if (verbose) write(*,*) 'Write static information to the output files'

   ! Write LW channel information
   if (channel_info%nchannels_lw .ne. 0) then
      allocate(dummy_lint_1dveca(channel_info%nchannels_lw))
      allocate(dummy_lint_1dvecb(channel_info%nchannels_lw))
      allocate(dummy_sreal_1dveca(channel_info%nchannels_lw))
      count=0
      do i_coef=1,channel_info%nchannels_total
         if (channel_info%channel_lw_flag(i_coef) == 1) then
            count = count + 1
            dummy_lint_1dveca(count)  = i_coef
            dummy_lint_1dvecb(count)  = channel_info%channel_ids_instr(i_coef)
            dummy_sreal_1dveca(count) = channel_info%channel_wl_abs(i_coef)
         end if
      end do

      call nc_write_array(netcdf_info%ncid_lwrtm, 'lw_channel_abs_ids', &
              netcdf_info%vid_lw_channel_abs_ids, dummy_lint_1dveca, &
              1, 1, channel_info%nchannels_lw)
      call nc_write_array(netcdf_info%ncid_lwrtm, 'lw_channel_instr_ids', &
              netcdf_info%vid_lw_channel_instr_ids, dummy_lint_1dvecb, &
              1, 1, channel_info%nchannels_lw)
      call nc_write_array(netcdf_info%ncid_lwrtm, 'lw_channel_wvl', &
              netcdf_info%vid_lw_channel_wvl, dummy_sreal_1dveca, &
              1, 1, channel_info%nchannels_lw)

      deallocate(dummy_lint_1dveca)
      deallocate(dummy_lint_1dvecb)
      deallocate(dummy_sreal_1dveca)
   end if


   ! Write SW channel information
   if (channel_info%nchannels_sw .ne. 0) then
      allocate(dummy_lint_1dveca(channel_info%nchannels_sw))
      allocate(dummy_lint_1dvecb(channel_info%nchannels_sw))
      allocate(dummy_sreal_1dveca(channel_info%nchannels_sw))
      count=0
      do i_coef=1,channel_info%nchannels_total
         if (channel_info%channel_sw_flag(i_coef) == 1) then
            count = count + 1
            dummy_lint_1dveca(count)  = i_coef
            dummy_lint_1dvecb(count)  = channel_info%channel_ids_instr(i_coef)
            dummy_sreal_1dveca(count) = channel_info%channel_wl_abs(i_coef)
         end if
      end do

      call nc_write_array(netcdf_info%ncid_swrtm, 'sw_channel_abs_ids', &
              netcdf_info%vid_sw_channel_abs_ids, dummy_lint_1dveca, &
              1, 1, channel_info%nchannels_sw)
      call nc_write_array(netcdf_info%ncid_swrtm, 'sw_channel_instr_ids', &
              netcdf_info%vid_sw_channel_instr_ids, dummy_lint_1dvecb, &
              1, 1, channel_info%nchannels_sw)
      call nc_write_array(netcdf_info%ncid_swrtm, 'sw_channel_wvl', &
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

   ! Copy preprocessing grid data into RTTOV profile structure
   ! Create a lowest layer from the surface properties
   count = 0
   do jdim=preproc_dims%min_lat,preproc_dims%max_lat
      do idim=preproc_dims%min_lon,preproc_dims%max_lon
         count = count + 1

         ! If using RTTOV version 11.3 or greater then
         ! set gas units to 1, specifying gas input in kg/kg
         ! (2 = ppmv moist air, 1 = kg/kg, 0 = old method, -1 = ppmv dry air)
#ifdef NEW_RTTOV
         profiles(count)%gas_units = 1
#endif
         ! Profile information
         profiles(count)%p(:nlayers) = preproc_prtm%pressure(idim,jdim,:) * &
              pa2hpa ! convert from Pa to hPa
         profiles(count)%t(:nlayers) = preproc_prtm%temperature(idim,jdim,:)
         ! convert from kg/kg to ppmv by multiplying with dry air molecule
         ! weight(28.9644)/molcule weight of gas (e.g. o3  47.9982)*1.0E6
#ifdef NEW_RTTOV
         profiles(count)%q(:nlayers) = &
              preproc_prtm%spec_hum(idim,jdim,:)
         profiles(count)%o3(:nlayers) = &
              preproc_prtm%ozone(idim,jdim,:)
#else
         profiles(count)%q(:nlayers) = &
              preproc_prtm%spec_hum(idim,jdim,:) * q_mixratio_to_ppmv
         profiles(count)%o3(:nlayers) = &
              preproc_prtm%ozone(idim,jdim,:) * o3_mixratio_to_ppmv
#endif
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

         profiles(count)%date(1) = year
         profiles(count)%date(2) = month
         profiles(count)%date(3) = day
         profiles(count)%elevation = 0. ! One day, we will do something here
         profiles(count)%latitude  = preproc_geoloc%latitude(jdim)
         ! Manual may say this is 0-360, but src/emsi_atlas/mod_iratlas.F90
         ! line 790 disagrees
         profiles(count)%longitude = preproc_geoloc%longitude(idim)
         ! Use poor man's approach to snow fraction
         if (preproc_prtm%snow_albedo(idim,jdim) > 0.) &
              profiles(count)%snow_frac = 1.
         profiles(count)%zenangle    = preproc_geo%satza(idim,jdim,1)
         ! Only needed if addsolar=.true. Multiple views not yet important
!         profiles(count)%sunzenangle = preproc_geo%solza(idim,jdim,1)
!         profiles(count)%sunazangle  = preproc_geo%solazi(idim,jdim,1)
         ! We don't know which direction the satellite is (as it isn't output)
!         profiles(count)%azangle     = 180. - preproc_geo%relazi(idim,jdim,1) +&
!              preproc_geo%solazi(idim,jdim,1)
!         write(*,*)profiles(count)%p(:nlayers)
!         write(*,*)""
!         write(*,*)""
!         write(*,*) exp(preproc_prtm%lnsp(idim,jdim))
!         write(*,*)profiles(count)%latitude,profiles(count)%longitude
!         write(*,*)""
!         write(*,*)""
!         write(*,*)""
!         write(*,*)preproc_prtm%pressure(idim,jdim,:)
!         
!         
!         stop

         ! Write profiles structure to PRTM file (array operations needed to
         ! recast structure in form nc_write_array recognises)
         i_ = idim - preproc_dims%min_lon + 1
         j_ = jdim - preproc_dims%min_lat + 1
         call nc_write_array(netcdf_info%ncid_prtm, 'lon_rtm', &
              netcdf_info%vid_lon_pw, &
              (/profiles(count)%longitude/), &
              1, i_, 1)
         call nc_write_array(netcdf_info%ncid_prtm, 'lat_rtm', &
              netcdf_info%vid_lat_pw, &
              (/profiles(count)%latitude/), &
              1, j_, 1)
         call nc_write_array(netcdf_info%ncid_prtm, 'pprofile_rtm', &
              netcdf_info%vid_pprofile_lev_pw, &
              reshape(profiles(count)%p, (/nlevels,1,1/)), &
              1, 1, nlevels, 1, i_, 1, 1, j_, 1)
         call nc_write_array(netcdf_info%ncid_prtm, 'tprofile_rtm', &
              netcdf_info%vid_tprofile_lev_pw, &
              reshape(profiles(count)%t, (/nlevels,1,1/)), &
              1, 1, nlevels, 1, i_, 1, 1, j_, 1)
         call nc_write_array(netcdf_info%ncid_prtm, 'hprofile_rtm', &
              netcdf_info%vid_hprofile_lev_pw, &
              reshape(preproc_prtm%phi_lev(idim, jdim,:), &
              (/nlevels,1,1/)), 1, 1, nlevels, 1, i_, 1, 1, j_, 1)
         call nc_write_array(netcdf_info%ncid_prtm, 'qprofile_rtm', &
              netcdf_info%vid_qprofile_lev_pw, &
              reshape(profiles(count)%q, (/nlevels,1,1/)), &
              1, 1, nlevels, 1, i_, 1, 1, j_, 1)
         call nc_write_array(netcdf_info%ncid_prtm, 'o3profile_rtm', &
              netcdf_info%vid_o3profile_lev_pw, &
              reshape(profiles(count)%o3, (/nlevels,1,1/)), &
              1, 1, nlevels, 1, i_, 1, 1, j_, 1)
      end do
   end do

   ! Write fields not in profiles structure
!   call nc_write_array(netcdf_info%ncid_prtm, 'lsf_rtm', &
!        netcdf_info%vid_lsf_pw, &
!        preproc_prtm%land_sea_mask, &
!        1, 1, preproc_dims%xdim, 1, 1, preproc_dims%ydim)

   ! Do RTTOV calculations for long and shortwave in turn
   if (verbose) write(*,*) 'Do RTTOV calculations'

   do i_coef=1,2
      ! Set factors that differ between long and shortwave
      if (i_coef == 1) then
         ! Longwave
         nchan = channel_info%nchannels_lw

         if (nchan .eq. 0) cycle

         allocate(input_chan(nchan))
         input_chan = channel_info%channel_ids_rttov_coef_lw

         ! This assumes the recommended structure of the RTTOV coef library
         coef_full_path = trim(adjustl(coef_path))//'/rttov7pred54L/'// &
              trim(adjustl(coef_file))
      else
         ! Shortwave
         nchan = channel_info%nchannels_sw

         if (nchan .eq. 0) cycle

         allocate(input_chan(nchan))
         input_chan = channel_info%channel_ids_rttov_coef_sw

         coef_full_path = trim(adjustl(coef_path))//'/rttov9pred54L/'// &
              trim(adjustl(coef_file))
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
      allocate(emis_atlas(nchan))
      allocate(calcemis(nchan))

      chanprof%prof = 1
      do j=1,nchan
         chanprof(j)%chan = j
      end do

      if (verbose) write(*,*) 'Allocate RTTOV structures'
      call rttov_alloc_rad(stat, nchan, radiance, nlayers, ALLOC, radiance2, &
           init=.true._jplm)
      if (stat /= errorstatus_success) then
         write(*,*) 'ERROR: rttov_alloc_rad(), errorstatus = ', stat
         stop error_stop_code
      end if
      call rttov_alloc_transmission(stat, transmission, nlayers, nchan, ALLOC, &
           init=.true._jplm)
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
      imonth=month
      call rttov_setup_emis_atlas(stat, opts, imonth, coefs, emiss_path, &
           ir_atlas_single_instrument=.true._jplm)
      if (stat /= errorstatus_success) then
         write(*,*) 'ERROR: rttov_setup_emis_atlas(), errorstatus = ', stat
         stop error_stop_code
      end if

      ! Loop over profiles (as the conditions for processing LW and SW are
      ! different, we can't just pass the whole array)
      count = 0
      if (verbose) write(*,*) 'Run RTTOV'
      do jdim=preproc_dims%min_lat,preproc_dims%max_lat
         do idim=preproc_dims%min_lon,preproc_dims%max_lon
            count = count + 1

            ! Process points that contain information and satisfy the zenith
            ! angle restrictions of the coefficient file
            if ((i_coef == 1 .and. preproc_dims%counter_lw(idim,jdim) > 0 .and. &
                 profiles(count)%zenangle < zenmax) .or. &
                (i_coef == 2 .and. preproc_dims%counter_sw(idim,jdim) > 0 .and. &
                 profiles(count)%zenangle < zenmaxv9)) then

               if (i_coef == 1) then
                  ! Fetch emissivity from atlas
                  call rttov_get_emis(stat, opts, chanprof, profiles(count:count), &
                       coefs, emissivity=emis_atlas)
                  if (stat /= errorstatus_success) then
                     write(*,*) 'ERROR: rttov_get_emis(), errorstatus = ', stat
                     stop error_stop_code
                  end if
                  emissivity%emis_in = emis_atlas

                  ! Fetch emissivity from the MODIS CIMSS emissivity product
                  if (i_coef == 1 .and. use_modis_emis) then
                     where (preproc_surf%emissivity(idim,jdim,:) .ne. sreal_fill_value)
                        emissivity%emis_in = preproc_surf%emissivity(idim,jdim,:)
                     end where
                  end if

                  calcemis = emissivity%emis_in <= dither
               end if

               ! Call RTTOV for this profile
               call rttov_direct(stat, chanprof, opts, profiles(count:count), &
                    coefs, transmission, radiance, radiance2, calcemis, &
                    emissivity, traj=traj)
               if (stat /= errorstatus_success) then
                  write(*,*) 'ERROR: rttov_direct(), errorstatus = ', stat
                  stop error_stop_code
               end if

               write_rttov = .true.
            else
               write_rttov = .false.
            end if

            ! Remove the Rayleigh component from the RTTOV tranmittances.
            ! (Private comunication from Philip Watts.)
            if (i_coef == 2) then
               p_0 = 1013.

               sec_vza = 1. / cos(profiles(count)%zenangle * d2r)

               do i_ = 1, nchan
                  ! Rayleigh optical thickness for the atmosphere down to 1013
                  ! hPa (Hansen and Travis, 1974)
                  lambda = dummy_sreal_1dveca(i_)
                  tau_ray_0 = .008569 * lambda**(-4) * &
                     (1. + .0113 * lambda**(-2) + .00013 * lambda**(-4))

                  do j_ = 1, nlevels
                     ! Pressure and path dependent Rayleigh optical thickness
                     tau_ray_p = tau_ray_0 * profiles(count)%p(j_) / p_0 * sec_vza

                     ! Corrected level transmittances
                     transmission%tau_levels(j_, i_) = &
                        transmission%tau_levels(j_, i_) / exp(-tau_ray_p)
                  enddo

                  ! Corrected total transmittances
                  transmission%tau_total(i_) = &
                     transmission%tau_total(i_) / exp(-tau_ray_p)
               enddo
            end if

            ! Reformat and write output to NCDF files
            if (i_coef == 1) then
               call write_ir_rttov(netcdf_info, preproc_dims, &
                    idim-preproc_dims%min_lon+1, jdim-preproc_dims%min_lat+1, &
                    nchan, profiles(count)%nlevels, emissivity, transmission, &
                    radiance, radiance2, write_rttov)
            else
               call write_solar_rttov(netcdf_info, preproc_dims, coefs, &
                    idim-preproc_dims%min_lon+1, jdim-preproc_dims%min_lat+1, &
                    nchan, profiles(count)%nlevels, profiles(count)%zenangle, &
                    emissivity, transmission, radiance, radiance2, write_rttov)
            end if
         end do
      end do


      if (verbose) write(*,*) 'Deallocate structures'

      call rttov_deallocate_emis_atlas(coefs)
      call rttov_alloc_traj(stat, 1, nchan, opts, nlevels, coefs, DEALLOC, traj)
      call rttov_alloc_transmission(stat, transmission, nlayers, nevals, DEALLOC)
      call rttov_alloc_rad(stat, nevals, radiance, nlayers, DEALLOC, radiance2)
      call rttov_dealloc_coefs(stat, coefs)

      deallocate(input_chan)
      deallocate(chanprof)
      deallocate(emissivity)
      deallocate(emis_atlas)
      deallocate(calcemis)
   end do
if (channel_info%nchannels_sw .ne. 0) then
   deallocate(dummy_sreal_1dveca)
end if
   call rttov_alloc_prof(stat, nprof, profiles, nlevels, opts, DEALLOC)
   deallocate(profiles)

   if (verbose) write(*,*) '>>>>>>>>>>>>>>> Leaving rttov_driver()'

end subroutine rttov_driver


include 'write_ir_rttov.F90'
include 'write_solar_rttov.F90'


end module rttov_driver_m
