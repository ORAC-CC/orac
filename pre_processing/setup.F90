!-------------------------------------------------------------------------------
! Name: setup.F90
!
! Purpose:
! Three equivalent routines (one for each satellite) that determine the date and
! time of the observation and set the specific details (e.g. internal channel
! numbers) for that platform.
!
! Description and Algorithm details:
! 1) Check L1B and Geo filenames are consistent with each other
! 2) Determine date/time information from the filenames (number and string)
! 3) Set details in channel_info such as channel numbers
!
! Arguments:
! Name          Type   In/Out/Both Description
! ------------------------------------------------------------------------------
! args          struct both        Parameters of the swath file
! opts          struct in          Processing options
! source_attributes struct both    Descriptions of input files
! channel_info  struct both        Structure summarising the channels to be
!                                  processed
! verbose       logic  in          T: print status information; F: don't
!
! History:
! 2011/12/09, MJ: produces draft code for MODIS.
! 2012/01/17, MJ: removed bug wrt determining the month for MODIS.
! 2012/01/24, MJ: added code for AVHRR.
! 2012/07/17, CP: default code for AATSR
! 2012/07/29, CP: added in nchannels_total
! 2012/08/10, CP: debugged AATSR settings
! 2012/08/22, GT: Further debugging related to AATSR
! 2012/09/12, CP: added in gregorian to doy conversion into aatsr setup GREG2DOY
! 2012/09/13, CP: changed coeff numbering in sw file
! 2012/11/14, CP: remove platform name from aatsr
! 2013/08/14, GT: Added trim to write statement of L1b filename
! 2013/09/16, AP: removed channel_flag, preproc_dims, date
! 2014/09/10, AP: Order of RTTOV channels for AATSR reversed.
! 2014/09/19, AP: Fixed bug in reading platform for Metop AVHRR.
! 2014/10/15, GM: Changes related to supporting an arbitrary set of MODIS
!    channels.  Simplified changing the desired channels by automating the
!    setting of channel arrays/indexes.
! 2014/10/23, OS: Refactoring overly long lines of code, causing CRAY-ftn
!    compiler to exit
! 2014/12/01, CS: updated setup_avhrr to read new ECC_GAC AVHRR data
! 2015/01/15, AP: Eliminate channel_ids_abs.
! 2015/02/19, GM: Added SEVIRI support.
! 2015/02/24, GM: Added the ability to change which channels are processed at
!    run time.
! 2015/03/10, GM: Changes to support AATSR channel 1 and MODIS channels 3, 4, 5,
!    and 7 for sea surface reflectance.
! 2015/04/11, GM: Fixed a bug in the setting of the LW RTTOV channels for
!    SEVIRI.
! 2015/04/29, CP: Changed AATSR platform name to Envisat
! 2015/04/30, GM: Temporarily index channels in all_map_ids_abs_to_snow_and_ice
!    that are not supported in the snow and ice correction to nearby supported
!    channels.
! 2015/08/19, GM: Modifications to support the SEVIRI HRIT format.
! 2015/08/29, CP: Changes to support ATSR-2.
! 2016/04/07, SP: Reformulated to support multiple views.
! 2016/04/08, SP: Added multiple views for ATSR series sensors.
! 2016/04/08, SP: Updated MODIS sea flags for correct cox-munk bands.
! 2016/04/11, SP: Added Himawari-8 support.
! 2016/04/10, SP: Changes in common setup to support multiple views.
! 2016/05/16, SP: Added Suomi-NPP support.
! 2016/05/27, SP: Updates to enable RTTOV to work correctly with multi-views
! 2016/06/14, SP: Added Sentinel-3 SLSTR support.
! 2016/07/01, GT: Added allocation and population of
!    channel_info%map_ids_sw_to_channel and %map_ids_lw_to_channel
! 2016/07/15, AP: Add uncertainty estimates. Only AATSR currently filled in.
! 2016/08/04, GM: Added map_ids_channel_to_sw and map_ids_channel_to_lw.
! 2017/07/05, AP: Add NAll to track the total number of channels.
! 2017/12/12, GT: Changed Sentinel-3 platform name to Sentinel3a
! 2018/02/01, GT: Added source_attributes argument to setup_slstr, and populated
!    l1b_version and level1b_orbit_number attributes from the L1b file.
!    Extending this change to the other supported instruments is worth
!    considering...
! 2019/08/14, SP: Add Fengyun-4A support.
! 2021/03/09, AP: Add radiance bias corrections.
! 2021/03/10, AP: Gather setup calls into a single routine.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

module setup_m

   use preproc_structures_m, only: setup_args_t
   implicit none

contains

subroutine setup_aatsr(args, channel_ids_user, channel_info, verbose)

   use calender_m
   use channel_structures_m
   use preproc_constants_m

   implicit none

   type(setup_args_t),   intent(inout) :: args
   integer, pointer,     intent(in)    :: channel_ids_user(:)
   type(channel_info_t), intent(inout) :: channel_info
   logical,              intent(in)    :: verbose

   integer :: index1


   ! Static instrument channel definitions. (These should not be changed.)
   integer, parameter :: all_nchannels_total = 14

       ! 1,    2,    3,    4,    5,    6,    7
   real,    parameter :: all_channel_wl_abs (all_nchannels_total) = &
      (/ 0.55, 0.67, 0.87, 1.61, 3.74, 10.8, 12.0, &
         0.55, 0.67, 0.87, 1.61, 3.74, 10.8, 12.0 /)

   integer, parameter :: all_channel_sw_flag(all_nchannels_total) = &
      (/ 1,    1,    1,    1,    1,    0,    0,    &
         1,    1,    1,    1,    1,    0,    0 /)

   integer, parameter :: all_channel_lw_flag(all_nchannels_total) = &
      (/ 0,    0,    0,    0,    1,    1,    1,    &
         0,    0,    0,    0,    1,    1,    1 /)

   integer, parameter :: all_channel_ids_rttov_coef_sw(all_nchannels_total) = &
      (/ 7,    6,    5,    4,    3,    0,    0,    &
         7,    6,    5,    4,    3,    0,    0 /)

   integer, parameter :: all_channel_ids_rttov_coef_lw(all_nchannels_total) = &
      (/ 0,    0,    0,    0,    3,    2,    1,    &
         0,    0,    0,    0,    3,    2,    1 /)

   integer, parameter :: all_map_ids_abs_to_ref_band_land(all_nchannels_total) = &
      (/ 4,    1,    2,    6,    0,    0,    0,    &
         4,    1,    2,    6,    0,    0,    0 /)

   integer, parameter :: all_map_ids_abs_to_ref_band_sea(all_nchannels_total) = &
      (/ 2,    3,    4,    7,    9,    0,    0,    &
         2,    3,    4,    7,    9,    0,    0 /)

   integer, parameter :: all_map_ids_abs_to_snow_and_ice(all_nchannels_total) = &
      (/ 1,    1,    2,    3,    4,    0,    0,    &
         1,    1,    2,    3,    4,    0,    0 /)

   integer, parameter :: all_map_ids_view_number(all_nchannels_total) = &
      (/ 1,    1,    1,    1,    1,    1,    1,    &
         2,    2,    2,    2,    2,    2,    2 /)

   ! All values drawn from finetocoarse.pro by Haiyan Huang (following
   ! measurement_errors.pro by Sayer), except ch 5/12 which come from deleted
   ! parts of read_aatsr_orbit.c
   real,    parameter :: all_channel_fractional_uncertainty(all_nchannels_total) = &
      (/ 0.024, 0.032, 0.020, 0.033, 0.080, 0.023, 0.250, &
         0.024, 0.032, 0.020, 0.033, 0.080, 0.025, 0.250 /)

   real,    parameter :: all_channel_minimum_uncertainty(all_nchannels_total) = &
      (/ 0.0005, 0.0003, 0.0003, 0.0003, 0.08, 0.025, 0.25, &
         0.0005, 0.0003, 0.0003, 0.0003, 0.08, 0.025, 0.25 /)

   real,    parameter :: all_channel_numerical_uncertainty(all_nchannels_total) = &
      (/ 0.0081, 0.0067, 0.0066, 0.0068, 0.0, 0.0, 0.0, &
         0.0081, 0.0067, 0.0066, 0.0068, 0.0, 0.0, 0.0 /)

   real,    parameter :: all_channel_lnd_uncertainty(all_nchannels_total) = &
      (/ 0.0161, 0.0225, 0.0297, 0.0371, 0.0, 0.0, 0.0, &
         0.0119, 0.0175, 0.0279, 0.0345, 0.0, 0.0, 0.0 /)

   real,    parameter :: all_channel_sea_uncertainty(all_nchannels_total) = &
      (/ 0.0200, 0.0236, 0.0263, 0.0461, 0.0, 0.0, 0.0, &
         0.0132, 0.0150, 0.0161, 0.0294, 0.0, 0.0, 0.0 /)

   ! Bias correction
   real,    parameter :: all_channel_absolute_bias(all_nchannels_total) = &
      (/ 0.0,    0.0,    0.0,   0.0,    0.0,    0.0,    0.0, &
         0.0,    0.0,    0.0,   0.0,    0.0,    0.0,    0.0 /)
   real,    parameter :: all_channel_relative_bias(all_nchannels_total) = &
      (/ 1.0,    1.0,    1.0,   1.0,    1.0,    1.0,    1.0, &
         1.0,    1.0,    1.0,   1.0,    1.0,    1.0,    1.0 /)

   ! Only this below needs to be set to change the desired default channels. All
   ! other channel related arrays/indexes are set automatically given the static
   ! instrument channel definition above.
   integer, parameter :: channel_ids_default(6) = (/ 2, 3, 4, 5, 6, 7 /)


   if (verbose) write(*,*) '<<<<<<<<<<<<<<< Entering setup_aatsr()'

   if (verbose) write(*,*) 'args%l1b_file: ', trim(args%l1b_file)
   if (verbose) write(*,*) 'args%geo_file: ', trim(args%geo_file)

   ! check if l1b and geo files identical
   if (trim(adjustl(args%l1b_file)) .ne. &
       trim(adjustl(args%geo_file))) then
      write(*,*)
      write(*,*) 'ERROR: setup_aatsr(): Geolocation and L1b files are for ' // &
           'different orbits'
      write(*,*) 'args%l1b_file: ', trim(adjustl(args%geo_file))
      write(*,*) 'args%geo_file: ', trim(adjustl(args%l1b_file))

      stop error_stop_code
   end if

   ! which aatsr are we processing?

   if (trim(adjustl(args%sensor)) .eq. 'AATSR') then
      index1 = index(args%l1b_file, '.N1', back=.true.)
      args%platform = 'Envisat'
   else
      args%platform = 'ERS2'
      index1 = index(args%l1b_file, '.E2', back=.true.)
   end if

   ! Get year, month, day, hour and minute as strings
   args%cyear = trim(adjustl(args%l1b_file(index1-45:index1-42)))
   args%cmonth = trim(adjustl(args%l1b_file(index1-41:index1-40)))
   args%cday = trim(adjustl(args%l1b_file(index1-39:index1-38)))
   args%chour = trim(adjustl(args%l1b_file(index1-36:index1-35)))
   args%cminute = trim(adjustl(args%l1b_file(index1-34:index1-33)))

   ! get year, month, day, hour and minute as integers
   read(args%cyear, '(I4)') args%year
   read(args%cmonth, '(I2)') args%month
   read(args%cday, '(I2)') args%day
   read(args%chour, '(I2)') args%hour
   read(args%cminute, '(I2)') args%minute

   call GREG2DOY(args%year, args%month, args%day, args%doy)
   write(args%cdoy, '(i3.3)') args%doy

   ! The ATSR series has dual view capability
   channel_info%nviews  = 2

   ! now set up the channels
   call common_setup(channel_info, channel_ids_user, channel_ids_default, &
      all_channel_wl_abs, all_channel_sw_flag, all_channel_lw_flag, &
      all_channel_ids_rttov_coef_sw, all_channel_ids_rttov_coef_lw, &
      all_map_ids_abs_to_ref_band_land, all_map_ids_abs_to_ref_band_sea, &
      all_map_ids_abs_to_snow_and_ice, all_map_ids_view_number, &
      all_channel_fractional_uncertainty, all_channel_minimum_uncertainty, &
      all_channel_numerical_uncertainty, all_channel_lnd_uncertainty, &
      all_channel_sea_uncertainty, all_channel_absolute_bias, all_channel_relative_bias, all_nchannels_total)

   if (verbose) write(*,*) '>>>>>>>>>>>>>>> Leaving setup_aatsr()'

end subroutine setup_aatsr

subroutine setup_abi(args, channel_ids_user, channel_info, verbose)

   use calender_m
   use channel_structures_m
   use preproc_constants_m

   implicit none

   type(setup_args_t),   intent(inout) :: args
   integer, pointer,     intent(in)    :: channel_ids_user(:)
   type(channel_info_t), intent(inout) :: channel_info
   logical,              intent(in)    :: verbose

   integer :: index1, index2, index3


   ! Static instrument channel definitions. (These should not be changed.)
   integer, parameter :: all_nchannels_total = 16

       ! 1,       2,       3,       4,       5,       6,       7,       8
   real,    parameter :: all_channel_wl_abs(all_nchannels_total) = &
      (/ 0.470,   0.636,   0.864,   1.373,   1.609,   2.242,   3.890,   6.171, &
         6.927,   7.336,   8.444,   9.607,   10.331,  11.186,  12.266,  13.266 /)

   integer, parameter :: all_channel_sw_flag(all_nchannels_total) = &
      (/ 1,       1,       1,       1,       1,       1,       1,       0, &
         0,       0,       0,       0,       0,       0,       0,       0/)

   integer, parameter :: all_channel_lw_flag(all_nchannels_total) = &
      (/ 0,       0,       0,       0,       0,       0,       1,       1, &
         1,       1,       1,       1,       1,       1,       1       ,1/)

   integer, parameter :: all_channel_ids_rttov_coef_sw(all_nchannels_total) = &
      (/ 1,       2,       3,       4,       5,       6,       7,       0, &
         0,       0,       0,       0,       0,       0,       0,       0 /)

   integer, parameter :: all_channel_ids_rttov_coef_lw(all_nchannels_total) = &
      (/ 0,       0,       0,       0,       0,       0,       1,       2, &
         3,       4,       5,       6,       7,       8,       9,       10 /)

   integer, parameter :: all_map_ids_abs_to_ref_band_land(all_nchannels_total) = &
      (/ 3,       1,       2,       5,       6,       7,       0,       0, &
         0,       0,       0,       0,       0,       0,       0,       0 /)

   integer, parameter :: all_map_ids_abs_to_ref_band_sea(all_nchannels_total) = &
      (/ 1,       3,       4,       6,       7,       8,       9,       0, &
         0,       0,       0,       0,       0,       0,       0,       0 /)

   integer, parameter :: all_map_ids_abs_to_snow_and_ice(all_nchannels_total) = &
      (/ 1,       1,       2,       3,       3,       3,       4,       0, &
         0,       0,       0,       0,       0,       0,       0,       0 /)

   integer, parameter :: all_map_ids_view_number(all_nchannels_total) = &
      (/ 1,       1,       1,       1,       1,       1,       1,       1, &
         1,       1,       1,       1,       1,       1,       1,       1 /)

   real,    parameter :: all_channel_fractional_uncertainty(all_nchannels_total) = &
      (/ 0.,      0.,      0.,      0.,      0.,      0.,      0.,      0., &
         0.,      0.,      0.,      0.,      0.,      0.,      0.,      0. /)

   real,    parameter :: all_channel_minimum_uncertainty(all_nchannels_total) = &
      (/ 0.,      0.,      0.,      0.,      0.,      0.,      0.,      0., &
         0.,      0.,      0.,      0.,      0.,      0.,      0.,      0. /)

   real,    parameter :: all_channel_numerical_uncertainty(all_nchannels_total) = &
      (/ 0.,      0.,      0.,      0.,      0.,      0.,      0.,      0., &
         0.,      0.,      0.,      0.,      0.,      0.,      0.,      0. /)

   real,    parameter :: all_channel_lnd_uncertainty(all_nchannels_total) = &
      (/ 0.,      0.,      0.,      0.,      0.,      0.,      0.,      0., &
         0.,      0.,      0.,      0.,      0.,      0.,      0.,      0. /)

   real,    parameter :: all_channel_sea_uncertainty(all_nchannels_total) = &
      (/ 0.,      0.,      0.,      0.,      0.,      0.,      0.,      0., &
         0.,      0.,      0.,      0.,      0.,      0.,      0.,      0. /)

   ! Bias correction
   real,    parameter :: all_channel_absolute_bias(all_nchannels_total) = &
      (/ 0.,      0.,      0.,      0.,      0.,      0.,      0.,      0., &
         0.,      0.,      0.,      0.,      0.,      0.,      0.,      0. /)
   real,    parameter :: all_channel_relative_bias(all_nchannels_total) = &
      (/ 1.,      1.,      1.,      1.,      1.,      1.,      1.,      1., &
         1.,      1.,      1.,      1.,      1.,      1.,      1.,      1. /)

   ! Only this below needs to be set to change the desired default channels. All
   ! other channel related arrays/indexes are set automatically given the static
   ! instrument channel definition above.
   integer, parameter :: channel_ids_default(6) = (/ 2, 3, 5, 7, 14, 15 /)


   if (verbose) write(*,*) '<<<<<<<<<<<<<<< Entering setup_abi()'

   if (verbose) write(*,*) 'args%l1b_file: ', trim(args%l1b_file)
   if (verbose) write(*,*) 'args%geo_file: ', trim(args%geo_file)

   ! check if l1b and geo file are of the same granule
   index1 = index(args%l1b_file, '/', back=.true.)
   index2 = index(args%geo_file, '/', back=.true.)

   ! check if l1b and geo files identical
   if (trim(adjustl(args%l1b_file)) .ne. &
       trim(adjustl(args%geo_file))) then
      write(*,*)
      write(*,*) 'ERROR: setup_abi(): Geolocation and L1b files are ' // &
           'for different times'
      write(*,*) 'args%l1b_file: ', trim(adjustl(args%l1b_file))
      write(*,*) 'args%geo_file: ', trim(adjustl(args%geo_file))

      stop error_stop_code
   end if

   if (index(args%l1b_file, "G16_s") .gt. 0) then
      index3 = index(args%l1b_file, "G16_s") + 5
      args%platform = "GOES-16"
   else if(index(args%l1b_file, "G17_s") .gt. 0) then
      index3 = index(args%l1b_file, "G17_s") + 5
      args%platform = "GOES-17"
   else
      write(*,*) "Unsupported GOES platform, ", args%l1b_file
      stop
   end if

   if (verbose) write(*,*) "Satellite is: ", args%platform

   index2 = index(args%l1b_file, 'ABI-L1b-')

   ! get year, doy, hour and minute as strings
   args%cyear = trim(adjustl(args%l1b_file(index3:index3+3)))
   args%cdoy = trim(adjustl(args%l1b_file(index3+4:index3+6)))
   args%chour = trim(adjustl(args%l1b_file(index3+7:index3+8)))
   args%cminute = trim(adjustl(args%l1b_file(index3+9:index3+10)))
   read(args%cyear, '(I4)') args%year
   read(args%cdoy, '(I3)') args%doy
   read(args%chour, '(I2)') args%hour
   read(args%cminute, '(I2)') args%minute

   call DOY2GREG(args%doy, args%year, args%month, args%day)

   write(args%cmonth, '(i2.2)') args%month
   write(args%cday, '(i2.2)') args%day

   ! now set up the channels
   call common_setup(channel_info, channel_ids_user, channel_ids_default, &
      all_channel_wl_abs, all_channel_sw_flag, all_channel_lw_flag, &
      all_channel_ids_rttov_coef_sw, all_channel_ids_rttov_coef_lw, &
      all_map_ids_abs_to_ref_band_land, all_map_ids_abs_to_ref_band_sea, &
      all_map_ids_abs_to_snow_and_ice, all_map_ids_view_number, &
      all_channel_fractional_uncertainty, all_channel_minimum_uncertainty, &
      all_channel_numerical_uncertainty, all_channel_lnd_uncertainty, &
      all_channel_sea_uncertainty, all_channel_absolute_bias, all_channel_relative_bias, all_nchannels_total)

   if (verbose) write(*,*) '>>>>>>>>>>>>>>> Leaving setup_abi()'

end subroutine setup_abi

subroutine setup_agri(args, channel_ids_user, channel_info, verbose)

   use calender_m
   use channel_structures_m
   use preproc_constants_m

   implicit none

   type(setup_args_t),   intent(inout) :: args
   integer, pointer,     intent(in)    :: channel_ids_user(:)
   type(channel_info_t), intent(inout) :: channel_info
   logical,              intent(in)    :: verbose

   integer :: index3


   ! Static instrument channel definitions. (These should not be changed.)
   integer, parameter :: all_nchannels_total = 13

   ! 1,       2,       3,       4,       5,       6,       7,       8
   real,    parameter :: all_channel_wl_abs(all_nchannels_total) = &
      (/ 0.469,   0.631,   0.819,   1.375,   1.607,   2.226,   3.715, &
         6.231,  7.118,   8.588,   10.824,  12.071,  13.545 /)

   integer, parameter :: all_channel_sw_flag(all_nchannels_total) = &
      (/ 1,       1,       1,       1,       1,       1,       1,   &
         0,       0,       0,       0,       0,       0     /)

   integer, parameter :: all_channel_lw_flag(all_nchannels_total) = &
      (/ 0,       0,       0,       0,       0,       0,       1,   &
         1,       1,       1,       1,       1,       1     /)

   integer, parameter :: all_channel_ids_rttov_coef_sw(all_nchannels_total) = &
      (/ 1,       2,       3,       4,       5,       6,       7,   &
         0,       0,       0,       0,       0,       0     /)

   integer, parameter :: all_channel_ids_rttov_coef_lw(all_nchannels_total) = &
      (/ 0,       0,       0,       0,       0,       0,       1,   &
         2,       3,       4,       5,       6,       7      /)

   integer, parameter :: all_map_ids_abs_to_ref_band_land(all_nchannels_total) = &
      (/ 3,       1,       2,       5,       6,       7,       0,   &
         0,       0,       0,       0,       0,       0      /)

   integer, parameter :: all_map_ids_abs_to_ref_band_sea(all_nchannels_total) = &
      (/ 1,       3,       4,       6,       7,       8,       9,   &
         0,       0,       0,       0,       0,       0      /)

   integer, parameter :: all_map_ids_abs_to_snow_and_ice(all_nchannels_total) = &
      (/ 1,       1,       2,       3,       3,       3,       4,   &
         0,       0,       0,       0,       0,       0      /)

   integer, parameter :: all_map_ids_view_number(all_nchannels_total) = &
      (/ 1,       1,       1,       1,       1,       1,       1,   &
         1,       1,       1,       1,       1,       1      /)

   real,    parameter :: all_channel_fractional_uncertainty(all_nchannels_total) = &
      (/ 0.,      0.,      0.,      0.,      0.,      0.,      0., &
         0.,      0.,      0.,      0.,      0.,      0.     /)

   real,    parameter :: all_channel_minimum_uncertainty(all_nchannels_total) = &
      (/ 0.,      0.,      0.,      0.,      0.,      0.,      0., &
         0.,      0.,      0.,      0.,      0.,      0.     /)

   real,    parameter :: all_channel_numerical_uncertainty(all_nchannels_total) = &
      (/ 0.,      0.,      0.,      0.,      0.,      0.,      0., &
         0.,      0.,      0.,      0.,      0.,      0.     /)

   real,    parameter :: all_channel_lnd_uncertainty(all_nchannels_total) = &
      (/ 0.,      0.,      0.,      0.,      0.,      0.,      0., &
         0.,      0.,      0.,      0.,      0.,      0.     /)

   real,    parameter :: all_channel_sea_uncertainty(all_nchannels_total) = &
      (/ 0.,      0.,      0.,      0.,      0.,      0.,      0., &
         0.,      0.,      0.,      0.,      0.,      0.     /)

   ! Bias correction
   real,    parameter :: all_channel_absolute_bias(all_nchannels_total) = &
      (/ 0.,      0.,      0.,      0.,      0.,      0.,      0., &
         0.,      0.,      0.,      0.,      0.,      0.     /)
   real,    parameter :: all_channel_relative_bias(all_nchannels_total) = &
      (/ 1.,      1.,      1.,      1.,      1.,      1.,      1., &
         1.,      1.,      1.,      1.,      1.,      1.     /)

   ! Only this below needs to be set to change the desired default channels. All
   ! other channel related arrays/indexes are set automatically given the static
   ! instrument channel definition above.
   integer, parameter :: channel_ids_default(6) = (/ 2, 3, 5, 7, 11, 12 /)


   if (verbose) write(*,*) '<<<<<<<<<<<<<<< Entering setup_agri()'

   if (verbose) write(*,*) 'args%l1b_file: ', trim(args%l1b_file)
   if (verbose) write(*,*) 'args%geo_file: ', trim(args%geo_file)

   if (index(args%l1b_file, "FY4A") .gt. 0) then
      index3 = index(args%l1b_file, 'AGRI_FY4A_') + 10
      args%platform = "FY-4A"
   else if(index(args%l1b_file, "FY4B") .gt. 0) then
      index3 = index(args%l1b_file, 'AGRI_FY4A_') + 10
      args%platform = "FY-4B"
   else
      write(*,*) "Unsupported Fengyun platform, ", args%l1b_file
      stop
   end if

   if (verbose) write(*,*) "Satellite is: ", args%platform

   ! get year, doy, hour and minute as strings
   args%cyear = trim(adjustl(args%l1b_file(index3:index3+3)))
   args%cmonth = trim(adjustl(args%l1b_file(index3+4:index3+5)))
   args%cday = trim(adjustl(args%l1b_file(index3+6:index3+7)))
   args%chour = trim(adjustl(args%l1b_file(index3+8:index3+9)))
   args%cminute = trim(adjustl(args%l1b_file(index3+10:index3+11)))

   read(args%cyear, '(I4)') args%year
   read(args%cmonth, '(I2)') args%month
   read(args%cday, '(I2)') args%day
   read(args%chour, '(I2)') args%hour
   read(args%cminute, '(I2)') args%minute

   call GREG2DOY(args%year, args%month, args%day, args%doy)
   write(args%cdoy, '(i3.3)') args%doy

   ! now set up the channels
   call common_setup(channel_info, channel_ids_user, channel_ids_default, &
      all_channel_wl_abs, all_channel_sw_flag, all_channel_lw_flag, &
      all_channel_ids_rttov_coef_sw, all_channel_ids_rttov_coef_lw, &
      all_map_ids_abs_to_ref_band_land, all_map_ids_abs_to_ref_band_sea, &
      all_map_ids_abs_to_snow_and_ice, all_map_ids_view_number, &
      all_channel_fractional_uncertainty, all_channel_minimum_uncertainty, &
      all_channel_numerical_uncertainty, all_channel_lnd_uncertainty, &
      all_channel_sea_uncertainty, all_channel_absolute_bias, all_channel_relative_bias, all_nchannels_total)

   if (verbose) write(*,*) '>>>>>>>>>>>>>>> Leaving setup_agri()'

end subroutine setup_agri


subroutine setup_ahi(args, channel_ids_user, channel_info, verbose)

   use calender_m
   use channel_structures_m
   use preproc_constants_m

   implicit none

   type(setup_args_t),   intent(inout) :: args
   integer, pointer,     intent(in)    :: channel_ids_user(:)
   type(channel_info_t), intent(inout) :: channel_info
   logical,              intent(in)    :: verbose

   integer :: index1, index2


   ! Static instrument channel definitions. (These should not be changed.)
   integer, parameter :: all_nchannels_total = 16

       ! 1,       2,       3,       4,       5,       6,       7,       8
   real,    parameter :: all_channel_wl_abs(all_nchannels_total) = &
      (/ 0.47063, 0.51000, 0.63914, 0.85670, 1.6101,  2.2568,  3.8853,  6.2429, &
         6.9410,  7.3467,  8.5926,  9.6372,  10.4073, 11.2395, 12.3806, 13.2807 /)

   integer, parameter :: all_channel_sw_flag(all_nchannels_total) = &
      (/ 1,       1,       1,       1,       1,       1,       1,       0, &
         0,       0,       0,       0,       0,       0,       0,       0/)

   integer, parameter :: all_channel_lw_flag(all_nchannels_total) = &
      (/ 0,       0,       0,       0,       0,       0,       1,       1, &
         1,       1,       1,       1,       1,       1,       1       ,1/)

   integer, parameter :: all_channel_ids_rttov_coef_sw(all_nchannels_total) = &
      (/ 1,       2,       3,       4,       5,       6,       7,       0, &
         0,       0,       0,       0,       0,       0,       0,       0 /)

   integer, parameter :: all_channel_ids_rttov_coef_lw(all_nchannels_total) = &
      (/ 0,       0,       0,       0,       0,       0,       1,       2, &
         3,       4,       5,       6,       7,       8,       9,       10 /)

   integer, parameter :: all_map_ids_abs_to_ref_band_land(all_nchannels_total) = &
      (/ 3,       4,       1,       2,       6,       7,       0,       0, &
         0,       0,       0,       0,       0,       0,       0,       0 /)

   integer, parameter :: all_map_ids_abs_to_ref_band_sea(all_nchannels_total) = &
      (/ 2,       2,       3,       4,       7,       8,       9,       0, &
         0,       0,       0,       0,       0,       0,       0,       0 /)

   integer, parameter :: all_map_ids_abs_to_snow_and_ice(all_nchannels_total) = &
      (/ 1,       1,       1,       2,       3,       3,       4,       0, &
         0,       0,       0,       0,       0,       0,       0,       0 /)
   integer, parameter :: all_map_ids_view_number(all_nchannels_total) = &
      (/ 1,       1,       1,       1,       1,       1,       1,       1, &
         1,       1,       1,       1,       1,       1,       1,       1 /)

   real,    parameter :: all_channel_fractional_uncertainty(all_nchannels_total) = &
      (/ 0.,      0.,      0.,      0.,      0.,      0.,      0.,      0., &
         0.,      0.,      0.,      0.,      0.,      0.,      0.,      0. /)

   real,    parameter :: all_channel_minimum_uncertainty(all_nchannels_total) = &
      (/ 0.,      0.,      0.,      0.,      0.,      0.,      0.,      0., &
         0.,      0.,      0.,      0.,      0.,      0.,      0.,      0. /)

   real,    parameter :: all_channel_numerical_uncertainty(all_nchannels_total) = &
      (/ 0.,      0.,      0.,      0.,      0.,      0.,      0.,      0., &
         0.,      0.,      0.,      0.,      0.,      0.,      0.,      0. /)

   real,    parameter :: all_channel_lnd_uncertainty(all_nchannels_total) = &
      (/ 0.,      0.,      0.,      0.,      0.,      0.,      0.,      0., &
         0.,      0.,      0.,      0.,      0.,      0.,      0.,      0. /)

   real,    parameter :: all_channel_sea_uncertainty(all_nchannels_total) = &
      (/ 0.,      0.,      0.,      0.,      0.,      0.,      0.,      0., &
         0.,      0.,      0.,      0.,      0.,      0.,      0.,      0. /)

   ! Bias correction
   real,    parameter :: all_channel_absolute_bias(all_nchannels_total) = &
      (/ 0.,      0.,      0.,      0.,      0.,      0.,      0.,      0., &
         0.,      0.,      0.,      0.,      0.,      0.,      0.,      0. /)
   real,    parameter :: all_channel_relative_bias(all_nchannels_total) = &
      (/ 1.,      1.,      1.,      1.,      1.,      1.,      1.,      1., &
         1.,      1.,      1.,      1.,      1.,      1.,      1.,      1. /)

   ! Only this below needs to be set to change the desired default channels. All
   ! other channel related arrays/indexes are set automatically given the static
   ! instrument channel definition above.
   integer, parameter :: channel_ids_default(6) = (/ 3, 4, 5, 7, 14, 15 /)


   if (verbose) write(*,*) '<<<<<<<<<<<<<<< Entering setup_himawari()'

   if (verbose) write(*,*) 'args%l1b_file: ', trim(args%l1b_file)
   if (verbose) write(*,*) 'args%geo_file: ', trim(args%geo_file)

   ! check if l1b and geo file are of the same granule
   index1 = index(args%l1b_file, '/', back=.true.)
   index2 = index(args%geo_file, '/', back=.true.)

   ! check if l1b and geo files identical
   if (trim(adjustl(args%l1b_file)) .ne. &
       trim(adjustl(args%geo_file))) then
      write(*,*)
      write(*,*) 'ERROR: setup_ahi(): Geolocation and L1b files are ' // &
           'for different times'
      write(*,*) 'args%l1b_file: ', trim(adjustl(args%l1b_file))
      write(*,*) 'args%geo_file: ', trim(adjustl(args%geo_file))

      stop error_stop_code
   end if
   if (index(args%l1b_file, "HS_H08") .gt. 0) then
      args%platform = "Himawari-8"
   else if (index(args%l1b_file, "HS_H09") .gt. 0) then
      args%platform = "Himawari-9"
   else
      write(*,*) "Unidentified Himawari variant: ", trim(args%l1b_file)
      stop
   end if
   if (verbose) write(*,*) "Satellite is: ", args%platform

   ! The code below extracts date/time info from the segment name.
   ! Note that it requires the segment name to be in the generic format
   ! that's specified by the JMA. Weird filenames will break things.

   index2 = index(args%l1b_file, 'HS_H')

   ! get year, doy, hour and minute as strings
   index2 = index2+7
   args%cyear = trim(adjustl(args%l1b_file(index2:index2+3)))
   args%cmonth = trim(adjustl(args%l1b_file(index2+4:index2+5)))
   args%cday = trim(adjustl(args%l1b_file(index2+6:index2+7)))
   args%chour = trim(adjustl(args%l1b_file(index2+9:index2+10)))
   args%cminute = trim(adjustl(args%l1b_file(index2+11:index2+12)))

   ! get year, doy, hour and minute as integers
   read(args%cyear, '(I4)') args%year
   read(args%cmonth, '(I2)') args%month
   read(args%cday, '(I2)') args%day
   read(args%chour, '(I2)') args%hour
   read(args%cminute, '(I2)') args%minute

   call GREG2DOY(args%year, args%month, args%day, args%doy)
   write(args%cdoy, '(i3.3)') args%doy

   ! now set up the channels
   call common_setup(channel_info, channel_ids_user, channel_ids_default, &
      all_channel_wl_abs, all_channel_sw_flag, all_channel_lw_flag, &
      all_channel_ids_rttov_coef_sw, all_channel_ids_rttov_coef_lw, &
      all_map_ids_abs_to_ref_band_land, all_map_ids_abs_to_ref_band_sea, &
      all_map_ids_abs_to_snow_and_ice, all_map_ids_view_number, &
      all_channel_fractional_uncertainty, all_channel_minimum_uncertainty, &
      all_channel_numerical_uncertainty, all_channel_lnd_uncertainty, &
      all_channel_sea_uncertainty, all_channel_absolute_bias, all_channel_relative_bias, all_nchannels_total)

   if (verbose) write(*,*) '>>>>>>>>>>>>>>> Leaving setup_ahi()'

end subroutine setup_ahi


subroutine setup_avhrr(args, channel_ids_user, channel_info, verbose)

   use calender_m
   use channel_structures_m
   use preproc_constants_m

   implicit none

   type(setup_args_t),   intent(inout) :: args
   integer, pointer,     intent(in)    :: channel_ids_user(:)
   type(channel_info_t), intent(inout) :: channel_info
   logical,              intent(in)    :: verbose

   integer                    :: index1, index2
   integer                    :: i, j, k, l
   character(len=path_length) :: str1, str2


   ! Static instrument channel definitions. (These should not be changed.)
   integer, parameter :: all_nchannels_total = 6

       ! 1,    2,      3,    4,    5,    6
   real,    parameter :: all_channel_wl_abs (all_nchannels_total) = &
      (/ 0.63, 0.8625, 1.61, 3.74, 10.8, 12.0 /)

   integer, parameter :: all_channel_sw_flag(all_nchannels_total) = &
      (/ 1,    1,      1,    1,    0,    0 /)

   integer, parameter :: all_channel_lw_flag(all_nchannels_total) = &
      (/ 0,    0,      0,    1,    1,    1 /)

   integer, parameter :: all_channel_ids_rttov_coef_sw(all_nchannels_total) = &
      (/ 1,    2,      3,    4,    0,    0 /)

   integer, parameter :: all_channel_ids_rttov_coef_lw(all_nchannels_total) = &
      (/ 0,    0,      0,    1,    2,    3 /)

   integer, parameter :: all_map_ids_abs_to_ref_band_land(all_nchannels_total) = &
      (/ 1,    2,      6,    0,    0,    0 /)

   integer, parameter :: all_map_ids_abs_to_ref_band_sea(all_nchannels_total) = &
      (/ 3,    4,      7,    9,    0,    0 /)

   integer, parameter :: all_map_ids_abs_to_snow_and_ice(all_nchannels_total) = &
      (/ 1,    2,      3,    4,    0,    0 /)

   integer, parameter :: all_map_ids_view_number(all_nchannels_total) = &
      (/ 1,    1,      1,    1,    1,    1 /)

   real,    parameter :: all_channel_fractional_uncertainty(all_nchannels_total) = &
      (/ 0.,   0.,     0.,   0.,   0.,   0. /)

   real,    parameter :: all_channel_minimum_uncertainty(all_nchannels_total) = &
      (/ 0.,   0.,     0.,   0.,   0.,   0. /)

   real,    parameter :: all_channel_numerical_uncertainty(all_nchannels_total) = &
      (/ 0.,   0.,     0.,   0.,   0.,   0. /)

   real,    parameter :: all_channel_lnd_uncertainty(all_nchannels_total) = &
      (/ 0.,   0.,     0.,   0.,   0.,   0. /)

   real,    parameter :: all_channel_sea_uncertainty(all_nchannels_total) = &
      (/ 0.,   0.,     0.,   0.,   0.,   0. /)

   ! Bias correction
   real,    parameter :: all_channel_absolute_bias(all_nchannels_total) = &
      (/ 0.,   0.,     0.,   0.,   0.,   0. /)
   real,    parameter :: all_channel_relative_bias(all_nchannels_total) = &
      (/ 1.,   1.,     1.,   1.,   1.,   1. /)

   ! Only this below needs to be set to change the desired default channels. All
   ! other channel related arrays/indexes are set automatically given the static
   ! instrument channel definition above.
   integer, parameter :: channel_ids_default(6) = (/ 1, 2, 3, 4, 5, 6 /)


   if (verbose) write(*,*) '<<<<<<<<<<<<<<< Entering setup_avhrr()'

   if (verbose) write(*,*) 'args%l1b_file: ', trim(args%l1b_file)
   if (verbose) write(*,*) 'args%geo_file: ', trim(args%geo_file)

   ! check if old/new avhrr filename
   i = index(args%l1b_file, '/', back=.true.)

   if (trim(adjustl(args%l1b_file(i+1:i+8))) .eq.'ECC_GAC_') then

      if (verbose) write(*,*) ' *** new avhrr input file'

      ! check if l1b and geo file are for the same orbit
      index1 = index(args%l1b_file, 'ECC_GAC_avhrr', back=.true.)
      index2 = index(args%geo_file, 'ECC_GAC_sunsatangles', back=.true.)

      if (trim(adjustl(args%l1b_file(1:index1-1))) .ne. &
           trim(adjustl(args%geo_file(1:index2-1)))) then
         write(*,*)
         write(*,*) 'ERROR: setup_avhrr(): Geolocation and L1b files are ' // &
              'for different orbits'
         write(*,*) 'args%l1b_file: ', trim(adjustl(args%l1b_file))
         write(*,*) 'args%geo_file: ', trim(adjustl(args%geo_file))

         stop error_stop_code
      end if

      str1 = args%l1b_file
      do k = 1, 4
         l = len_trim(str1)
         j = index(str1, '_', back=.true.)

         str2 = str1
         str1 = str1(1:j-1)
         str2 = str2(j+1:)

         if (k .eq. 2) then
            ! get year, month, day, hour and minute as strings
            args%cyear = trim(adjustl(str2(1:4)))
            args%cmonth = trim(adjustl(str2(5:6)))
            args%cday = trim(adjustl(str2(7:8)))
            args%chour = trim(adjustl(str2(10:11)))
            args%cminute = trim(adjustl(str2(12:13)))
         end if

         if (k .eq. 4) then
            ! which avhrr are we processing?
            args%platform = trim(adjustl(str2))
         end if
      end do

      ! get year, month, day, hour and minute as integers
      read(args%cyear, '(I4)') args%year
      read(args%cmonth, '(I2)') args%month
      read(args%cday, '(I2)') args%day
      read(args%chour, '(I2)') args%hour
      read(args%cminute, '(I2)') args%minute

   else

      if (verbose) write(*,*) ' *** old avhrr input file'

      ! check if l1b and angles file are for the same orbit
      index1 = index(args%l1b_file, '_avhrr', back=.true.)
      index2 = index(args%geo_file, '_sunsatangles', back=.true.)

      if (trim(adjustl(args%l1b_file(1:index1-1))) .ne. &
           trim(adjustl(args%geo_file(1:index2-1)))) then
         write(*,*)
         write(*,*) 'ERROR: setup_avhrr(): Geolocation and L1b files are ' // &
              'for different orbits'
         write(*,*) 'args%l1b_file: ', trim(adjustl(args%l1b_file))
         write(*,*) 'args%geo_file: ', trim(adjustl(args%geo_file))

         stop error_stop_code
      end if

      str1 = args%l1b_file
      do k = 1, 7
         l = len_trim(str1)
         j = index(str1, '_', back=.true.)

         str2 = str1
         str1 = str1(1:j-1)
         str2 = str2(j+1:)

         if (k .eq. 6) then
            ! get hour and minute as strings
            args%chour = trim(adjustl(str2(1:2)))
            args%cminute = trim(adjustl(str2(3:4)))
         end if
         if (k .eq. 7) then
            ! get year, month, day as strings
            args%cyear = trim(adjustl(str2(1:4)))
            args%cmonth = trim(adjustl(str2(5:6)))
            args%cday = trim(adjustl(str2(7:8)))
         end if
      end do

      ! one last time for platform
      l = len_trim(str1)
      j = index(str1, '/', back=.true.)
      str2 = str1
      str1 = str1(1:j-1)
      str2 = str2(j+1:)
      args%platform = trim(adjustl(str2))

      ! get year, month, day, hour and minute as integers
      read(args%cyear, '(I4)') args%year
      read(args%cmonth, '(I2)') args%month
      read(args%cday, '(I2)') args%day
      read(args%chour, '(I2)') args%hour
      read(args%cminute, '(I2)') args%minute
   end if

   ! added doy calculation for avhrr
   call GREG2DOY(args%year, args%month, args%day, args%doy)
   write(args%cdoy, '(i3.3)') args%doy

   ! AVHRR only has a single viewing geometry
   channel_info%nviews = 1

   ! now set up the channels
   call common_setup(channel_info, channel_ids_user, channel_ids_default, &
      all_channel_wl_abs, all_channel_sw_flag, all_channel_lw_flag, &
      all_channel_ids_rttov_coef_sw, all_channel_ids_rttov_coef_lw, &
      all_map_ids_abs_to_ref_band_land, all_map_ids_abs_to_ref_band_sea, &
      all_map_ids_abs_to_snow_and_ice, all_map_ids_view_number, &
      all_channel_fractional_uncertainty, all_channel_minimum_uncertainty, &
      all_channel_numerical_uncertainty, all_channel_lnd_uncertainty, &
      all_channel_sea_uncertainty, all_channel_absolute_bias, all_channel_relative_bias, all_nchannels_total)

   if (verbose) write(*,*) '>>>>>>>>>>>>>>> Leaving setup_avhrr()'

end subroutine setup_avhrr


subroutine setup_modis(args, channel_ids_user, channel_info, verbose)

   use calender_m
   use channel_structures_m
   use preproc_constants_m

   implicit none

   type(setup_args_t),   intent(inout) :: args
   integer, pointer,     intent(in)    :: channel_ids_user(:)
   type(channel_info_t), intent(inout) :: channel_info
   logical,              intent(in)    :: verbose

   integer :: index1, index2


   ! Static instrument channel definitions. (These should not be changed.)
   integer, parameter :: all_nchannels_total = 36

       ! 1,         2,         3,         4,         5,         6,
       ! 7,         8,         9,         10,        11,        12,
       ! 13,        14,        15,        16,        17,        18,
       ! 19,        20,        21,        22,        23,        24,
       ! 25,        26,        27,        28,        29,        30,
       ! 31,        32,        33,        34,        35,        36
   real,    parameter :: all_channel_wl_abs (all_nchannels_total) = &
      (/ 0.67,      0.87,      4.690e-01, 5.550e-01, 1.240e+00, 1.60, &
         2.130e+00, 4.125e-01, 4.430e-01, 4.880e-01, 5.310e-01, 5.510e-01, &
         6.670e-01, 6.780e-01, 7.480e-01, 8.695e-01, 9.050e-01, 9.360e-01, &
         9.400e-01, 3.70,      3.959e+00, 3.959e+00, 4.050e+00, 4.466e+00, &
         4.516e+00, 1.375e+00, 6.715e+00, 7.325e+00, 8.550e+00, 9.730e+00, &
         11.017,    12.032,    1.334e+01, 1.363e+01, 1.394e+01, 1.423e+01 /)
!  real,    parameter :: all_channel_wl_abs (all_nchannels_total) = &
!     (/ 6.450e-01, 8.585e-01, 4.690e-01, 5.550e-01, 1.240e+00, 1.640e+00, &
!        2.130e+00, 4.125e-01, 4.430e-01, 4.880e-01, 5.310e-01, 5.510e-01, &
!        6.670e-01, 6.780e-01, 7.480e-01, 8.695e-01, 9.050e-01, 9.360e-01, &
!        9.400e-01, 3.750e+00, 3.959e+00, 3.959e+00, 4.050e+00, 4.466e+00, &
!        4.516e+00, 1.375e+00, 6.715e+00, 7.325e+00, 8.550e+00, 9.730e+00, &
!        1.103e+01, 1.202e+01, 1.334e+01, 1.363e+01, 1.394e+01, 1.423e+01 /)

   integer, parameter :: all_channel_sw_flag(all_nchannels_total) = &
      (/ 1,         1,         1,         1,         1,         1, &
         1,         1,         1,         1,         1,         1, &
         1,         1,         1,         1,         1,         1, &
         1,         1,         1,         1,         1,         0, &
         0,         1,         0,         0,         0,         0, &
         0,         0,         0,         0,         0,         0  /)

   integer, parameter :: all_channel_lw_flag(all_nchannels_total) = &
      (/ 0,         0,         0,         0,         0,         0, &
         0,         0,         0,         0,         0,         0, &
         0,         0,         0,         0,         0,         0, &
         0,         1,         1,         1,         1,         1, &
         1,         0,         1,         1,         1,         1, &
         1,         1,         1,         1,         1,         1  /)

   integer, parameter :: all_channel_ids_rttov_coef_sw(all_nchannels_total) = &
      (/ 1,         2,         3,         4,         5,         6, &
         7,         8,         9,         10,        11,        12, &
         13,        14,        15,        16,        17,        18, &
         19,        20,        21,        22,        23,        0, &
         0,         26,        0,         0,         0,         0, &
         0,         0,         0,         0,         0,         0  /)

   integer, parameter :: all_channel_ids_rttov_coef_lw(all_nchannels_total) = &
      (/ 0,         0,         0,         0,         0,         0, &
         0,         0,         0,         0,         0,         0, &
         0,         0,         0,         0,         0,         0, &
         0,         1,         2,         3,         4,         5, &
         6,         0,         7,         8,         9,         10, &
         11,        12,        13,        14,        15,        16 /)

   integer, parameter :: all_map_ids_abs_to_ref_band_land(all_nchannels_total) = &
      (/ 1,         2,         3,         4,         5,         6, &
         7,         0,         0,         0,         0,         0, &
         0,         0,         0,         0,         0,         0, &
         0,         0,         0,         0,         0,         0, &
         0,        -1,         0,         0,         0,         0, &
         0,         0,         0,         0,         0,         0 /)

   integer, parameter :: all_map_ids_abs_to_ref_band_sea(all_nchannels_total) = &
      (/ 3,         4,         1,         2,         5,         7, &
         8,         1,         1,         1,         2,         2, &
         3,         3,         3,         4,         4,         4, &
         4,         9,         9,         9,         9,         0, &
         0,         6,         0,         0,         0,         0, &
         0,         0,         0,         0,         0,         0 /)

   integer, parameter :: all_map_ids_abs_to_snow_and_ice(all_nchannels_total) = &
      (/ 1,         2,         1,         1,         2,         3, &
         3,         0,         0,         0,         0,         0, &
         0,         0,         0,         0,         0,         0, &
         0,         4,         0,         0,         0,         0, &
         0,         0,         0,         0,         0,         0, &
         0,         0,         0,         0,         0,         0 /)

   integer, parameter :: all_map_ids_view_number(all_nchannels_total) = &
      (/ 1,         1,         1,         1,         1,         1, &
         1,         1,         1,         1,         1,         1, &
         1,         1,         1,         1,         1,         1, &
         1,         1,         1,         1,         1,         1, &
         1,         1,         1,         1,         1,         1, &
         1,         1,         1,         1,         1,         1 /)

   real,    parameter :: all_channel_fractional_uncertainty(all_nchannels_total) = &
      (/ 0.,        0.,        0.,        0.,        0.,        0., &
         0.,        0.,        0.,        0.,        0.,        0., &
         0.,        0.,        0.,        0.,        0.,        0., &
         0.,        0.,        0.,        0.,        0.,        0., &
         0.,        0.,        0.,        0.,        0.,        0., &
         0.,        0.,        0.,        0.,        0.,        0. /)

   real,    parameter :: all_channel_minimum_uncertainty(all_nchannels_total) = &
      (/ 0.,        0.,        0.,        0.,        0.,        0., &
         0.,        0.,        0.,        0.,        0.,        0., &
         0.,        0.,        0.,        0.,        0.,        0., &
         0.,        0.,        0.,        0.,        0.,        0., &
         0.,        0.,        0.,        0.,        0.,        0., &
         0.,        0.,        0.,        0.,        0.,        0. /)

   real,    parameter :: all_channel_numerical_uncertainty(all_nchannels_total) = &
      (/ 0.,        0.,        0.,        0.,        0.,        0., &
         0.,        0.,        0.,        0.,        0.,        0., &
         0.,        0.,        0.,        0.,        0.,        0., &
         0.,        0.,        0.,        0.,        0.,        0., &
         0.,        0.,        0.,        0.,        0.,        0., &
         0.,        0.,        0.,        0.,        0.,        0. /)

   real,    parameter :: all_channel_lnd_uncertainty(all_nchannels_total) = &
      (/ 0.,        0.,        0.,        0.,        0.,        0., &
         0.,        0.,        0.,        0.,        0.,        0., &
         0.,        0.,        0.,        0.,        0.,        0., &
         0.,        0.,        0.,        0.,        0.,        0., &
         0.,        0.,        0.,        0.,        0.,        0., &
         0.,        0.,        0.,        0.,        0.,        0. /)

   real,    parameter :: all_channel_sea_uncertainty(all_nchannels_total) = &
      (/ 0.,        0.,        0.,        0.,        0.,        0., &
         0.,        0.,        0.,        0.,        0.,        0., &
         0.,        0.,        0.,        0.,        0.,        0., &
         0.,        0.,        0.,        0.,        0.,        0., &
         0.,        0.,        0.,        0.,        0.,        0., &
         0.,        0.,        0.,        0.,        0.,        0. /)

   ! Bias correction
   real,    parameter :: all_channel_absolute_bias(all_nchannels_total) = &
      (/ 0.,        0.,        0.,        0.,        0.,        0., &
         0.,        0.,        0.,        0.,        0.,        0., &
         0.,        0.,        0.,        0.,        0.,        0., &
         0.,        0.,        0.,        0.,        0.,        0., &
         0.,        0.,        0.,        0.,        0.,        0., &
         0.,        0.,        0.,        0.,        0.,        0. /)
   real,    parameter :: all_channel_relative_bias(all_nchannels_total) = &
      (/ 1.,        1.,        1.,        1.,        1.,        1., &
         1.,        1.,        1.,        1.,        1.,        1., &
         1.,        1.,        1.,        1.,        1.,        1., &
         1.,        1.,        1.,        1.,        1.,        1., &
         1.,        1.,        1.,        1.,        1.,        1., &
         1.,        1.,        1.,        1.,        1.,        1. /)

   ! Only this below needs to be set to change the desired default channels. All
   ! other channel related arrays/indexes are set automatically given the static
   ! instrument channel definition above.
   integer, parameter :: channel_ids_default(6) = (/ 1, 2, 6, 20, 31, 32 /)


   if (verbose) write(*,*) '<<<<<<<<<<<<<<< Entering setup_modis()'

   if (verbose) write(*,*) 'args%l1b_file: ', trim(args%l1b_file)
   if (verbose) write(*,*) 'args%geo_file: ', trim(args%geo_file)

   ! check if l1b and geo file are of the same granule
   index1 = index(args%l1b_file, '/', back=.true.)
   index2 = index(args%geo_file, '/', back=.true.)

   if (trim(adjustl(args%l1b_file(index1+10:index1+26))) .ne. &
       trim(adjustl(args%geo_file(index2+7:index2+23)))) then
      write(*,*)
      write(*,*) 'ERROR: setup_modis(): Geolocation and L1b files are for ' // &
           'different granules'
      write(*,*) 'args%l1b_file: ', trim(adjustl(args%l1b_file))
      write(*,*) 'args%geo_file: ', trim(adjustl(args%geo_file))

      stop error_stop_code
   end if

   ! which modis are we processing?
   index1 = index(args%l1b_file, '1KM.')
   if (trim(adjustl(args%l1b_file(index1-5:index1-3))) .eq. 'MYD') &
        args%platform = 'AQUA'
   if (trim(adjustl(args%l1b_file(index1-5:index1-3))) .eq. 'MOD') &
        args%platform = 'TERRA'

   ! get year, doy, hour and minute as strings
   args%cyear = trim(adjustl(args%l1b_file(index1+5:index1+8)))
   args%cdoy = trim(adjustl(args%l1b_file(index1+9:index1+11)))
   args%chour = trim(adjustl(args%l1b_file(index1+13:index1+14)))
   args%cminute = trim(adjustl(args%l1b_file(index1+15:index1+16)))

   ! get year, doy, hour and minute as integers
   read(args%cyear, '(I4)') args%year
   read(args%cdoy, '(I3)') args%doy
   read(args%chour, '(I2)') args%hour
   read(args%cminute, '(I2)') args%minute

   ! transform doy to date in year
   call DOY2GREG(args%doy, args%year, args%month, args%day)

   ! get month and day as text
   write(args%cmonth, '(i2.2)') args%month
   write(args%cday, '(i2.2)') args%day

   ! MODIS only has a single viewing geometry
   channel_info%nviews = 1

   ! now set up the channels
   call common_setup(channel_info, channel_ids_user, channel_ids_default, &
      all_channel_wl_abs, all_channel_sw_flag, all_channel_lw_flag, &
      all_channel_ids_rttov_coef_sw, all_channel_ids_rttov_coef_lw, &
      all_map_ids_abs_to_ref_band_land, all_map_ids_abs_to_ref_band_sea, &
      all_map_ids_abs_to_snow_and_ice, all_map_ids_view_number, &
      all_channel_fractional_uncertainty, all_channel_minimum_uncertainty, &
      all_channel_numerical_uncertainty, all_channel_lnd_uncertainty, &
      all_channel_sea_uncertainty, all_channel_absolute_bias, all_channel_relative_bias, all_nchannels_total)

   if (verbose) write(*,*) '>>>>>>>>>>>>>>> Leaving setup_modis()'

end subroutine setup_modis


subroutine setup_seviri(args, channel_ids_user, channel_info, verbose)

   use calender_m
   use channel_structures_m
   use preproc_constants_m

   implicit none

   type(setup_args_t),   intent(inout) :: args
   integer, pointer,     intent(in)    :: channel_ids_user(:)
   type(channel_info_t), intent(inout) :: channel_info
   logical,              intent(in)    :: verbose

   integer :: index1, index2


   ! Static instrument channel definitions. (These should not be changed.)
   integer, parameter :: all_nchannels_total = 11

       ! 1,     2,    3,    4,    5,    6,    7,    8,    9,     10,    11
   real,    parameter :: all_channel_wl_abs(all_nchannels_total) = &
      (/ 0.635, 0.81, 1.64, 3.92, 6.25, 7.35, 8.70, 9.66, 10.80, 12.00, 13.40 /)

   integer, parameter :: all_channel_sw_flag(all_nchannels_total) = &
      (/ 1,     1,    1,    1,    0,    0,    0,    0,    0,     0,     0 /)

   integer, parameter :: all_channel_lw_flag(all_nchannels_total) = &
      (/ 0,     0,    0,    1,    1,    1,    1,    1,    1,     1,     1 /)

   integer, parameter :: all_channel_ids_rttov_coef_sw(all_nchannels_total) = &
      (/ 1,     2,    3,    4,    0,    0,    0,    0,    0,     0,     0 /)

   integer, parameter :: all_channel_ids_rttov_coef_lw(all_nchannels_total) = &
      (/ 0,     0,    0,    1,    2,    3,    4,    5,    6,     7,     8 /)

   integer, parameter :: all_map_ids_abs_to_ref_band_land(all_nchannels_total) = &
      (/ 1,     2,    6,    0,    0,    0,    0,    0,    0,     0,     0 /)

   integer, parameter :: all_map_ids_abs_to_ref_band_sea(all_nchannels_total) = &
      (/ 3,     4,    7,    9,    0,    0,    0,    0,    0,     0,     0 /)

   integer, parameter :: all_map_ids_abs_to_snow_and_ice(all_nchannels_total) = &
      (/ 1,     2,    3,    4,    0,    0,    0,    0,    0,     0,     0 /)

   integer, parameter :: all_map_ids_view_number(all_nchannels_total) = &
      (/ 1,     1,    1,    1,    1,    1,    1,    1,    1,     1,     1 /)

   real,    parameter :: all_channel_fractional_uncertainty(all_nchannels_total) = &
      (/ 0.,    0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,    0.,    0. /)

   real,    parameter :: all_channel_minimum_uncertainty(all_nchannels_total) = &
      (/ 0.,    0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,    0.,    0. /)

   real,    parameter :: all_channel_numerical_uncertainty(all_nchannels_total) = &
      (/ 0.,    0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,    0.,    0. /)

   real,    parameter :: all_channel_lnd_uncertainty(all_nchannels_total) = &
      (/ 0.,    0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,    0.,    0. /)

   real,    parameter :: all_channel_sea_uncertainty(all_nchannels_total) = &
      (/ 0.,    0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,    0.,    0. /)

   ! Bias correction
   real,    parameter :: all_channel_absolute_bias(all_nchannels_total) = &
      (/ 0.,    0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,    0.,    0. /)
   real,    parameter :: all_channel_relative_bias(all_nchannels_total) = &
      (/ 1.,    1.,   1.,   1.,   1.,   1.,   1.,   1.,   1.,    1.,    1. /)

   ! Only this below needs to be set to change the desired default channels. All
   ! other channel related arrays/indexes are set automatically given the static
   ! instrument channel definition above.
   integer, parameter :: channel_ids_default(6) = (/ 1, 2, 3, 4, 9, 10 /)


   if (verbose) write(*,*) '<<<<<<<<<<<<<<< Entering setup_seviri()'

   if (verbose) write(*,*) 'args%l1b_file: ', trim(args%l1b_file)
   if (verbose) write(*,*) 'args%geo_file: ', trim(args%geo_file)

   ! check if l1b and geo file are of the same granule
   index1 = index(args%l1b_file, '/', back=.true.)
   index2 = index(args%geo_file, '/', back=.true.)

   ! check if l1b and geo files identical
   if (trim(adjustl(args%l1b_file)) .ne. &
       trim(adjustl(args%geo_file))) then
      write(*,*)
      write(*,*) 'ERROR: setup_seviri(): Geolocation and L1b files are for ' // &
           'different times'
      write(*,*) 'args%l1b_file: ', trim(adjustl(args%l1b_file))
      write(*,*) 'args%geo_file: ', trim(adjustl(args%geo_file))

      stop error_stop_code
   end if

   index1 = index(args%l1b_file, '.h5')

   if (index1 .ne. 0) then
      index2 = index(args%l1b_file, "/", back=.true.)
      index2 = index2 + index(args%l1b_file(index2 + 1:), '_')
      call determine_seviri_platform_from_metoffice(args%l1b_file, args%platform)
   else
      ! Check if file is HRIT or NAT.
      index1 = index(args%l1b_file, '.nat')

      ! which MSG are we processing?
      !
      ! MSG2-SEVI-MSG15-0100-NA-20100507144241.667000000Z-995378.nat
      ! H-000-MSG1__-MSG1________-_________-EPI______-200603031200-__
      !
      if (index1 .ne. 0) then
         index2 = index(args%l1b_file, '-')
         args%platform = args%l1b_file(index2-4:index2-1)
      else
         index2 = index(args%l1b_file, '__-')
         args%platform = args%l1b_file(index2+3:index2+6)
      end if
      index2 = index2 + index(args%l1b_file(index2 + 1:), '-')
      index2 = index2 + index(args%l1b_file(index2 + 1:), '-')
      index2 = index2 + index(args%l1b_file(index2 + 1:), '-')
      index2 = index2 + index(args%l1b_file(index2 + 1:), '-')
   end if

   ! get year, doy, hour and minute as strings
   args%cyear = trim(adjustl(args%l1b_file(index2+1:index2+4)))
   args%cmonth = trim(adjustl(args%l1b_file(index2+5:index2+6)))
   args%cday = trim(adjustl(args%l1b_file(index2+7:index2+8)))
   args%chour = trim(adjustl(args%l1b_file(index2+9:index2+10)))
   args%cminute = trim(adjustl(args%l1b_file(index2+11:index2+12)))

   ! get year, doy, hour and minute as integers
   read(args%cyear, '(I4)') args%year
   read(args%cmonth, '(I2)') args%month
   read(args%cday, '(I2)') args%day
   read(args%chour, '(I2)') args%hour
   read(args%cminute, '(I2)') args%minute

   if (index1 .ne. 0) then
      ! Time in native filename is end of scan, which takes 12 mins. So subtract
      ! 12 to get the actual start time of the image.
      ! We cannot tell if the file is RSS or FDS. RSS files are 5 mins, not 15.
      ! This checks the default FDS times (corresponding to 00, 15, 30, 45 mins)
      ! If a match is found, assume it's FDS and subtract 12 mins
      ! If no match, assume it's RSS and subtract 4 mins
      if (args%minute .eq. 12 .or. args%minute .eq. 27 .or. &
          args%minute .eq. 42 .or. args%minute .eq. 57) then
          args%minute = args%minute - 12
      else
          args%minute = args%minute - 4
      end if
   end if

   call GREG2DOY(args%year, args%month, args%day, args%doy)
   write(args%cdoy, '(i3.3)') args%doy

   ! SEVIRI only has a single viewing geometry
   channel_info%nviews = 1

   ! now set up the channels
   call common_setup(channel_info, channel_ids_user, channel_ids_default, &
      all_channel_wl_abs, all_channel_sw_flag, all_channel_lw_flag, &
      all_channel_ids_rttov_coef_sw, all_channel_ids_rttov_coef_lw, &
      all_map_ids_abs_to_ref_band_land, all_map_ids_abs_to_ref_band_sea, &
      all_map_ids_abs_to_snow_and_ice, all_map_ids_view_number, &
      all_channel_fractional_uncertainty, all_channel_minimum_uncertainty, &
      all_channel_numerical_uncertainty, all_channel_lnd_uncertainty, &
      all_channel_sea_uncertainty, all_channel_absolute_bias, all_channel_relative_bias, all_nchannels_total)

   if (verbose) write(*,*) '>>>>>>>>>>>>>>> Leaving setup_seviri()'

end subroutine setup_seviri


subroutine setup_slstr(args, source_attributes, channel_ids_user, &
     channel_info, verbose)

   use calender_m
   use channel_structures_m
   use preproc_constants_m
   use orac_ncdf_m
   use source_attributes_m

   implicit none

   type(setup_args_t),        intent(inout) :: args
   type(source_attributes_t), intent(inout) :: source_attributes
   integer, pointer,          intent(in)    :: channel_ids_user(:)
   type(channel_info_t),      intent(inout) :: channel_info
   logical,                   intent(in)    :: verbose

   integer                    :: index2

   ! Variables for dealing with netcdf files (required for timestamping)
   integer                         :: fid, ierr
   character(len=path_length)      :: geo_start, l1b_start
   character(len=attribute_length) :: l1b_source, l1b_institute, l1b_orbit_no_s
   integer(kind=8)                 :: l1b_orbit_no

   ! Static instrument channel definitions. (These should not be changed.)
   integer, parameter :: all_nchannels_total = 18

       ! 1,       2,       3,       4,       5,       6,       7,       8       9
   real,    parameter :: all_channel_wl_abs(all_nchannels_total) = &
      (/ 0.555,   0.659,   0.865,   1.375,   1.640,   2.250,   3.740,   10.85,  12.00, &
         0.555,   0.659,   0.865,   1.375,   1.640,   2.250,   3.740,   10.85,  12.00 /)

   integer, parameter :: all_channel_sw_flag(all_nchannels_total) = &
      (/ 1,       1,       1,       1,       1,       1,       1,       0,      0, &
         1,       1,       1,       1,       1,       1,       1,       0,      0  /)

   integer, parameter :: all_channel_lw_flag(all_nchannels_total) = &
      (/ 0,       0,       0,       0,       0,       0,       1,       1,      1, &
         0,       0,       0,       0,       0,       0,       1,       1,      1  /)
   integer, parameter :: all_channel_ids_rttov_coef_sw(all_nchannels_total) = &
      (/ 1,       2,       3,       4,       5,       6,       7,       8,      9, &
         1,       2,       3,       4,       5,       6,       7,       8,      9  /)

   integer, parameter :: all_channel_ids_rttov_coef_lw(all_nchannels_total) = &
      (/ 0,       0,       0,       0,       0,       0,       1,       2,      3, &
         0,       0,       0,       0,       0,       0,       1,       2,      3  /)

   integer, parameter :: all_map_ids_abs_to_ref_band_land(all_nchannels_total) = &
      (/ 4,       1,       2,       5,       6,       7,       0,       0,      0, &
         4,       1,       2,       5,       6,       7,       0,       0,      0  /)

   integer, parameter :: all_map_ids_abs_to_ref_band_sea(all_nchannels_total) = &
      (/ 2,       3,       4,       6,       7,       8,       9,       0,      0, &
         2,       3,       4,       6,       7,       8,       9,       0,      0  /)

   integer, parameter :: all_map_ids_abs_to_snow_and_ice(all_nchannels_total) = &
      (/ 1,       1,       2,       3,       3,       3,       4,       0,      0, &
         1,       1,       2,       3,       3,       3,       4,       0,      0  /)

   integer, parameter :: all_map_ids_view_number(all_nchannels_total) = &
      (/ 1,       1,       1,       1,       1,       1,       1,       1,      1,&
         2,       2,       2,       2,       2,       2,       2,       2,      2  /)

   real,    parameter :: all_channel_fractional_uncertainty(all_nchannels_total) = &
      (/ 0.,      0.,      0.,      0.,      0.,      0.,      0.,      0.,     0., &
         0.,      0.,      0.,      0.,      0.,      0.,      0.,      0.,     0. /)

   real,    parameter :: all_channel_minimum_uncertainty(all_nchannels_total) = &
      (/ 0.,      0.,      0.,      0.,      0.,      0.,      0.,      0.,     0., &
         0.,      0.,      0.,      0.,      0.,      0.,      0.,      0.,     0. /)

   real,    parameter :: all_channel_numerical_uncertainty(all_nchannels_total) = &
      (/ 0.,      0.,      0.,      0.,      0.,      0.,      0.,      0.,     0., &
         0.,      0.,      0.,      0.,      0.,      0.,      0.,      0.,     0. /)

   real,    parameter :: all_channel_lnd_uncertainty(all_nchannels_total) = &
      (/ 0.,      0.,      0.,      0.,      0.,      0.,      0.,      0.,     0., &
         0.,      0.,      0.,      0.,      0.,      0.,      0.,      0.,     0. /)

   real,    parameter :: all_channel_sea_uncertainty(all_nchannels_total) = &
      (/ 0.,      0.,      0.,      0.,      0.,      0.,      0.,      0.,     0., &
         0.,      0.,      0.,      0.,      0.,      0.,      0.,      0.,     0. /)

   ! Bias correction
   ! The calibration of SLSTR is under ongoing revisions, such that the
   ! application of empirical correction factors is currently required.
   ! These are taken from the SLSTR User Guide and/or personal communications.
   real,    parameter :: all_channel_absolute_bias(all_nchannels_total) = &
      (/ 0.,      0.,      0.,      0.,      0.,      0.,      0.,      0.,     0., &
         0.,      0.,      0.,      0.,      0.,      0.,      0.,      0.,     0. /)
   real,    parameter :: all_channel_relative_bias(all_nchannels_total) = &
      (/ 0.95,    1.,      1.,      1.,      1.10,    1.10,    1.,      1.,     1., &
         0.92,    0.93,    0.93,    1.,      1.,      1.,      1.,      1.,     1. /)

   ! Only this below needs to be set to change the desired default channels. All
   ! other channel related arrays/indexes are set automatically given the static
   ! instrument channel definition above.
   integer, parameter :: channel_ids_default(6) = (/ 2, 3, 5, 7, 8, 9 /)


   if (verbose) write(*,*) '<<<<<<<<<<<<<<< Entering setup_slstr()'

   if (verbose) write(*,*) 'args%l1b_file: ', trim(args%l1b_file)
   if (verbose) write(*,*) 'args%geo_file: ', trim(args%geo_file)

   ! check if l1b and geo file are of the same granule

   call ncdf_open(fid, args%l1b_file, 'setup_slstr()')
   ierr = nf90_get_att(fid, nf90_global, "start_time", l1b_start)
   if (ierr.ne.NF90_NOERR) then
      print*, 'ERROR: setup_slstr(): Error getting start_time from file ', trim(args%l1b_file)
      stop
   end if
   ! Extract level1 processing centre, processor version and absolute
   ! orbit number from l1b file
   ierr = nf90_get_att(fid, nf90_global, "institution", l1b_institute)
   if (ierr.ne.NF90_NOERR) then
      print*, 'ERROR: setup_slstr(): Error getting institution from file ', &
           trim(args%l1b_file)
      stop
   end if
   ierr = nf90_get_att(fid, nf90_global, "source", l1b_source)
   if (ierr.ne.NF90_NOERR) then
      print*, 'ERROR: setup_slstr(): Error getting source from file ', &
           trim(args%l1b_file)
      stop
   end if
   ierr = nf90_get_att(fid, nf90_global, "absolute_orbit_number", l1b_orbit_no)
   if (ierr.ne.NF90_NOERR) then
      print*, 'ERROR: setup_slstr(): Error getting absolute_orbit_number from file ', &
           trim(args%l1b_file)
      stop
   end if

   call ncdf_close(fid, 'setup_slstr(args%l1b_file)')

   call ncdf_open(fid, args%geo_file, 'setup_slstr()')
   ierr = nf90_get_att(fid, nf90_global, "start_time", geo_start)
   if (ierr.ne.NF90_NOERR) then
      print*, 'ERROR: setup_slstr(): Error getting start_time from file ', &
           trim(args%geo_file)
      stop
   end if
   call ncdf_close(fid, 'setup_slstr(args%geo_file)')
   if (trim(l1b_start).ne.trim(geo_start)) then
      print*, "ERROR: Start times for geo and image granules don't match: "
      write(*,*) trim(l1b_start)
      write(*,*) trim(geo_start)
      stop
   end if
   index2 = 1
   index2 = index(args%l1b_file, "S3A")
   if (index2 .gt. 1) then
      args%platform = "Sentinel3a"
   else
      index2 = index(args%l1b_file, "S3B")
      if (index2 .gt. 1) then
         args%platform = "Sentinel3b"
      else
         write(*,*) "ERROR: Platform must be S3A or S3B"
         stop
      end if
   end if

   ! Populate the source_attributes structure with source and orbit
   ! number information
   source_attributes%level1b_version = trim(l1b_source)// &
        ', processing centre: '//trim(l1b_institute)
   ! The orbit number is stored as a 5-digit string. 100,000 orbits corresponds
   ! to approximately 18.25 years for LEO.
   write(l1b_orbit_no_s, '(I5.5)') l1b_orbit_no
   source_attributes%level1b_orbit_number = trim(adjustl(l1b_orbit_no_s))
   if (verbose) then
      write(*,*) "Satellite is: ", args%platform
      write(*,*) "Source attribute is: ", trim(source_attributes%level1b_version)
      write(*,*) "Orbit number is: ", trim(source_attributes%level1b_orbit_number)
   end if

   ! The code below extracts date/time info from the l1b start time.

   ! get year, doy, hour and minute as strings
   index2 = 1
   args%cyear = trim(adjustl(l1b_start(index2:index2+4)))
   args%cmonth = trim(adjustl(l1b_start(index2+5:index2+6)))
   args%cday = trim(adjustl(l1b_start(index2+8:index2+9)))
   args%chour = trim(adjustl(l1b_start(index2+11:index2+12)))
   args%cminute = trim(adjustl(l1b_start(index2+14:index2+15)))
   args%csecond = trim(adjustl(l1b_start(index2+17:index2+18)))

   ! get year, doy, hour and minute as integers
   read(args%cyear, '(I4)') args%year
   read(args%cmonth, '(I2)') args%month
   read(args%cday, '(I2)') args%day
   read(args%chour, '(I2)') args%hour
   read(args%cminute, '(I2)') args%minute
   read(args%csecond, '(I2)') args%second
   if (args%second .ge. 30) then
      args%minute = args%minute+1
      if (args%minute .ge. 60) then
         args%minute = 0
         args%hour = args%hour+1
         if (args%hour .ge. 24) then
            args%hour = 0
            args%day = args%day+1
            write(args%cday, '(i0.2)') args%day
         end if
         write(args%chour, '(i0.2)') args%hour
      end if
      write(args%cminute, '(i0.2)') args%minute
   end if
   call GREG2DOY(args%year, args%month, args%day, args%doy)
   write(args%cdoy, '(i3.3)') args%doy

   ! SLSTR has two views, nadir and oblique
   channel_info%nviews = 2

   ! now set up the channels
   call common_setup(channel_info, channel_ids_user, channel_ids_default, &
      all_channel_wl_abs, all_channel_sw_flag, all_channel_lw_flag, &
      all_channel_ids_rttov_coef_sw, all_channel_ids_rttov_coef_lw, &
      all_map_ids_abs_to_ref_band_land, all_map_ids_abs_to_ref_band_sea, &
      all_map_ids_abs_to_snow_and_ice, all_map_ids_view_number, &
      all_channel_fractional_uncertainty, all_channel_minimum_uncertainty, &
      all_channel_numerical_uncertainty, all_channel_lnd_uncertainty, &
      all_channel_sea_uncertainty, all_channel_absolute_bias, all_channel_relative_bias, all_nchannels_total)

   if (verbose) write(*,*) '>>>>>>>>>>>>>>> Leaving setup_slstr()'

end subroutine setup_slstr


subroutine setup_viirs_mband(args, channel_ids_user, channel_info, verbose)

   use calender_m
   use channel_structures_m
   use preproc_constants_m

   implicit none

   type(setup_args_t),   intent(inout) :: args
   integer, pointer,     intent(in)    :: channel_ids_user(:)
   type(channel_info_t), intent(inout) :: channel_info
   logical,              intent(in)    :: verbose

   character(len=path_length) :: geo_dtstr, l1b_dtstr

   integer                    :: index1, index2


   ! Static instrument channel definitions. (These should not be changed.)
   integer, parameter :: all_nchannels_total = 16

       ! 1,       2,       3,       4,       5,       6,       7,       8
   real,    parameter :: all_channel_wl_abs(all_nchannels_total) = &
      (/ 0.412,   0.445,   0.488,   0.555,   0.672,   0.746,   0.865,   1.240, &
         1.378,   1.610,   2.250,   3.700,   4.050,   8.550,   10.763,  12.013 /)

   integer, parameter :: all_channel_sw_flag(all_nchannels_total) = &
      (/ 1,       1,       1,       1,       1,       1,       1,       1, &
         1,       1,       1,       1,       0,       0,       0,       0/)

   integer, parameter :: all_channel_lw_flag(all_nchannels_total) = &
      (/ 0,       0,       0,       0,       0,       0,       0,       0, &
         0,       0,       0,       1,       1,       1,       1       ,1/)

   integer, parameter :: all_channel_ids_rttov_coef_sw(all_nchannels_total) = &
      (/ 3,       4,       5,       6,       7,       8,       9,       11, &
         12,      13,      15,      17,      18,      19,      20,      22 /)

   integer, parameter :: all_channel_ids_rttov_coef_lw(all_nchannels_total) = &
      (/ 0,       0,       0,       0,       0,       0,       0,       0, &
         0,       0,       0,       2,       3,       4,       5,       7 /)

   integer, parameter :: all_map_ids_abs_to_ref_band_land(all_nchannels_total) = &
      (/ 3,       3,       3,       4,       1,       2,       2,       5, &
         5,       6,       7,       0,       0,       0,       0,       0 /)

   integer, parameter :: all_map_ids_abs_to_ref_band_sea(all_nchannels_total) = &
      (/ 1,       1,       1,       2,       3,       3,       4,       5, &
         5,       7,       8,       9,       0,       0,       0,       0 /)

   integer, parameter :: all_map_ids_abs_to_snow_and_ice(all_nchannels_total) = &
      (/ 1,       1,       1,       1,       1,       1,       2,       3, &
         3,       3,       3,       4,       0,       0,       0,       0 /)

   integer, parameter :: all_map_ids_view_number(all_nchannels_total) = &
      (/ 1,       1,       1,       1,       1,       1,       1,       1, &
         1,       1,       1,       1,       1,       1,       1,       1 /)

   real,    parameter :: all_channel_fractional_uncertainty(all_nchannels_total) = &
      (/ 0.,      0.,      0.,      0.,      0.,      0.,      0.,      0., &
         0.,      0.,      0.,      0.,      0.,      0.,      0.,      0. /)

   real,    parameter :: all_channel_minimum_uncertainty(all_nchannels_total) = &
      (/ 0.,      0.,      0.,      0.,      0.,      0.,      0.,      0., &
         0.,      0.,      0.,      0.,      0.,      0.,      0.,      0. /)

   real,    parameter :: all_channel_numerical_uncertainty(all_nchannels_total) = &
      (/ 0.,      0.,      0.,      0.,      0.,      0.,      0.,      0., &
         0.,      0.,      0.,      0.,      0.,      0.,      0.,      0. /)

   real,    parameter :: all_channel_lnd_uncertainty(all_nchannels_total) = &
      (/ 0.,      0.,      0.,      0.,      0.,      0.,      0.,      0., &
         0.,      0.,      0.,      0.,      0.,      0.,      0.,      0. /)

   real,    parameter :: all_channel_sea_uncertainty(all_nchannels_total) = &
      (/ 0.,      0.,      0.,      0.,      0.,      0.,      0.,      0., &
         0.,      0.,      0.,      0.,      0.,      0.,      0.,      0. /)

   ! Bias correction
   real,    parameter :: all_channel_absolute_bias(all_nchannels_total) = &
      (/ 0.,      0.,      0.,      0.,      0.,      0.,      0.,      0., &
         0.,      0.,      0.,      0.,      0.,      0.,      0.,      0. /)
   real,    parameter :: all_channel_relative_bias(all_nchannels_total) = &
      (/ 1.,      1.,      1.,      1.,      1.,      1.,      1.,      1., &
         1.,      1.,      1.,      1.,      1.,      1.,      1.,      1. /)

   ! Only this below needs to be set to change the desired default channels. All
   ! other channel related arrays/indexes are set automatically given the static
   ! instrument channel definition above.
   integer, parameter :: channel_ids_default(6) = (/ 5, 7, 10, 12, 15, 16 /)


   if (verbose) write(*,*) '<<<<<<<<<<<<<<< Entering setup_viirs_mband()'

   if (verbose) write(*,*) 'args%l1b_file: ', trim(args%l1b_file)
   if (verbose) write(*,*) 'args%geo_file: ', trim(args%geo_file)

   ! Assume Suomi-NPP by default
   args%platform = "SuomiNPP"
   ! check if l1b and geo file are of the same granule
   index1 = index(args%l1b_file, 'npp_d', .true.)
   index2 = index(args%geo_file, 'npp_d', .true.)

   if (index1 .le. 0) then
      index1 = index(args%l1b_file, 'j01_d', .true.)
      index2 = index(args%geo_file, 'j01_d', .true.)
      if (index1 .le. 0) then
         write(*,*)'ERROR: setup_viirs_iband(): Unsupported platform'
         stop error_stop_code
      end if
      args%platform = "NOAA20"
   end if


   l1b_dtstr = trim(adjustl(args%l1b_file(index2+5:index2+5+25)))
   geo_dtstr = trim(adjustl(args%geo_file(index2+5:index2+5+25)))

   ! check if l1b and geo files identical
   if (trim(adjustl(l1b_dtstr)) .ne. &
       trim(adjustl(geo_dtstr))) then
      write(*,*)
      write(*,*) 'ERROR: setup_viirs_mband(): Geolocation and L1b files are ' // &
           'for different times'
      write(*,*) 'args%l1b_file: ', trim(adjustl(args%geo_file))
      write(*,*) 'args%geo_file: ', trim(adjustl(args%l1b_file))

      stop error_stop_code
   end if

   if (verbose) write(*,*) "Satellite is: ", args%platform

   ! The code below extracts date/time info from the segment name.
   ! Note that it requires the segment name to be in the generic format
   ! that's specified by the NOAA. Weird filenames will break things.

   ! get year, doy, hour and minute as strings
   index2 = index2+5
   args%cyear = trim(adjustl(args%l1b_file(index2:index2+4)))
   args%cmonth = trim(adjustl(args%l1b_file(index2+4:index2+5)))
   args%cday = trim(adjustl(args%l1b_file(index2+6:index2+7)))
   args%chour = trim(adjustl(args%l1b_file(index2+10:index2+11)))
   args%cminute = trim(adjustl(args%l1b_file(index2+12:index2+13)))
   ! get year, doy, hour and minute as integers
   read(args%cyear, '(I4)') args%year
   read(args%cmonth, '(I2)') args%month
   read(args%cday, '(I2)') args%day
   read(args%chour, '(I2)') args%hour
   read(args%cminute, '(I2)') args%minute
   call GREG2DOY(args%year, args%month, args%day, args%doy)
   write(args%cdoy, '(i3.3)') args%doy

   ! VIIRS only has a single viewing geometry
   channel_info%nviews = 1

   ! now set up the channels
   call common_setup(channel_info, channel_ids_user, channel_ids_default, &
      all_channel_wl_abs, all_channel_sw_flag, all_channel_lw_flag, &
      all_channel_ids_rttov_coef_sw, all_channel_ids_rttov_coef_lw, &
      all_map_ids_abs_to_ref_band_land, all_map_ids_abs_to_ref_band_sea, &
      all_map_ids_abs_to_snow_and_ice, all_map_ids_view_number, &
      all_channel_fractional_uncertainty, all_channel_minimum_uncertainty, &
      all_channel_numerical_uncertainty, all_channel_lnd_uncertainty, &
      all_channel_sea_uncertainty, all_channel_absolute_bias, all_channel_relative_bias, all_nchannels_total)

   if (verbose) write(*,*) '>>>>>>>>>>>>>>> Leaving setup_viirs_mband()'

end subroutine setup_viirs_mband



subroutine setup_viirs_iband(args, channel_ids_user, channel_info, verbose)

   use calender_m
   use channel_structures_m
   use preproc_constants_m

   implicit none

   type(setup_args_t),   intent(inout) :: args
   integer, pointer,     intent(in)    :: channel_ids_user(:)
   type(channel_info_t), intent(inout) :: channel_info
   logical,              intent(in)    :: verbose

   character(len=path_length) :: geo_dtstr, l1b_dtstr

   integer                    :: index1, index2


   ! Static instrument channel definitions. (These should not be changed.)
   integer, parameter :: all_nchannels_total = 5

       ! 1,       2,       3,       4,       5,       6,       7,       8
   real,    parameter :: all_channel_wl_abs(all_nchannels_total) = &
      (/ 0.640,   0.865,   1.610,   3.740,   11.45 /)

   integer, parameter :: all_channel_sw_flag(all_nchannels_total) = &
      (/ 1,       1,       1,       1,       0 /)

   integer, parameter :: all_channel_lw_flag(all_nchannels_total) = &
      (/ 0,       0,       0,       1,       1 /)

   integer, parameter :: all_channel_ids_rttov_coef_sw(all_nchannels_total) = &
      (/ 1,       2,       13,      16,      21 /)

   integer, parameter :: all_channel_ids_rttov_coef_lw(all_nchannels_total) = &
      (/ 0,       0,       0,       1,       6 /)

   integer, parameter :: all_map_ids_abs_to_ref_band_land(all_nchannels_total) = &
      (/ 1,       2,       6,       0,       0 /)

   integer, parameter :: all_map_ids_abs_to_ref_band_sea(all_nchannels_total) = &
      (/ 3,       4,       7,       9,       0 /)

   integer, parameter :: all_map_ids_abs_to_snow_and_ice(all_nchannels_total) = &
      (/ 1,       2,       3,       4,       0 /)

   integer, parameter :: all_map_ids_view_number(all_nchannels_total) = &
      (/ 1,       1,       1,       1,       1 /)

   real,    parameter :: all_channel_fractional_uncertainty(all_nchannels_total) = &
      (/ 0.,      0.,      0.,      0.,      0./)

   real,    parameter :: all_channel_minimum_uncertainty(all_nchannels_total) = &
      (/ 0.,      0.,      0.,      0.,      0./)

   real,    parameter :: all_channel_numerical_uncertainty(all_nchannels_total) = &
      (/ 0.,      0.,      0.,      0.,      0./)

   real,    parameter :: all_channel_lnd_uncertainty(all_nchannels_total) = &
      (/ 0.,      0.,      0.,      0.,      0./)

   real,    parameter :: all_channel_sea_uncertainty(all_nchannels_total) = &
      (/ 0.,      0.,      0.,      0.,      0./)

   ! Bias correction
   real,    parameter :: all_channel_absolute_bias(all_nchannels_total) = &
      (/ 0.,      0.,      0.,      0.,      0./)
   real,    parameter :: all_channel_relative_bias(all_nchannels_total) = &
      (/ 1.,      1.,      1.,      1.,      1./)

   ! Only this below needs to be set to change the desired default channels. All
   ! other channel related arrays/indexes are set automatically given the static
   ! instrument channel definition above.
   integer, parameter :: channel_ids_default(6) = (/ 1, 2, 3, 4, 5, 5 /)


   if (verbose) write(*,*) '<<<<<<<<<<<<<<< Entering setup_viirs_iband()'

   if (verbose) write(*,*) 'args%l1b_file: ', trim(args%l1b_file)
   if (verbose) write(*,*) 'args%geo_file: ', trim(args%geo_file)

   ! Assume Suomi-NPP by default
   args%platform = "SuomiNPP"
   ! check if l1b and geo file are of the same granule
   index1 = index(args%l1b_file, 'npp_d', .true.)
   index2 = index(args%geo_file, 'npp_d', .true.)

   if (index1 .le. 0) then
      index1 = index(args%l1b_file, 'j01_d', .true.)
      index2 = index(args%geo_file, 'j01_d', .true.)
      if (index1 .le. 0) then
         write(*,*)'ERROR: setup_viirs_iband(): Unsupported platform'
         stop error_stop_code
      end if
      args%platform = "NOAA20"
   end if


   l1b_dtstr = trim(adjustl(args%l1b_file(index2+5:index2+5+25)))
   geo_dtstr = trim(adjustl(args%geo_file(index2+5:index2+5+25)))

   ! check if l1b and geo files identical
   if (trim(adjustl(l1b_dtstr)) .ne. &
       trim(adjustl(geo_dtstr))) then
      write(*,*)
      write(*,*) 'ERROR: setup_viirs_iband(): Geolocation and L1b files are ' // &
           'for different times'
      write(*,*) 'args%l1b_file: ', trim(adjustl(args%geo_file))
      write(*,*) 'args%geo_file: ', trim(adjustl(args%l1b_file))

      stop error_stop_code
   end if

   if (verbose) write(*,*) "Satellite is: ", args%platform

   ! The code below extracts date/time info from the segment name.
   ! Note that it requires the segment name to be in the generic format
   ! that's specified by the NOAA. Weird filenames will break things.

   index2 = index(args%l1b_file, 'npp_d')

   if (index2 .le. 0) then
      index2 = index(args%l1b_file, 'j01_d', .true.)
      if (index2 .le. 0) then
         write(*,*)'ERROR: setup_viirs_iband(): Unsupported platform'
         stop error_stop_code
      end if
   end if

   ! get year, doy, hour and minute as strings
   index2 = index2+5
   args%cyear = trim(adjustl(args%l1b_file(index2:index2+4)))
   args%cmonth = trim(adjustl(args%l1b_file(index2+4:index2+5)))
   args%cday = trim(adjustl(args%l1b_file(index2+6:index2+7)))
   args%chour = trim(adjustl(args%l1b_file(index2+10:index2+11)))
   args%cminute = trim(adjustl(args%l1b_file(index2+12:index2+13)))

   ! get year, doy, hour and minute as integers
   read(args%cyear, '(I4)') args%year
   read(args%cmonth, '(I2)') args%month
   read(args%cday, '(I2)') args%day
   read(args%chour, '(I2)') args%hour
   read(args%cminute, '(I2)') args%minute
   call GREG2DOY(args%year, args%month, args%day, args%doy)
   write(args%cdoy, '(i3.3)') args%doy

   ! VIIRS only has a single viewing geometry
   channel_info%nviews = 1

   ! now set up the channels
   call common_setup(channel_info, channel_ids_user, channel_ids_default, &
      all_channel_wl_abs, all_channel_sw_flag, all_channel_lw_flag, &
      all_channel_ids_rttov_coef_sw, all_channel_ids_rttov_coef_lw, &
      all_map_ids_abs_to_ref_band_land, all_map_ids_abs_to_ref_band_sea, &
      all_map_ids_abs_to_snow_and_ice, all_map_ids_view_number, &
      all_channel_fractional_uncertainty, all_channel_minimum_uncertainty, &
      all_channel_numerical_uncertainty, all_channel_lnd_uncertainty, &
      all_channel_sea_uncertainty, all_channel_absolute_bias, all_channel_relative_bias, all_nchannels_total)

   if (verbose) write(*,*) '>>>>>>>>>>>>>>> Leaving setup_viirs_iband()'

end subroutine setup_viirs_iband


subroutine common_setup(channel_info, channel_ids_user, channel_ids_default, &
   all_channel_wl_abs, all_channel_sw_flag, all_channel_lw_flag, &
   all_channel_ids_rttov_coef_sw, all_channel_ids_rttov_coef_lw, &
   all_map_ids_abs_to_ref_band_land, all_map_ids_abs_to_ref_band_sea, &
   all_map_ids_abs_to_snow_and_ice, all_map_ids_view_number, &
   all_channel_fractional_uncertainty, all_channel_minimum_uncertainty, &
   all_channel_numerical_uncertainty, all_channel_lnd_uncertainty, &
   all_channel_sea_uncertainty, all_channel_absolute_bias, all_channel_relative_bias, all_nchannels_total)

   use channel_structures_m

   implicit none

   type(channel_info_t), intent(inout) :: channel_info
   integer, pointer,     intent(in)    :: channel_ids_user(:)
   integer,              intent(in)    :: channel_ids_default(:)
   real,                 intent(in)    :: all_channel_wl_abs (:)
   integer,              intent(in)    :: all_channel_sw_flag(:)
   integer,              intent(in)    :: all_channel_lw_flag(:)
   integer,              intent(in)    :: all_channel_ids_rttov_coef_sw(:)
   integer,              intent(in)    :: all_channel_ids_rttov_coef_lw(:)
   integer,              intent(in)    :: all_map_ids_abs_to_ref_band_land(:)
   integer,              intent(in)    :: all_map_ids_abs_to_ref_band_sea(:)
   integer,              intent(in)    :: all_map_ids_abs_to_snow_and_ice(:)
   integer,              intent(in)    :: all_map_ids_view_number(:)
   real,                 intent(in)    :: all_channel_fractional_uncertainty(:)
   real,                 intent(in)    :: all_channel_minimum_uncertainty(:)
   real,                 intent(in)    :: all_channel_numerical_uncertainty(:)
   real,                 intent(in)    :: all_channel_lnd_uncertainty(:)
   real,                 intent(in)    :: all_channel_sea_uncertainty(:)
   real,                 intent(in)    :: all_channel_absolute_bias(:)
   real,                 intent(in)    :: all_channel_relative_bias(:)
   integer,              intent(in)    :: all_nchannels_total

   integer, dimension(:), allocatable  :: unique_views

   integer :: i, j, n_views
   integer :: i_sw
   integer :: i_lw


   channel_info%all_nchannels_total = all_nchannels_total

   if (associated(channel_ids_user)) then
      channel_info%nchannels_total = size(channel_ids_user)
   else
      channel_info%nchannels_total = size(channel_ids_default)
   end if

   allocate(unique_views(channel_info%nchannels_total))
   call allocate_channel_info(channel_info)

   if (associated(channel_ids_user)) then
      channel_info%channel_ids_instr = channel_ids_user
   else
      channel_info%channel_ids_instr = channel_ids_default
   end if

   do i = 1, channel_info%nchannels_total
      channel_info%channel_wl_abs (i) = &
         all_channel_wl_abs (channel_info%channel_ids_instr(i))

      channel_info%channel_sw_flag(i) = &
         all_channel_sw_flag(channel_info%channel_ids_instr(i))
      channel_info%channel_lw_flag(i) = &
         all_channel_lw_flag(channel_info%channel_ids_instr(i))
      channel_info%channel_view_ids(i) = &
         all_map_ids_view_number(channel_info%channel_ids_instr(i))

      channel_info%channel_fractional_uncertainty(i) = &
         all_channel_fractional_uncertainty(channel_info%channel_ids_instr(i))
      channel_info%channel_minimum_uncertainty(i) = &
         all_channel_minimum_uncertainty(channel_info%channel_ids_instr(i))

      ! Forward model uncertainties combine numerical and surface errors
      channel_info%channel_fm_lnd_uncertainty(i) = sqrt( &
         all_channel_numerical_uncertainty(channel_info%channel_ids_instr(i)) * &
         all_channel_numerical_uncertainty(channel_info%channel_ids_instr(i)) + &
         all_channel_lnd_uncertainty(channel_info%channel_ids_instr(i)) * &
         all_channel_lnd_uncertainty(channel_info%channel_ids_instr(i)))
      channel_info%channel_fm_sea_uncertainty(i) = sqrt( &
         all_channel_numerical_uncertainty(channel_info%channel_ids_instr(i)) * &
         all_channel_numerical_uncertainty(channel_info%channel_ids_instr(i)) + &
         all_channel_sea_uncertainty(channel_info%channel_ids_instr(i)) * &
         all_channel_sea_uncertainty(channel_info%channel_ids_instr(i)))

      channel_info%channel_absolute_bias(i) = &
         all_channel_absolute_bias(channel_info%channel_ids_instr(i))
      channel_info%channel_relative_bias(i) = &
         all_channel_relative_bias(channel_info%channel_ids_instr(i))
   end do

   ! This section computes the actual number of views in the scene
   ! It loops over all channels to determine unique views
   n_views = 1
   unique_views(1) = channel_info%channel_view_ids(1)
   first: do i = 2, channel_info%nchannels_total
      do j = 1, n_views
         if (unique_views(j) == channel_info%channel_view_ids(i)) then
            cycle first
         end if
      end do
      n_views = n_views + 1
      unique_views(n_views) = channel_info%channel_view_ids(i)
   end do first

   channel_info%nviews = n_views

   channel_info%nchannels_sw = &
      sum(channel_info%channel_sw_flag(1:channel_info%nchannels_total))
   allocate(channel_info%map_ids_sw_to_channel(channel_info%nchannels_sw))
   allocate(channel_info%channel_ids_rttov_coef_sw(channel_info%nchannels_sw))
   allocate(channel_info%sw_rttov_viewone_id(channel_info%nchannels_sw))
   allocate(channel_info%sw_view_ids(channel_info%nchannels_sw))
   channel_info%channel_ids_rttov_coef_sw = 0

   channel_info%nchannels_lw = &
      sum(channel_info%channel_lw_flag(1:channel_info%nchannels_total))
   allocate(channel_info%map_ids_lw_to_channel(channel_info%nchannels_lw))
   allocate(channel_info%channel_ids_rttov_coef_lw(channel_info%nchannels_lw))
   allocate(channel_info%lw_rttov_viewone_id(channel_info%nchannels_lw))
   allocate(channel_info%lw_view_ids(channel_info%nchannels_lw))
   channel_info%channel_ids_rttov_coef_lw = 0


   i_sw = 1
   i_lw = 1
   do i = 1, channel_info%nchannels_total
      if (channel_info%channel_sw_flag(i) .ne. 0) then
         channel_info%map_ids_sw_to_channel(i_sw) = i
         channel_info%map_ids_channel_to_sw(i) = i_sw
         channel_info%channel_ids_rttov_coef_sw(i_sw) = &
            all_channel_ids_rttov_coef_sw(channel_info%channel_ids_instr(i))

         channel_info%sw_view_ids(i_sw) = channel_info%channel_view_ids(i)

         channel_info%map_ids_abs_to_ref_band_land(i_sw) = &
            all_map_ids_abs_to_ref_band_land(channel_info%channel_ids_instr(i))

         channel_info%map_ids_abs_to_ref_band_sea (i_sw) = &
            all_map_ids_abs_to_ref_band_sea (channel_info%channel_ids_instr(i))

         channel_info%map_ids_abs_to_snow_and_ice (i_sw) = &
            all_map_ids_abs_to_snow_and_ice (channel_info%channel_ids_instr(i))

         ! RTTOV only has one set of channel coefficients - it doesn't repeat
         ! for instruments with multiple view angles. Therefore we have to
         ! select the equivalent RTTOV channel (in view = 1) for each of the
         ! actual instrument views (view > 1). For simplicity we also calculate
         ! for view = 1.
         do j = 1, channel_info%nchannels_total
            if ((channel_info%channel_view_ids(j) .eq. 1) .and. &
                (channel_info%channel_wl_abs(j) .eq. channel_info%channel_wl_abs(i))) then
               channel_info%sw_rttov_viewone_id(i_sw) = j
            end if
         end do
         i_sw = i_sw + 1
      end if

      if (channel_info%channel_lw_flag(i) .ne. 0) then
         channel_info%map_ids_lw_to_channel(i_lw) = i
         channel_info%map_ids_channel_to_lw(i) = i_lw
         channel_info%channel_ids_rttov_coef_lw(i_lw) = &
            all_channel_ids_rttov_coef_lw(channel_info%channel_ids_instr(i))

         channel_info%lw_view_ids(i_lw) = channel_info%channel_view_ids(i)

         do j = 1, channel_info%nchannels_total
            if ((channel_info%channel_view_ids(j) .eq. 1) .and. &
                (channel_info%channel_wl_abs(j) .eq. channel_info%channel_wl_abs(i))) then
               channel_info%lw_rttov_viewone_id(i_lw) = j
            end if
         end do

         i_lw = i_lw + 1
      end if
   end do

end subroutine common_setup

subroutine determine_seviri_platform_from_metoffice(l1_file, platform)
   ! See http://support.hdfgroup.org/ftp/HDF5/current/src/unpacked/fortran/examples/compound.f90
   use common_constants_m, only: error_stop_code
   use hdf5
   implicit none

   character(len=*),   intent(in)  :: l1_file
   character(len=*),   intent(out) :: platform

   integer(4)                      :: platform_number
   integer                         :: error
   integer(kind=HID_T)             :: file_id, group_id, dset_id
   integer(kind=HID_T)             :: type1_id, type2_id
   integer(kind=HSIZE_T)           :: count(1), type_size

   integer(kind=SIZE_T), parameter :: type_init = 4, offset = 0

   call h5open_f(error)
   call h5fopen_f(l1_file, H5F_ACC_RDONLY_F, file_id, error)
   call h5gopen_f(file_id, "MSG/Prologue", group_id, error)
   call h5dopen_f(group_id, "GeneralInfo", dset_id, error)

   call h5tcopy_f(H5T_NATIVE_INTEGER, type2_id, error)
   call h5tset_size_f(type2_id, type_init, error)
   call h5tget_size_f(type2_id, type_size, error)
   call h5tcreate_f(H5T_COMPOUND_F, type_size, type1_id, error)
   call h5tinsert_f(type1_id, "SatId", offset, type2_id, error)

   call h5dread_f(dset_id, type1_id, platform_number, count, error)

   call h5tclose_f(type2_id, error)
   call h5tclose_f(type1_id, error)
   call h5dclose_f(dset_id, error)
   call h5gclose_f(group_id, error)
   call h5fclose_f(file_id, error)
   call h5close_f(error)

   ! https://github.com/pytroll/mpop/blob/master/mpop/satin/msg_seviri_hdf.py#L30
   select case (platform_number)
   case(321)
      platform = "MSG1"
   case(322)
      platform = "MSG2"
   case(323)
      platform = "MSG3"
   case(324)
      platform = "MSG4"
   case default
      write(*,*) "ERROR: Unrecognised platform number ", platform_number
      stop error_stop_code
   end select

end subroutine determine_seviri_platform_from_metoffice

subroutine setup_imager(args, opts, source_attributes, channel_info, verbose)

   use channel_structures_m, only: channel_info_t
   use common_constants_m
   use preproc_structures_m, only: preproc_opts_t, setup_args_t
   use read_aatsr_m
   use read_abi_m
   use read_agri_m
   use read_avhrr_m
   !use read_goes_imager_m
   use read_himawari_m
   use read_modis_m
   use read_seviri_m
   use read_slstr_m
   use read_viirs_iband_m
   use read_viirs_mband_m
   use source_attributes_m, only: source_attributes_t

   implicit none

   type(setup_args_t),        intent(inout) :: args
   type(preproc_opts_t),      intent(in)    :: opts
   type(source_attributes_t), intent(inout) :: source_attributes
   type(channel_info_t),      intent(inout) :: channel_info
   logical,                   intent(in)    :: verbose

   real(kind=sreal), parameter :: loc_limit(4) = (/ -90.0, -180.0, 90.0, 180.0 /)

   select case (trim(args%sensor))
   case('AATSR', 'ATSR2')
      call setup_aatsr(args, opts%channel_ids, channel_info, verbose)

      ! Get array dimensions and along-track offset for the daylight side. If
      ! we're processing daylight data, we may want to chunk process after this.
      call read_aatsr_dimensions(args%l1b_file, args%n_across_track, &
           args%n_along_track, args%along_track_offset, args%day_night, &
           loc_limit, args%n_along_track2, args%along_track_offset2, verbose)

   case('ABI')
      call setup_abi(args, opts%channel_ids, channel_info, verbose)

      ! Get dimensions of the ABI image.
      call read_abi_dimensions(args%geo_file, args%n_across_track, &
           args%n_along_track, verbose)

   case('AGRI')
      call setup_agri(args, opts%channel_ids, channel_info, verbose)
      ! Get dimensions of the AGRI image.
      ! At present only full-disk images are supported
      call read_agri_dimensions(args%geo_file, args%n_across_track, &
           args%n_along_track, verbose)

   case('AHI')
      call setup_ahi(args, opts%channel_ids, channel_info, verbose)

      ! Get dimensions of the AHI image.
      ! Subsetting AHI full-disk images are now supported
      call read_himawari_dimensions(args%geo_file, args%n_across_track, &
           args%n_along_track, args%startx, args%endx, args%starty, &
           args%endy, verbose)

   case('AVHRR')
      call setup_avhrr(args, opts%channel_ids, channel_info, verbose)

      ! get dimensions of the avhrr orbit
      call read_avhrr_dimensions(args%geo_file, args%n_across_track, args%n_along_track)

!   case('GIMG') then
!      call setup_goes_imager(args, opts%channel_ids, channel_info, verbose)

!      ! Get dimensions of the GOES-Imager image.
!      call read_goes_imager_dimensions(args%geo_file, args%n_across_track, &
!           args%n_along_track, args%startx, args%endx, args%starty, args%endy, verbose)

   case('MODIS')
      call setup_modis(args, opts%channel_ids, channel_info, verbose)

      ! get dimensions of the modis granule
      call read_modis_dimensions(args%geo_file, args%n_across_track, args%n_along_track)

   case('SEVIRI')
      call setup_seviri(args, opts%channel_ids, channel_info, verbose)

      ! get dimensions of the seviri image.
      ! For SEVIRI the native level 1.5 image data can come as a subimage of the
      ! the full disk image. Regardless, n_across_track and n_along_track are
      ! set to the constant dimensions of a full disk image as it is convenient
      ! to operate relative to the full disk. startx, endx, starty, endy are
      ! assumed to be given relative to the full disk. As a result, if they are
      ! not being used (not all > 0) then they will be set to the actual image
      ! in the image file and if they are being used (all > 0) then they need to
      ! be checked independently relative to the actual image, both done in the
      ! following call.
      call read_seviri_dimensions(args%geo_file, args%n_across_track, &
           args%n_along_track, args%startx, args%endx, &
           args%starty, args%endy, verbose)

   case('SLSTR')
      call setup_slstr(args, source_attributes, opts%channel_ids, &
           channel_info, verbose)

      ! Get dimensions of the SLSTR image.
      ! At present the full scene will always be processed
      call read_slstr_dimensions(args%l1b_file, args%n_across_track, &
           args%n_along_track, verbose)

   case('VIIRSI')
      call setup_viirs_iband(args, opts%channel_ids, channel_info, verbose)

      ! Get dimensions of the VIIRS image.
      ! At present the full scene will always be processed
      call read_viirs_iband_dimensions(args%geo_file, args%n_across_track, &
           args%n_along_track, verbose)

   case('VIIRSM')
      call setup_viirs_mband(args, opts%channel_ids, channel_info, verbose)

      ! Get dimensions of the VIIRS image.
      ! At present the full scene will always be processed
      call read_viirs_mband_dimensions(args%geo_file, args%n_across_track, &
           args%n_along_track, verbose)

   case default
      write(*,*) 'ERROR: Invalid sensor: ', trim(adjustl(args%sensor))
      stop error_stop_code

   end select

end subroutine setup_imager

end module setup_m
