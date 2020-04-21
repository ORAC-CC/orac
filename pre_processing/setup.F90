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
! l1b_path_file string in          Full path to level 1B data
! geo_path_file string in          Full path to geolocation data
! platform      string both        Name of satellite
! year          sint   out         Year
! month         sint   out         Month of year (1-12)
! doy           sint   out         Day of year (1-366)
! day           sint   out         Day of month (1-31)
! hour          sint   out         Hour of day (0-59)
! minute        sint   out         Minute of hour (0-59)
! cyear         string out         Year, as a 4 character string
! cmonth        string out         Month of year, as a 2 character string
! cdoy          string out         Day of year, as a 3 character string
! cday          string out         Day of month, as a 2 character string
! chour         string out         Hour of day, as a 2 character string
! cminute       string out         Minute of hour, as a 2 character string
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
! 2019/8/14, SP: Add Fengyun-4A support.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

module setup_m

   implicit none

contains

subroutine setup_aatsr(l1b_path_file, geo_path_file, platform, sensor, year, &
   month, day, doy, hour, minute, cyear, cmonth, cday, cdoy, chour, cminute, &
   channel_ids_user, channel_info, verbose)

   use calender_m
   use channel_structures_m
   use preproc_constants_m
   use preproc_structures_m

   implicit none

   character(len=*),     intent(in)    :: l1b_path_file
   character(len=*),     intent(in)    :: geo_path_file
   character(len=*),     intent(out)   :: platform
   character(len=*),     intent(in)    :: sensor
   integer(kind=sint),   intent(out)   :: year, month, day, doy
   integer(kind=sint),   intent(out)   :: hour, minute
   character(len=*),     intent(out)   :: cyear, cmonth, cday
   character(len=*),     intent(out)   :: cdoy, chour, cminute
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

   ! Only this below needs to be set to change the desired default channels. All
   ! other channel related arrays/indexes are set automatically given the static
   ! instrument channel definition above.
   integer, parameter :: channel_ids_default(6) = (/ 2, 3, 4, 5, 6, 7 /)


   if (verbose) write(*,*) '<<<<<<<<<<<<<<< Entering setup_aatsr()'

   if (verbose) write(*,*) 'l1b_path_file: ', trim(l1b_path_file)
   if (verbose) write(*,*) 'geo_path_file: ', trim(geo_path_file)

   ! check if l1b and geo files identical
   if (trim(adjustl(l1b_path_file)) .ne. &
       trim(adjustl(geo_path_file))) then
      write(*,*)
      write(*,*) 'ERROR: setup_aatsr(): Geolocation and L1b files are for ' // &
           'different orbits'
      write(*,*) 'l1b_path_file: ', trim(adjustl(geo_path_file))
      write(*,*) 'geo_path_file: ', trim(adjustl(l1b_path_file))

      stop error_stop_code
   end if

   ! which aatsr are we processing?

   if (trim(adjustl(sensor)) .eq. 'AATSR') then
      index1 = index(l1b_path_file, '.N1', back=.true.)
      platform = 'Envisat'
   else
      platform = 'ERS2'
      index1 = index(l1b_path_file, '.E2', back=.true.)
   end if

   ! Get year, month, day, hour and minute as strings
   cyear = trim(adjustl(l1b_path_file(index1-45:index1-42)))
   cmonth = trim(adjustl(l1b_path_file(index1-41:index1-40)))
   cday = trim(adjustl(l1b_path_file(index1-39:index1-38)))
   chour = trim(adjustl(l1b_path_file(index1-36:index1-35)))
   cminute = trim(adjustl(l1b_path_file(index1-34:index1-33)))

   ! get year, month, day, hour and minute as integers
   read(cyear(1:len_trim(cyear)), '(I4)') year
   read(cmonth, '(I2)') month
   read(cday, '(I2)') day
   read(chour(1:len_trim(chour)), '(I2)') hour
   read(cminute(1:len_trim(cminute)), '(I2)') minute

   call GREG2DOY(year, month, day, doy)
   write(cdoy, '(i3.3)') doy

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
      all_channel_sea_uncertainty, all_nchannels_total)

   if (verbose) write(*,*) '>>>>>>>>>>>>>>> Leaving setup_aatsr()'

end subroutine setup_aatsr

subroutine setup_abi(l1b_path_file, geo_path_file, platform, year, month, day, &
   doy, hour, minute, cyear, cmonth, cday, cdoy, chour, cminute, channel_ids_user, &
   channel_info, verbose)

   use calender_m
   use channel_structures_m
   use preproc_constants_m
   use preproc_structures_m

   implicit none

   character(len=*),     intent(in)    :: l1b_path_file
   character(len=*),     intent(in)    :: geo_path_file
   character(len=*),     intent(out)   :: platform
   integer(kind=sint),   intent(out)   :: year, month, day, doy
   integer(kind=sint),   intent(out)   :: hour, minute
   character(len=*),     intent(out)   :: cyear, cmonth, cday
   character(len=*),     intent(out)   :: cdoy, chour, cminute
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

   ! Only this below needs to be set to change the desired default channels. All
   ! other channel related arrays/indexes are set automatically given the static
   ! instrument channel definition above.
   integer, parameter :: channel_ids_default(6) = (/ 2, 3, 5, 7, 14, 15 /)


   if (verbose) write(*,*) '<<<<<<<<<<<<<<< Entering setup_abi()'

   if (verbose) write(*,*) 'l1b_path_file: ', trim(l1b_path_file)
   if (verbose) write(*,*) 'geo_path_file: ', trim(geo_path_file)

   ! check if l1b and geo file are of the same granule
   index1 = index(l1b_path_file, '/', back=.true.)
   index2 = index(geo_path_file, '/', back=.true.)

   ! check if l1b and geo files identical
   if (trim(adjustl(l1b_path_file)) .ne. &
       trim(adjustl(geo_path_file))) then
      write(*,*)
      write(*,*) 'ERROR: setup_abi(): Geolocation and L1b files are ' // &
           'for different times'
      write(*,*) 'l1b_path_file: ', trim(adjustl(geo_path_file))
      write(*,*) 'geo_path_file: ', trim(adjustl(l1b_path_file))

      stop error_stop_code
   end if

   if (index(l1b_path_file, "G16_s") .gt. 0) then
      index3 = index(l1b_path_file, "G16_s") + 5
      platform = "GOES-16"
   else if(index(l1b_path_file, "G17_s") .gt. 0) then
      index3 = index(l1b_path_file, "G17_s") + 5
      platform = "GOES-17"
   else
      write(*,*) "Unsupported GOES platform, ", l1b_path_file
      stop
   end if

   if (verbose) write(*,*) "Satellite is: ", platform

   index2 = index(l1b_path_file, 'ABI-L1b-')

   ! get year, doy, hour and minute as strings
   cyear = trim(adjustl(l1b_path_file(index3:index3+3)))
   cdoy = trim(adjustl(l1b_path_file(index3+4:index3+6)))
   chour = trim(adjustl(l1b_path_file(index3+7:index3+8)))
   cminute = trim(adjustl(l1b_path_file(index3+9:index3+10)))
   read(cyear(1:len_trim(cyear)), '(I4)') year
   read(cdoy(1:len_trim(cdoy)), '(I3)') doy
   read(chour(1:len_trim(chour)), '(I2)') hour
   read(cminute(1:len_trim(cminute)), '(I2)') minute

   call DOY2GREG(doy, year, month, day)

   write(cmonth, '(i2.2)') month
   write(cday, '(i2.2)') day

   ! now set up the channels
   call common_setup(channel_info, channel_ids_user, channel_ids_default, &
      all_channel_wl_abs, all_channel_sw_flag, all_channel_lw_flag, &
      all_channel_ids_rttov_coef_sw, all_channel_ids_rttov_coef_lw, &
      all_map_ids_abs_to_ref_band_land, all_map_ids_abs_to_ref_band_sea, &
      all_map_ids_abs_to_snow_and_ice, all_map_ids_view_number, &
      all_channel_fractional_uncertainty, all_channel_minimum_uncertainty, &
      all_channel_numerical_uncertainty, all_channel_lnd_uncertainty, &
      all_channel_sea_uncertainty, all_nchannels_total)

   if (verbose) write(*,*) '>>>>>>>>>>>>>>> Leaving setup_abi()'

end subroutine setup_abi

subroutine setup_agri(l1b_path_file, geo_path_file, platform, year, month, day, &
                      doy, hour, minute, cyear, cmonth, cday, cdoy, chour, cminute, &
                      channel_ids_user, channel_info, verbose)

   use calender_m
   use channel_structures_m
   use preproc_constants_m
   use preproc_structures_m

   implicit none

   character(len=*),     intent(in)    :: l1b_path_file
   character(len=*),     intent(in)    :: geo_path_file
   character(len=*),     intent(out)   :: platform
   integer(kind=sint),   intent(out)   :: year, month, day, doy
   integer(kind=sint),   intent(out)   :: hour, minute
   character(len=*),     intent(out)   :: cyear, cmonth, cday
   character(len=*),     intent(out)   :: cdoy, chour, cminute
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

   ! Only this below needs to be set to change the desired default channels. All
   ! other channel related arrays/indexes are set automatically given the static
   ! instrument channel definition above.
   integer, parameter :: channel_ids_default(6) = (/ 2, 3, 5, 7, 11, 12 /)


   if (verbose) write(*,*) '<<<<<<<<<<<<<<< Entering setup_agri()'

   if (verbose) write(*,*) 'l1b_path_file: ', trim(l1b_path_file)
   if (verbose) write(*,*) 'geo_path_file: ', trim(geo_path_file)

   if (index(l1b_path_file, "FY4A") .gt. 0) then
      index3 = index(l1b_path_file, 'AGRI_FY4A_') + 10
      platform = "FY-4A"
   else if(index(l1b_path_file, "FY4B") .gt. 0) then
      index3 = index(l1b_path_file, 'AGRI_FY4A_') + 10
      platform = "FY-4B"
   else
      write(*,*) "Unsupported Fengyun platform, ", l1b_path_file
      stop
   end if

   if (verbose) write(*,*) "Satellite is: ", platform

   ! get year, doy, hour and minute as strings
   cyear = trim(adjustl(l1b_path_file(index3:index3+3)))
   cmonth = trim(adjustl(l1b_path_file(index3+4:index3+5)))
   cday = trim(adjustl(l1b_path_file(index3+6:index3+7)))
   chour = trim(adjustl(l1b_path_file(index3+8:index3+9)))
   cminute = trim(adjustl(l1b_path_file(index3+10:index3+11)))

   read(cyear(1:len_trim(cyear)), '(I4)') year
   read(cmonth(1:len_trim(cmonth)), '(I2)') month
   read(cday(1:len_trim(cday)), '(I2)') day
   read(chour(1:len_trim(chour)), '(I2)') hour
   read(cminute(1:len_trim(cminute)), '(I2)') minute

   call GREG2DOY(year, month, day, doy)
   write(cdoy, '(i3.3)') doy

   ! now set up the channels
   call common_setup(channel_info, channel_ids_user, channel_ids_default, &
        all_channel_wl_abs, all_channel_sw_flag, all_channel_lw_flag, &
        all_channel_ids_rttov_coef_sw, all_channel_ids_rttov_coef_lw, &
        all_map_ids_abs_to_ref_band_land, all_map_ids_abs_to_ref_band_sea, &
        all_map_ids_abs_to_snow_and_ice, all_map_ids_view_number, &
        all_channel_fractional_uncertainty, all_channel_minimum_uncertainty, &
        all_channel_numerical_uncertainty, all_channel_lnd_uncertainty, &
        all_channel_sea_uncertainty, all_nchannels_total)

   if (verbose) write(*,*) '>>>>>>>>>>>>>>> Leaving setup_agri()'

end subroutine setup_agri


subroutine setup_ahi(l1b_path_file, geo_path_file, platform, year, month, day, &
   doy, hour, minute, cyear, cmonth, cday, cdoy, chour, cminute, channel_ids_user, &
   channel_info, verbose)

   use calender_m
   use channel_structures_m
   use preproc_constants_m
   use preproc_structures_m

   implicit none

   character(len=*),     intent(in)    :: l1b_path_file
   character(len=*),     intent(in)    :: geo_path_file
   character(len=*),     intent(out)   :: platform
   integer(kind=sint),   intent(out)   :: year, month, day, doy
   integer(kind=sint),   intent(out)   :: hour, minute
   character(len=*),     intent(out)   :: cyear, cmonth, cday
   character(len=*),     intent(out)   :: cdoy, chour, cminute
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

   ! Only this below needs to be set to change the desired default channels. All
   ! other channel related arrays/indexes are set automatically given the static
   ! instrument channel definition above.
   integer, parameter :: channel_ids_default(6) = (/ 3, 4, 5, 7, 14, 15 /)


   if (verbose) write(*,*) '<<<<<<<<<<<<<<< Entering setup_himawari()'

   if (verbose) write(*,*) 'l1b_path_file: ', trim(l1b_path_file)
   if (verbose) write(*,*) 'geo_path_file: ', trim(geo_path_file)

   ! check if l1b and geo file are of the same granule
   index1 = index(l1b_path_file, '/', back=.true.)
   index2 = index(geo_path_file, '/', back=.true.)

   ! check if l1b and geo files identical
   if (trim(adjustl(l1b_path_file)) .ne. &
       trim(adjustl(geo_path_file))) then
      write(*,*)
      write(*,*) 'ERROR: setup_ahi(): Geolocation and L1b files are ' // &
           'for different times'
      write(*,*) 'l1b_path_file: ', trim(adjustl(geo_path_file))
      write(*,*) 'geo_path_file: ', trim(adjustl(l1b_path_file))

      stop error_stop_code
   end if
   if (index(l1b_path_file, "HS_H08") .gt. 0) then
      platform = "Himawari-8"
   else if (index(l1b_path_file, "HS_H09") .gt. 0) then
      platform = "Himawari-9"
   else
      write(*,*) "Unidentified Himawari variant: ", trim(l1b_path_file)
      stop
   end if
   if (verbose) write(*,*) "Satellite is: ", platform

   ! The code below extracts date/time info from the segment name.
   ! Note that it requires the segment name to be in the generic format
   ! that's specified by the JMA. Weird filenames will break things.

   index2 = index(l1b_path_file, 'HS_H')

   ! get year, doy, hour and minute as strings
   index2 = index2+7
   cyear = trim(adjustl(l1b_path_file(index2:index2+3)))
   cmonth = trim(adjustl(l1b_path_file(index2+4:index2+5)))
   cday = trim(adjustl(l1b_path_file(index2+6:index2+7)))
   chour = trim(adjustl(l1b_path_file(index2+9:index2+10)))
   cminute = trim(adjustl(l1b_path_file(index2+11:index2+12)))

   ! get year, doy, hour and minute as integers
   read(cyear(1:len_trim(cyear)), '(I4)') year
   read(cmonth(1:len_trim(cmonth)), '(I2)') month
   read(cday(1:len_trim(cday)), '(I2)') day
   read(chour(1:len_trim(chour)), '(I2)') hour
   read(cminute(1:len_trim(cminute)), '(I2)') minute

   call GREG2DOY(year, month, day, doy)
   write(cdoy, '(i3.3)') doy

   ! now set up the channels
   call common_setup(channel_info, channel_ids_user, channel_ids_default, &
      all_channel_wl_abs, all_channel_sw_flag, all_channel_lw_flag, &
      all_channel_ids_rttov_coef_sw, all_channel_ids_rttov_coef_lw, &
      all_map_ids_abs_to_ref_band_land, all_map_ids_abs_to_ref_band_sea, &
      all_map_ids_abs_to_snow_and_ice, all_map_ids_view_number, &
      all_channel_fractional_uncertainty, all_channel_minimum_uncertainty, &
      all_channel_numerical_uncertainty, all_channel_lnd_uncertainty, &
      all_channel_sea_uncertainty, all_nchannels_total)

   if (verbose) write(*,*) '>>>>>>>>>>>>>>> Leaving setup_ahi()'

end subroutine setup_ahi


subroutine setup_avhrr(l1b_path_file, geo_path_file, platform, year, month, day, &
   doy, hour, minute, cyear, cmonth, cday, cdoy, chour, cminute, channel_ids_user, &
   channel_info, verbose)

   use calender_m
   use channel_structures_m
   use preproc_constants_m
   use preproc_structures_m

   implicit none

   character(len=*),     intent(in)    :: l1b_path_file
   character(len=*),     intent(in)    :: geo_path_file
   character(len=*),     intent(out)   :: platform
   integer(kind=sint),   intent(out)   :: year, month, day, doy
   integer(kind=sint),   intent(out)   :: hour, minute
   character(len=*),     intent(out)   :: cyear, cmonth, cday
   character(len=*),     intent(out)   :: cdoy, chour, cminute
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

   ! Only this below needs to be set to change the desired default channels. All
   ! other channel related arrays/indexes are set automatically given the static
   ! instrument channel definition above.
   integer, parameter :: channel_ids_default(6) = (/ 1, 2, 3, 4, 5, 6 /)


   if (verbose) write(*,*) '<<<<<<<<<<<<<<< Entering setup_avhrr()'

   if (verbose) write(*,*) 'l1b_path_file: ', trim(l1b_path_file)
   if (verbose) write(*,*) 'geo_path_file: ', trim(geo_path_file)

   ! check if old/new avhrr filename
   i = index(l1b_path_file, '/', back=.true.)

   if (trim(adjustl(l1b_path_file(i+1:i+8))) .eq.'ECC_GAC_') then

      if (verbose) write(*,*) ' *** new avhrr input file'

      ! check if l1b and geo file are for the same orbit
      index1 = index(l1b_path_file, 'ECC_GAC_avhrr', back=.true.)
      index2 = index(geo_path_file, 'ECC_GAC_sunsatangles', back=.true.)

      if (trim(adjustl(l1b_path_file(1:index1-1))) .ne. &
           trim(adjustl(geo_path_file(1:index2-1)))) then
         write(*,*)
         write(*,*) 'ERROR: setup_avhrr(): Geolocation and L1b files are ' // &
              'for different orbits'
         write(*,*) 'l1b_path_file: ', trim(adjustl(geo_path_file))
         write(*,*) 'geo_path_file: ', trim(adjustl(l1b_path_file))

         stop error_stop_code
      end if

      str1 = l1b_path_file
      do k = 1, 4
         l = len_trim(str1)
         j = index(str1, '_', back=.true.)

         str2 = str1
         str1 = str1(1:j-1)
         str2 = str2(j+1:)

         if (k .eq. 2) then
            ! get year, month, day, hour and minute as strings
            cyear = trim(adjustl(str2(1:4)))
            cmonth = trim(adjustl(str2(5:6)))
            cday = trim(adjustl(str2(7:8)))
            chour = trim(adjustl(str2(10:11)))
            cminute = trim(adjustl(str2(12:13)))
         end if

         if (k .eq. 4) then
            ! which avhrr are we processing?
            platform = trim(adjustl(str2))
         end if
      end do

      ! get year, month, day, hour and minute as integers
      read(cyear(1:len_trim(cyear)), '(I4)') year
      read(cmonth, '(I2)') month
      read(cday, '(I2)') day
      read(chour(1:len_trim(chour)), '(I2)') hour
      read(cminute(1:len_trim(cminute)), '(I2)') minute

   else

      if (verbose) write(*,*) ' *** old avhrr input file'

      ! check if l1b and angles file are for the same orbit
      index1 = index(l1b_path_file, '_avhrr', back=.true.)
      index2 = index(geo_path_file, '_sunsatangles', back=.true.)

      if (trim(adjustl(l1b_path_file(1:index1-1))) .ne. &
           trim(adjustl(geo_path_file(1:index2-1)))) then
         write(*,*)
         write(*,*) 'ERROR: setup_avhrr(): Geolocation and L1b files are ' // &
              'for different orbits'
         write(*,*) 'l1b_path_file: ', trim(adjustl(geo_path_file))
         write(*,*) 'geo_path_file: ', trim(adjustl(l1b_path_file))

         stop error_stop_code
      end if

      str1 = l1b_path_file
      do k = 1, 7
         l = len_trim(str1)
         j = index(str1, '_', back=.true.)

         str2 = str1
         str1 = str1(1:j-1)
         str2 = str2(j+1:)

         if (k .eq. 6) then
            ! get hour and minute as strings
            chour = trim(adjustl(str2(1:2)))
            cminute = trim(adjustl(str2(3:4)))
         end if
         if (k .eq. 7) then
            ! get year, month, day as strings
            cyear = trim(adjustl(str2(1:4)))
            cmonth = trim(adjustl(str2(5:6)))
            cday = trim(adjustl(str2(7:8)))
         end if
      end do

      ! one last time for platform
      l = len_trim(str1)
      j = index(str1, '/', back=.true.)
      str2 = str1
      str1 = str1(1:j-1)
      str2 = str2(j+1:)
      platform = trim(adjustl(str2))

      ! get year, month, day, hour and minute as integers
      read(cyear(1:len_trim(cyear)), '(I4)') year
      read(cmonth, '(I2)') month
      read(cday, '(I2)') day
      read(chour(1:len_trim(chour)), '(I2)') hour
      read(cminute(1:len_trim(cminute)), '(I2)') minute
   end if

   ! added doy calculation for avhrr
   call GREG2DOY(year, month, day, doy)
   write(cdoy, '(i3.3)') doy

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
      all_channel_sea_uncertainty, all_nchannels_total)

   if (verbose) write(*,*) '>>>>>>>>>>>>>>> Leaving setup_avhrr()'

end subroutine setup_avhrr


subroutine setup_modis(l1b_path_file, geo_path_file, platform, year, month, day, &
   doy, hour, minute, cyear, cmonth, cday, cdoy, chour, cminute, channel_ids_user, &
   channel_info, verbose)

   use calender_m
   use channel_structures_m
   use preproc_constants_m
   use preproc_structures_m

   implicit none

   character(len=*),     intent(in)    :: l1b_path_file
   character(len=*),     intent(in)    :: geo_path_file
   character(len=*),     intent(out)   :: platform
   integer(kind=sint),   intent(out)   :: year, month, day, doy
   integer(kind=sint),   intent(out)   :: hour, minute
   character(len=*),     intent(out)   :: cyear, cmonth, cday
   character(len=*),     intent(out)   :: cdoy, chour, cminute
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

   ! Only this below needs to be set to change the desired default channels. All
   ! other channel related arrays/indexes are set automatically given the static
   ! instrument channel definition above.
   integer, parameter :: channel_ids_default(6) = (/ 1, 2, 6, 20, 31, 32 /)


   if (verbose) write(*,*) '<<<<<<<<<<<<<<< Entering setup_modis()'

   if (verbose) write(*,*) 'l1b_path_file: ', trim(l1b_path_file)
   if (verbose) write(*,*) 'geo_path_file: ', trim(geo_path_file)

   ! check if l1b and geo file are of the same granule
   index1 = index(l1b_path_file, '/', back=.true.)
   index2 = index(geo_path_file, '/', back=.true.)

   if (trim(adjustl(l1b_path_file(index1+10:index1+26))) .ne. &
       trim(adjustl(geo_path_file(index2+7:index2+23)))) then
      write(*,*)
      write(*,*) 'ERROR: setup_modis(): Geolocation and L1b files are for ' // &
           'different granules'
      write(*,*) 'l1b_path_file: ', trim(adjustl(geo_path_file))
      write(*,*) 'geo_path_file: ', trim(adjustl(l1b_path_file))

      stop error_stop_code
   end if

   ! which modis are we processing?
   index1 = index(l1b_path_file, '1KM.')
   if (trim(adjustl(l1b_path_file(index1-5:index1-3))) .eq. 'MYD') &
        platform = 'AQUA'
   if (trim(adjustl(l1b_path_file(index1-5:index1-3))) .eq. 'MOD') &
        platform = 'TERRA'

   ! get year, doy, hour and minute as strings
   cyear = trim(adjustl(l1b_path_file(index1+5:index1+8)))
   cdoy = trim(adjustl(l1b_path_file(index1+9:index1+11)))
   chour = trim(adjustl(l1b_path_file(index1+13:index1+14)))
   cminute = trim(adjustl(l1b_path_file(index1+15:index1+16)))

   ! get year, doy, hour and minute as integers
   read(cyear(1:len_trim(cyear)), '(I4)') year
   read(cdoy(1:len_trim(cdoy)), '(I3)') doy
   read(chour(1:len_trim(chour)), '(I2)') hour
   read(cminute(1:len_trim(cminute)), '(I2)') minute

   ! transform doy to date in year
   call DOY2GREG(doy, year, month, day)

   ! get month and day as text
   write(cmonth, '(i2.2)') month
   write(cday, '(i2.2)') day

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
      all_channel_sea_uncertainty, all_nchannels_total)

   if (verbose) write(*,*) '>>>>>>>>>>>>>>> Leaving setup_modis()'

end subroutine setup_modis


subroutine setup_seviri(l1b_path_file, geo_path_file, platform, year, month, day, &
   doy, hour, minute, cyear, cmonth, cday, cdoy, chour, cminute, channel_ids_user, &
   channel_info, verbose)

   use calender_m
   use channel_structures_m
   use preproc_constants_m
   use preproc_structures_m

   implicit none

   character(len=*),     intent(in)    :: l1b_path_file
   character(len=*),     intent(in)    :: geo_path_file
   character(len=*),     intent(out)   :: platform
   integer(kind=sint),   intent(out)   :: year, month, day, doy
   integer(kind=sint),   intent(out)   :: hour, minute
   character(len=*),     intent(out)   :: cyear, cmonth, cday
   character(len=*),     intent(out)   :: cdoy, chour, cminute
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

   ! Only this below needs to be set to change the desired default channels. All
   ! other channel related arrays/indexes are set automatically given the static
   ! instrument channel definition above.
   integer, parameter :: channel_ids_default(6) = (/ 1, 2, 3, 4, 9, 10 /)


   if (verbose) write(*,*) '<<<<<<<<<<<<<<< Entering setup_seviri()'

   if (verbose) write(*,*) 'l1b_path_file: ', trim(l1b_path_file)
   if (verbose) write(*,*) 'geo_path_file: ', trim(geo_path_file)

   ! check if l1b and geo file are of the same granule
   index1 = index(l1b_path_file, '/', back=.true.)
   index2 = index(geo_path_file, '/', back=.true.)

   ! check if l1b and geo files identical
   if (trim(adjustl(l1b_path_file)) .ne. &
       trim(adjustl(geo_path_file))) then
      write(*,*)
      write(*,*) 'ERROR: setup_seviri(): Geolocation and L1b files are for ' // &
           'different times'
      write(*,*) 'l1b_path_file: ', trim(adjustl(geo_path_file))
      write(*,*) 'geo_path_file: ', trim(adjustl(l1b_path_file))

      stop error_stop_code
   end if

   index1 = index(l1b_path_file, '.h5')

   if (index1 .ne. 0) then
      index2 = index(l1b_path_file, "/", back=.true.)
      index2 = index2 + index(l1b_path_file(index2 + 1:), '_')
      call determine_seviri_platform_from_metoffice(l1b_path_file, platform)
   else
      ! Check if file is HRIT or NAT.
      index1 = index(l1b_path_file, '.nat')

      ! which MSG are we processing?
      !
      ! MSG2-SEVI-MSG15-0100-NA-20100507144241.667000000Z-995378.nat
      ! H-000-MSG1__-MSG1________-_________-EPI______-200603031200-__
      !
      if (index1 .ne. 0) then
         index2 = index(l1b_path_file, '-')
         platform = l1b_path_file(index2-4:index2-1)
      else
         index2 = index(l1b_path_file, '__-')
         platform = l1b_path_file(index2+3:index2+6)
      end if
      index2 = index2 + index(l1b_path_file(index2 + 1:), '-')
      index2 = index2 + index(l1b_path_file(index2 + 1:), '-')
      index2 = index2 + index(l1b_path_file(index2 + 1:), '-')
      index2 = index2 + index(l1b_path_file(index2 + 1:), '-')
   end if

   ! get year, doy, hour and minute as strings
   cyear = trim(adjustl(l1b_path_file(index2+1:index2+4)))
   cmonth = trim(adjustl(l1b_path_file(index2+5:index2+6)))
   cday = trim(adjustl(l1b_path_file(index2+7:index2+8)))
   chour = trim(adjustl(l1b_path_file(index2+9:index2+10)))
   cminute = trim(adjustl(l1b_path_file(index2+11:index2+12)))

   ! get year, doy, hour and minute as integers
   read(cyear(1:len_trim(cyear)), '(I4)') year
   read(cmonth(1:len_trim(cmonth)), '(I2)') month
   read(cday(1:len_trim(cday)), '(I2)') day
   read(chour(1:len_trim(chour)), '(I2)') hour
   read(cminute(1:len_trim(cminute)), '(I2)') minute

   if (index1 .ne. 0) then
      ! Time in native filename is end of scan, which takes 12 mins. So subtract
      ! 12 to get the actual start time of the image.
      ! We cannot tell if the file is RSS or FDS. RSS files are 5 mins, not 15.
      ! This checks the default FDS times (corresponding to 00, 15, 30, 45 mins)
      ! If a match is found, assume it's FDS and subtract 12 mins
      ! If no match, assume it's RSS and subtract 4 mins
      if (minute .eq. 12 .or. minute .eq. 27 .or. &
          minute .eq. 42 .or. minute .eq. 57) then
          minute = minute - 12
      else
          minute = minute - 4
      end if
   end if

   call GREG2DOY(year, month, day, doy)
   write(cdoy, '(i3.3)') doy

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
      all_channel_sea_uncertainty, all_nchannels_total)

   if (verbose) write(*,*) '>>>>>>>>>>>>>>> Leaving setup_seviri()'

end subroutine setup_seviri


subroutine setup_slstr(l1b_path_file, geo_path_file, source_attributes, platform, &
     year, month, day, doy, hour, minute, cyear, cmonth, cday, cdoy, chour, cminute, &
     channel_ids_user, channel_info, verbose)

   use calender_m
   use channel_structures_m
   use preproc_constants_m
   use preproc_structures_m
   use orac_ncdf_m
   use source_attributes_m

   implicit none

   character(len=*),          intent(in)    :: l1b_path_file
   character(len=*),          intent(in)    :: geo_path_file
   type(source_attributes_t), intent(inout) :: source_attributes
   character(len=*),          intent(out)   :: platform
   integer(kind=sint),        intent(out)   :: year, month, day, doy
   integer(kind=sint),        intent(out)   :: hour, minute
   character(len=*),          intent(out)   :: cyear, cmonth, cday
   character(len=*),          intent(out)   :: cdoy, chour, cminute
   integer, pointer,          intent(in)    :: channel_ids_user(:)
   type(channel_info_t),      intent(inout) :: channel_info
   logical,                   intent(in)    :: verbose

   integer                    :: index2, second
   character(len=date_length) :: csecond

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

   ! Only this below needs to be set to change the desired default channels. All
   ! other channel related arrays/indexes are set automatically given the static
   ! instrument channel definition above.
   integer, parameter :: channel_ids_default(6) = (/ 2, 3, 5, 7, 8, 9 /)


   if (verbose) write(*,*) '<<<<<<<<<<<<<<< Entering setup_slstr()'

   if (verbose) write(*,*) 'l1b_path_file: ', trim(l1b_path_file)
   if (verbose) write(*,*) 'geo_path_file: ', trim(geo_path_file)

   ! check if l1b and geo file are of the same granule

   call ncdf_open(fid, l1b_path_file, 'setup_slstr()')
   ierr = nf90_get_att(fid, nf90_global, "start_time", l1b_start)
   if (ierr.ne.NF90_NOERR) then
      print*, 'ERROR: setup_slstr(): Error getting start_time from file ', trim(l1b_path_file)
      stop
   end if
   ! Extract level1 processing centre, processor version and absolute
   ! orbit number from l1b file
   ierr = nf90_get_att(fid, nf90_global, "institution", l1b_institute)
   if (ierr.ne.NF90_NOERR) then
      print*, 'ERROR: setup_slstr(): Error getting institution from file ', &
           trim(l1b_path_file)
      stop
   end if
   ierr = nf90_get_att(fid, nf90_global, "source", l1b_source)
   if (ierr.ne.NF90_NOERR) then
      print*, 'ERROR: setup_slstr(): Error getting source from file ', &
           trim(l1b_path_file)
      stop
   end if
   ierr = nf90_get_att(fid, nf90_global, "absolute_orbit_number", l1b_orbit_no)
   if (ierr.ne.NF90_NOERR) then
      print*, 'ERROR: setup_slstr(): Error getting absolute_orbit_number from file ', &
           trim(l1b_path_file)
      stop
   end if

   call ncdf_close(fid, 'setup_slstr(l1b_path_file)')

   call ncdf_open(fid, geo_path_file, 'setup_slstr()')
   ierr = nf90_get_att(fid, nf90_global, "start_time", geo_start)
   if (ierr.ne.NF90_NOERR) then
      print*, 'ERROR: setup_slstr(): Error getting start_time from file ', &
           trim(geo_path_file)
      stop
   end if
   call ncdf_close(fid, 'setup_slstr(geo_path_file)')
   if (trim(l1b_start).ne.trim(geo_start)) then
      print*, "ERROR: Start times for geo and image granules don't match: "
      write(*,*) trim(l1b_start)
      write(*,*) trim(geo_start)
      stop
   end if
   index2 = 1
   index2 = index(l1b_path_file, "S3A")
   if (index2 .gt. 1) then
      platform = "Sentinel3a"
   else
      index2 = index(l1b_path_file, "S3B")
      if (index2 .gt. 1) then
         platform = "Sentinel3b"
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
      write(*,*) "Satellite is: ", platform
      write(*,*) "Source attribute is: ", trim(source_attributes%level1b_version)
      write(*,*) "Orbit number is: ", trim(source_attributes%level1b_orbit_number)
   end if

   ! The code below extracts date/time info from the l1b start time.

   ! get year, doy, hour and minute as strings
   index2 = 1
   cyear = trim(adjustl(l1b_start(index2:index2+4)))
   cmonth = trim(adjustl(l1b_start(index2+5:index2+6)))
   cday = trim(adjustl(l1b_start(index2+8:index2+9)))
   chour = trim(adjustl(l1b_start(index2+11:index2+12)))
   cminute = trim(adjustl(l1b_start(index2+14:index2+15)))
   csecond = trim(adjustl(l1b_start(index2+17:index2+18)))

   ! get year, doy, hour and minute as integers
   read(cyear(1:len_trim(cyear)), '(I4)') year
   read(cmonth(1:len_trim(cmonth)), '(I2)') month
   read(cday(1:len_trim(cday)), '(I2)') day
   read(chour(1:len_trim(chour)), '(I2)') hour
   read(cminute(1:len_trim(cminute)), '(I2)') minute
   read(csecond(1:len_trim(csecond)), '(I2)') second
   if (second .ge. 30) then
      minute = minute+1
      if (minute .ge. 60) then
         minute = 0
         hour = hour+1
         if (hour .ge. 24) then
            hour = 0
            day = day+1
            write(cday, '(i0.2)') day
         end if
         write(chour, '(i0.2)') hour
      end if
      write(cminute, '(i0.2)') minute
   end if
   call GREG2DOY(year, month, day, doy)
   write(cdoy, '(i3.3)') doy

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
      all_channel_sea_uncertainty, all_nchannels_total)

   if (verbose) write(*,*) '>>>>>>>>>>>>>>> Leaving setup_slstr()'

end subroutine setup_slstr


subroutine setup_viirs_mband(l1b_path_file, geo_path_file, platform, year, month, day, &
   doy, hour, minute, cyear, cmonth, cday, cdoy, chour, cminute, channel_ids_user, &
   channel_info, verbose)

   use calender_m
   use channel_structures_m
   use preproc_constants_m
   use preproc_structures_m

   implicit none

   character(len=*),     intent(in)    :: l1b_path_file
   character(len=*),     intent(in)    :: geo_path_file
   character(len=*),     intent(out)   :: platform
   integer(kind=sint),   intent(out)   :: year, month, day, doy
   integer(kind=sint),   intent(out)   :: hour, minute
   character(len=*),     intent(out)   :: cyear, cmonth, cday
   character(len=*),     intent(out)   :: cdoy, chour, cminute
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

   ! Only this below needs to be set to change the desired default channels. All
   ! other channel related arrays/indexes are set automatically given the static
   ! instrument channel definition above.
   integer, parameter :: channel_ids_default(6) = (/ 5, 7, 10, 12, 15, 16 /)


   if (verbose) write(*,*) '<<<<<<<<<<<<<<< Entering setup_viirs_mband()'

   if (verbose) write(*,*) 'l1b_path_file: ', trim(l1b_path_file)
   if (verbose) write(*,*) 'geo_path_file: ', trim(geo_path_file)

   ! Assume Suomi-NPP by default
   platform = "SuomiNPP"
   ! check if l1b and geo file are of the same granule
   index1 = index(l1b_path_file, 'npp_d', .true.)
   index2 = index(geo_path_file, 'npp_d', .true.)

   if (index1 .le. 0) then
      index1 = index(l1b_path_file, 'j01_d', .true.)
      index2 = index(geo_path_file, 'j01_d', .true.)
      if (index1 .le. 0) then
         write(*,*)'ERROR: setup_viirs_iband(): Unsupported platform'
         stop error_stop_code
      end if
      platform = "NOAA20"
   end if


   l1b_dtstr = trim(adjustl(l1b_path_file(index2+5:index2+5+25)))
   geo_dtstr = trim(adjustl(geo_path_file(index2+5:index2+5+25)))

   ! check if l1b and geo files identical
   if (trim(adjustl(l1b_dtstr)) .ne. &
       trim(adjustl(geo_dtstr))) then
      write(*,*)
      write(*,*) 'ERROR: setup_viirs_mband(): Geolocation and L1b files are ' // &
           'for different times'
      write(*,*) 'l1b_path_file: ', trim(adjustl(geo_path_file))
      write(*,*) 'geo_path_file: ', trim(adjustl(l1b_path_file))

      stop error_stop_code
   end if

   if (verbose) write(*,*) "Satellite is: ", platform

   ! The code below extracts date/time info from the segment name.
   ! Note that it requires the segment name to be in the generic format
   ! that's specified by the NOAA. Weird filenames will break things.

   index2 = index(l1b_path_file, 'npp_d')

   ! get year, doy, hour and minute as strings
   index2 = index2+5
   cyear = trim(adjustl(l1b_path_file(index2:index2+4)))
   cmonth = trim(adjustl(l1b_path_file(index2+4:index2+5)))
   cday = trim(adjustl(l1b_path_file(index2+6:index2+7)))
   chour = trim(adjustl(l1b_path_file(index2+10:index2+11)))
   cminute = trim(adjustl(l1b_path_file(index2+12:index2+13)))

   ! get year, doy, hour and minute as integers
   read(cyear(1:len_trim(cyear)), '(I4)') year
   read(cmonth(1:len_trim(cmonth)), '(I2)') month
   read(cday(1:len_trim(cday)), '(I2)') day
   read(chour(1:len_trim(chour)), '(I2)') hour
   read(cminute(1:len_trim(cminute)), '(I2)') minute
   call GREG2DOY(year, month, day, doy)
   write(cdoy, '(i3.3)') doy

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
      all_channel_sea_uncertainty, all_nchannels_total)

   if (verbose) write(*,*) '>>>>>>>>>>>>>>> Leaving setup_viirs_mband()'

end subroutine setup_viirs_mband



subroutine setup_viirs_iband(l1b_path_file, geo_path_file, platform, year, month, day, &
   doy, hour, minute, cyear, cmonth, cday, cdoy, chour, cminute, channel_ids_user, &
   channel_info, verbose)

   use calender_m
   use channel_structures_m
   use preproc_constants_m
   use preproc_structures_m

   implicit none

   character(len=*),     intent(in)    :: l1b_path_file
   character(len=*),     intent(in)    :: geo_path_file
   character(len=*),     intent(out)   :: platform
   integer(kind=sint),   intent(out)   :: year, month, day, doy
   integer(kind=sint),   intent(out)   :: hour, minute
   character(len=*),     intent(out)   :: cyear, cmonth, cday
   character(len=*),     intent(out)   :: cdoy, chour, cminute
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

   ! Only this below needs to be set to change the desired default channels. All
   ! other channel related arrays/indexes are set automatically given the static
   ! instrument channel definition above.
   integer, parameter :: channel_ids_default(6) = (/ 1, 2, 3, 4, 5, 5 /)


   if (verbose) write(*,*) '<<<<<<<<<<<<<<< Entering setup_viirs_iband()'

   if (verbose) write(*,*) 'l1b_path_file: ', trim(l1b_path_file)
   if (verbose) write(*,*) 'geo_path_file: ', trim(geo_path_file)

   ! Assume Suomi-NPP by default
   platform = "SuomiNPP"
   ! check if l1b and geo file are of the same granule
   index1 = index(l1b_path_file, 'npp_d', .true.)
   index2 = index(geo_path_file, 'npp_d', .true.)

   if (index1 .le. 0) then
      index1 = index(l1b_path_file, 'j01_d', .true.)
      index2 = index(geo_path_file, 'j01_d', .true.)
      if (index1 .le. 0) then
         write(*,*)'ERROR: setup_viirs_iband(): Unsupported platform'
         stop error_stop_code
      end if
      platform = "NOAA20"
   end if


   l1b_dtstr = trim(adjustl(l1b_path_file(index2+5:index2+5+25)))
   geo_dtstr = trim(adjustl(geo_path_file(index2+5:index2+5+25)))

   ! check if l1b and geo files identical
   if (trim(adjustl(l1b_dtstr)) .ne. &
       trim(adjustl(geo_dtstr))) then
      write(*,*)
      write(*,*) 'ERROR: setup_viirs_iband(): Geolocation and L1b files are ' // &
           'for different times'
      write(*,*) 'l1b_path_file: ', trim(adjustl(geo_path_file))
      write(*,*) 'geo_path_file: ', trim(adjustl(l1b_path_file))

      stop error_stop_code
   end if

   if (verbose) write(*,*) "Satellite is: ", platform

   ! The code below extracts date/time info from the segment name.
   ! Note that it requires the segment name to be in the generic format
   ! that's specified by the NOAA. Weird filenames will break things.

   index2 = index(l1b_path_file, 'npp_d')

   if (index2 .le. 0) then
      index2 = index(l1b_path_file, 'j01_d', .true.)
      if (index2 .le. 0) then
         write(*,*)'ERROR: setup_viirs_iband(): Unsupported platform'
         stop error_stop_code
      end if
   end if

   ! get year, doy, hour and minute as strings
   index2 = index2+5
   cyear = trim(adjustl(l1b_path_file(index2:index2+4)))
   cmonth = trim(adjustl(l1b_path_file(index2+4:index2+5)))
   cday = trim(adjustl(l1b_path_file(index2+6:index2+7)))
   chour = trim(adjustl(l1b_path_file(index2+10:index2+11)))
   cminute = trim(adjustl(l1b_path_file(index2+12:index2+13)))

   ! get year, doy, hour and minute as integers
   read(cyear(1:len_trim(cyear)), '(I4)') year
   read(cmonth(1:len_trim(cmonth)), '(I2)') month
   read(cday(1:len_trim(cday)), '(I2)') day
   read(chour(1:len_trim(chour)), '(I2)') hour
   read(cminute(1:len_trim(cminute)), '(I2)') minute
   call GREG2DOY(year, month, day, doy)
   write(cdoy, '(i3.3)') doy

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
      all_channel_sea_uncertainty, all_nchannels_total)

   if (verbose) write(*,*) '>>>>>>>>>>>>>>> Leaving setup_viirs_iband()'

end subroutine setup_viirs_iband


subroutine common_setup(channel_info, channel_ids_user, channel_ids_default, &
   all_channel_wl_abs, all_channel_sw_flag, all_channel_lw_flag, &
   all_channel_ids_rttov_coef_sw, all_channel_ids_rttov_coef_lw, &
   all_map_ids_abs_to_ref_band_land, all_map_ids_abs_to_ref_band_sea, &
   all_map_ids_abs_to_snow_and_ice, all_map_ids_view_number, &
   all_channel_fractional_uncertainty, all_channel_minimum_uncertainty, &
   all_channel_numerical_uncertainty, all_channel_lnd_uncertainty, &
   all_channel_sea_uncertainty, all_nchannels_total)

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

end module setup_m
