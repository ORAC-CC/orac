module setup_instrument

   implicit none

contains

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
! Name            Type In/Out/Both Description
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
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine setup_aatsr(l1b_path_file,geo_path_file,platform,year,month,day,doy, &
     hour,minute,cyear,cmonth,cday,cdoy,chour,cminute,channel_info,verbose)

   use calender
   use preproc_constants
   use preproc_structures
   use channel_structures

   implicit none

   character(len=path_length),     intent(in)    :: l1b_path_file
   character(len=path_length),     intent(in)    :: geo_path_file
   character(len=platform_length), intent(out)   :: platform
   integer(kind=sint),             intent(out)   :: year,month,day,doy
   integer(kind=sint),             intent(out)   :: hour,minute
   character(len=date_length),     intent(out)   :: cyear,cmonth,cday
   character(len=date_length),     intent(out)   :: cdoy,chour,cminute
   type(channel_info_s),           intent(inout) :: channel_info
   logical,                        intent(in)    :: verbose

   integer                                       :: intdummy1

   if (verbose) write(*,*) '<<<<<<<<<<<<<<< Entering setup_aatsr()'

   if (verbose) write(*,*) 'l1b_path_file: ', trim(l1b_path_file)
   if (verbose) write(*,*) 'geo_path_file: ', trim(geo_path_file)

   !check if l1b and angles files identical
   if (trim(adjustl(l1b_path_file)) .ne. &
       trim(adjustl(geo_path_file))) then
      write(*,*)
      write(*,*) 'ERROR: setup_avhrr(): Geolocation and L1b files are for ' // &
               & 'different granules'
      write(*,*) 'l1b_path_file: ', trim(adjustl(geo_path_file))
      write(*,*) 'geo_path_file: ', trim(adjustl(l1b_path_file))

      stop error_stop_code
   end if

   !which aatsr are we processing?
   intdummy1=index(trim(adjustl(l1b_path_file)),'.N1',back=.true.)
   platform='ENV'

   !Get year, month,day,hour and minute as character
   cyear=trim(adjustl(l1b_path_file(intdummy1-45:intdummy1-42)))
   cmonth=trim(adjustl(l1b_path_file(intdummy1-41:intdummy1-40)))
   cday=trim(adjustl(l1b_path_file(intdummy1-39:intdummy1-38)))
   chour=trim(adjustl(l1b_path_file(intdummy1-36:intdummy1-35)))
   cminute=trim(adjustl(l1b_path_file(intdummy1-34:intdummy1-33)))

   !get year, month, day, hour and minute as integers
   read(cyear(1:len_trim(cyear)), '(I4)') year
   read(cmonth,'(i2)') month
   read(cday,'(i2)') day
   read(chour(1:len_trim(chour)), '(I2)') hour
   read(cminute(1:len_trim(cminute)), '(I2)') minute

   call GREG2DOY(year, month, day, doy)
   write(cdoy, '(i3.3)') doy

   ! now set up the channels
   channel_info%nviews = 1
   channel_info%nchannels_total = 6
   call allocate_channel_info(channel_info)

   ! numbering wrt instrument definition
   ! AATSR has a 0.55 micron channel so instrument channel numbering starts at 2
   channel_info%channel_ids_instr = (/ 2, 3, 4, 5, 6, 7 /)
   !which are the wl of those channels (approximate)
   channel_info%channel_wl_abs = (/ 0.67, 0.87, 1.61, 3.74, 10.8, 12.0 /)
   channel_info%channel_view_ids = 1

   ! which channels have sw/lw components
   channel_info%channel_sw_flag = (/ 1, 1, 1, 1, 0, 0 /)
   channel_info%nchannels_sw = 4
   channel_info%channel_lw_flag = (/ 0, 0, 0, 1, 1, 1 /)
   channel_info%nchannels_lw = 3

   !channel number wrt RTTOV coefficient file (channels in ascending wavenumber)
   allocate(channel_info%channel_ids_rttov_coef_sw(channel_info%nchannels_sw))
   channel_info%channel_ids_rttov_coef_sw = (/ 5, 4, 3, 2 /)
   allocate(channel_info%channel_ids_rttov_coef_lw(channel_info%nchannels_lw))
   channel_info%channel_ids_rttov_coef_lw = (/ 3, 2, 1 /)

   channel_info%map_ids_abs_to_ref_band_land = (/ 1, 2, 6, 0 /)
   channel_info%map_ids_abs_to_ref_band_sea  = (/ 1, 2, 6, 20 /)

   if (verbose) write(*,*) '>>>>>>>>>>>>>>> Leaving setup_aatsr()'

end subroutine setup_aatsr


subroutine setup_avhrr(l1b_path_file,geo_path_file,platform,year,month,day,doy, &
     hour,minute,cyear,cmonth,cday,cdoy,chour,cminute,channel_info,verbose)

   use calender
   use preproc_constants
   use preproc_structures
   use channel_structures

   implicit none

   character(len=path_length),     intent(in)    :: l1b_path_file
   character(len=path_length),     intent(in)    :: geo_path_file
   character(len=platform_length), intent(out)   :: platform
   integer(kind=sint),             intent(out)   :: year,month,day,doy
   integer(kind=sint),             intent(out)   :: hour,minute
   character(len=date_length),     intent(out)   :: cyear,cmonth,cday
   character(len=date_length),     intent(out)   :: cdoy,chour,cminute
   type(channel_info_s),           intent(inout) :: channel_info
   logical,                        intent(in)    :: verbose
   integer                                       :: intdummy1,intdummy2
   integer                                       :: i, j, k, l
   character(len=path_length)                    :: strdummy1, strdummy2

   if (verbose) write(*,*) '<<<<<<<<<<<<<<< Entering setup_avhrr()'

   if (verbose) write(*,*) 'l1b_path_file: ', trim(l1b_path_file)
   if (verbose) write(*,*) 'geo_path_file: ', trim(geo_path_file)

   ! check if old/new avhrr filename
   i = index(trim(adjustl(l1b_path_file)),'/', back=.true.)

   if (trim(adjustl(l1b_path_file(i+1:i+8))) .eq.'ECC_GAC_') then

       if (verbose) write(*,*) ' *** new avhrr input file'

       !check if l1b and angles file are for the same orbit
       intdummy1=index(trim(adjustl(l1b_path_file)),'ECC_GAC_avhrr',back=.true.)
       intdummy2=index(trim(adjustl(geo_path_file)),'ECC_GAC_sunsatangles',back=.true.)

       if (trim(adjustl(l1b_path_file(1:intdummy1-1))) .ne. &
           trim(adjustl(geo_path_file(1:intdummy2-1)))) then
          write(*,*)
          write(*,*) 'ERROR: setup_avhrr(): Geolocation and L1b files are for ' // &
                   & 'different granules'
          write(*,*) 'l1b_path_file: ', trim(adjustl(geo_path_file))
          write(*,*) 'geo_path_file: ', trim(adjustl(l1b_path_file))

          stop error_stop_code
       end if

       strdummy1 = l1b_path_file
       do k=1, 4

           l=len(trim(strdummy1))
           j=index(strdummy1,'_', back=.true.)

           strdummy2=strdummy1
           strdummy1=strdummy1(1:j-1)
           strdummy2=strdummy2(j+1:)

           if (k .eq. 2) then
               !get year, month,day,hour and minute as character
               cyear=trim(adjustl(strdummy2(1:4)))
               cmonth=trim(adjustl(strdummy2(5:6)))
               cday=trim(adjustl(strdummy2(7:8)))
               chour=trim(adjustl(strdummy2(10:11)))
               cminute=trim(adjustl(strdummy2(12:13)))
           end if

           if (k .eq. 4) then
               !which avhrr are we processing?
               platform=trim(adjustl(strdummy2))
           end if

       enddo


       !get year, month, day, hour and minute as integers
       read(cyear(1:len_trim(cyear)), '(I4)') year
       read(cmonth,'(i2)') month
       read(cday,'(i2)') day
       read(chour(1:len_trim(chour)), '(I2)') hour
       read(cminute(1:len_trim(cminute)), '(I2)') minute

   else

       if (verbose) write(*,*) ' *** old avhrr input file'

       !check if l1b and angles file are for the same orbit
       intdummy1=index(trim(adjustl(l1b_path_file)),'_avhrr',back=.true.)
       intdummy2=index(trim(adjustl(geo_path_file)),'_sunsatangles',back=.true.)

       if (trim(adjustl(l1b_path_file(1:intdummy1-1))) .ne. &
           trim(adjustl(geo_path_file(1:intdummy2-1)))) then
          write(*,*)
          write(*,*) 'ERROR: setup_avhrr(): Geolocation and L1b files are for ' // &
                   & 'different granules'
          write(*,*) 'l1b_path_file: ', trim(adjustl(geo_path_file))
          write(*,*) 'geo_path_file: ', trim(adjustl(l1b_path_file))

          stop error_stop_code
       end if

       strdummy1 = l1b_path_file
       do k=1, 7

           l=len(trim(strdummy1))
           j=index(strdummy1,'_', back=.true.)

           strdummy2=strdummy1
           strdummy1=strdummy1(1:j-1)
           strdummy2=strdummy2(j+1:)

           if (k .eq. 6) then
               !get hour and minute as character
               chour=trim(adjustl(strdummy2(1:2)))
               cminute=trim(adjustl(strdummy2(3:4)))
           end if
           if (k .eq. 7) then
               !get year, month,day as character
               cyear=trim(adjustl(strdummy2(1:4)))
               cmonth=trim(adjustl(strdummy2(5:6)))
               cday=trim(adjustl(strdummy2(7:8)))
           end if

       enddo

       ! one last time for platform
       l=len(trim(strdummy1))
       j=index(strdummy1,'/', back=.true.)
       strdummy2=strdummy1
       strdummy1=strdummy1(1:j-1)
       strdummy2=strdummy2(j+1:)
       platform=trim(adjustl(strdummy2))

       !get year, month, day, hour and minute as integers
       read(cyear(1:len_trim(cyear)), '(I4)') year
       read(cmonth,'(i2)') month
       read(cday,'(i2)') day
       read(chour(1:len_trim(chour)), '(I2)') hour
       read(cminute(1:len_trim(cminute)), '(I2)') minute

   end if

   !added doy calculation for avhrr
   call GREG2DOY(year,month,day,doy)
   write(cdoy, '(i3.3)') doy

   !now set up the channels
   channel_info%nviews = 1
   channel_info%nchannels_total = 6
   call allocate_channel_info(channel_info)

   !numbering wrt instrument definition
   !3=3A,4=3B,5=4,6=5 (not how it is in the HDF5 input file but habit by now)
   channel_info%channel_ids_instr = (/ 1, 2, 3, 4, 5, 6 /)
   !which are the wl of those channels (approximate)
   channel_info%channel_wl_abs = (/ 0.63, 0.8625, 1.61, 3.74, 10.8, 12.0 /)
   channel_info%channel_view_ids = 1

   !which channels have sw/lw components
   channel_info%channel_sw_flag = (/ 1, 1, 1, 1, 0, 0 /)
   channel_info%nchannels_sw = 4
   channel_info%channel_lw_flag = (/ 0, 0, 0, 1, 1, 1 /)
   channel_info%nchannels_lw = 3

   !channel number wrt RTTOV coefficient file
   allocate(channel_info%channel_ids_rttov_coef_sw(channel_info%nchannels_sw))
   channel_info%channel_ids_rttov_coef_sw = (/ 1, 2, 3, 4 /)
   allocate(channel_info%channel_ids_rttov_coef_lw(channel_info%nchannels_lw))
   channel_info%channel_ids_rttov_coef_lw = (/ 1, 2, 3 /)

   channel_info%map_ids_abs_to_ref_band_land = (/ 1, 2, 6, 0 /)
   channel_info%map_ids_abs_to_ref_band_sea  = (/ 1, 2, 6, 20 /)

   if (verbose) write(*,*) '>>>>>>>>>>>>>>> Leaving setup_avhrr()'

end subroutine setup_avhrr


subroutine setup_modis(l1b_path_file,geo_path_file,platform,year,month,day,doy, &
     hour,minute,cyear,cmonth,cday,cdoy,chour,cminute,channel_info,verbose)

   use calender
   use channel_structures
   use date_type_structures
   use preproc_constants
   use preproc_structures

   implicit none

   character(len=path_length),     intent(in)    :: l1b_path_file
   character(len=path_length),     intent(in)    :: geo_path_file
   character(len=platform_length), intent(out)   :: platform
   integer(kind=sint),             intent(out)   :: year,month,day,doy
   integer(kind=sint),             intent(out)   :: hour,minute
   character(len=date_length),     intent(out)   :: cyear,cmonth,cday
   character(len=date_length),     intent(out)   :: cdoy,chour,cminute
   type(channel_info_s),           intent(inout) :: channel_info
   logical,                        intent(in)    :: verbose

   integer                                       :: intdummy1,intdummy2

   ! Static instrument channel definitions. (These should not be changed.)
   integer, parameter :: all_nchannels_total                                   = 36
                                                                                  ! 1,         2,         3,         4,         5,         6,         7,         8,         9,         10,        11,        12,        13,        14,        15,        16,        17,        18,        19,        20,        21,        22,        23,        24,        25,        26,        27,        28,        29,        30,        31,        32,        33,        34,        35,        36
   real,    parameter :: all_channel_wl_abs (all_nchannels_total)            &
        &  = (/ 0.67,      0.87,      4.690e-01, 5.550e-01, 1.240e+00, 1.60, &
        &     2.130e+00, 4.125e-01, 4.430e-01, 4.880e-01, 5.310e-01, 5.510e&
        &-01, 6.670e-01, 6.780e-01, 7.480e-01, 8.695e-01, 9.050e-01, 9.360e&
        &-01, 9.400e-01, 3.70,      3.959e+00, 3.959e+00, 4.050e+00, 4.466e&
        &+00, 4.516e+00, 1.375e+00, 6.715e+00, 7.325e+00, 8.550e+00, 9.730e&
        &+00, 11.017,    12.032,    1.334e+01, 1.363e+01, 1.394e+01, 1.423e&
        &+01 /)

!  real,    parameter :: all_channel_wl_abs (all_nchannels_total)              = (/ 6.450e-01, 8.585e-01, 4.690e-01, 5.550e-01, 1.240e+00, 1.640e+00, 2.130e+00, 4.125e-01, 4.430e-01, 4.880e-01, 5.310e-01, 5.510e-01, 6.670e-01, 6.780e-01, 7.480e-01, 8.695e-01, 9.050e-01, 9.360e-01, 9.400e-01, 3.750e+00, 3.959e+00, 3.959e+00, 4.050e+00, 4.466e+00, 4.516e+00, 1.375e+00, 6.715e+00, 7.325e+00, 8.550e+00, 9.730e+00, 1.103e+01, 1.202e+01, 1.334e+01, 1.363e+01, 1.394e+01, 1.423e+01 /)
   integer, parameter :: all_channel_sw_flag(all_nchannels_total)            &
        &  = (/ 1,         1,         1,         1,         1,         1,    &
        &     1,         1,         1,         1,         1,         1,      &
        &   1,         1,         1,         1,         1,         1,        &
        & 1,         1,         1,         1,         1,         0,         0&
        &,         1,         0,         0,         0,         0,         0, &
        &        0,         0,         0,         0,         0  /)

   integer, parameter :: all_channel_lw_flag(all_nchannels_total)            &
        &  = (/ 0,         0,         0,         0,         0,         0,    &
        &     0,         0,         0,         0,         0,         0,      &
        &   0,         0,         0,         0,         0,         0,        &
        & 0,         1,         1,         1,         1,         1,         1&
        &,         0,         1,         1,         1,         1,         1, &
        &        1,         1,         1,         1,         1  /)

   integer, parameter :: all_channel_ids_rttov_coef_sw(all_nchannels_total)  &
        &  = (/ 1,         2,         3,         4,         5,         6,    &
        &     7,         8,         9,         10,        11,        12,     &
        &   13,        14,        15,        16,        17,        18,       &
        & 19,        20,        21,        22,        23,        0,         0&
        &,         24,        0,         0,         0,         0,         0, &
        &        0,         0,         0,         0,         0  /)

   integer, parameter :: all_channel_ids_rttov_coef_lw(all_nchannels_total)  &
        &  = (/ 0,         0,         0,         0,         0,         0,    &
        &     0,         0,         0,         0,         0,         0,      &
        &   0,         0,         0,         0,         0,         0,        &
        & 0,         1,         2,         3,         4,         5,         6&
        &,         0,         7,         8,         9,         10,        11,&
        &        12,        13,        14,        15,        16 /)

   integer, parameter ::&
        & all_map_ids_abs_to_ref_band_land(all_nchannels_total) = (/ 1,      &
        &   2,         0,         0,         0,         6,         0,        &
        & 0,         0,         0,         0,         0,         0,         0&
        &,         0,         0,         0,         0,         0,         0, &
        &        0,         0,         0,         0,         0,         0,   &
        &      0,         0,         0,         0,         0,         0,     &
        &    0,         0,         0,         0 /)

   integer, parameter :: all_map_ids_abs_to_ref_band_sea&
        & (all_nchannels_total) = (/ 1,         2,         0,         0,     &
        &    0,         6,         0,         0,         0,         0,       &
        &  0,         0,         0,         0,         0,         0,         &
        & 0,         0,         0,         20,        0,         0,         0&
        &,         0,         0,         0,         0,         0,         0, &
        &        0,         0,         0,         0,         0,         0,   &
        &      0 /)


   ! Only these need to be set to change the desired channels. All other channel
   ! related arrays/indexes are set automatically given the static instrument
   ! channel definition above.

   ! The legacy channels
   integer, parameter :: nchannels_total = 6
   integer, parameter :: channel_ids_instr(nchannels_total) = &
      (/ 1, 2, 6, 20, 31, 32 /)

   ! All currently supported channels
!  integer, parameter :: nchannels_total = 15
!  integer, parameter :: channel_ids_instr(nchannels_total) = &
!    (/ 1, 2, 6, 20, 24, 25, 27, 28, 29, 30, 31, 32, 33, 35, 36 /)

   if (verbose) write(*,*) '<<<<<<<<<<<<<<< Entering setup_modis()'

   if (verbose) write(*,*) 'l1b_path_file: ', trim(l1b_path_file)
   if (verbose) write(*,*) 'geo_path_file: ', trim(geo_path_file)

   !check if l1b and geo file are of the same granule
   intdummy1=index(trim(adjustl(l1b_path_file)),'/',back=.true.)
   intdummy2=index(trim(adjustl(geo_path_file)),'/',back=.true.)

   if (trim(adjustl(l1b_path_file(intdummy1+10:intdummy1+26))) .ne. &
       trim(adjustl(geo_path_file(intdummy2+7:intdummy2+23)))) then
      write(*,*)
      write(*,*) 'ERROR: setup_modis(): Geolocation and L1b files are for ' // &
               & 'different granules'
      write(*,*) 'l1b_path_file: ', trim(adjustl(geo_path_file))
      write(*,*) 'geo_path_file: ', trim(adjustl(l1b_path_file))

      stop error_stop_code
   end if

   !which modis are we processing?
   intdummy1=index(trim(adjustl(l1b_path_file)),'1KM.')
   if (trim(adjustl(l1b_path_file(intdummy1-5:intdummy1-3))) .eq. 'MYD') &
        platform='AQUA'
   if (trim(adjustl(l1b_path_file(intdummy1-5:intdummy1-3))) .eq. 'MOD') &
        platform='TERRA'

   !get year, doy, hour and minute as integers
   cyear=trim(adjustl(l1b_path_file(intdummy1+5:intdummy1+8)))
   cdoy=trim(adjustl(l1b_path_file(intdummy1+9:intdummy1+11)))
   chour=trim(adjustl(l1b_path_file(intdummy1+13:intdummy1+14)))
   cminute=trim(adjustl(l1b_path_file(intdummy1+15:intdummy1+16)))

   read(cdoy(1:len_trim(cdoy)), '(I3)') doy
   read(cyear(1:len_trim(cyear)), '(I4)') year
   read(chour(1:len_trim(chour)), '(I2)') hour
   read(cminute(1:len_trim(cminute)), '(I2)') minute

   !transform doy to date in year
   call DOY2GREG(doy,year,month,day)

   !get month and day as text
   write(cmonth, '(i2.2)') month
   write(cday, '(i2.2)') day

   channel_info%nviews = 1

   !now set up the channels
   channel_info%nchannels_total = nchannels_total

   call allocate_channel_info(channel_info)

   channel_info%channel_ids_instr = channel_ids_instr

   call common_setup(channel_info, all_nchannels_total, all_channel_wl_abs, &
      all_channel_sw_flag, all_channel_lw_flag, all_channel_ids_rttov_coef_sw, &
      all_channel_ids_rttov_coef_lw, all_map_ids_abs_to_ref_band_land, &
      all_map_ids_abs_to_ref_band_sea)

   if (verbose) write(*,*) '>>>>>>>>>>>>>>> Leaving setup_modis()'

end subroutine setup_modis


subroutine common_setup(channel_info, all_nchannels_total, all_channel_wl_abs, &
      all_channel_sw_flag, all_channel_lw_flag, all_channel_ids_rttov_coef_sw, &
      all_channel_ids_rttov_coef_lw, all_map_ids_abs_to_ref_band_land, &
      all_map_ids_abs_to_ref_band_sea)

   use channel_structures

   implicit none

   type(channel_info_s), intent(inout) :: channel_info
   integer,              intent(in)    :: all_nchannels_total
   real,                 intent(in)    :: all_channel_wl_abs (:)
   integer,              intent(in)    :: all_channel_sw_flag(:)
   integer,              intent(in)    :: all_channel_lw_flag(:)
   integer,              intent(in)    :: all_channel_ids_rttov_coef_sw(:)
   integer,              intent(in)    :: all_channel_ids_rttov_coef_lw(:)
   integer,              intent(in)    :: all_map_ids_abs_to_ref_band_land(:)
   integer,              intent(in)    :: all_map_ids_abs_to_ref_band_sea(:)

   integer :: i
   integer :: i_sw
   integer :: i_lw

   do i = 1, channel_info%nchannels_total
      channel_info%channel_wl_abs (i) = &
         all_channel_wl_abs (channel_info%channel_ids_instr(i))

      channel_info%channel_sw_flag(i) = &
         all_channel_sw_flag(channel_info%channel_ids_instr(i))
      channel_info%channel_lw_flag(i) = &
         all_channel_lw_flag(channel_info%channel_ids_instr(i))
   enddo

   channel_info%channel_view_ids = 1

   channel_info%nchannels_sw = &
      sum(channel_info%channel_sw_flag(1:channel_info%nchannels_total))
   allocate(channel_info%channel_ids_rttov_coef_sw(channel_info%nchannels_sw))
   channel_info%channel_ids_rttov_coef_sw = 0

   channel_info%nchannels_lw = &
      sum(channel_info%channel_lw_flag(1:channel_info%nchannels_total))
   allocate(channel_info%channel_ids_rttov_coef_lw(channel_info%nchannels_lw))
   channel_info%channel_ids_rttov_coef_lw = 0

   i_sw = 1
   i_lw = 1
   do i = 1, channel_info%nchannels_total
      if (channel_info%channel_sw_flag(i) .ne. 0) then
         channel_info%channel_ids_rttov_coef_sw(i_sw) = &
            all_channel_ids_rttov_coef_sw(channel_info%channel_ids_instr(i))

         channel_info%map_ids_abs_to_ref_band_land(i_sw) = &
            all_map_ids_abs_to_ref_band_land(channel_info%channel_ids_instr(i))

         channel_info%map_ids_abs_to_ref_band_sea (i_sw) = &
            all_map_ids_abs_to_ref_band_sea (channel_info%channel_ids_instr(i))

         i_sw = i_sw + 1
      endif
      if (channel_info%channel_lw_flag(i) .ne. 0) then
         channel_info%channel_ids_rttov_coef_lw(i_lw) = &
            all_channel_ids_rttov_coef_lw(channel_info%channel_ids_instr(i))
         i_lw = i_lw + 1
      endif
   enddo

end subroutine common_setup

end module setup_instrument
