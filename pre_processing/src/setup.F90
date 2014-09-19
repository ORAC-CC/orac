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
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

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

   integer(kind=sint)                            :: intdummy1,intdummy2

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

   !Get year, doy, hour and minute as integers
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

   !now set up the channels
   channel_info%nviews = 1
   channel_info%nchannels_total = 6
   call allocate_channel_info(channel_info)

   !numbering wrt instrument definition
   channel_info%channel_ids_instr = (/ 1, 2, 6, 20, 31, 32 /)
   !numbering wrt to increasing wl, starting at 1
   channel_info%channel_ids_abs = (/ 1, 2, 3, 4, 5, 6 /)
   !which are the wl of those channels (approximate)
   channel_info%channel_wl_abs = (/ 0.67, 0.87, 1.60, 3.70, 11.017, 12.032 /)
   channel_info%channel_view_ids = 1

   !which channels have sw/lw components
   channel_info%channel_sw_flag = (/ 1, 1, 1, 1, 0, 0 /)
   channel_info%nchannels_sw = 4
   channel_info%channel_lw_flag = (/ 0, 0, 0, 1, 1, 1 /)
   channel_info%nchannels_lw = 3
   !channel number wrt RTTOV coefficient file
   allocate(channel_info%channel_ids_rttov_coef_sw(channel_info%nchannels_sw))
   channel_info%channel_ids_rttov_coef_sw = (/ 1, 2, 6, 20 /)
   allocate(channel_info%channel_ids_rttov_coef_lw(channel_info%nchannels_lw))
   channel_info%channel_ids_rttov_coef_lw = (/ 1, 11, 12 /)

   if (verbose) write(*,*) '>>>>>>>>>>>>>>> Leaving setup_modis()'

end subroutine setup_modis


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

   integer(kind=sint)                            :: intdummy1,intdummy2

   if (verbose) write(*,*) '<<<<<<<<<<<<<<< Entering setup_avhrr()'

   if (verbose) write(*,*) 'l1b_path_file: ', trim(l1b_path_file)
   if (verbose) write(*,*) 'geo_path_file: ', trim(geo_path_file)

   !check if l1b and angles file are or the same orbit
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

   !which avhrr are we processing?
   intdummy1=index(trim(adjustl(l1b_path_file)),'.h5',back=.true.)
   intdummy2=index(trim(adjustl(l1b_path_file)),'/',back=.true.)
   platform=trim(adjustl(l1b_path_file(intdummy2+1:intdummy1-47)))

   !Get year, month,day,hour and minute as character
   cyear=trim(adjustl(l1b_path_file(intdummy1-45:intdummy1-42)))
   cmonth=trim(adjustl(l1b_path_file(intdummy1-41:intdummy1-40)))
   cday=trim(adjustl(l1b_path_file(intdummy1-39:intdummy1-38)))
   chour=trim(adjustl(l1b_path_file(intdummy1-36:intdummy1-35)))
   cminute=trim(adjustl(l1b_path_file(intdummy1-34:intdummy1-33)))

   !Get year, month, day, hour and minute as integers
   read(cyear(1:len_trim(cyear)), '(I4)') year
   read(cmonth,'(i2)') month
   read(cday,'(i2)') day
   read(chour(1:len_trim(chour)), '(I2)') hour
   read(cminute(1:len_trim(cminute)), '(I2)') minute

   !added doy calculation for avhrr
   call GREG2DOY(year,month,day,doy)
   write(cdoy, '(i3.3)') doy

   !now set up the channels
   channel_info%nviews = 1
   channel_info%nchannels_total = 6
   call allocate_channel_info(channel_info)

   !numbering wrt instrument definition
   !3=3B,6=3A (that's how it is in the HDF5 input file)
   channel_info%channel_ids_instr = (/ 1, 2, 3, 4, 5, 6 /)
   !numbering wrt to increasing wl, starting at 1
   !3=3A,4=3B
   channel_info%channel_ids_abs = (/ 1, 2, 3, 4, 5, 6 /)
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

   if (verbose) write(*,*) '>>>>>>>>>>>>>>> Leaving setup_avhrr()'

end subroutine setup_avhrr

!---------------------------------------------------------
!---------------------------------------------------------

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

   integer(kind=sint)                            :: intdummy1

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

   !Get year, month, day, hour and minute as integers
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
   ! numbering wrt to increasing wl, starting at 1 (0.67 micron)
   channel_info%channel_ids_abs = (/ 1, 2, 3, 4, 5, 6 /)
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

   if (verbose) write(*,*) '>>>>>>>>>>>>>>> Leaving setup_aatsr()'

end subroutine setup_aatsr

end module setup_instrument
