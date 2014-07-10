module setup_instrument

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
! Name            Type    In/Out/Both Description
! ------------------------------------------------------------------------------
! path_to_l1b_file string in   Full path to level 1B data
! path_to_geo_file string in   Full path to geolocation data
! platform         string both Name of satellite
! doy              stint  out  Day of year (1-366)
! year             stint  out  Year
! month            stint  out  Month of year (1-12)
! day              stint  out  Day of month (1-31)
! hour             stint  out  Hour of day (0-59)
! minute           stint  out  Minute of hour (0-59)
! cyear            string out  Year, as a 4 character string
! cmonth           string out  Month of year, as a 2 character string
! cday             string out  Day of month, as a 2 character string
! chour            string out  Hour of day, as a 2 character string
! cminute          string out  Minute of hour, as a 2 character string
! channel_info     struct both Structure summarising the channels to be
!                              processed
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
!
! $Id$
!
! Bugs:
! none known
!-------------------------------------------------------------------------------

subroutine setup_modis(path_to_l1b_file,path_to_geo_file,platform,doy, &
     year,month,day,hour,minute,cyear,cmonth,cday,chour,cminute,channel_info)

   use calender
   use preproc_constants
   use preproc_structures
   use date_type_structures
   use channel_structures

   implicit none

   character(len=pathlength),     intent(in)  :: path_to_l1b_file
   character(len=pathlength),     intent(in)  :: path_to_geo_file
   character(len=platformlength), intent(out) :: platform
   integer(kind=stint),           intent(out) :: doy,year,month,day,hour,minute
   character(len=datelength),     intent(out) :: cyear,chour,cminute,cmonth,cday
   type(channel_info_s),          intent(inout) :: channel_info

   character(len=datelength)                  :: cdoy
   integer(kind=stint)                        :: intdummy1,intdummy2

   !check if l1b and geo file are of the same granule
   intdummy1=index(trim(adjustl(path_to_l1b_file)),'/',back=.true.)
   intdummy2=index(trim(adjustl(path_to_geo_file)),'/',back=.true.)

   if(trim(adjustl(path_to_l1b_file(intdummy1+10:intdummy1+26))) .ne. &
        & trim(adjustl(path_to_geo_file(intdummy2+7:intdummy2+23)))) then
      write(*,*)
      write(*,*) 'Geolocation and L1b files are for different granules!!!'
      write(*,*) trim(adjustl(path_to_geo_file))
      write(*,*) trim(adjustl(path_to_l1b_file))

      stop
   endif

   !which modis are we processing?
   intdummy1=index(trim(adjustl(path_to_l1b_file)),'1KM.')
   if(trim(adjustl(path_to_l1b_file(intdummy1-5:intdummy1-3))) .eq. 'MYD') &
        platform='AQUA'
   if(trim(adjustl(path_to_l1b_file(intdummy1-5:intdummy1-3))) .eq. 'MOD') &
        platform='TERRA'

   !Get year and doy,hour and minute as integers
   cyear=trim(adjustl(path_to_l1b_file(intdummy1+5:intdummy1+8)))
   cdoy=trim(adjustl(path_to_l1b_file(intdummy1+9:intdummy1+11)))
   chour=trim(adjustl(path_to_l1b_file(intdummy1+13:intdummy1+14)))
   cminute=trim(adjustl(path_to_l1b_file(intdummy1+15:intdummy1+16)))

   read(cdoy(1:len_trim(cdoy)), '(I3)') doy
   read(cyear(1:len_trim(cyear)), '(I4)') year
   read(chour(1:len_trim(chour)), '(I2)') hour
   read(cminute(1:len_trim(cminute)), '(I2)') minute

   !tranform doy to date in year
   call DOY2GREG(doy,year,month,day)

   !get month and day as text
   write(cmonth,'(i2)') month
   if(month .lt. 10) cmonth='0'//trim(adjustl(cmonth))
   write(cday,'(i2)') day
   if(day .lt. 10) cday='0'//trim(adjustl(cday))

   !now set up the channels
   !numbering wrt instrument definition
   channel_info%channel_ids_instr= (/ 1, 2, 6, 20, 31, 32 /)
   !numbering wrt to increasing wl, starting at 1
   channel_info%channel_ids_abs= (/ 1, 2, 3, 4, 5, 6 /)     

   !which channels have sw/lw components
   channel_info%channel_sw_flag=(/ 1, 1, 1, 1, 0, 0 /)
   channel_info%channel_lw_flag=(/ 0, 0, 0, 1, 1, 1 /)
   channel_info%nchannels_sw=sum(channel_info%channel_sw_flag)
   allocate(channel_info%channel_ids_rttov_coef_sw(channel_info%nchannels_sw))
   channel_info%nchannels_lw=sum(channel_info%channel_lw_flag)
   allocate(channel_info%channel_ids_rttov_coef_lw(channel_info%nchannels_lw))

   !process everything
   channel_info%channel_proc_flag=(/ 1, 1, 1, 1, 1, 1 /)
   !which are the wl of those channels (approximate)
   channel_info%channel_wl_abs=(/ 0.67, 0.87, 1.60, 3.70, 11.017, 12.032 /)
   !channel number wrt RTTOV coefficient file
   channel_info%channel_ids_rttov_coef_sw=(/ 1, 2, 6, 20 /)
   channel_info%channel_ids_rttov_coef_lw=(/ 1, 11, 12 /)

end subroutine setup_modis

!---------------------------------------------------------
!---------------------------------------------------------

subroutine setup_avhrr(path_to_l1b_file,path_to_geo_file,platform,doy, &
     year,month,day,hour,minute,cyear,cmonth,cday,chour,cminute,channel_info)

   use calender
   use preproc_constants
   use preproc_structures
   use channel_structures

   implicit none

   character(len=pathlength),     intent(in)  :: path_to_l1b_file
   character(len=pathlength),     intent(in)  :: path_to_geo_file
   character(len=platformlength), intent(out) :: platform
   integer(kind=stint),           intent(out) :: doy,year,month,day,hour,minute
   character(len=datelength),     intent(out) :: cyear,chour,cminute,cmonth,cday
   type(channel_info_s),          intent(inout) :: channel_info

   integer(kind=stint)                        :: intdummy1,intdummy2

   !check if l1b and angles file are or the same orbit
   intdummy1=index(trim(adjustl(path_to_l1b_file)),'_avhrr',back=.true.)
   intdummy2=index(trim(adjustl(path_to_geo_file)),'_sunsatangles',back=.true.)
   if(trim(adjustl(path_to_l1b_file(1:intdummy1-1))) .ne. &
        & trim(adjustl(path_to_geo_file(1:intdummy2-1)))) then
      write(*,*)
      write(*,*) 'Geolocation and L1b files are for different granules!!!'
      write(*,*) trim(adjustl(path_to_geo_file))
      write(*,*) trim(adjustl(path_to_l1b_file))

      stop
   endif

   !which avhrr are we processing?
   intdummy1=index(trim(adjustl(path_to_l1b_file)),'.h5',back=.true.)
   platform=trim(adjustl(path_to_l1b_file(intdummy1-52:intdummy1-47)))

   !Get year, month,day,hour and minute as character
   cyear=trim(adjustl(path_to_l1b_file(intdummy1-45:intdummy1-42)))
   cmonth=trim(adjustl(path_to_l1b_file(intdummy1-41:intdummy1-40)))
   cday=trim(adjustl(path_to_l1b_file(intdummy1-39:intdummy1-38)))
   chour=trim(adjustl(path_to_l1b_file(intdummy1-36:intdummy1-35)))
   cminute=trim(adjustl(path_to_l1b_file(intdummy1-34:intdummy1-33)))

   !Get year, month,day,hour and minute as integers
   read(cday,'(i2)') day
   read(cmonth,'(i2)') month
   read(cyear(1:len_trim(cyear)), '(I4)') year
   read(chour(1:len_trim(chour)), '(I2)') hour
   read(cminute(1:len_trim(cminute)), '(I2)') minute

   !added doy calculation for avhrr
   call GREG2DOY(year,month,day,doy)

   !now set up the channels (AP: Strictly, this definition does nothing.)
   !numbering wrt instrument definition
   !3=3B,6=3A (that's how it is in the HDF5 input file)
   channel_info%channel_ids_instr= (/ 1, 2, 3, 4, 5, 6 /)
   !numbering wrt to increasing wl, starting at 1
   !3=3A,4=3B
   channel_info%channel_ids_abs= (/ 1, 2, 3, 4, 5, 6 /)     
   !which channels have sw/lw components
   channel_info%channel_sw_flag=(/ 1, 1, 1, 1, 0, 0 /)
   channel_info%channel_lw_flag=(/ 0, 0, 0, 1, 1, 1 /)
   channel_info%nchannels_sw=sum(channel_info%channel_sw_flag)
   allocate(channel_info%channel_ids_rttov_coef_sw(channel_info%nchannels_sw))
   channel_info%nchannels_lw=sum(channel_info%channel_lw_flag)
   allocate(channel_info%channel_ids_rttov_coef_lw(channel_info%nchannels_lw))
   !process everything
   channel_info%channel_proc_flag=(/ 1, 1, 1, 1, 1, 1 /)
   !which are the wl of those channels (approximate)
   channel_info%channel_wl_abs=(/ 0.63, 0.8625, 1.61, 3.74, 10.8, 12.0 /)
   !channel number wrt RTTOV coefficient file
   channel_info%channel_ids_rttov_coef_sw=(/ 1, 2, 3, 4 /)
   channel_info%channel_ids_rttov_coef_lw=(/ 1, 2, 3 /)

end subroutine setup_avhrr

!---------------------------------------------------------
!---------------------------------------------------------

subroutine setup_aatsr(path_to_l1b_file,path_to_geo_file,platform,doy, &
     year,month,day,hour,minute,cyear,cmonth,cday,chour,cminute,channel_info)

   use calender
   use preproc_constants
   use preproc_structures
   use channel_structures

   implicit none

   character(len=pathlength),     intent(in)  :: path_to_l1b_file
   character(len=pathlength),     intent(in)  :: path_to_geo_file
   character(len=platformlength), intent(out) :: platform
   integer(kind=stint),           intent(out) :: doy,year,month,day,hour,minute
   character(len=datelength),     intent(out) :: cyear,chour,cminute,cmonth,cday
   type(channel_info_s),          intent(inout) :: channel_info

   integer(kind=stint)                        :: intdummy1

   !check if l1b and angles files identical
   if(trim(adjustl(path_to_l1b_file)) .ne. trim(adjustl(path_to_geo_file))) then
      write(*,*)
      write(*,*) 'Geolocation and L1b files are for different granules!!!'
      write(*,*) trim(adjustl(path_to_geo_file))
      write(*,*) trim(adjustl(path_to_l1b_file))

      stop
   endif

   !which aatsr are we processing?
   intdummy1=index(trim(adjustl(path_to_l1b_file)),'.N1',back=.true.)
   platform='ENV'
   write(*,*)'L1B file: ',trim(adjustl(path_to_l1b_file))
   write(*,*)'Geo file: ',trim(adjustl(path_to_geo_file))

   !Get year, month,day,hour and minute as character
   cyear=trim(adjustl(path_to_l1b_file(intdummy1-45:intdummy1-42)))
   cmonth=trim(adjustl(path_to_l1b_file(intdummy1-41:intdummy1-40)))
   cday=trim(adjustl(path_to_l1b_file(intdummy1-39:intdummy1-38)))
   chour=trim(adjustl(path_to_l1b_file(intdummy1-36:intdummy1-35)))
   cminute=trim(adjustl(path_to_l1b_file(intdummy1-34:intdummy1-33)))

   !Get year, month,day,hour and minute as integers
   read(cday,'(i2)') day
   read(cmonth,'(i2)') month
   read(cyear(1:len_trim(cyear)), '(I4)') year
   read(chour(1:len_trim(chour)), '(I2)') hour
   read(cminute(1:len_trim(cminute)), '(I2)') minute

   call GREG2DOY(year, month, day, DOY)

   ! now set up the channels
   ! Number of viewing geometries: AATSR of course has two, but initially
   ! we'll pretend it only has one.
   channel_info%nviews = 1
   ! numbering wrt instrument definition
   ! AATSR has a 0.55 micron channel so instrument channel numbering starts at 2
   channel_info%channel_ids_instr= (/ 2, 3, 4, 5, 6, 7 /)
   ! numbering wrt to increasing wl, starting at 1 (0.67 micron)
   channel_info%channel_ids_abs= (/ 1, 2, 3, 4, 5, 6 /)     
   ! which channels have sw/lw components
   channel_info%channel_sw_flag=(/ 1, 1, 1, 1, 0, 0 /)
   channel_info%channel_lw_flag=(/ 0, 0, 0, 1, 1, 1 /)
   channel_info%nchannels_sw=sum(channel_info%channel_sw_flag)
   allocate(channel_info%channel_ids_rttov_coef_sw(channel_info%nchannels_sw))
   channel_info%nchannels_lw=sum(channel_info%channel_lw_flag)
   allocate(channel_info%channel_ids_rttov_coef_lw(channel_info%nchannels_lw))
   
   ! View index of each channel - trivial as (for now) dealing with one only
   channel_info%channel_view_ids=(/ 1, 1, 1, 1, 1, 1 /)
   !process everything
   channel_info%channel_proc_flag=(/ 1, 1, 1, 1, 1, 1 /)
   !which are the wl of those channels (approximate)
   channel_info%channel_wl_abs=(/ 0.67, 0.87, 1.61, 3.74, 10.8, 12.0 /)
   !channel number wrt RTTOV coefficient file
   channel_info%channel_ids_rttov_coef_sw=(/ 2, 3, 4, 5 /)
   channel_info%channel_ids_rttov_coef_lw=(/ 1, 2, 3 /)

end subroutine setup_aatsr

end module setup_instrument
