!-------------------------------------------------------------------------------
! Name: set_ecmwf.F90
!
! Purpose:
! Determines necessary ECMWF files for preprocessing.
!
! Description and Algorithm details:
! 1) Set root folder dependent on badc argument.
! 2) Round hour to nearest multiple of six on the same day to select file.
! 3) Generate ECMWF filename.
!
! Arguments:
! Name           Type   In/Out/Both Description
! ------------------------------------------------------------------------------
! hour           stint  in  Hour of day (0-59)
! cyear          string in  Year, as a 4 character string
! cmonth         string in  Month of year, as a 2 character string
! cday           string in  Day of month, as a 2 character string
! chour          string in  Hour of day, as a 2 character string
! ecmwf_path     string in  If badc, folder in which to find GGAM files.
!                           Otherwise, folder in which to find GRB files.
! ecmwf_path2    string in  If badc, folder in which to find GGAS files.
! ecmwf_path3    string in  If badc, folder in which to find GPAM files.
! ecmwf_pathout  string out If badc, full path to appropriate GGAM file.
!                           Otherwise, full path to appropriate GRB file.
! ecmwf_path2out string out If badc, full path to appropriate GGAS file.
! ecmwf_path3out string out If badc, full path to appropriate GPAM file.
! ecmwf_flag     int    in  0: A single GRB file; 1: 3 NCDF files; 2: BADC files
!                           4: One ECMWF forecast GRB file, 5 and 6 NOAA GFS grib
! assume_full_path
!                logic  in  T: inputs are filenames; F: folder names
!
! History:
! 2012/01/16, MJ; writes initial code version.
! 2012/01/19, MJ: fixed a potential bug in the file determination.
! 2012/08/06, CP: modified to include 3 new ecmwf pths to read data from the
!    BADC added in badc flag
! 2012/08/13, CP: modified badc paths
! 2012/11/14, CP: modified badc paths to include year/month/day changed gpam and
!    ggap
! 2012/12/06, CP: changed how ecmwf paths are defined because of looping chunks
!    and tidied up file
! 2013/03/06, CP: changed ggam from grb to netcdf file
! 2013/06/11, CP: set default ecmwf paths
! 2013/10/21, AP: removed redundant arguments. Tidying.
! 2014/02/03, AP: made badc a logical variable
! 2014/04/21, GM: Added logical option assume_full_path.
! 2014/05/02, AP: Replaced code with CASE statements. Added third option.
! 2015/11/26, GM: Changes to support temporal interpolation between ecmwf files.
! 2016/01/22, GM: If assume_full_path = .false. and a file from the next day is
!    required use ecmwf_path_file*(2).
! 2016/01/22, GM: Bug fix: time_int_fac was not being computed for
!    assume_full_path=.true.
! 2016/04/03, SP: Add option to process ECMWF forecast in single NetCDF4 file
!    Note: This should work with either the OPER or FCST streams from ECMWF.
! 2016/05/26, GT: Added code for automatically constructing the filenames
!    of the HR ERA data (copied from changes made, but committed to R3970
!    version of code by CP).
! 2016/07/31, GM: Tidying of the code drop above.
! 2017/01/31, SP: Add ecmwf_flag=5, for reading NOAA GFS forecast (ExtWork)
! 2017/04/11, SP: Added ecmwf_flag=6, for working with GFS analysis files.
! 2017/07/05, SP: Added ecmwf_flag=7, for working with new format GFS (ExtWork)
! 2019/29/01, MC: Bug fix: input ECMWF file name structure was incorrect for BADC
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine set_ecmwf(granule, opts, ecmwf_flag, imager_geolocation, &
   imager_time, time_int_fac, assume_full_path)

   use calender_m
   use imager_structures_m
   use preproc_constants_m
   use preproc_structures_m, only: preproc_opts_t, setup_args_t

   implicit none

   type(setup_args_t),         intent(in)    :: granule
   type(preproc_opts_t),       intent(inout) :: opts
   integer,                    intent(in)  :: ecmwf_flag
   type(imager_geolocation_t), intent(in)  :: imager_geolocation
   type(imager_time_t),        intent(in)  :: imager_time
   real,                       intent(out) :: time_int_fac
   logical,                    intent(in)  :: assume_full_path

   integer       :: i_path1, i_path2
   integer(sint) :: year, month, day, hour
   integer(sint) :: year2, month2, day2, hour2
   integer       :: day_before
   real(dreal)   :: jday, jday0, jday1, jday2
   real(dreal)   :: day_real, day_real2
   character     :: cera_year*4, cera_month*2, cera_day*2, cera_hour*2
   character     :: cera_year2*4, cera_month2*2, cera_day2*2, cera_hour2*2

   real(dreal)   :: time_fac

   ! Use 3-hourly NOAA GFS data, otherwise use 6-hourly ECMWF data
   if (ecmwf_flag .eq. 6 .or. ecmwf_flag .eq. 7 .or. ecmwf_flag .eq. 8) then
      time_fac = 3._dreal / 24._dreal
   else
      time_fac = 6._dreal / 24._dreal
   end if

   ! Rather than deal with whether the next 6 hour file is in the next month,
   ! in the next year, or if the year is a leap year it is more straight
   ! forward to convert to Julian day space, then operate, then convert back.
   if (opts%ecmwf_time_int_method .eq. 1 .or. opts%ecmwf_time_int_method .eq. 2) then
      jday = find_center_time(imager_geolocation, imager_time)

      jday0 = floor(jday / time_fac           ) * time_fac
      jday1 = floor(jday / time_fac + 1._dreal) * time_fac

      time_int_fac = (jday - jday0) / (jday1 - jday0)
   end if

   if (assume_full_path) then
      ! for ecmwf_flag=2, ensure NCDF file is listed in ecmwf_pathout
      if (index(opts%ecmwf_path(1), '.nc') .gt. 0) then
         opts%ecmwf_path_file  = opts%ecmwf_path
         opts%ecmwf_path_file2 = opts%ecmwf_path2
         opts%ecmwf_path_file3 = opts%ecmwf_path3
      else if (index(opts%ecmwf_path2(1), '.nc') .gt. 0) then
         opts%ecmwf_path_file  = opts%ecmwf_path2
         opts%ecmwf_path_file2 = opts%ecmwf_path
         opts%ecmwf_path_file3 = opts%ecmwf_path3
      else if (index(opts%ecmwf_path3(1), '.nc') .gt. 0) then
         opts%ecmwf_path_file  = opts%ecmwf_path3
         opts%ecmwf_path_file2 = opts%ecmwf_path
         opts%ecmwf_path_file3 = opts%ecmwf_path2
      else
         opts%ecmwf_path_file  = opts%ecmwf_path
         opts%ecmwf_path_file2 = opts%ecmwf_path2
         opts%ecmwf_path_file3 = opts%ecmwf_path3
      end if
   else
      if (opts%ecmwf_time_int_method .eq. 0) then
         ! pick last ERA interim file wrt sensor time (as on the same day)
         select case (granule%hour)
         case(0:5)
            cera_hour = '00'
         case(6:11)
            cera_hour = '06'
         case(12:17)
            cera_hour = '12'
         case(18:23)
            cera_hour = '18'
         case default
            cera_hour = '00'
         end select

         i_path1 = 1

         call make_ecmwf_name(granule%cyear, granule%cmonth, granule%cday, &
              cera_hour, ecmwf_flag, opts%ecmwf_path(1), opts%ecmwf_path2(1), &
              opts%ecmwf_path3(1), opts%ecmwf_path_file(1), &
              opts%ecmwf_path_file2(1), opts%ecmwf_path_file3(1))
      else if (opts%ecmwf_time_int_method .eq. 1) then
         ! Pick the closest ERA interim file wrt sensor time
         if (jday - jday0 .lt. jday1 - jday) then
            jday2 = jday0
         else
            jday2 = jday1
         end if

         call JD2GREG(jday0, year, month, day_real)
         day = int(day_real, sint)

         day_before = day

         call JD2GREG(jday2, year, month, day_real)
         day = int(day_real, sint)
         hour = int((day_real - day) * 24._dreal, sint)

         write(cera_year,  '(I4.4)') year
         write(cera_month, '(I2.2)') month
         write(cera_day,   '(I2.2)') day
         write(cera_hour,  '(I2.2)') hour

         if (day_before .eq. day) then
            i_path1 = 1
         else
            i_path1 = 2
         end if

         call make_ecmwf_name(cera_year, cera_month, cera_day, cera_hour, &
              ecmwf_flag, opts%ecmwf_path(i_path1), opts%ecmwf_path2(i_path1), &
              opts%ecmwf_path3(i_path1), opts%ecmwf_path_file(1), &
              opts%ecmwf_path_file2(1), opts%ecmwf_path_file3(1))
      else if (opts%ecmwf_time_int_method .eq. 2) then
         ! Pick the ERA interim files before and after wrt sensor time

         call JD2GREG(jday0, year, month, day_real)
         day  = int(day_real, sint)
         hour = int((day_real - day) * 24._dreal, sint)

         write(cera_year,  '(I4.4)') year
         write(cera_month, '(I2.2)') month
         write(cera_day,   '(I2.2)') day
         write(cera_hour,  '(I2.2)') hour

         call JD2GREG(jday, year2, month2, day_real2)
         day2  = int(day_real2, sint)
         hour2 = int((day_real2 - day2) * 24._dreal, sint)

         write(cera_year2,  '(I4.4)') year2
         write(cera_month2, '(I2.2)') month2
         write(cera_day2,   '(I2.2)') day2
         write(cera_hour2,  '(I2.2)') hour2

         if (cera_day2 .eq. granule%cday) then
            i_path1 = 1
         else
            ! The mid point is in the next day so the path to the first file is
            ! the 2nd path provided by the user.
            i_path1 = 2
         end if

         call make_ecmwf_name(cera_year, cera_month, cera_day, cera_hour, &
              ecmwf_flag, opts%ecmwf_path(i_path1), opts%ecmwf_path2(i_path1), &
              opts%ecmwf_path3(i_path1), opts%ecmwf_path_file(1), &
              opts%ecmwf_path_file2(1), opts%ecmwf_path_file3(1))

         ! now look at the next file
         day_before = day

         call JD2GREG(jday1, year, month, day_real)
         day = int(day_real, sint)
         hour = int((day_real - day) * 24._dreal, sint)

         write(cera_year,  '(I4.4)') year
         write(cera_month, '(I2.2)') month
         write(cera_day,   '(I2.2)') day
         write(cera_hour,  '(I2.2)') hour

        if (i_path1 .eq. 1) then
            if (day_before .eq. day) then
               i_path2 = 1
            else
               i_path2 = 2
            end if
         else
            i_path2 = 2
         end if

         call make_ecmwf_name(cera_year, cera_month, cera_day, cera_hour, &
              ecmwf_flag, opts%ecmwf_path(i_path2), opts%ecmwf_path2(i_path2), &
              opts%ecmwf_path3(i_path2), opts%ecmwf_path_file(2), &
              opts%ecmwf_path_file2(2), opts%ecmwf_path_file3(2))
      else
         write(*,*) 'ERROR: invalid set_ecmwf() time_interp_method: ', &
              opts%ecmwf_time_int_method
         stop error_stop_code
      end if
   end if

   if (opts%ecmwf_path_hr(1) .eq. '') then
      call build_ecmwf_HR_file_from_LR(opts%ecmwf_path_file(1), &
              opts%ecmwf_hr_path_file(1))
      if (opts%ecmwf_time_int_method .eq. 2) then
         call build_ecmwf_HR_file_from_LR(opts%ecmwf_path_file(2), &
              opts%ecmwf_hr_path_file(2))
      end if
   else if (assume_full_path) then
      opts%ecmwf_hr_path_file(1) = opts%ecmwf_path_hr(1)
      if (opts%ecmwf_time_int_method .eq. 2) &
           opts%ecmwf_hr_path_file(2) = opts%ecmwf_path_hr(2)
   else
      if (opts%ecmwf_time_int_method .ne. 2) then
         call build_ecmwf_HR_file_from_LR2(opts%ecmwf_path(i_path1), &
              opts%ecmwf_path_file(1), opts%ecmwf_path_hr(1), &
              opts%ecmwf_hr_path_file(1))
      else
         call build_ecmwf_HR_file_from_LR2(opts%ecmwf_path(i_path1), &
              opts%ecmwf_path_file(1), opts%ecmwf_path_hr(1), &
              opts%ecmwf_hr_path_file(1))
         call build_ecmwf_HR_file_from_LR2(opts%ecmwf_path(i_path2), &
              opts%ecmwf_path_file(2), opts%ecmwf_path_hr(1), &
              opts%ecmwf_hr_path_file(2))
      end if
   end if

end subroutine set_ecmwf

#ifdef __PGI
real*8 function find_center_time(imager_geolocation, imager_time) &
   result(center_time)
#else
real(dreal) function find_center_time(imager_geolocation, imager_time) &
   result(center_time)
#endif

   use imager_structures_m
   use preproc_constants_m

   implicit none

   type(imager_geolocation_t), intent(in) :: imager_geolocation
   type(imager_time_t),        intent(in) :: imager_time

   integer     :: i, j
   real(dreal) :: start_time
   real(dreal) :: end_time

   do i = 1, imager_geolocation%ny
      do j = imager_geolocation%startx, imager_geolocation%endx
          if (imager_time%time(j,i) .ne. dreal_fill_value) then
             start_time = imager_time%time(j,i)
             goto 98
          end if
      end do
   end do

98 do i = imager_geolocation%ny, 1, -1
      do j = imager_geolocation%endx, imager_geolocation%startx, -1
          if (imager_time%time(j,i) .ne. dreal_fill_value) then
             end_time = imager_time%time(j,i)
             goto 99
          end if
      end do
   end do

99 center_time = (start_time + end_time) / 2.

end function find_center_time


subroutine make_ecmwf_name(cyear, cmonth, cday, chour, ecmwf_flag, ecmwf_path, &
   ecmwf_path2, ecmwf_path3, ecmwf_path_file, ecmwf_path_file2, ecmwf_path_file3)

   use preproc_constants_m

   implicit none

   character(len=*), intent(in)  :: cyear, cmonth, cday, chour
   character(len=*), intent(in)  :: ecmwf_path
   character(len=*), intent(in)  :: ecmwf_path2
   character(len=*), intent(in)  :: ecmwf_path3
   character(len=*), intent(out) :: ecmwf_path_file
   character(len=*), intent(out) :: ecmwf_path_file2
   character(len=*), intent(out) :: ecmwf_path_file3
   integer,          intent(in)  :: ecmwf_flag

   select case (ecmwf_flag)
   case(0)
      ecmwf_path_file = trim(adjustl(ecmwf_path))//'/ERA_Interim_an_'// &
           trim(adjustl(cyear))//trim(adjustl(cmonth))// &
           trim(adjustl(cday))//'_'//trim(adjustl(chour))//'+00.grb'
   case(1)
      ecmwf_path_file = trim(adjustl(ecmwf_path))//'/ggas/'// &
           trim(adjustl(cyear))//'/'//trim(adjustl(cmonth))//'/'// &
           trim(adjustl(cday))//'/'//'ggas'// &
           trim(adjustl(cyear))//trim(adjustl(cmonth))// &
           trim(adjustl(cday))//trim(adjustl(chour))//'00.nc'
      ecmwf_path_file2 = trim(adjustl(ecmwf_path2))//'/ggam/'// &
           trim(adjustl(cyear))//'/'//trim(adjustl(cmonth))//'/'// &
           trim(adjustl(cday))//'/'//'ggam'// &
           trim(adjustl(cyear))//trim(adjustl(cmonth))// &
           trim(adjustl(cday))//trim(adjustl(chour))//'00.nc'
      ecmwf_path_file3 = trim(adjustl(ecmwf_path3))//'/spam/'// &
           trim(adjustl(cyear))//'/'//trim(adjustl(cmonth))//'/'// &
           trim(adjustl(cday))//'/'//'spam'// &
           trim(adjustl(cyear))//trim(adjustl(cmonth))// &
           trim(adjustl(cday))//trim(adjustl(chour))//'00.nc'
   case(2)
      ecmwf_path_file = trim(adjustl(ecmwf_path2))//'/gg/as/'// &
           trim(adjustl(cyear))//'/'//trim(adjustl(cmonth))//'/'// &
           trim(adjustl(cday))//'/'//'ggas'// &
           trim(adjustl(cyear))//trim(adjustl(cmonth))// &
           trim(adjustl(cday))//trim(adjustl(chour))//'00.nc'
      ecmwf_path_file2 = trim(adjustl(ecmwf_path))//'/gg/am/'// &
           trim(adjustl(cyear))//'/'//trim(adjustl(cmonth))//'/'// &
           trim(adjustl(cday))//'/'//'ggam'// &
           trim(adjustl(cyear))//trim(adjustl(cmonth))// &
           trim(adjustl(cday))//trim(adjustl(chour))//'00.grb'
      ecmwf_path_file3 = trim(adjustl(ecmwf_path3))//'/sp/am/'// &
           trim(adjustl(cyear))//'/'//trim(adjustl(cmonth))//'/'// &
           trim(adjustl(cday))//'/'//'spam'// &
           trim(adjustl(cyear))//trim(adjustl(cmonth))// &
           trim(adjustl(cday))//trim(adjustl(chour))//'00.grb'
   case(4)
      ecmwf_path_file = trim(adjustl(ecmwf_path))//'/ECMWF_OPER_'// &
           trim(adjustl(cyear))//trim(adjustl(cmonth))// &
           trim(adjustl(cday))//'_'//trim(adjustl(chour))//'+00.nc'
   case(5)
      ecmwf_path_file = trim(adjustl(ecmwf_path))//'/ECMWF_ERA5_'// &
           trim(adjustl(cyear))//trim(adjustl(cmonth))// &
           trim(adjustl(cday))//'_'//trim(adjustl(chour))//'_0.5.nc'
   case(6)
      ecmwf_path_file = trim(adjustl(ecmwf_path))//'/gfs_4_'// &
           trim(adjustl(cyear))//trim(adjustl(cmonth))// &
           trim(adjustl(cday))//'_'//trim(adjustl(chour))//'00_000.grb2'
   case(7)
      ecmwf_path_file = trim(adjustl(ecmwf_path))//'/GFS_'// &
           trim(adjustl(cyear))//trim(adjustl(cmonth))// &
           trim(adjustl(cday))//trim(adjustl(chour))//'00.grb'
   case(8)
      ecmwf_path_file = trim(adjustl(ecmwf_path))//'/GFS_'// &
           trim(adjustl(cyear))//trim(adjustl(cmonth))// &
           trim(adjustl(cday))//trim(adjustl(chour))//'00.grb.nc'
   case default
      write(*,*) 'ERROR: set_ecmwf(): Unknown ECMWF file format flag. ' // &
                 'Please select 0, 1, 2, 3, or 4.'
      stop error_stop_code
   end select

end subroutine make_ecmwf_name


subroutine build_ecmwf_HR_file_from_LR(ecmwf_path_file, ecmwf_hr_path_file)

   use preproc_constants_m

   implicit none

   character(len=*), intent(in)  :: ecmwf_path_file
   character(len=*), intent(out) :: ecmwf_hr_path_file

   character(len=path_length) :: base, suffix
   integer :: cut_off, ecmwf_path_file_length

   cut_off = index(ecmwf_path_file, '.', back=.true.)
   ecmwf_path_file_length = len_trim(ecmwf_path_file)
   base = trim(adjustl(ecmwf_path_file(1:(cut_off-1))))
   suffix = trim(adjustl(ecmwf_path_file((cut_off+1):ecmwf_path_file_length)))
   ecmwf_hr_path_file = trim(adjustl(base)) // '_HR.' // trim(adjustl(suffix))

end subroutine build_ecmwf_HR_file_from_LR


subroutine build_ecmwf_HR_file_from_LR2(ecmwf_path, ecmwf_path_file, &
                                        ecmwf_hr_path, ecmwf_hr_path_file)

   use preproc_constants_m

   implicit none

   character(len=*), intent(in)  :: ecmwf_path
   character(len=*), intent(in)  :: ecmwf_path_file
   character(len=*), intent(in)  :: ecmwf_hr_path
   character(len=*), intent(out) :: ecmwf_hr_path_file

   character(len=path_length) :: temp_file, &
                                 hr_ext, ecmwf_hour_hr
   integer                    :: cut_off, ecmwf_file_length
   character                  :: yyyy*4, mm*2, dd*2, hh*2

   temp_file = ecmwf_path_file
   ecmwf_file_length = len_trim(ecmwf_path)
   cut_off = index(temp_file, '.', back=.true.)

   yyyy = trim(adjustl(temp_file(ecmwf_file_length+6:(cut_off-9))))
   mm   = trim(adjustl(temp_file(ecmwf_file_length+10:(cut_off-7))))
   dd   = trim(adjustl(temp_file(ecmwf_file_length+12:(cut_off-5))))
   hh   = trim(adjustl(temp_file(ecmwf_file_length+14:(cut_off-3))))

   select case (hh)
   case('00')
      ecmwf_hour_hr = '0'
   case('06')
      ecmwf_hour_hr = '600'
   case('12')
      ecmwf_hour_hr = '1200'
   case('18')
      ecmwf_hour_hr = '1800'
   end select

   hr_ext ='/'//trim(adjustl(yyyy))//'/'//trim(adjustl(mm))//'/'// &
           trim(adjustl(dd))//'/ERA_Interim_an_'//trim(adjustl(yyyy))// &
           trim(adjustl(mm))//trim(adjustl(dd))//'_'// &
           trim(adjustl(ecmwf_hour_hr))//'+00_HR.grb'
   ecmwf_hr_path_file = trim(adjustl(ecmwf_hr_path))//trim(adjustl(hr_ext))

end subroutine build_ecmwf_HR_file_from_LR2
