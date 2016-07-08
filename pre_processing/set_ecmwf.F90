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
!                           4: One ECMWF forecast GRB file
! assume_full_path
!                logic  in  T: inputs are filenames; F: folder names
!
! History:
! 2012/01/16, MJ; writes initial code version.
! 2012/01/19, MJ: fixed a potential bug in the file determination.
! 2012/08/06, CP: modified to include 3 new ecmwf pths to read data from the
!    BADC added in badc flag
! 2012/08/13, CP: modified badc paths
! 2012/11/14, CP: modified badc paths to include year/month/day changed gpam ggap
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
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine set_ecmwf(sensor,cyear,cmonth,cday,chour,cminute,ecmwf_path,ecmwf_path2, &
   ecmwf_path3,ecmwf_path_file,ecmwf_path_file2,ecmwf_path_file3,ecmwf_flag, &
   imager_geolocation,imager_time,time_interp_method,time_int_fac,assume_full_path)

   use calender_m
   use imager_structures_m
   use preproc_constants_m

   implicit none

   character(len=*),           intent(in)  :: sensor
   character(len=*),           intent(in)  :: cyear,cmonth,cday,chour,cminute
   character(len=*),           intent(in)  :: ecmwf_path(2)
   character(len=*),           intent(in)  :: ecmwf_path2(2)
   character(len=*),           intent(in)  :: ecmwf_path3(2)
   character(len=*),           intent(out) :: ecmwf_path_file(2)
   character(len=*),           intent(out) :: ecmwf_path_file2(2)
   character(len=*),           intent(out) :: ecmwf_path_file3(2)
   integer,                    intent(in)  :: ecmwf_flag
   type(imager_geolocation_t), intent(in)  :: imager_geolocation
   type(imager_time_t),        intent(in)  :: imager_time
   integer,                    intent(in)  :: time_interp_method
   real,                       intent(out) :: time_int_fac
   logical,                    intent(in)  :: assume_full_path

   integer       :: i_path
   integer(sint) :: year, month, day, hour
   integer       :: day_before
   real(dreal)   :: jday, jday0, jday1, jday2
   real(dreal)   :: day_real
   character     :: cera_year*4, cera_month*2, cera_day*2, cera_hour*2

   ! Rather than deal with whether the next 6 hour file is in the next month,
   ! in the next year, or if the year is a leap year it is more straight
   ! forward to convert to Julian day space, then operate, then convert back.
   if (time_interp_method .eq. 1 .or. time_interp_method .eq. 2) then
      jday = find_center_time(imager_geolocation, imager_time)

      jday0 = floor(jday / (6._dreal / 24._dreal)           ) * 6._dreal / 24._dreal
      jday1 = floor(jday / (6._dreal / 24._dreal) + 1._dreal) * 6._dreal / 24._dreal

      time_int_fac = (jday - jday0) / (jday1 - jday0)
   end if
   if (assume_full_path) then
      ! for ecmwf_flag=2, ensure NCDF file is listed in ecmwf_pathout
      if (index(ecmwf_path(1),'.nc') .gt. 0) then
         ecmwf_path_file  = ecmwf_path
         ecmwf_path_file2 = ecmwf_path2
         ecmwf_path_file3 = ecmwf_path3
      else if (index(ecmwf_path2(1),'.nc') .gt. 0) then
         ecmwf_path_file  = ecmwf_path2
         ecmwf_path_file2 = ecmwf_path
         ecmwf_path_file3 = ecmwf_path3
      else if (index(ecmwf_path3(1),'.nc') .gt. 0) then
         ecmwf_path_file  = ecmwf_path3
         ecmwf_path_file2 = ecmwf_path
         ecmwf_path_file3 = ecmwf_path2
      else
         ecmwf_path_file  = ecmwf_path
         ecmwf_path_file2 = ecmwf_path2
         ecmwf_path_file3 = ecmwf_path3
      end if
   else
      if (time_interp_method .eq. 0) then
         ! pick last ERA interim file wrt sensor time (as on the same day)
         read(chour, *) hour
         select case (hour)
         case(0:5)
            cera_hour='00'
         case(6:11)
            cera_hour='06'
         case(12:17)
            cera_hour='12'
         case(18:23)
            cera_hour='18'
         case default
            cera_hour='00'
         end select

         call make_ecmwf_name(cyear,cmonth,cday,cera_hour,ecmwf_flag,ecmwf_path(1), &
            ecmwf_path2(1),ecmwf_path3(1),ecmwf_path_file(1),ecmwf_path_file2(1), &
            ecmwf_path_file3(1))
      else if (time_interp_method .eq. 1) then
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
            i_path = 1
         else
            i_path = 2
         end if

         call make_ecmwf_name(cera_year,cera_month,cera_day,cera_hour,ecmwf_flag, &
            ecmwf_path(i_path),ecmwf_path2(i_path),ecmwf_path3(i_path), &
            ecmwf_path_file(1),ecmwf_path_file2(1),ecmwf_path_file3(1))
      else if (time_interp_method .eq. 2) then
         ! Pick the ERA interim files before and after wrt sensor time

         call JD2GREG(jday0, year, month, day_real)
         day  = int(day_real, sint)
         hour = int((day_real - day) * 24._dreal, sint)

         write(cera_year,  '(I4.4)') year
         write(cera_month, '(I2.2)') month
         write(cera_day,   '(I2.2)') day
         write(cera_hour,  '(I2.2)') hour

         call make_ecmwf_name(cera_year,cera_month,cera_day,cera_hour,ecmwf_flag, &
            ecmwf_path(1),ecmwf_path2(1),ecmwf_path3(1),ecmwf_path_file(1), &
            ecmwf_path_file2(1), ecmwf_path_file3(1))

         day_before = day

         call JD2GREG(jday1, year, month, day_real)
         day = int(day_real, sint)
         hour = int((day_real - day) * 24._dreal, sint)

         write(cera_year,  '(I4.4)') year
         write(cera_month, '(I2.2)') month
         write(cera_day,   '(I2.2)') day
         write(cera_hour,  '(I2.2)') hour

         if (day_before .eq. day) then
            i_path = 1
         else
            i_path = 2
         end if

         call make_ecmwf_name(cera_year,cera_month,cera_day,cera_hour,ecmwf_flag, &
            ecmwf_path(i_path),ecmwf_path2(i_path),ecmwf_path3(i_path), &
            ecmwf_path_file(2),ecmwf_path_file2(2), ecmwf_path_file3(2))
      else
         write(*,*) 'ERROR: invalid set_ecmwf() time_interp_method: ', time_interp_method
         stop error_stop_code
      end if
   end if

end subroutine set_ecmwf


real(dreal) function find_center_time(imager_geolocation, imager_time) &
   result(center_time)

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


subroutine make_ecmwf_name(cyear,cmonth,cday,chour,ecmwf_flag,ecmwf_path, &
   ecmwf_path2,ecmwf_path3,ecmwf_path_file,ecmwf_path_file2,ecmwf_path_file3)

   use preproc_constants_m

   implicit none

   character(len=*), intent(in)  :: cyear,cmonth,cday,chour
   character(len=*), intent(in)  :: ecmwf_path
   character(len=*), intent(in)  :: ecmwf_path2
   character(len=*), intent(in)  :: ecmwf_path3
   character(len=*), intent(out) :: ecmwf_path_file
   character(len=*), intent(out) :: ecmwf_path_file2
   character(len=*), intent(out) :: ecmwf_path_file3
   integer,          intent(in)  :: ecmwf_flag

   select case (ecmwf_flag)
   case(0)
      ecmwf_path_file=trim(adjustl(ecmwf_path))//'/ERA_Interim_an_'// &
           trim(adjustl(cyear))//trim(adjustl(cmonth))// &
           trim(adjustl(cday))//'_'//trim(adjustl(chour))//'+00.grb'
   case(1)
      ecmwf_path_file=trim(adjustl(ecmwf_path))//'/ggas'// &
           trim(adjustl(cyear))//trim(adjustl(cmonth))// &
           trim(adjustl(cday))//trim(adjustl(chour))//'00.nc'
      ecmwf_path_file2=trim(adjustl(ecmwf_path2))//'/ggam'// &
           trim(adjustl(cyear))//trim(adjustl(cmonth))// &
           trim(adjustl(cday))//trim(adjustl(chour))//'00.nc'
      ecmwf_path_file3=trim(adjustl(ecmwf_path3))//'/gpam'// &
           trim(adjustl(cyear))//trim(adjustl(cmonth))// &
           trim(adjustl(cday))//trim(adjustl(chour))//'00.nc'
   case(2)
      ecmwf_path_file=trim(adjustl(ecmwf_path2))//'/ggas'// &
           trim(adjustl(cyear))//trim(adjustl(cmonth))// &
           trim(adjustl(cday))//trim(adjustl(chour))//'00.nc'
      ecmwf_path_file2=trim(adjustl(ecmwf_path))//'/ggam'// &
           trim(adjustl(cyear))//trim(adjustl(cmonth))// &
           trim(adjustl(cday))//trim(adjustl(chour))//'00.grb'
      ecmwf_path_file3=trim(adjustl(ecmwf_path3))//'/spam'// &
           trim(adjustl(cyear))//trim(adjustl(cmonth))// &
           trim(adjustl(cday))//trim(adjustl(chour))//'00.grb'
   case(4)
      ecmwf_path_file=trim(adjustl(ecmwf_path))//'/ECMWF_OPER_'// &
           trim(adjustl(cyear))//trim(adjustl(cmonth))// &
           trim(adjustl(cday))//'_'//trim(adjustl(chour))//'+00.nc'
   case default
      write(*,*) 'ERROR: set_ecmwf(): Unknown ECMWF file format flag. ' // &
               & 'Please select 0, 1, 2, 3, or 4.'
      stop error_stop_code
   end select

end subroutine make_ecmwf_name
