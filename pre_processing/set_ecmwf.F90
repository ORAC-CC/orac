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
! nwp_path     string in  If badc, folder in which to find GGAM files.
!                           Otherwise, folder in which to find GRB files.
! nwp_path2    string in  If badc, folder in which to find GGAS files.
! nwp_path3    string in  If badc, folder in which to find GPAM files.
! nwp_pathout  string out If badc, full path to appropriate GGAM file.
!                           Otherwise, full path to appropriate GRB file.
! nwp_path2out string out If badc, full path to appropriate GGAS file.
! nwp_path3out string out If badc, full path to appropriate GPAM file.
! nwp_flag     int    in  0: A single GRB file; 1: 3 NCDF files; 2: BADC files
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
!    required use nwp_path_file*(2).
! 2016/01/22, GM: Bug fix: time_int_fac was not being computed for
!    assume_full_path=.true.
! 2016/04/03, SP: Add option to process ECMWF forecast in single NetCDF4 file
!    Note: This should work with either the OPER or FCST streams from ECMWF.
! 2016/05/26, GT: Added code for automatically constructing the filenames
!    of the HR ERA data (copied from changes made, but committed to R3970
!    version of code by CP).
! 2016/07/31, GM: Tidying of the code drop above.
! 2017/01/31, SP: Add nwp_flag=5, for reading NOAA GFS forecast (ExtWork)
! 2017/04/11, SP: Added nwp_flag=6, for working with GFS analysis files.
! 2017/07/05, SP: Added nwp_flag=7, for working with new format GFS (ExtWork)
! 2019/29/01, MC: Bug fix: input ECMWF file name structure was incorrect for BADC
! 2022/01/19, GT: Added a line to set nwp_time_factor to 1.0 hours if we're
!                 using BADC ERA5.
!                 Also added code to automatically check for BADC provisional
!                 ERA5 data if the standard BADC ERA5 data isn't found.
! 2023/06/26, GT: nwp_time_factor change for BADC ERA5 is now changed in
!                 orac_preproc.F90, so it can be overriden by the driver file.
! 2024/03/12, GT: Bug-fix to ensure that, if needed, provisional ERA5t filename
!                 is correctly set for surface parameters, when reading BADC
!                 ERA5 data.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine set_ecmwf(granule, opts, nwp_flag, imager_geolocation, &
   imager_time, time_int_fac, assume_full_path)

   use calender_m
   use imager_structures_m
   use preproc_constants_m
   use preproc_structures_m, only: preproc_opts_t, setup_args_t

   implicit none

   type(setup_args_t),         intent(in)    :: granule
   type(preproc_opts_t),       intent(inout) :: opts
   integer,                    intent(in)    :: nwp_flag
   type(imager_geolocation_t), intent(in)    :: imager_geolocation
   type(imager_time_t),        intent(in)    :: imager_time
   real,                       intent(out)   :: time_int_fac
   logical,                    intent(in)    :: assume_full_path

   integer       :: i_path1, i_path2
   integer(sint) :: year, month, day, hour
   integer(sint) :: year2, month2, day2, hour2
   integer       :: day_before
   real(dreal)   :: jday, jday0, jday1, jday2
   real(dreal)   :: day_real, day_real2
   real(dreal)   :: hr_marker
   character     :: cera_year*4, cera_month*2, cera_day*2, cera_hour*2
   character     :: cera_year2*4, cera_month2*2, cera_day2*2, cera_hour2*2

   ! Rather than deal with whether the next n_hour file is in the next month,
   ! in the next year, or if the year is a leap year it is more straight
   ! forward to convert to Julian day space, then operate, then convert back.
   if (opts%ecmwf_time_int_method .eq. 1 .or. opts%ecmwf_time_int_method .eq. 2) then
      jday = find_center_time(imager_geolocation, imager_time)

      jday0 = floor(jday / ( opts%nwp_time_factor  / 24._dreal)) * opts%nwp_time_factor / 24._dreal
      jday1 = floor(jday / (opts%nwp_time_factor / 24._dreal) + 1._dreal) * opts%nwp_time_factor / 24._dreal

      time_int_fac = (jday - jday0) / (jday1 - jday0)
   end if
   if (assume_full_path .and. nwp_flag .ne. 2) then
      ! for nwp_flag=2, ensure NCDF file is listed in nwp_pathout
      if (index(opts%nwp_fnames%nwp_path(1), '.nc') .gt. 0) then
         opts%nwp_fnames%nwp_path_file  = opts%nwp_fnames%nwp_path
         opts%nwp_fnames%nwp_path_file2 = opts%nwp_fnames%nwp_path2
         opts%nwp_fnames%nwp_path_file3 = opts%nwp_fnames%nwp_path3
      else if (index(opts%nwp_fnames%nwp_path2(1), '.nc') .gt. 0) then
         opts%nwp_fnames%nwp_path_file  = opts%nwp_fnames%nwp_path2
         opts%nwp_fnames%nwp_path_file2 = opts%nwp_fnames%nwp_path
         opts%nwp_fnames%nwp_path_file3 = opts%nwp_fnames%nwp_path3
      else if (index(opts%nwp_fnames%nwp_path3(1), '.nc') .gt. 0) then
         opts%nwp_fnames%nwp_path_file  = opts%nwp_fnames%nwp_path3
         opts%nwp_fnames%nwp_path_file2 = opts%nwp_fnames%nwp_path
         opts%nwp_fnames%nwp_path_file3 = opts%nwp_fnames%nwp_path2
      else
         opts%nwp_fnames%nwp_path_file  = opts%nwp_fnames%nwp_path
         opts%nwp_fnames%nwp_path_file2 = opts%nwp_fnames%nwp_path2
         opts%nwp_fnames%nwp_path_file3 = opts%nwp_fnames%nwp_path3
      end if
   else
      if (opts%ecmwf_time_int_method .eq. 0) then
         ! pick last ERA interim file wrt sensor time (as on the same day)
         hr_marker = opts%nwp_time_factor
         do while(hr_marker < 24.)
            if (granule%hour .lt. hr_marker) then
                write(cera_hour,  '(I2.2)') hr_marker - opts%nwp_time_factor
                exit
            endif
            hr_marker = hr_marker + opts%nwp_time_factor
         end do

         i_path1 = 1

         call make_ecmwf_name(granule%cyear, granule%cmonth, granule%cday, &
              cera_hour, nwp_flag, opts%nwp_fnames, 1, 1)
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
              nwp_flag, opts%nwp_fnames, i_path1, 1)
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
              nwp_flag, opts%nwp_fnames, i_path1, 1)

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
              nwp_flag, opts%nwp_fnames, i_path2, 2)
      else
         write(*,*) 'ERROR: invalid set_ecmwf() time_interp_method: ', &
              opts%ecmwf_time_int_method
         stop error_stop_code
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


subroutine make_ecmwf_name(cyear, cmonth, cday, chour, nwp_flag, nwp_fnames, idx_1, idx_2)

    use preproc_constants_m
    use preproc_structures_m, only: preproc_nwp_fnames_t

    implicit none

    character(len=*), intent(in)              :: cyear, cmonth, cday, chour
    integer,          intent(in)              :: nwp_flag
    type(preproc_nwp_fnames_t), intent(inout) :: nwp_fnames
    integer,          intent(in)              :: idx_1
    integer,          intent(in)              :: idx_2

    logical :: f_tester

    character(len=path_length) :: nwp_path, nwp_path2, nwp_path3

    nwp_path = nwp_fnames%nwp_path(idx_1)
    nwp_path2 = nwp_fnames%nwp_path2(idx_1)
    nwp_path3 = nwp_fnames%nwp_path3(idx_1)
    select case (nwp_flag)
    case(0)
    ! NOAA GFS
        nwp_fnames%nwp_path_file(idx_2) = trim(adjustl(nwp_path))//'/gfs_4_'// &
           trim(adjustl(cyear))//trim(adjustl(cmonth))// &
           trim(adjustl(cday))//'_'//trim(adjustl(chour))//'00_000.grb2'
    case(1)
    ! ECMWF OPER or ERA5 as single netCDF4 file
        nwp_fnames%nwp_path_file(idx_2) = trim(adjustl(nwp_path))//'/ECMWF_OPER_'// &
           trim(adjustl(cyear))//trim(adjustl(cmonth))// &
           trim(adjustl(cday))//'_'//trim(adjustl(chour))//'+00.nc'
        inquire(file=trim(nwp_fnames%nwp_path_file(idx_2)), exist=f_tester)
        if (.not. f_tester) then
            nwp_fnames%nwp_path_file(idx_2) = trim(adjustl(nwp_path))//'/ECMWF_ERA5_'// &
               trim(adjustl(cyear))//trim(adjustl(cmonth))// &
               trim(adjustl(cday))//'_'//trim(adjustl(chour))//'_0.5.nc'
            inquire(file=trim(nwp_fnames%nwp_path_file(idx_2)), exist=f_tester)
        endif
        if (.not. f_tester) then
            print*,"ERROR: NWP data not found: ", trim(nwp_fnames%nwp_path_file(idx_2))
            stop
        endif
    case(2)
    ! ECMWF ERA5 in JASMIN format
        call determine_jasmin_filenames_era5(nwp_fnames, cyear, cmonth, cday, chour, idx_2)
    case(4)
    ! ECMWF ERA-Interim in JASMIN format
        nwp_fnames%nwp_path_file(idx_2) = trim(adjustl(nwp_path2))//'/gg/as/'// &
           trim(adjustl(cyear))//'/'//trim(adjustl(cmonth))//'/'// &
           trim(adjustl(cday))//'/'//'ggas'// &
           trim(adjustl(cyear))//trim(adjustl(cmonth))// &
           trim(adjustl(cday))//trim(adjustl(chour))//'00.nc'
        nwp_fnames%nwp_path_file2 = trim(adjustl(nwp_path))//'/gg/am/'// &
           trim(adjustl(cyear))//'/'//trim(adjustl(cmonth))//'/'// &
           trim(adjustl(cday))//'/'//'ggam'// &
           trim(adjustl(cyear))//trim(adjustl(cmonth))// &
           trim(adjustl(cday))//trim(adjustl(chour))//'00.grb'
        nwp_fnames%nwp_path_file3 = trim(adjustl(nwp_path3))//'/sp/am/'// &
           trim(adjustl(cyear))//'/'//trim(adjustl(cmonth))//'/'// &
           trim(adjustl(cday))//'/'//'spam'// &
           trim(adjustl(cyear))//trim(adjustl(cmonth))// &
           trim(adjustl(cday))//trim(adjustl(chour))//'00.grb'
    case default
        write(*,*) 'ERROR: set_ecmwf(): Unknown ECMWF file format flag. ' // &
                 'Please select 0, 1, 2, 3, or 4.'
        stop error_stop_code
    end select

end subroutine make_ecmwf_name


subroutine determine_jasmin_filenames_era5(nwp_fnames, cyear, cmonth, cday, chour, idx)
    use orac_ncdf_m
    use preproc_constants_m
    use preproc_structures_m

    type(preproc_nwp_fnames_t), intent(inout) :: nwp_fnames
    character(len=*), intent(in)              :: cyear, cmonth, cday, chour
    integer, intent(in)                       :: idx

    character(len=path_length)                :: ml_dir, sfc_dir, filebase
    logical                                   :: f_tester

    filebase = '/ecmwf-era5_oper_an_'
    ml_dir = trim(adjustl(nwp_fnames%nwp_path(idx)))//'/an_ml/'// &
             trim(adjustl(cyear))//'/'// &
             trim(adjustl(cmonth))//'/'// &
             trim(adjustl(cday))//trim(adjustl(filebase))//'ml_'// &
             trim(adjustl(cyear))// &
             trim(adjustl(cmonth))// &
             trim(adjustl(cday))// &
             trim(adjustl(chour))//'00.'

    ! Now check if we can find files matching this pattern. If not, we
    ! redefine filenase for the provisional ERA5 data record and try
    ! again
    inquire(file=trim(adjustl(ml_dir))//'q.nc', exist=f_tester)
    if (.not. f_tester) then
       !print*,"Standard ERA5 data not found, trying provisional ERA5"
       filebase = '/ecmwf-era5t_oper_an_'

       ml_dir = trim(adjustl(nwp_fnames%nwp_path(idx)))//'/an_ml/'// &
             trim(adjustl(cyear))//'/'// &
             trim(adjustl(cmonth))//'/'// &
             trim(adjustl(cday))//trim(adjustl(filebase))//'ml_'// &
             trim(adjustl(cyear))// &
             trim(adjustl(cmonth))// &
             trim(adjustl(cday))// &
             trim(adjustl(chour))//'00.'

       inquire(file=trim(adjustl(ml_dir))//'q.nc', exist=f_tester)
       if (.not. f_tester) then
          print*,"ERROR: JASMIN NWP data not found: ", trim(ml_dir)
          stop
       end if
    end if

    ! Set up surface path
    sfc_dir = trim(adjustl(nwp_fnames%nwp_path(idx)))//'/an_sfc/'// &
              trim(adjustl(cyear))//'/'// &
              trim(adjustl(cmonth))//'/'// &
              trim(adjustl(cday))//trim(adjustl(filebase))//'sfc_'// &
              trim(adjustl(cyear))// &
              trim(adjustl(cmonth))// &
              trim(adjustl(cday))// &
              trim(adjustl(chour))//'00.'

    ! Set up surface variables
    nwp_fnames%ci_f(idx) = trim(adjustl(sfc_dir))//'ci.nc'
    nwp_fnames%asn_f(idx) = trim(adjustl(sfc_dir))//'asn.nc'
    nwp_fnames%tcwv_f(idx) = trim(adjustl(sfc_dir))//'tcwv.nc'
    nwp_fnames%sd_f(idx) = trim(adjustl(sfc_dir))//'sd.nc'
    nwp_fnames%u10_f(idx) = trim(adjustl(sfc_dir))//'10u.nc'
    nwp_fnames%v10_f(idx) = trim(adjustl(sfc_dir))//'10v.nc'
    nwp_fnames%t2_f(idx) = trim(adjustl(sfc_dir))//'2t.nc'
    nwp_fnames%skt_f(idx) =  trim(adjustl(sfc_dir))//'skt.nc'
    nwp_fnames%sstk_f(idx) = trim(adjustl(sfc_dir))//'sst.nc'
    nwp_fnames%cape_f(idx) = trim(adjustl(sfc_dir))//'cape.nc'

    ! Set up model level variables
    nwp_fnames%q_f(idx) = trim(adjustl(ml_dir))//'q.nc'
    nwp_fnames%t_f(idx) = trim(adjustl(ml_dir))//'t.nc'
    nwp_fnames%o3_f(idx) = trim(adjustl(ml_dir))//'o3.nc'
    nwp_fnames%lnsp_f(idx) = trim(adjustl(ml_dir))//'lnsp.nc'
    nwp_fnames%u_f(idx) = trim(adjustl(ml_dir))//'u.nc'
    nwp_fnames%v_f(idx) = trim(adjustl(ml_dir))//'v.nc'
    nwp_fnames%z_f(idx) = trim(adjustl(ml_dir))//'z.nc'

end subroutine determine_jasmin_filenames_era5
