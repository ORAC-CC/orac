! Name: set_ecmwf.F90
!
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
! badc           logic  in  1: Use BADC files as formatted by the BADC in NCDF
!                           format. Otherwise: Assume ERA_Interim GRB format.
! verbose        logic  in  T: print status information; F: don't
!
! History:
! 2012/01/16: MJ writes initial code version.
! 2012/01/19: MJ fixed a potential bug in the file determination.
! 2012/08/06: CP modified to include 3 new ecmwf pths to read data from the
!                BADC added in badc flag
! 2012/08/13: CP modified badc paths
! 2012/11/14: CP modified badc paths to include year/month/day changed gpam ggap
! 2012/12/06: CP changed how ecmwf paths are defined because of looping chunks
!                and tidied up file
! 2013/03/06: CP changed ggam from grb to netcdf file
! 2013/06/11: CP set default ecmwf paths
! 2013/10/21: AP removed redundant arguments. Tidying.
! 2014/02/03: AP made badc a logical variable
! 2014/04/21: GM Added logical option assume_full_path.
!
! $Id$
!
! Bugs:
! none known
!

subroutine set_ecmwf(hour,cyear,cmonth,cday,chour,assume_full_path,ecmwf_path,&
   ecmwf_path2,ecmwf_path3,ecmwf_pathout,ecmwf_path2out,ecmwf_path3out,badc,&
   verbose)

   use preproc_constants

   implicit none

   integer(kind=stint),       intent(in)  :: hour
   character(len=datelength), intent(in)  :: cyear,chour,cmonth,cday
   logical,                   intent(in)  :: assume_full_path
   character(len=pathlength), intent(in)  :: ecmwf_path
   character(len=pathlength), intent(in)  :: ecmwf_path2
   character(len=pathlength), intent(in)  :: ecmwf_path3
   character(len=pathlength), intent(out) :: ecmwf_pathout
   character(len=pathlength), intent(out) :: ecmwf_path2out
   character(len=pathlength), intent(out) :: ecmwf_path3out
   logical,                   intent(in)  :: badc,verbose

   character                 :: cera_hour*2
   character(len=pathlength) :: ecmwf_path_root
   character(len=pathlength) :: ecmwf_path_root2
   character(len=pathlength) :: ecmwf_path_root3
   integer(kind=stint)       :: era_hours,era_hour,diff_hour,diff_hour_old

   if (assume_full_path) then
      ecmwf_pathout  = ecmwf_path
      ecmwf_path2out = ecmwf_path2
      ecmwf_path3out = ecmwf_path3
   else
      if (verbose) write(*,*) 'About to build full ecmwf path: ', trim(ecmwf_path)
      ecmwf_path_root=trim(adjustl(ecmwf_path))//'/'
      ecmwf_path_root2 = ecmwf_path_root
      ecmwf_path_root3 = ecmwf_path_root
      if (.not. badc) then
         ecmwf_path_root=trim(adjustl(ecmwf_path))//'/ERA_Interim_an_'// &
              & trim(adjustl(cyear))//trim(adjustl(cmonth))// &
              & trim(adjustl(cday))//'_'
      else
         ecmwf_path_root=trim(adjustl(ecmwf_path))//'/'

         ecmwf_path_root2=trim(adjustl(ecmwf_path2))//'/'

         ecmwf_path_root3=trim(adjustl(ecmwf_path3))//'/'
      endif

      if (verbose) write(*,*) 'set_ecmwf ecmwf_path_root: ',trim(ecmwf_path_root)

      ! pick closest ERA interim time wrt sensor time (on same day)
      diff_hour_old=999
      do era_hours=0,18,6
         diff_hour=hour-era_hours
         if(abs(diff_hour) .le. diff_hour_old) era_hour=era_hours
         diff_hour_old=diff_hour
      enddo
      write(cera_hour,'(i2)') era_hour
      if (era_hour .lt. 10) cera_hour='0'//trim(adjustl(cera_hour))

      !finalize the path to the era interim file
      if (.not. badc) then
         ecmwf_pathout=trim(adjustl(ecmwf_path_root))// &
              & trim(adjustl(cera_hour))//'+00.grb'
      else
         !note some files are now in netcdf format
         ecmwf_path2out=trim(adjustl(ecmwf_path_root2))//'ggas'// &
              & trim(adjustl(cyear))//trim(adjustl(cmonth))// &
              & trim(adjustl(cday))//trim(adjustl(cera_hour))//'00.nc'

         ! o3 and q
         ecmwf_pathout=trim(adjustl(ecmwf_path_root))//'ggam'// &
              & trim(adjustl(cyear))//trim(adjustl(cmonth))// &
              & trim(adjustl(cday))//trim(adjustl(cera_hour))//'00.nc'
         ! NB this must be called last
         ! t geo surface lnsp
         ecmwf_path3out=trim(adjustl(ecmwf_path_root3))//'gpam'// &
              & trim(adjustl(cyear))//trim(adjustl(cmonth))// &
              & trim(adjustl(cday))//trim(adjustl(cera_hour))//'00.nc'
      endif
   endif

   if (verbose) then
      write(*,*)'ecmwf_path:  ',trim(ecmwf_pathout)
      if (badc) then
         write(*,*)'ecmwf_path2: ',trim(ecmwf_path2out)
         write(*,*)'ecmwf_path3: ',trim(ecmwf_path3out)
      endif
   endif

end subroutine set_ecmwf
