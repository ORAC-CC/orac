!-------------------------------------------------------------------------------
! Name: read_ecmwf_wind
!
! Purpose:
!
! Description and Algorithm details:
!
! Arguments:
! Name Type In/Out/Both Description

! Return value:
! Name Type Description
! ------------------------------------------------------------------------------
!
! History:
! 2015/11/26, GM: Pulled this code from the main program into this subroutine so
!    that it could be executed twice, once for the ECMWF data on each side of
!    the satellite acquisition.
! 2015/12/17, OS: Added wrapper specific variables.
! 2016/02/02, OS: Now using ERA HR data for all options. TBD: make this optional.
! 2016/02/02, GM: Make use of the high resolution ECMWF input optional.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine read_ecmwf_wind(ecmwf_flag, ecmwf_path_file, ecmwf_HR_path_file, &
   ecmwf_path_file2, ecmwf_path_file3, ecmwf, ecmwf_HR, use_hr_ecmwf, verbose)

   use preproc_structures

   implicit none

   integer,                    intent(in)  :: ecmwf_flag
   character(len=path_length), intent(in)  :: ecmwf_path_file
   character(len=path_length), intent(in)  :: ecmwf_HR_path_file
   character(len=path_length), intent(in)  :: ecmwf_path_file2
   character(len=path_length), intent(in)  :: ecmwf_path_file3
   type(ecmwf_s),              intent(out) :: ecmwf
   type(ecmwf_s),              intent(out) :: ecmwf_HR
   logical,                    intent(in)  :: use_hr_ecmwf
   logical,                    intent(in)  :: verbose

   select case (ecmwf_flag)
   case(0)
      call read_ecmwf_wind_grib(ecmwf_path_file,ecmwf)
      if (verbose) write(*,*)'ecmwf_dims grib: ',ecmwf%xdim,ecmwf%ydim
      if (use_hr_ecmwf) then
         call read_ecmwf_wind_grib(ecmwf_HR_path_file,ecmwf_HR)
      end if
   case(1)
      call read_ecmwf_wind_nc(ecmwf_path_file,ecmwf_path_file2, &
           ecmwf_path_file3,ecmwf,.false.)
      if (verbose) write(*,*)'ecmwf_dims ncdf: ',ecmwf%xdim,ecmwf%ydim
      if (use_hr_ecmwf) then
         call read_ecmwf_wind_nc(ecmwf_HR_path_file,"n/a","n/a",ecmwf_HR,.true.)
      end if
   case(2)
      call read_ecmwf_wind_badc(ecmwf_path_file,ecmwf_path_file2, &
           ecmwf_path_file3,ecmwf,.false.)
      if (verbose) write(*,*)'ecmwf_dims badc: ',ecmwf%xdim,ecmwf%ydim
      if (use_hr_ecmwf) then
         call read_ecmwf_wind_badc(ecmwf_HR_path_file,"n/a","n/a",ecmwf_HR,.true.)
      end if
   case(3)
      call read_ecmwf_wind_dwd(ecmwf_path_file,ecmwf)
      if (verbose) write(*,*)'ecmwf_dims ncdf: ',ecmwf%xdim,ecmwf%ydim,ecmwf%kdim
      if (use_hr_ecmwf) then
         call read_ecmwf_wind_dwd(ecmwf_HR_path_file,ecmwf_HR)
      end if
   end select
   if (verbose) then
      write(*,*) 'U10) Min: ',minval(ecmwf%u10),', Max: ',maxval(ecmwf%u10)
      write(*,*) 'V10) Min: ',minval(ecmwf%v10),', Max: ',maxval(ecmwf%v10)
      write(*,*) 'SKINT) Min: ',minval(ecmwf%skin_temp),', Max: ',maxval(ecmwf%skin_temp)
   end if
   call rearrange_ecmwf(ecmwf,.false.)
   if (use_hr_ecmwf) then
      call rearrange_ecmwf(ecmwf_HR,.true.)
   end if

end subroutine read_ecmwf_wind


!-------------------------------------------------------------------------------
! Name: read_ecmwf
!
! Purpose:
!
! Description and Algorithm details:
!
! Arguments:
! Name Type In/Out/Both Description

! Return value:
! Name Type Description
! ------------------------------------------------------------------------------
!
! History:
! 2015/11/26, GM: Pulled this code from the main program into this subroutine so
!    that it could be executed twice, once for the ECMWF data on each side of
!    the satellite acquisition.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine read_ecmwf(ecmwf_flag, ecmwf_path_file, ecmwf_path_file2, &
   ecmwf_path_file3, ecmwf, preproc_dims, preproc_geoloc, preproc_prtm, verbose)

   use preproc_structures

   implicit none

   integer,                    intent(in)    :: ecmwf_flag
   character(len=path_length), intent(in)    :: ecmwf_path_file
   character(len=path_length), intent(in)    :: ecmwf_path_file2
   character(len=path_length), intent(in)    :: ecmwf_path_file3
   type(ecmwf_s),              intent(in)    :: ecmwf
   type(preproc_dims_s),       intent(in)    :: preproc_dims
   type(preproc_geoloc_s),     intent(in)    :: preproc_geoloc
   type(preproc_prtm_s),       intent(inout) :: preproc_prtm
   logical,                    intent(in)    :: verbose

   select case (ecmwf_flag)
   case(0)
      call read_ecmwf_grib(ecmwf_path_file,preproc_dims,preproc_geoloc, &
           preproc_prtm,verbose)
   case(1)
      if (verbose) write(*,*) 'Reading ecmwf path: ',trim(ecmwf_path_file)
      call read_ecmwf_nc(ecmwf_path_file,ecmwf,preproc_dims,preproc_geoloc, &
           preproc_prtm,verbose)

      if (verbose) write(*,*) 'Reading ecmwf path: ',trim(ecmwf_path_file2)
      call read_ecmwf_nc(ecmwf_path_file2,ecmwf,preproc_dims,preproc_geoloc, &
           preproc_prtm,verbose)

      if (verbose) write(*,*) 'Reading ecmwf path: ',trim(ecmwf_path_file3)
      call read_ecmwf_nc(ecmwf_path_file3,ecmwf,preproc_dims,preproc_geoloc, &
           preproc_prtm,verbose)
   case(2)
      if (verbose) write(*,*) 'Reading ecmwf path: ',trim(ecmwf_path_file)
      call read_ecmwf_nc(ecmwf_path_file,ecmwf,preproc_dims,preproc_geoloc, &
           preproc_prtm,verbose)

      if (verbose) write(*,*) 'Reading ecmwf path: ',trim(ecmwf_path_file2)
      call read_ecmwf_grib(ecmwf_path_file2,preproc_dims,preproc_geoloc, &
           preproc_prtm,verbose)

      if (verbose) write(*,*) 'Reading ecmwf path: ',trim(ecmwf_path_file3)
      call read_ecmwf_grib(ecmwf_path_file3,preproc_dims,preproc_geoloc, &
           preproc_prtm,verbose)
   case(3)
      if (verbose) write(*,*) 'Reading ecmwf path: ',trim(ecmwf_path_file)
      call read_ecmwf_nc(ecmwf_path_file,ecmwf,preproc_dims,preproc_geoloc, &
           preproc_prtm,verbose)
   end select

end subroutine read_ecmwf
