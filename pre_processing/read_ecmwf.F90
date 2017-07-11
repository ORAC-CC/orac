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
! 2016/02/03, GM: Use read_ecmwf_wind_grib() to read in the HR data in case 2.
! 2016/02/03, GM: Set the fill_value for sea_ice_cover to sreal_fill_value.
! 2016/04/03, SP: Add option to process ECMWF forecast in single NetCDF4 file
!    Note: This should work with either the OPER or FCST streams from ECMWF.
! 2016/04/26, AP: There are no high res files compatible with ecmwf_flag=1.
!    Merge _dwd routines with _nc.
! 2017/02/04, SP: Add ecmwf_flag=5, for reading NOAA GFS forecast (EKWork)
! 2017/04/11, SP: Added ecmwf_flag=6, for working with GFS analysis files.
! 2017/06/21, OS: inout declaration bug fix for cray-fortran compiler
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine read_ecmwf_wind(ecmwf_flag, ecmwf_path_file, ecmwf_HR_path_file, &
   ecmwf_path_file2, ecmwf_path_file3, ecmwf, ecmwf_HR, use_hr_ecmwf, &
   ecmwf_nlevels, verbose)

   use preproc_structures_m

   implicit none

   integer,                    intent(in)  :: ecmwf_flag
   character(len=path_length), intent(in)  :: ecmwf_path_file
   character(len=path_length), intent(in)  :: ecmwf_HR_path_file
   character(len=path_length), intent(in)  :: ecmwf_path_file2
   character(len=path_length), intent(in)  :: ecmwf_path_file3
   type(ecmwf_t),              intent(inout) :: ecmwf
   type(ecmwf_t),              intent(inout) :: ecmwf_HR
   logical,                    intent(in)  :: use_hr_ecmwf
   integer,                    intent(in)  :: ecmwf_nlevels
   logical,                    intent(in)  :: verbose

   ! Set the number of levels in the input file, defaults to 61
   select case(ecmwf_nlevels)
   case(31)
      ecmwf%kdim=31
   case(60)
      ecmwf%kdim=60
   case(91)
      ecmwf%kdim=91
   case(137)
      ecmwf%kdim=137
   case default
      ecmwf%kdim=60
   end select

   select case (ecmwf_flag)
   case(0)
      call read_ecmwf_wind_grib(ecmwf_path_file,ecmwf,.false.,ecmwf_flag)
      if (verbose) write(*,*)'ecmwf_dims grib: ',ecmwf%xdim,ecmwf%ydim
      if (use_hr_ecmwf) then
         call read_ecmwf_wind_grib(ecmwf_HR_path_file,ecmwf_HR,.true.,ecmwf_flag)
      end if
   case(1)
      call read_ecmwf_wind_nc(ecmwf,ecmwf_path_file,ecmwf_path_file2, &
           ecmwf_path_file3)
      if (verbose) write(*,*)'ecmwf_dims ncdf: ',ecmwf%xdim,ecmwf%ydim
      if (use_hr_ecmwf) then
         call read_ecmwf_wind_grib(ecmwf_HR_path_file,ecmwf_HR,.true.,ecmwf_flag)
      end if
   case(2)
      call read_ecmwf_wind_badc(ecmwf_path_file,ecmwf_path_file2, &
           ecmwf_path_file3,ecmwf)
      if (verbose) write(*,*)'ecmwf_dims badc: ',ecmwf%xdim,ecmwf%ydim
      if (use_hr_ecmwf) then
         call read_ecmwf_wind_grib(ecmwf_HR_path_file,ecmwf_HR,.true.,ecmwf_flag)
      end if
   case(3)
      call read_ecmwf_wind_nc(ecmwf,ecmwf_path_file)
      if (verbose) write(*,*)'ecmwf_dims ncdf: ',ecmwf%xdim,ecmwf%ydim,ecmwf%kdim
      if (use_hr_ecmwf) then
         call read_ecmwf_wind_nc(ecmwf_HR,ecmwf_HR_path_file)
      end if
   case(4)
      call read_ecmwf_wind_nc(ecmwf,ecmwf_path_file)
      if (verbose) write(*,*)'ecmwf_dims ncdf: ',ecmwf%xdim,ecmwf%ydim
      if (use_hr_ecmwf) then
         call read_ecmwf_wind_nc(ecmwf_HR,ecmwf_HR_path_file)
      end if
   case(5)
      call read_ecmwf_wind_grib(ecmwf_path_file,ecmwf,.false.,ecmwf_flag)
      if (verbose) write(*,*)'ecmwf_dims grib: ',ecmwf%xdim,ecmwf%ydim
   case(6)
      call read_ecmwf_wind_grib(ecmwf_path_file,ecmwf,.false.,ecmwf_flag)
      if (verbose) write(*,*)'ecmwf_dims grib: ',ecmwf%xdim,ecmwf%ydim
   case(7)
      call read_ecmwf_wind_grib(ecmwf_path_file,ecmwf,.false.,ecmwf_flag)
      if (verbose) write(*,*)'ecmwf_dims grib: ',ecmwf%xdim,ecmwf%ydim
   case default
      write(*,*) "Incorrect ECMWF flag, must be between 0-7."
      stop
   end select
   if (verbose) then
      write(*,*) 'U10) Min: ',minval(ecmwf%u10),', Max: ',maxval(ecmwf%u10)
      write(*,*) 'V10) Min: ',minval(ecmwf%v10),', Max: ',maxval(ecmwf%v10)
      write(*,*) 'SKINT) Min: ',minval(ecmwf%skin_temp),', Max: ', &
           maxval(ecmwf%skin_temp)
   end if

   ! It is possible for this field to have fill.  It is set to -9e+33 for DWD
   ! and 9999.0 for Ox/RAL.  Here we set it to ORAC's value.
   where (ecmwf%sea_ice_cover .lt. 0.0 .or. ecmwf%sea_ice_cover .gt. 1.0) &
      ecmwf%sea_ice_cover = sreal_fill_value
   if (use_hr_ecmwf) then
      where (ecmwf_HR%sea_ice_cover .lt. 0.0 .or. &
             ecmwf_HR%sea_ice_cover .gt. 1.0) &
         ecmwf_HR%sea_ice_cover = sreal_fill_value
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
! 2016/04/04, SP: Added option 4, which processes ECMWF forecast/analysis data
!    that's stored in a single NetCDF file.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine read_ecmwf(ecmwf_flag, ecmwf_path_file, ecmwf_path_file2, &
   ecmwf_path_file3, ecmwf, preproc_dims, preproc_geoloc, preproc_prtm, verbose)

   use preproc_structures_m

   implicit none

   integer,                    intent(in)    :: ecmwf_flag
   character(len=path_length), intent(in)    :: ecmwf_path_file
   character(len=path_length), intent(in)    :: ecmwf_path_file2
   character(len=path_length), intent(in)    :: ecmwf_path_file3
   type(ecmwf_t),              intent(in)    :: ecmwf
   type(preproc_dims_t),       intent(in)    :: preproc_dims
   type(preproc_geoloc_t),     intent(in)    :: preproc_geoloc
   type(preproc_prtm_t),       intent(inout) :: preproc_prtm
   logical,                    intent(in)    :: verbose

   select case (ecmwf_flag)
   case(0)
      if (verbose) write(*,*) 'Reading ecmwf path: ',trim(ecmwf_path_file)
      call read_ecmwf_grib(ecmwf_path_file,preproc_dims,preproc_geoloc, &
           preproc_prtm,verbose)
   case(1)
      if (verbose) write(*,*) 'Reading ecmwf path: ',trim(ecmwf_path_file)
      call read_ecmwf_nc(ecmwf_path_file,ecmwf,preproc_dims,preproc_geoloc, &
           preproc_prtm,verbose,ecmwf_flag)

      if (verbose) write(*,*) 'Reading ecmwf path: ',trim(ecmwf_path_file2)
      call read_ecmwf_nc(ecmwf_path_file2,ecmwf,preproc_dims,preproc_geoloc, &
           preproc_prtm,verbose,ecmwf_flag)

      if (verbose) write(*,*) 'Reading ecmwf path: ',trim(ecmwf_path_file3)
      call read_ecmwf_nc(ecmwf_path_file3,ecmwf,preproc_dims,preproc_geoloc, &
           preproc_prtm,verbose,ecmwf_flag)
   case(2)
      if (verbose) write(*,*) 'Reading ecmwf path: ',trim(ecmwf_path_file)
      call read_ecmwf_nc(ecmwf_path_file,ecmwf,preproc_dims,preproc_geoloc, &
           preproc_prtm,verbose,ecmwf_flag)

      if (verbose) write(*,*) 'Reading ecmwf path: ',trim(ecmwf_path_file2)
      call read_ecmwf_grib(ecmwf_path_file2,preproc_dims,preproc_geoloc, &
           preproc_prtm,verbose)

      if (verbose) write(*,*) 'Reading ecmwf path: ',trim(ecmwf_path_file3)
      call read_ecmwf_grib(ecmwf_path_file3,preproc_dims,preproc_geoloc, &
           preproc_prtm,verbose)
   case(3)
      if (verbose) write(*,*) 'Reading ecmwf path: ',trim(ecmwf_path_file)
      call read_ecmwf_nc(ecmwf_path_file,ecmwf,preproc_dims,preproc_geoloc, &
           preproc_prtm,verbose,ecmwf_flag)
   case(4)
      if (verbose) write(*,*) 'Reading ecmwf path: ',trim(ecmwf_path_file)
      call read_ecmwf_nc(ecmwf_path_file,ecmwf,preproc_dims,preproc_geoloc, &
           preproc_prtm,verbose,ecmwf_flag)
   case(5)
      if (verbose) write(*,*) 'Reading gfs path: ',trim(ecmwf_path_file)
      call read_gfs_grib(ecmwf_path_file,preproc_dims,preproc_geoloc, &
           preproc_prtm,verbose)
   case(6)
      if (verbose) write(*,*) 'Reading gfs path: ',trim(ecmwf_path_file)
      call read_gfs_grib(ecmwf_path_file,preproc_dims,preproc_geoloc, &
           preproc_prtm,verbose)
   case(7)
      if (verbose) write(*,*) 'Reading gfs path: ',trim(ecmwf_path_file)
      call read_gfs_grib(ecmwf_path_file,preproc_dims,preproc_geoloc, &
           preproc_prtm,verbose)
   end select


end subroutine read_ecmwf
