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
! 2016/04/26, AP: There are no high res files compatible with nwp_flag=1.
!    Merge _dwd routines with _nc.
! 2017/02/04, SP: Add nwp_flag=5, for reading NOAA GFS forecast (ExtWork)
! 2017/04/11, SP: Added nwp_flag=6, for working with GFS analysis files.
! 2017/06/21, OS: inout declaration bug fix for cray-fortran compiler
! 2018/11/05, SP: Switch nwp_flag=5 from GFS (dead code) to ECMWF ERA5
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine read_ecmwf_wind(nwp_flag, nwp_fnames, idx, ecmwf, &
   nwp_nlevels, verbose)

   use preproc_structures_m

   implicit none

   integer,          intent(in)              :: nwp_flag
   type(preproc_nwp_fnames_t), intent(inout) :: nwp_fnames
   integer,          intent(in)              :: idx
   type(ecmwf_t),    intent(inout)           :: ecmwf
   integer,          intent(in)              :: nwp_nlevels
   logical,          intent(in)              :: verbose
   
   ! Set the number of levels in the input file, defaults to 61
   select case(nwp_nlevels)
   case(31)
      ecmwf%kdim = 31
   case(60)
      ecmwf%kdim = 60
   case(91)
      ecmwf%kdim = 91
   case(137)
      ecmwf%kdim = 137
   case default
      ecmwf%kdim = 60
   end select

   select case (nwp_flag)
   case(0)
      call read_ecmwf_wind_grib(nwp_fnames%nwp_path_file(idx), ecmwf, nwp_flag)
      if (verbose) write(*,*)'ecmwf_dims grib: ', ecmwf%xdim, ecmwf%ydim
   case(1)
      call read_ecmwf_wind_nc(ecmwf, nwp_fnames%nwp_path_file(idx), nwp_flag)
      if (verbose) write(*,*)'ecmwf_dims ncdf: ', ecmwf%xdim, ecmwf%ydim
   case(2)
      call read_era5_jasmin_wind_nc(ecmwf, nwp_fnames, idx, nwp_flag)
      if (verbose) write(*,*)'ecmwf_dims ncdf: ', ecmwf%xdim, ecmwf%ydim
   case(3)
      call read_ecmwf_wind_nc(ecmwf, nwp_fnames%nwp_path_file(idx), nwp_flag)
      if (verbose) write(*,*)'ecmwf_dims ncdf: ', ecmwf%xdim, ecmwf%ydim, ecmwf%kdim
   case(4)
      call read_ecmwf_wind_badc(nwp_fnames%nwp_path_file(idx), nwp_fnames%nwp_path_file2(idx), &
           nwp_fnames%nwp_path_file3(idx), ecmwf)
      if (verbose) write(*,*)'ecmwf_dims badc: ', ecmwf%xdim, ecmwf%ydim
   case default
      write(*,*) "Incorrect ECMWF flag, must be between 0-4."
      stop
   end select
   if (verbose) then
      write(*,*) 'U10) Min: ', minval(ecmwf%u10), ', Max: ', maxval(ecmwf%u10)
      write(*,*) 'V10) Min: ', minval(ecmwf%v10), ', Max: ', maxval(ecmwf%v10)
      write(*,*) 'SKINT) Min: ', minval(ecmwf%skin_temp), ', Max: ', &
           maxval(ecmwf%skin_temp)
   end if

   ! It is possible for this field to have fill.  It is set to -9e+33 for DWD
   ! and 9999.0 for Ox/RAL.  Here we set it to ORAC's value.
   where (ecmwf%sea_ice_cover .lt. 0.0 .or. ecmwf%sea_ice_cover .gt. 1.0) &
      ecmwf%sea_ice_cover = sreal_fill_value

   call rearrange_ecmwf(ecmwf)

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
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine read_ecmwf(nwp_flag,nwp_fnames, idx, ecmwf, preproc_dims, &
                      preproc_geoloc, preproc_prtm, verbose)

   use preproc_structures_m

   implicit none

   integer,                intent(in)        :: nwp_flag
   type(preproc_nwp_fnames_t), intent(inout) :: nwp_fnames
   integer,          intent(in)              :: idx
   type(ecmwf_t),          intent(in)        :: ecmwf
   type(preproc_dims_t),   intent(in)        :: preproc_dims
   type(preproc_geoloc_t), intent(in)        :: preproc_geoloc
   type(preproc_prtm_t),   intent(inout)     :: preproc_prtm
   logical,                intent(in)        :: verbose

   select case (nwp_flag)
   case(0)
      if (verbose) write(*,*) 'Reading gfs path: ', trim(nwp_fnames%nwp_path_file(idx))
      call read_gfs_grib(nwp_fnames%nwp_path_file(idx), preproc_dims, preproc_geoloc, &
           preproc_prtm, verbose)
   case(1)
      if (verbose) write(*,*) 'Reading ECMWF path: ', trim(nwp_fnames%nwp_path_file(idx))
      call read_ecmwf_nc(nwp_fnames%nwp_path_file(idx), ecmwf, preproc_dims, preproc_geoloc, &
           preproc_prtm, verbose, nwp_flag)
   case(2)
      if (verbose) write(*,*) 'Reading JASMIN ERA5 path: ', trim(nwp_fnames%nwp_path_file(idx))
      call read_era5_jasmin_nc(nwp_fnames, idx, ecmwf, preproc_dims, preproc_geoloc, &
           preproc_prtm, verbose, nwp_flag)
   case(3)
      if (verbose) write(*,*) 'Reading ecmwf path: ', trim(nwp_fnames%nwp_path_file(idx))
      call read_ecmwf_nc(nwp_fnames%nwp_path_file(idx), ecmwf, preproc_dims, preproc_geoloc, &
           preproc_prtm, verbose, nwp_flag)
   case(4)
      if (verbose) write(*,*) 'Reading ecmwf path: ', trim(nwp_fnames%nwp_path_file(idx))
      call read_ecmwf_nc(nwp_fnames%nwp_path_file(idx), ecmwf, preproc_dims, preproc_geoloc, &
           preproc_prtm, verbose, nwp_flag)

      if (verbose) write(*,*) 'Reading ecmwf path: ', trim(nwp_fnames%nwp_path_file2(idx))
      call read_ecmwf_grib(nwp_fnames%nwp_path_file2(idx), preproc_dims, preproc_geoloc, &
           preproc_prtm, verbose)

      if (verbose) write(*,*) 'Reading ecmwf path: ', trim(nwp_fnames%nwp_path_file3(idx))
      call read_ecmwf_grib(nwp_fnames%nwp_path_file3(idx), preproc_dims, preproc_geoloc, &
           preproc_prtm, verbose)
   end select


end subroutine read_ecmwf
