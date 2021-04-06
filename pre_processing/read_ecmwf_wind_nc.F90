!-------------------------------------------------------------------------------
! Name: read_ecmwf_wind_nc.F90
!
! Purpose:
! Read surface wind components and lat/lon from ECMWF file. Intended for use
! with the interpolated BADC files in NCDF format (filename
! g[gam|gas|pam]YYYYMMDDHH00.nc). Successor to read_ecmwf_dimensions_nc.F90.
!
! Description and Algorithm details:
! 1) Set a|bvector values (set in interpolation).
! 2) Process each file.
!
! Arguments:
! Name       Type   In/Out/Both Description
! ------------------------------------------------------------------------------
! ecmwf_path string in   Full path to a ECMWF NCDF file to read.
! ecmwf2path string in   "
! ecmwf3path string in   "
! ecmwf      struct both Structure summarising contents of ECMWF files.
!
! History:
! 2014/05/07, AP: First version.
! 2014/07/24, GM: Nullify pointers to safely use the associated intrinsic.
! 2014/11/21, GM: Nullify recently added ecmwf%skin_temp.
! 2015/07/03, OS: added error status variable to nc_open call
! 2015/07/10, OS: undo previous commit
! 2015/11/17, OS: added reading of snow_depth and sea_ice_cover data
! 2016/01/29, GM: Add ecmwf_wind_init() and use it in read_ecmwf_wind_nc().
! 2016/02/02, OS: Now reads into HR ERA structure if flag is set.
! 2016/02/03, GM: Move ecmwf_wind_init() into ecmwf.F90.
! 2016/04/04, SP: Add option to process ECMWF forecast in single NetCDF4 file
!    Note: This should work with either the OPER or FCST streams from ECMWF.
! 2016/04/26, AP: Make paths 2 and 3 optional so this wrapper is more general.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine read_ecmwf_wind_nc(ecmwf, ecmwf_path, ecmwf_flag, ecmwf2path, ecmwf3path)

   use preproc_constants_m

   implicit none

   type(ecmwf_t),    intent(inout)        :: ecmwf
   character(len=*), intent(in)           :: ecmwf_path
   integer,          intent(in)           :: ecmwf_flag
   character(len=*), intent(in), optional :: ecmwf2path
   character(len=*), intent(in), optional :: ecmwf3path

   call ecmwf_wind_init(ecmwf)
   if (ecmwf_flag .le. 5 .or. ecmwf_flag .gt. 8) then
      call ecmwf_abvec_init(ecmwf)
   else
      allocate(ecmwf%avec(ecmwf%kdim))
      allocate(ecmwf%bvec(ecmwf%kdim))
   end if

   ! loop over given files (order not necessarily known)
   call read_ecmwf_wind_nc_file(ecmwf_path, ecmwf)
   if (present(ecmwf2path)) call read_ecmwf_wind_nc_file(ecmwf2path, ecmwf)
   if (present(ecmwf3path)) call read_ecmwf_wind_nc_file(ecmwf3path, ecmwf)

end subroutine read_ecmwf_wind_nc

!-------------------------------------------------------------------------------
! Name: read_ecmwf_wind_file
!
! Purpose:
! Read surface wind components and lat/lon from ECMWF file. Intended for use
! with the interpolated BADC files in NCDF format (filename
! g[gam|gas|pam]YYYYMMDDHH00.nc). Successor to read_ecmwf_dimensions_nc.F90.
!
! Description and Algorithm details:
! 1) Open file.
! 2) Search for desired dimensions. If size unknown, save. Otherwise, check
!    it is consistent with the previous value.
! 3) Search for desired variables. If not yet stored, allocate ecmwf structure
!    and store array.
! 4) Close file.
!
! Arguments:
! Name       Type   In/Out/Both Description
! ------------------------------------------------------------------------------
! ecmwf_path string in   Full path to a ECMWF NCDF file to read.
! ecmwf      struct both Structure summarising contents of ECMWF files.
!
! History:
! 2014/05/07, AP: First version.
! 2014/11/04, AP: Added skin_temp reading.
! 2015/11/17, OS: Added snow_depth and sea_ice_cover reading.
! 2016/04/26, AP: Merge with _dwd version.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine read_ecmwf_wind_nc_file(ecmwf_path, ecmwf)

   use orac_ncdf_m
   use preproc_constants_m

   implicit none

   character(len=*), intent(in)    :: ecmwf_path
   type(ecmwf_t),    intent(inout) :: ecmwf

   real, allocatable               :: val(:,:,:,:)
   integer                         :: fid, i, ndim, nvar, size
   character(len=var_length)       :: name

   ! open file
   call ncdf_open(fid, ecmwf_path, 'read_ecmwf_wind_nc_file()')

   ! check field dimensions for consistency
   if (nf90_inquire(fid, ndim, nvar) .ne. 0) &
        call h_e_e('wind_nc_file', 'Bad inquire.')
   do i = 1, ndim
      if (nf90_inquire_dimension(fid, i, name, size) .ne. 0) &
           call h_e_e('wind_nc_file', 'Bad dimension.')
      select case (name)
      case('lon', 'longitude')
         if (ecmwf%xdim .eq. 0) then
            ecmwf%xdim = size
         else
            if (ecmwf%xdim .ne. size) &
                 call h_e_e('wind_nc_file', 'Inconsistent lon.')
         end if
      case('lat', 'latitude')
         if (ecmwf%ydim .eq. 0) then
            ecmwf%ydim = size
         else
            if (ecmwf%ydim .ne. size) &
                 call h_e_e('wind_nc_file', 'Inconsistent lat.')
         end if
      case ('nhym', 'hybrid', 'hybrid_1')
         ! the vertical coordinate is incorrectly named in gpam so skip it
         if (name .eq. 'hybrid' .and. size .eq. 1) cycle

         if (ecmwf%kdim .eq. 0) then
            ecmwf%kdim = size
         else
            if (ecmwf%kdim .ne. size) &
                 call h_e_e('wind_nc_file', 'Inconsistent vertical.')
         end if
      end select
   end do

   ! read wind fields and geolocation from files
   do i = 1, nvar
      if (nf90_inquire_variable(fid, i, name) .ne. 0) &
           call h_e_e('wind_nc_file', 'Bad variable.')
      select case (name)
      case('longitude', 'lon')
         if (.not.associated(ecmwf%lon)) then
            allocate(ecmwf%lon(ecmwf%xdim))
            call ncdf_read_array(fid, name, ecmwf%lon)
         end if
      case('latitude', 'lat')
         if (.not.associated(ecmwf%lat)) then
            allocate(ecmwf%lat(ecmwf%ydim))
            call ncdf_read_array(fid, name, ecmwf%lat)
         end if
      case('U10', 'U10M')
         if (.not.associated(ecmwf%u10)) then
            allocate(ecmwf%u10(ecmwf%xdim,ecmwf%ydim))
            allocate(val(ecmwf%xdim,ecmwf%ydim,1,1))
            call ncdf_read_array(fid, name, val)
            ecmwf%u10 = val(:,:,1,1)
            deallocate(val)
         end if
      case('V10', 'V10M')
         if (.not.associated(ecmwf%v10)) then
            allocate(ecmwf%v10(ecmwf%xdim,ecmwf%ydim))
            allocate(val(ecmwf%xdim,ecmwf%ydim,1,1))
            call ncdf_read_array(fid, name, val)
            ecmwf%v10 = val(:,:,1,1)
            deallocate(val)
         end if
      case('SKT')
         if (.not.associated(ecmwf%skin_temp)) then
            allocate(ecmwf%skin_temp(ecmwf%xdim,ecmwf%ydim))
            allocate(val(ecmwf%xdim,ecmwf%ydim,1,1))
            call ncdf_read_array(fid, name, val)
            ecmwf%skin_temp = val(:,:,1,1)
            deallocate(val)
         end if
      case('SD', 'sd')
         if (.not.associated(ecmwf%snow_depth)) then
            allocate(ecmwf%snow_depth(ecmwf%xdim,ecmwf%ydim))
            allocate(val(ecmwf%xdim,ecmwf%ydim,1,1))
            call ncdf_read_array(fid, name, val)
            ecmwf%snow_depth = val(:,:,1,1)
            deallocate(val)
         end if
      case('CI', 'ci')
         if (.not.associated(ecmwf%sea_ice_cover)) then
            allocate(ecmwf%sea_ice_cover(ecmwf%xdim,ecmwf%ydim))
            allocate(val(ecmwf%xdim,ecmwf%ydim,1,1))
            call ncdf_read_array(fid, name, val)
            ecmwf%sea_ice_cover = val(:,:,1,1)
            deallocate(val)
         end if
      end select
   end do

   call ncdf_close(fid, 'read_wind_nc_file()')

end subroutine read_ecmwf_wind_nc_file
