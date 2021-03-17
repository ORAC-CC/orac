!-------------------------------------------------------------------------------
! Name: surface_reflectance.F90
!
! Purpose:
! Module for surface reflectance subroutines.
!
! History:
! 2014/05/23, GM: First version.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

module surface_reflectance_m

   implicit none

contains

#include "get_surface_reflectance.F90"
#include "select_modis_albedo_file.F90"


!-------------------------------------------------------------------------------
! Name: read_swansea_climatology()
!
! Purpose:
! Reads a climatology of the s parameters of the Swansea surface reflectance
! model from a file. Abuses the mcd43c3 structure to output the data to
! simplify get_surface_reflectance().
!
! Description and Algorithm details:
! 1) Open file and copy arrays.
!
! Arguments:
! Name               Type    In/Out/Both Description
! ------------------------------------------------------------------------------
! swansea_surf_path  string  in  Full path to the Swansea climatology
! nbands             int     in  Number of bands to open
! bands              int arr in  List of band numbers to open
! data               mcd43c3 out Structure to hold output arrays.
!
! History:
! 2018/08/01, AP: First version.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine read_swansea_climatology(swansea_surf_path, nbands, bands, &
     data)

   use mcd43c_m
   use orac_ncdf_m
#ifdef __PGI
   use ieee_arithmetic
#endif
   implicit none

   character(len=*),   intent(in)  :: swansea_surf_path
   integer,            intent(in)  :: nbands
   integer,            intent(in)  :: bands(:)
   type(mcd43c3_t),    intent(out) :: data

   integer                         :: fid, i
   integer                         :: ndim, ntmp
   real, allocatable, dimension(:) :: dim_values
   character(len=var_length)       :: name

   ! Use MODIS channel ordering as we're borrowing MCD43C3 code
   integer,              parameter :: n_swanbands = 6, n_swanviews = 2
   character(len=14),    parameter :: swan_s_band_names(n_swanbands) = [ &
        'surface_s_670 ', 'surface_s_870 ', '              ', 'surface_s_550 ', &
        '              ', 'surface_s_1600']
   character(len=17),    parameter :: swan_p_band_names(n_swanviews) = [ &
        'surface_p_nadir  ', 'surface_p_forward']


   data%nbands = nbands
   allocate(data%bands(data%nbands))
   data%bands = bands

   call ncdf_open(fid, swansea_surf_path, 'read_swansea_climatology()')

   ! Read length of dimensions
   if (nf90_inquire(fid, ndim) /= NF90_NOERR) call h_s_e('Bad inquire.')
   do i = 1, ndim
      if (nf90_inquire_dimension(fid, i, name, ntmp) /= NF90_NOERR) &
           call h_s_e('Bad dimension.')
      select case (name)
      case('lon')
         data%nlon = ntmp
      case('lat')
         data%nlat = ntmp
      end select
   end do

   ! Read edges of grid
   allocate(dim_values(data%nlon))
   call ncdf_read_array(fid, 'lon', dim_values)
   data%lon0 = dim_values(1)
   data%lon_invdel = 1.0 / (dim_values(2) - dim_values(1))
   deallocate(dim_values)

   allocate(dim_values(data%nlat))
   call ncdf_read_array(fid, 'lat', dim_values)
   data%lat0 = dim_values(1)
   data%lat_invdel = 1.0 / (dim_values(2) - dim_values(1))
   deallocate(dim_values)

   ! Read Swansea s climatology fields
   allocate(data%wsa(data%nlon, data%nlat, data%nbands))
   do i = 1, data%nbands
      call ncdf_read_array(fid, swan_s_band_names(bands(i)), data%wsa(:,:,i))
   end do
#ifdef __PGI
   where (ieee_is_nan(data%wsa)) data%wsa = sreal_fill_value
#else
   where (isnan(data%wsa)) data%wsa = sreal_fill_value
#endif

   ! Read Seansea p climatology fields
   allocate(data%bsa(data%nlon, data%nlat, n_swanviews))
   do i = 1, n_swanviews
      call ncdf_read_array(fid, swan_p_band_names(i), data%bsa(:,:,i))
   end do
#ifdef __PGI
    where (ieee_is_nan(data%bsa)) data%bsa = sreal_fill_value
#else
   where (isnan(data%bsa)) data%bsa = sreal_fill_value
#endif

   ! Placeholders because we're borrowing variables from mcd43c3
   allocate(data%quality(1,1))
   allocate(data%local_solar_noon(1,1))
   allocate(data%percent_inputs(1,1))
   allocate(data%percent_snow(1,1))
   allocate(data%bandids(1))

   call ncdf_close(fid, 'read_swansea_climatology()')

end subroutine read_swansea_climatology


! handle_swansea_error()
subroutine h_s_e(message)

   use common_constants_m, only: error_stop_code
   implicit none

   character(len=*), intent(in) :: message

   write(*,*) 'ERROR: get_swansea_surface_reflectance(): ' // trim(message)
   stop error_stop_code

end subroutine h_s_e

end module surface_reflectance_m
