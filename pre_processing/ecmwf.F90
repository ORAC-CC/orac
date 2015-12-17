!-------------------------------------------------------------------------------
! Name: ecmwf.F90
!
! Purpose:
! Define module of variables types which hold the ecmwf input data.
!
! History:
! 2012/01/10, MJ: writes sample code for ERA Interim data.
! 2012/08/02, CP: changed to accommodate badc netcdf files with different
!    dimensions
! 2013/10/23, AP: Tidying
! 2014/02/10, AP: removed _nc_ structures as redundant. Shortened DIM names.
! 2014/05/06, AP: Simplified to just one structure.
! 2014/11/04, OS: Added skin temperature to ecmwf structure.
! 2014/11/19, GM: C #includes should use double quotes.
! 2014/02/04, OS: Added include of read_ecmwf_wind_dwd.F90.
! 2014/02/04, OS: Added snow_depth and sea_ice_cover fields for high res ERA
!    data.
! 2015/11/26, GM: Added dup_ecmwf_allocation() and linearly_combine_ecmwfs() to
!    facilitate linear interpolation between ecmwf_s structures.
! 2015/12/17, OS: Added low_res flag.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

module ecmwf_m

   use preproc_constants

   implicit none

   type ecmwf_s
      integer(kind=lint)                        :: xdim,ydim,kdim
      real(kind=sreal), dimension(:),   pointer :: lat,lon
      real(kind=sreal), dimension(:),   pointer :: avec,bvec
      real(kind=sreal), dimension(:,:), pointer :: u10,v10,skin_temp,snow_depth,sea_ice_cover
   end type ecmwf_s

contains

#include "deallocate_ecmwf_structures.F90"
#include "compute_geopot_coordinate.F90"
#include "read_ecmwf.F90"
#include "read_ecmwf_wind_nc.F90"
#include "read_ecmwf_wind_grib.F90"
#include "read_ecmwf_wind_badc.F90"
#include "read_ecmwf_wind_dwd.F90"
#include "read_ecmwf_nc.F90"
#include "read_ecmwf_grib.F90"
#include "rearrange_ecmwf.F90"


subroutine dup_ecmwf_allocation(ecmwf, ecmwf2, low_res)

   implicit none

   type(ecmwf_s), intent(in)  :: ecmwf
   type(ecmwf_s), intent(out) :: ecmwf2
   logical,       intent(in)  :: low_res

   ecmwf2%xdim = ecmwf%xdim
   ecmwf2%ydim = ecmwf%ydim
   ecmwf2%kdim = ecmwf%kdim

   allocate(ecmwf2%avec(61))
   allocate(ecmwf2%bvec(61))
   allocate(ecmwf2%lon(ecmwf%xdim))
   allocate(ecmwf2%lat(ecmwf%ydim))
   if (low_res) allocate(ecmwf2%u10(ecmwf%xdim,ecmwf%ydim))
   if (low_res) allocate(ecmwf2%v10(ecmwf%xdim,ecmwf%ydim))
   allocate(ecmwf2%skin_temp(ecmwf%xdim,ecmwf%ydim))
   allocate(ecmwf2%snow_depth(ecmwf%xdim,ecmwf%ydim))
   allocate(ecmwf2%sea_ice_cover(ecmwf%xdim,ecmwf%ydim))

end subroutine dup_ecmwf_allocation


subroutine linearly_combine_ecmwfs(a, b, ecmwf1, ecmwf2, ecmwf, low_res)

   implicit none

   real,          intent(in)  :: a
   real,          intent(in)  :: b
   type(ecmwf_s), intent(in)  :: ecmwf1
   type(ecmwf_s), intent(in)  :: ecmwf2
   type(ecmwf_s), intent(out) :: ecmwf
   logical,       intent(in)  :: low_res

   ecmwf%lat              = a * ecmwf1%lat           + b * ecmwf2%lat
   ecmwf%lon              = a * ecmwf1%lon           + b * ecmwf2%lon
   ecmwf%avec             = a * ecmwf1%avec          + b * ecmwf2%avec
   ecmwf%bvec             = a * ecmwf1%bvec          + b * ecmwf2%bvec
   if (low_res) ecmwf%u10 = a * ecmwf1%u10           + b * ecmwf2%u10
   if (low_res) ecmwf%v10 = a * ecmwf1%v10           + b * ecmwf2%v10
   ecmwf%skin_temp        = a * ecmwf1%skin_temp     + b * ecmwf2%skin_temp
   ecmwf%snow_depth       = a * ecmwf1%snow_depth    + b * ecmwf2%snow_depth
   ecmwf%sea_ice_cover    = a * ecmwf1%sea_ice_cover + b * ecmwf2%sea_ice_cover

end subroutine linearly_combine_ecmwfs

end module ecmwf_m
