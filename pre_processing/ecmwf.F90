!-------------------------------------------------------------------------------
! Name: ecmwf.F90
!
! Purpose:
! Define module of variables types which hold the ecmwf input data.
!
! History:
! 2012/01/10, MJ: Writes sample code for ERA Interim data.
! 2012/08/02, CP: Changed to accommodate badc netcdf files with different
!    dimensions
! 2013/10/23, AP: Tidying
! 2014/02/10, AP: Removed _nc_ structures as redundant. Shortened DIM names.
! 2014/05/06, AP: Simplified to just one structure.
! 2014/11/04, OS: Added skin temperature to ecmwf structure.
! 2014/11/19, GM: C #includes should use double quotes.
! 2014/02/04, OS: Added include of read_ecmwf_wind_dwd.F90.
! 2014/02/04, OS: Added snow_depth and sea_ice_cover fields for high res ERA
!    data.
! 2015/11/26, GM: Added dup_ecmwf_allocation() and linearly_combine_ecmwfs() to
!    facilitate linear interpolation between ecmwf_s structures.
! 2015/12/17, OS: Added low_res flag.
! 2016/02/03, GM: Added parameter arrays avec and bvec as they were being
!    duplicated in subroutines.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

module ecmwf_m

   use preproc_constants

   implicit none

   real(kind=sreal), parameter :: avec(61) = &
      [0.000000,     2.000000E+01, 3.842534E+01, 6.364780E+01, &
       9.563696E+01, 1.344833E+02, 1.805844E+02, 2.347791E+02, &
       2.984958E+02, 3.739719E+02, 4.646182E+02, 5.756511E+02, &
       7.132180E+02, 8.836604E+02, 1.094835E+03, 1.356475E+03, &
       1.680640E+03, 2.082274E+03, 2.579889E+03, 3.196422E+03, &
       3.960292E+03, 4.906707E+03, 6.018020E+03, 7.306633E+03, &
       8.765055E+03, 1.037612E+04, 1.207745E+04, 1.377532E+04, &
       1.537980E+04, 1.681947E+04, 1.804518E+04, 1.902770E+04, &
       1.975511E+04, 2.022220E+04, 2.042986E+04, 2.038448E+04, &
       2.009740E+04, 1.958433E+04, 1.886475E+04, 1.796136E+04, &
       1.689947E+04, 1.570645E+04, 1.441112E+04, 1.304322E+04, &
       1.163276E+04, 1.020950E+04, 8.802355E+03, 7.438805E+03, &
       6.144316E+03, 4.941777E+03, 3.850913E+03, 2.887697E+03, &
       2.063780E+03, 1.385913E+03, 8.553618E+02, 4.673335E+02, &
       2.103939E+02, 6.588924E+01, 7.367743,     0.000000,     &
       0.000000 ]
   real(kind=sreal), parameter :: bvec(61) = &
      [0.0000000,     0.0000000,     0.0000000,     0.0000000,     &
       0.0000000,     0.0000000,     0.0000000,     0.0000000,     &
       0.0000000,     0.0000000,     0.0000000,     0.0000000,     &
       0.0000000,     0.0000000,     0.0000000,     0.0000000,     &
       0.0000000,     0.0000000,     0.0000000,     0.0000000,     &
       0.0000000,     0.0000000,     0.0000000,     0.0000000,     &
       7.5823496E-05, 4.6139490E-04, 1.8151561E-03, 5.0811172E-03, &
       1.1142910E-02, 2.0677876E-02, 3.4121163E-02, 5.1690407E-02, &
       7.3533833E-02, 9.9674702E-02, 0.1300225,     0.1643843,     &
       0.2024759,     0.2439331,     0.2883230,     0.3351549,     &
       0.3838921,     0.4339629,     0.4847715,     0.5357099,     &
       0.5861684,     0.6355475,     0.6832686,     0.7287858,     &
       0.7715966,     0.8112534,     0.8473749,     0.8796569,     &
       0.9078839,     0.9319403,     0.9518215,     0.9676452,     &
       0.9796627,     0.9882701,     0.9940194,     0.9976301,     &
       1.0000000 ]

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


subroutine ecmwf_wind_init(ecmwf)

   implicit none

   type(ecmwf_s), intent(out) :: ecmwf

   ecmwf%xdim=0
   ecmwf%ydim=0
   ecmwf%kdim=0

   nullify(ecmwf%lon)
   nullify(ecmwf%lat)
   nullify(ecmwf%u10)
   nullify(ecmwf%v10)
   nullify(ecmwf%skin_temp)
   nullify(ecmwf%snow_depth)
   nullify(ecmwf%sea_ice_cover)

end subroutine ecmwf_wind_init


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
