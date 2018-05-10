!-------------------------------------------------------------------------------
! Name: cox_munk_constants_m
!
! Purpose:
! Defines a module which defines the wavelength bands and associated default
! optical properties for the cox_munk_*() and get_ocean_colour() routines.
!
! History:
! 2016/07/12, GT: Separated out from cox_munk_m, so that these constants could
!    be used by ocean_colour_m before cox_munk_m is defined (as cox_munk_m now
!    depends on ocean_colour_m!)
! 2016/07/13, GT: Added in total absorption and backscatter arrays.
! 2016/12/08, GM: Added constants for 1.375 um and formally recomputed constants
!    using an external program to find a few small errors.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------
module cox_munk_constants_m

   use preproc_constants_m

   implicit none

   ! Approximate median chlorophyll-A concentration from GlobCOLOUR (mg/m3)
   real(kind=sreal), parameter :: chlconc = 0.18

   ! Refractive index of air
   real(kind=sreal), parameter :: n_air = 1.00029

   !----------------------------------------------------------------------------
   ! Wavelength dependent coefficients. Values are taken and/or computed from
   ! the sources indicated in Sayer et al., 2010.
   !----------------------------------------------------------------------------
   integer,          parameter :: n_lambda = 9

   real(kind=sreal), parameter :: lambda(n_lambda)  = (/ 4.700e-01, 5.500e-01, 6.500e-01, 8.700e-01, 1.240e+00, 1.375e+00, 1.600e+00, 2.130e+00, 3.700e+00 /)

   ! Clean sea water refractive indices (n, k)
   real(kind=sreal), parameter :: nr(n_lambda)      = (/ 1.345e+00, 1.341e+00, 1.338e+00, 1.334e+00, 1.327e+00, 1.325e+00, 1.323e+00, 1.313e+00, 1.374e+00 /)
   real(kind=sreal), parameter :: ni(n_lambda)      = (/ 9.520e-10, 1.960e-09, 1.640e-08, 3.714e-07, 3.551e-05, 1.220e-04, 8.550e-05, 5.729e-04, 3.600e-03 /)

   ! Water absorption at these wavelengths, a_w
   real(kind=sreal), parameter :: baseabs(n_lambda) = (/ 1.600e-02, 6.400e-02, 3.500e-01, 5.365e+00, 3.599e+02, 1.115e+03, 6.715e+02, 3.380e+03, 1.223e+04 /)

   ! Water back-scattering coefficients, b_w
   real(kind=sreal), parameter :: basebsc(n_lambda) = (/ 3.780e-03, 1.930e-03, 9.379e-04, 2.662e-04, 5.759e-05, 3.685e-05, 1.915e-05, 5.563e-06, 5.120e-07 /)

   ! White cap reflectance at same wavelengths (rho_wc or Rwc)
   real(kind=sreal), parameter :: Rwc(n_lambda)     = (/ 4.408e-01, 4.024e-01, 3.544e-01, 2.488e-01, 7.120e-02, 6.400e-03, 0.000e+00, 0.000e+00, 0.000e+00 /)

   ! Coefficients required to calculate the chl-a absorption coefficient (U, a2)
   real(kind=sreal), parameter :: coef1(n_lambda)   = (/ 3.460e-02, 2.790e-03, 3.286e-03, 0.000e+00, 0.000e+00, 0.000e+00, 0.000e+00, 0.000e+00, 0.000e+00 /)
   real(kind=sreal), parameter :: coef2(n_lambda)   = (/ 1.240e-02, 6.400e-03, 5.200e-03, 0.000e+00, 0.000e+00, 0.000e+00, 0.000e+00, 0.000e+00, 0.000e+00 /)

   ! Total absorption coefficients (calculated from above values...)
   real(kind=sreal), parameter :: totalabs(n_lambda)= (/ 2.694e-02, 6.585e-02, 3.518e-01, 5.365e+00, 3.599e+02, 1.115e+03, 6.715e+02, 3.380e+03, 1.223e+04 /)
   ! Total backscatter coefficients (calculated from above values...)
   real(kind=sreal), parameter :: totalbsc(n_lambda)= (/ 3.761e-03, 2.594e-03, 1.879e-03, 1.239e-03, 8.667e-04, 7.944e-04, 7.056e-04, 5.771e-04, 4.188e-04 /)

end module cox_munk_constants_m
