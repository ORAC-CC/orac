!-------------------------------------------------------------------------------
! Name: cox_munk_constants_m
!
! Purpose:
! Defines a module which defines the wavelength bands and associated default
! optical properties for the cox_munk_*() and get_ocean_colour() routines.
!
! History:
! 2016/07/12, GT: Separated out from cox_munk_m, so that these constants could
!                 be used by ocean_colour_m before cox_munk_m is defined (as
!                 cox_munk_m now depends on ocean_colour_m!)
! 2016/07/13, GT: Added in total absorption and backscatter arrsys.
! $Id:
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
   ! Wavelength dependent coefficients. Values are taken from the sources
   ! indicated in Sayer et al., 2010.
   !----------------------------------------------------------------------------
   integer,          parameter :: n_lambda = 8

   real(kind=sreal), parameter :: lambda(n_lambda)  = (/ 0.47,     0.55,    0.65,    0.87,    1.24,    1.60,    2.13,     3.7     /)

   ! Clean sea water refractive indices
   real(kind=sreal), parameter :: nr(n_lambda)      = (/ 1.345,    1.341,   1.338,   1.334,   1.327,   1.323,   1.313e+0, 1.374   /)
!  real(kind=sreal), parameter :: ni(n_lambda)      = (/ 9.55e-10, 1.96e-9, 2.23e-8, 3.91e-7, 3.55e-5, 8.55e-5, 5.73e-4,  3.60e-3 /)
   real(kind=sreal), parameter :: ni(n_lambda)      = (/ 9.55e-10, 1.96e-9, 1.64e-8, 3.71e-7, 3.55e-5, 8.55e-5, 5.73e-4,  3.60e-3 /)

   ! Water absorption at these wavelengths
!  real(kind=sreal), parameter :: baseabs(n_lambda) = (/ 0.016,    0.064,   0.410,   5.65,    360.0,   672.0,   3.38e+3,  1.22e4  /)
   real(kind=sreal), parameter :: baseabs(n_lambda) = (/ 0.016,    0.064,   0.350,   5.37,    360.0,   672.0,   3.38e+3,  1.22e4  /)

   ! Water back-scattering coefficients
!  real(kind=sreal), parameter :: basebsc(n_lambda) = (/ 3.84e-3,  1.93e-3, 8.87e-4, 2.65e-4, 6.22e-5, 1.91e-5, 6.28e-6,  5.00e-7 /)
   real(kind=sreal), parameter :: basebsc(n_lambda) = (/ 3.84e-3,  1.95e-3, 9.01e-4, 2.80e-4, 6.22e-5, 2.11e-5, 6.28e-6,  6.05e-7 /)

   ! White cap reflectance at same wavelengths
!  real(kind=sreal), parameter :: Rwc(n_lambda)     = (/ 0.4,      0.4,     0.4,     0.24,    0.071,   0.06,    0.0,      0.0     /)
   real(kind=sreal), parameter :: Rwc(n_lambda)     = (/ 0.44,     0.40,    0.35,    0.25,    0.071,   0.06,    0.0,      0.0     /)

   ! Coefficients required to calculate the chl-a absorption coefficient.
!  real(kind=sreal), parameter :: coef1(n_lambda)   = (/ 3.48e-2,  2.79e-3, 5.46e-3, 0.0,     0.0,     0.0,     0.0,      0.0     /)
!  real(kind=sreal), parameter :: coef2(n_lambda)   = (/ 1.27e-2,  6.40e-3, 8.50e-3, 0.0,     0.0,     0.0,     0.0,      0.0     /)
   real(kind=sreal), parameter :: coef1(n_lambda)   = (/ 3.48e-2,  2.79e-3, 4.71e-3, 0.0,     0.0,     0.0,     0.0,      0.0     /)
   real(kind=sreal), parameter :: coef2(n_lambda)   = (/ 1.27e-2,  6.40e-3, 7.50e-3, 0.0,     0.0,     0.0,     0.0,      0.0     /)

   ! Total absorption coefficients (calculated from above values...)
   real(kind=sreal), parameter :: totalabs(n_lambda)= (/ 2.70e-2,  6.59e-2, 3.53e-1, 5.37,  360.0,   672.0,     3.38e+3,  1.22e+4 /)
   ! Total backscatter coefficients (calculated from above values...)
   real(kind=sreal), parameter :: totalbsc(n_lambda)= (/ 3.79e-3,  2.60e-3, 1.86e-3, 1.25e-3, 8.69e-3, 7.07e-4, 5.78e-4,  4.19e-4 /)

end module cox_munk_constants_m
