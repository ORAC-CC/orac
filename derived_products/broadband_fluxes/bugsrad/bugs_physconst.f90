

module bugsrad_physconst

   use kinds
   implicit none
   private

   ! Physical and math constants
   real (kind=dbl_kind), parameter, public ::      &
      gravity              = 9.80665_dbl_kind      &! (m s^-2)
     ,cp_dry_air           = 1.004e+03_dbl_kind    &! (J kg^-1 K^-1)
     ,R_d                  = 287.0_dbl_kind        &! (J K^-1 kg^-1)
     ,R_star               = 8.3143e+03_dbl_kind   &! (J K^-1 kmol^-1)
     ,sol_const            = 1.360e+03_dbl_kind    &! (W m^-2)
     ,P_std                = 1.01325e+05_dbl_kind  &! (Pa)
     ,T_std                = 273.15_dbl_kind       &! (K)
     ,PI                   = 3.1415926535_dbl_kind &! (-)
     ,molar_volume         = 2.2421e+4_dbl_kind    &! (cm3-atm)
     ,N_av                 = 6.0221367e23           ! (molecules/mol)


   ! Molecular weights (g/mol)
   real (kind=dbl_kind), parameter, public ::      &
     MW_dry_air            = 28.964_dbl_kind       &
    ,MW_h2o                = 18.016_dbl_kind       &
    ,MW_co2                = 44.010_dbl_kind       &
    ,MW_o3                 = 48.000_dbl_kind       &
    ,MW_ch4                = 16.042_dbl_kind       &
    ,MW_n2o                = 44.016_dbl_kind

   !Other
   integer (kind=int_kind), parameter, public:: &
     mbs                   = 6                 !Number of shortwave spectral bands
   real (kind=dbl_kind), parameter, public ::                &
     epsilon               = MW_h2o/MW_dry_air               &
    ,f_virt                = (1._dbl_kind - epsilon)/epsilon

   real (kind=dbl_kind), dimension(mbs), parameter, public :: &
     ri = (/ 1.19234e-26, 7.6491e-28, 9.0856e-29, 1.97266e-29 &
             ,6.13005e-30, 2.06966e-30 /)

end module bugsrad_physconst
