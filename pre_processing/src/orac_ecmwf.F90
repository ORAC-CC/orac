! Name: orac_ecmwf.F90
!
!
! Purpose:
! A module containing the following routines for reading NetCDF ECMWF files:
!    COMPUTE_GEOPOT_COORDINATE - Calculates geopotential levels (phi_lev,
!       phi_lay) from surface pressure, temperature, and specific humidity.
!    READ_ECMWF_DIMENSIONS_NC  - Reads dimensions of ECMWF file.
!    READ_ECMWF_LAT_LAT_NC     - Reads latitude/longitude from GPAM file.
!    READ_ECMWF_NC             - Reads data field from file.
! See the specific source files for detailed header information for these
! procedures.
!
! History:
! 2014/02/10: AP Original version, combining the original files:
!                nc_read_file.F90, nc_open.F90
!
! $Id$
!
! Bugs:
! none known
!

module orac_ecmwf

   use netcdf
   use orac_ncdf
   use rearrange
   use ecmwf_structures
   use preproc_structures
   use preproc_constants

   implicit none

   real, dimension(61) :: avector=[ &
               0.0000000E+00,20.00000, 38.42534, 63.64780, &
              95.63696,     134.4833, 180.5844, 234.7791, 298.4958, &
             373.9719,      464.6182, 575.6511, 713.2180, 883.6604, &
            1094.835,      1356.475, 1680.640, 2082.274, 2579.889, &
            3196.422,      3960.292, 4906.707, 6018.020, 7306.633, &
            8765.055,     10376.12, 12077.45, 13775.32, 15379.80, &
           16819.47,      18045.18, 19027.70, 19755.11, 20222.20, &
           20429.86,      20384.48, 20097.40, 19584.33, 18864.75, & 
           17961.36,      16899.47, 15706.45, 14411.12, 13043.22, &
           11632.76,      10209.50,  8802.355, 7438.805, 6144.316, &
            4941.777,      3850.913, 2887.697, 2063.780, 1385.913, &
             855.3618,      467.3335, 210.3939,  65.88924,  7.367743, &
               0.0000000E+00, 0.0000000E+00 ]
   real, dimension(61) :: &
                   bvector= [ 0.0000000E+00,  0.0000000E+00 , 0.0000000E+00,  &
                 0.0000000E+00, 0.0000000E+00, 0.0000000E+00, 0.0000000E+00,  &
                 0.0000000E+00, 0.0000000E+00, 0.0000000E+00, 0.0000000E+00,  &
                 0.0000000E+00, 0.0000000E+00, 0.0000000E+00, 0.0000000E+00,  &
                 0.0000000E+00, 0.0000000E+00, 0.0000000E+00, 0.0000000E+00,  &
                 0.0000000E+00, 0.0000000E+00, 0.0000000E+00, 0.0000000E+00,  &
                 0.0000000E+00, 7.5823496E-05, 4.6139490E-04, 1.8151561E-03,  &
                 5.0811172E-03, 1.1142910E-02, 2.0677876E-02, 3.4121163E-02,  &
                 5.1690407E-02, 7.3533833E-02, 9.9674702E-02, 0.1300225,      &
                 0.1643843,     0.2024759,     0.2439331,     0.2883230,      &
                 0.3351549,     0.3838921,     0.4339629,     0.4847715,      &
                 0.5357099,     0.5861684,     0.6355475,     0.6832686,      &
                 0.7287858,     0.7715966,     0.8112534,     0.8473749,      &
                 0.8796569,     0.9078839,     0.9319403,     0.9518215,      &
                 0.9676452,     0.9796627,     0.9882701,     0.9940194,      &
                 0.9976301,     1.000000 ]
contains
   include "compute_geopot_coordinate.F90"

   include "read_ecmwf_dimensions_nc.F90"

   include "read_ecmwf_lat_lon_nc.F90"

   include "read_ecmwf_nc.F90"
end module orac_ecmwf
