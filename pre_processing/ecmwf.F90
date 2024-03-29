!-------------------------------------------------------------------------------
! Name: ecmwf.F90
!
! Purpose:
! Define module of variables types which hold the ecmwf input data.
!
!
! NOTES:
! The nwp_flag option allows the user to set what type of NWP data to use.
! There are five options for this flag:
! 0: NOAA GFS data in a single GRIB file.
! 1: ECMWF Operational or ERA5 data as a single netCDF4 file
! 2: ECMWF ERA5 data in JASMIN format (GRIB, one file per variable)
! 3: DWD format (unknown details)
! 4: ERA-Interim in JASMIN format, three files.
!-------------------------------------------------------------------------------

module ecmwf_m

   use preproc_constants_m

   implicit none

   type ecmwf_t
      integer(kind=lint)                        :: xdim,ydim,kdim
      real(kind=sreal), dimension(:),   pointer :: lat,lon
      real(kind=sreal), dimension(:),   pointer :: avec,bvec
      real(kind=sreal), dimension(:,:), pointer :: u10,v10,skin_temp, &
                                                   snow_depth,sea_ice_cover
   end type ecmwf_t

contains

#include "deallocate_ecmwf_structures.F90"
#include "compute_geopot_coordinate.F90"
#include "read_ecmwf.F90"
#include "read_era5_jasmin.F90"
#include "read_ecmwf_wind_nc.F90"
#include "read_ecmwf_wind_grib.F90"
#include "read_ecmwf_wind_badc.F90"
#include "read_ecmwf_nc.F90"
#include "read_ecmwf_grib.F90"
#include "read_gfs_nc.F90"
#include "read_gfs_grib.F90"
#include "rearrange_ecmwf.F90"

subroutine ecmwf_abvec_init(ecmwf)

   ! Set up the A and B vectors based upon the number
   ! of model levels. Should be input through the driver
   ! file. If not, the default is 60 levels (ERA-Interim)
   ! Other choices are: 91 and 137 (HRES model)

   implicit none
   type(ecmwf_t), intent(inout) :: ecmwf

   select case(ecmwf%kdim)
   case(60)
      allocate(ecmwf%avec(ecmwf%kdim+1))
      allocate(ecmwf%bvec(ecmwf%kdim+1))
      ecmwf%avec =&
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
      ecmwf%bvec =&
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
   case(91)
      allocate(ecmwf%avec(ecmwf%kdim+1))
      allocate(ecmwf%bvec(ecmwf%kdim+1))
      ecmwf%avec =&
      [0.0000000,    2.00004,      3.980832,     7.387186,     &
       12.908319,    21.413612,    33.952858,    51.746601,    &
       76.167656,    108.715561,   150.986023,   204.637451,   &
       271.356506,   352.824493,   450.685791,   566.519226,   &
       701.813354,   857.945801,   1036.166504,  1237.585449,  &
       1463.16394,   1713.709595,  1989.87439,   2292.155518,  &
       2620.898438,  2976.302246,  3358.425781,  3767.196045,  &
       4202.416504,  4663.776367,  5150.859863,  5663.15625,   &
       6199.839355,  6759.727051,  7341.469727,  7942.92627,   &
       8564.624023,  9208.305664,  9873.560547,  10558.881836, &
       11262.484375, 11982.662109, 12713.897461, 13453.225586, &
       14192.009766, 14922.685547, 15638.053711, 16329.560547, &
       16990.623047, 17613.28125,  18191.029297, 18716.96875,  &
       19184.544922, 19587.513672, 19919.796875, 20175.394531, &
       20348.916016, 20434.158203, 20426.21875,  20319.011719, &
       20107.03125,  19785.357422, 19348.775391, 18798.822266, &
       18141.296875, 17385.595703, 16544.585938, 15633.566406, &
       14665.645508, 13653.219727, 12608.383789, 11543.166992, &
       10471.310547, 9405.222656,  8356.25293,   7335.164551,  &
       6353.920898,  5422.802734,  4550.21582,   3743.464355,  &
       3010.146973,  2356.202637,  1784.854614,  1297.656128,  &
       895.193542,   576.314148,   336.772369,   162.043427,   &
       54.208336,    6.575628,     0.00316,      0.0000000]
      ecmwf%bvec = &
      [0.0000000,    0.0000000,    0.0000000,    0.0000000, &
       0.0000000,    0.0000000,    0.0000000,    0.0000000, &
       0.0000000,    0.0000000,    0.0000000,    0.0000000, &
       0.0000000,    0.0000000,    0.0000000,    0.0000000, &
       0.0000000,    0.0000000,    0.0000000,    0.0000000, &
       0.0000000,    0.0000000,    0.0000000,    0.0000000, &
       0.0000000,    0.0000000,    0.0000000,    0.0000000, &
       0.0000000,    0.0000000,    0.0000000,    0.0000000, &
       0.0000000,    0.0000000,    0.0000000,    0.000014,  &
       0.000055,     0.000131,     0.000279,     0.000548,  &
       0.001,        0.001701,     0.002765,     0.004267,  &
       0.006322,     0.009035,     0.012508,     0.01686,   &
       0.022189,     0.02861,      0.036227,     0.045146,  &
       0.055474,     0.067316,     0.080777,     0.095964,  &
       0.112979,     0.131935,     0.152934,     0.176091,  &
       0.20152,      0.229315,     0.259554,     0.291993,  &
       0.326329,     0.362203,     0.399205,     0.436906,  &
       0.475016,     0.51328,      0.551458,     0.589317,  &
       0.626559,     0.662934,     0.698224,     0.732224,  &
       0.764679,     0.795385,     0.824185,     0.85095,   &
       0.875518,     0.897767,     0.917651,     0.935157,  &
       0.950274,     0.963007,     0.973466,     0.982238,  &
       0.989153,     0.994204,     0.99763,      1.0]

   case(137)
      allocate(ecmwf%avec(ecmwf%kdim+1))
      allocate(ecmwf%bvec(ecmwf%kdim+1))
      ecmwf%avec = &
      [0.0000000,    2.000365,     3.102241,     4.666084,     &
       6.827977,     9.746966,     13.605424,    18.608931,    &
       24.985718,    32.98571,     42.879242,    54.955463,    &
       69.520576,    86.895882,    107.415741,   131.425507,   &
       159.279404,   191.338562,   227.968948,   269.539581,   &
       316.420746,   368.982361,   427.592499,   492.616028,   &
       564.413452,   643.339905,   729.744141,   823.967834,   &
       926.34491,    1037.201172,  1156.853638,  1285.610352,  &
       1423.770142,  1571.622925,  1729.448975,  1897.519287,  &
       2076.095947,  2265.431641,  2465.770508,  2677.348145,  &
       2900.391357,  3135.119385,  3381.743652,  3640.468262,  &
       3911.490479,  4194.930664,  4490.817383,  4799.149414,  &
       5119.89502,   5452.990723,  5798.344727,  6156.074219,  &
       6526.946777,  6911.870605,  7311.869141,  7727.412109,  &
       8159.354004,  8608.525391,  9076.400391,  9562.682617,  &
       10065.978516, 10584.631836, 11116.662109, 11660.067383, &
       12211.547852, 12766.873047, 13324.668945, 13881.331055, &
       14432.139648, 14975.615234, 15508.256836, 16026.115234, &
       16527.322266, 17008.789063, 17467.613281, 17901.621094, &
       18308.433594, 18685.71875,  19031.289063, 19343.511719, &
       19620.042969, 19859.390625, 20059.931641, 20219.664063, &
       20337.863281, 20412.308594, 20442.078125, 20425.71875,  &
       20361.816406, 20249.511719, 20087.085938, 19874.025391, &
       19608.572266, 19290.226563, 18917.460938, 18489.707031, &
       18006.925781, 17471.839844, 16888.6875,   16262.046875, &
       15596.695313, 14898.453125, 14173.324219, 13427.769531, &
       12668.257813, 11901.339844, 11133.304688, 10370.175781, &
       9617.515625,  8880.453125,  8163.375,     7470.34375,   &
       6804.421875,  6168.53125,   5564.382813,  4993.796875,  &
       4457.375,     3955.960938,  3489.234375,  3057.265625,  &
       2659.140625,  2294.242188,  1961.5,       1659.476563,  &
       1387.546875,  1143.25,      926.507813,   734.992188,   &
       568.0625,     424.414063,   302.476563,   202.484375,   &
       122.101563,   62.78125,     22.835938,    3.757813,     &
       0.0000000,    0.0000000]
      ecmwf%bvec = &
      [0.0000000,    0.0000000,    0.0000000,    0.0000000, &
       0.0000000,    0.0000000,    0.0000000,    0.0000000, &
       0.0000000,    0.0000000,    0.0000000,    0.0000000, &
       0.0000000,    0.0000000,    0.0000000,    0.0000000, &
       0.0000000,    0.0000000,    0.0000000,    0.0000000, &
       0.0000000,    0.0000000,    0.0000000,    0.0000000, &
       0.0000000,    0.0000000,    0.0000000,    0.0000000, &
       0.0000000,    0.0000000,    0.0000000,    0.0000000, &
       0.0000000,    0.0000000,    0.0000000,    0.0000000, &
       0.0000000,    0.0000000,    0.0000000,    0.0000000, &
       0.0000000,    0.0000000,    0.0000000,    0.0000000, &
       0.0000000,    0.0000000,    0.0000000,    0.0000000, &
       0.0000000,    0.0000000,    0.0000000,    0.0000000, &
       0.0000000,    0.0000000,    0.0000000,    0.000007,  &
       0.000024,     0.000059,     0.000112,     0.000199,  &
       0.00034,      0.000562,     0.00089,      0.001353,  &
       0.001992,     0.002857,     0.003971,     0.005378,  &
       0.007133,     0.009261,     0.011806,     0.014816,  &
       0.018318,     0.022355,     0.026964,     0.032176,  &
       0.038026,     0.044548,     0.051773,     0.059728,  &
       0.068448,     0.077958,     0.088286,     0.099462,  &
       0.111505,     0.124448,     0.138313,     0.153125,  &
       0.16891,      0.185689,     0.203491,     0.222333,  &
       0.242244,     0.263242,     0.285354,     0.308598,  &
       0.332939,     0.358254,     0.384363,     0.411125,  &
       0.438391,     0.466003,     0.4938,       0.521619,  &
       0.549301,     0.576692,     0.603648,     0.630036,  &
       0.655736,     0.680643,     0.704669,     0.727739,  &
       0.749797,     0.770798,     0.790717,     0.809536,  &
       0.827256,     0.843881,     0.859432,     0.873929,  &
       0.887408,     0.8999,       0.911448,     0.922096,  &
       0.931881,     0.94086,      0.949064,     0.95655,   &
       0.963352,     0.969513,     0.975078,     0.980072,  &
       0.984542,     0.9885,       0.991984,     0.995003,  &
       0.99763,      1.0000000]
   case default
      ecmwf%kdim = 60
      allocate(ecmwf%avec(61))
      allocate(ecmwf%bvec(61))
      ecmwf%avec = &
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
      ecmwf%bvec = &
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
   end select

end subroutine ecmwf_abvec_init


subroutine ecmwf_wind_init(ecmwf)

   implicit none

   type(ecmwf_t), intent(inout) :: ecmwf

   ecmwf%xdim=0
   ecmwf%ydim=0
!   ecmwf%kdim=0

   nullify(ecmwf%lon)
   nullify(ecmwf%lat)
   nullify(ecmwf%u10)
   nullify(ecmwf%v10)
   nullify(ecmwf%skin_temp)
   nullify(ecmwf%snow_depth)
   nullify(ecmwf%sea_ice_cover)

end subroutine ecmwf_wind_init


subroutine dup_ecmwf_allocation(ecmwf, ecmwf2)

   implicit none

   type(ecmwf_t), intent(in)  :: ecmwf
   type(ecmwf_t), intent(inout) :: ecmwf2

   ecmwf2%xdim = ecmwf%xdim
   ecmwf2%ydim = ecmwf%ydim
   ecmwf2%kdim = ecmwf%kdim

   allocate(ecmwf2%lon(ecmwf%xdim))
   allocate(ecmwf2%lat(ecmwf%ydim))
   allocate(ecmwf2%avec(ecmwf%kdim+1))
   allocate(ecmwf2%bvec(ecmwf%kdim+1))
   allocate(ecmwf2%u10(ecmwf%xdim,ecmwf%ydim))
   allocate(ecmwf2%v10(ecmwf%xdim,ecmwf%ydim))
   allocate(ecmwf2%skin_temp(ecmwf%xdim,ecmwf%ydim))
   allocate(ecmwf2%snow_depth(ecmwf%xdim,ecmwf%ydim))
   allocate(ecmwf2%sea_ice_cover(ecmwf%xdim,ecmwf%ydim))

end subroutine dup_ecmwf_allocation


subroutine linearly_combine_ecmwfs(a, b, ecmwf1, ecmwf2, ecmwf)

   implicit none

   real,          intent(in)  :: a
   real,          intent(in)  :: b
   type(ecmwf_t), intent(in)  :: ecmwf1
   type(ecmwf_t), intent(in)  :: ecmwf2
   type(ecmwf_t), intent(inout) :: ecmwf

   ecmwf%lat  = a * ecmwf1%lat        + b * ecmwf2%lat
   ecmwf%lon  = a * ecmwf1%lon        + b * ecmwf2%lon
   ecmwf%avec = a * ecmwf1%avec       + b * ecmwf2%avec
   ecmwf%bvec = a * ecmwf1%bvec       + b * ecmwf2%bvec
   ecmwf%u10  = a * ecmwf1%u10        + b * ecmwf2%u10
   ecmwf%v10  = a * ecmwf1%v10        + b * ecmwf2%v10
   ecmwf%skin_temp = a * ecmwf1%skin_temp  + b * ecmwf2%skin_temp
   ecmwf%snow_depth = a * ecmwf1%snow_depth + b * ecmwf2%snow_depth

   ecmwf%sea_ice_cover = sreal_fill_value
   where (ecmwf1%sea_ice_cover .ne. sreal_fill_value .and. &
          ecmwf2%sea_ice_cover .ne. sreal_fill_value)
      ecmwf%sea_ice_cover = a * ecmwf1%sea_ice_cover + b * ecmwf2%sea_ice_cover
   end where

end subroutine linearly_combine_ecmwfs


! handle_ecmwf_error (h_e_e)
subroutine h_e_e(routine, message)

   implicit none

   character(len=*), intent(in) :: routine, message

   write(*,*) 'ERROR: read_ecmwf_'//trim(routine)//'(): '//trim(message)
   stop error_stop_code

end subroutine h_e_e

end module ecmwf_m
