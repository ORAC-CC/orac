!-------------------------------------------------------------------------------
! Name: spixel_routines.F90
!
! Purpose:
!
! History:
! 2016/10/21, AP: Original version.
!
! Bugs:
! None known.
!---------------------------------------------------------------------

module SPixel_routines_m

   use ORAC_Constants_m
   use SPixel_m

   implicit none

contains

!-------------------------------------------------------------------------------
! Name: Calculate_ND
!
! Purpose:
! Return the NDVI or NDSI for a pixel.
!
! Algorithm:
! The NDVI is calculated as,
!   (R_0.55 - R_1.6) / (R_0.55 + R_1.6).
! If the snow keyword IS PRESENT, the NDSI is instead calculated as,
!   (R_0.87 - R_0.67) / (R_0.87 + R_0.67).
!
! Arguments:
! Name Type In/Out/Both Description
!
! History:
! 2016/10/21, AP: Original version.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------
function Calculate_ND(SAD_Chan, SPixel, nd, snow) result(status)
   use SAD_Chan_m

   implicit none

   type(SAD_Chan_t),  intent(in)    :: SAD_Chan(:)
   type(SPixel_t),    intent(inout) :: SPixel
   real,              intent(out)   :: nd
   logical, optional, intent(in)    :: snow
   integer                          :: status

   real    :: wvn1, max1, min1, wvn2, max2, min2
   logical :: mask(SPixel%Ind%NSolar)
   integer :: ch1, ch2

   status = 0

   if (present(snow)) then
      ! Calculate Normalised Difference Snow Index. Ranges from Fig. 4 of doi:
      ! 10.1002/(SICI)1099-1085(199808/09)12:10/11<1723::AID-HYP691>3.0.CO;2-2

      ! Find visible ch near 0.55um (in range 0.4-1.1um)
      wvn1 = 11494.
      max1 = 25000.
      min1 = 9090.1
      ! Find IR ch near 1.6um (in range 1.5-2.5um)
      wvn2 = 6250.0
      max2 = 6666.7
      min2 = 4000.0
   else
      ! Calculate Normalised Difference Vegetion Index. Ranges from Wikipedia.

      ! Find IR ch near 0.87um (in range 0.7-1.1um)
      wvn1 = 11494.
      max1 = 14286.
      min1 = 9090.1

      ! Find visible ch near 0.67um (in range 0.4-0.7um)
      wvn2 = 14925.
      max2 = 25000.
      min2 = 14286.
   end if

   ! Ensure both channels are from the same view
   mask = SPixel%ViewIdx(SPixel%Ind%YSolar) == minval(SPixel%ViewIdx)

   ch1 = Find_Channel(wvn1, SPixel%Ind%NSolar, SAD_Chan, &
        SPixel%spixel_y_solar_to_ctrl_y_index, max=max1, min=min1, mask=mask)
   ch2 = Find_Channel(wvn2, SPixel%Ind%NSolar, SAD_Chan, &
        SPixel%spixel_y_solar_to_ctrl_y_index, max=max2, min=min2, mask=mask)
   if (ch1 ==0 .or. ch2 == 0) then
      status = GetSurfaceNDNoCh
      return
   end if

   ! Calculate normalised difference
   nd = (SPixel%Surface%Rs2(ch1,IRho_DD) - SPixel%Surface%Rs2(ch2,IRho_DD)) / &
        (SPixel%Surface%Rs2(ch1,IRho_DD) + SPixel%Surface%Rs2(ch2,IRho_DD))

end function Calculate_ND

#include "get_geometry.F90"
#include "get_indexing.F90"
#include "get_location.F90"
#include "get_lsf.F90"
#include "get_lwswrtm.F90"
#include "get_measurements.F90"
#include "get_rtm.F90"
#include "get_surface.F90"
#include "x_mdad.F90"
#include "x_sdad.F90"
#include "get_x.F90"

#include "get_spixel.F90"
#include "int_ctp.F90"

end module SPixel_routines_m
