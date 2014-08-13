!-------------------------------------------------------------------------------
! Name:
!    Int_Routines_def
!
! Purpose:
!
! Description:
!
! Arguments:
!    Name Type In/Out/Both Description
!    N/A
!
! Algorithm:
!    N/A
!
! Local variables:
!    Name Type Description
!    N/A
!
! History:
!     5th Aug 2014, Greg McGarragh: Original version.
!     5th Aug 2014, Greg McGarragh: Added subroutine find_Pc().
!
! Bugs:
!    None known.
!
! $Id: InterpolRoutines.F90 2266 2014-08-04 15:58:20Z gmcgarragh $
!
!---------------------------------------------------------------------

module Interpol_Routines_def

   implicit none

   private

   public :: Interpol_Solar, &
             Interpol_Solar_spline, &
             Interpol_Thermal, &
             Interpol_Thermal_spline

contains

#include "InterpolSolar.F90"
#include "InterpolSolar_spline.F90"
#include "InterpolThermal.F90"
#include "InterpolThermal_spline.F90"


subroutine find_Pc(Ctrl, Np, P, Pc, i_Pc, status)

   use CTRL_def

   implicit none

   type(CTRL_t), intent(in)  :: Ctrl
   integer,      intent(in)  :: Np
   real,         intent(in)  :: P(:)
   real,         intent(in)  :: Pc
   integer,      intent(out) :: i_Pc
   integer,      intent(out) :: status

   integer :: i
   character(ECPLogReclen) :: message

   status = 0

   if (Pc > P(Np)) then
      ! When Pc is above highest pressure in RTM
      i_Pc = Np-1
      if (abs(Pc - P(Np)) > 50.0) then
         ! When there is a difference of more than 50 hPa between Pc and RTM level
         write(unit=message, fmt=*) &
            'WARNING: Interpol_Solar(), Extrapolation, high, P(1), P(Np), Pc: ', &
            P(1), P(Np), Pc
         call Write_Log(Ctrl, trim(message), status) ! Write to log
      end if
   else if (Pc < P(1)) then
      ! When Pc is below the lowest pressure in RTM
      i_Pc = 1
      if (abs(Pc - P(1)) > 50.0) then
         ! When there is a difference of more than 50 hPa between Pc and RTM level
         write(unit=message, fmt=*) &
            'WARNING: Interpol_Solar(), Extrapolation, low, P(1), P(Np), Pc: ', &
            P(1), P(Np), Pc
         call Write_Log(Ctrl, trim(message), status) ! Write to log
      end if
   else if (Pc == P(Np)) then
      i_Pc = Np-1
   else
      ! Search through RTM levels sequentially to find those bounding Pc
      do i = 1, Np-1
         if (Pc >= P(i) .and. Pc < P(i+1)) then
            i_Pc = i ! Set index equal to the lower bounding RTM level
            status = 0
            exit
         end if

         status = 1 ! Bounding levels not found
      end do
   end if

end subroutine find_Pc

end module Interpol_Routines_def
