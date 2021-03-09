!-------------------------------------------------------------------------------
! Name: interpol_routines.F90
!
! Purpose:
! Defines Interpol_Routines_m module, which wraps the various FM interpolation
! routines.
!
! History:
! 2014/08/05, GM: Original version.
! 2014/08/05, GM: Added subroutine find_Pc().
!
! Bugs:
! None known.
!---------------------------------------------------------------------

module Interpol_Routines_m

   implicit none

   private

   public :: Interpol_Solar, &
             Interpol_Solar_spline, &
             Interpol_Thermal, &
             Interpol_Thermal_spline

contains

#include "interpol_solar.F90"
#include "interpol_solar_spline.F90"
#include "interpol_thermal.F90"
#include "interpol_thermal_spline.F90"


!-------------------------------------------------------------------------------
! Name: find_Pc
!
! Purpose:
! Locate the pressure level that bounds a given value.
!
! Description and Algorithm details:
!
! Arguments:
! Name   Type       In/Out/Both Description
! ------------------------------------------------------------------------------
! Ctrl   struct     In          Control structure
! Np     int        In          Number of pressure levels
! P      real array In          Pressure of those levels
! Pc     real       In          Pressure level to search for
! i_Pc   int        Out         Index of pressure level that bounds Pc
! status int        Out         Set to 1 if bounding level not found
!
! History:
! 2014/08/05, GM: Original version.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------
subroutine find_Pc(Ctrl, Np, P, Pc, i_Pc, status)

   use Ctrl_m

   implicit none

   type(Ctrl_t), intent(in)  :: Ctrl
   integer,      intent(in)  :: Np
   real,         intent(in)  :: P(:)
   real,         intent(in)  :: Pc
   integer,      intent(out) :: i_Pc
   integer,      intent(out) :: status

   integer :: i

   status = 0

   if (Pc >= P(Np)) then
      ! When Pc is above highest pressure in RTM
      i_Pc = Np-1
      if (abs(Pc - P(Np)) > 50.0) then
         ! When there is a difference of more than 50 hPa between Pc and RTM level
#ifdef DEBUG
         write(*,*) 'WARNING: Interpol_*(): Extrapolation, high, ' // &
            'P(1), P(Np), Pc: ', P(1), P(Np), Pc
#endif
      end if
   else if (Pc < P(1)) then
      ! When Pc is below the lowest pressure in RTM
      i_Pc = 1
      if (abs(Pc - P(1)) > 50.0) then
         ! When there is a difference of more than 50 hPa between Pc and RTM level
#ifdef DEBUG
         write(*,*) 'WARNING: Interpol_*(): Extrapolation, low, ' // &
            'P(1), P(Np), Pc: ', P(1), P(Np), Pc
#endif
      end if
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

end module Interpol_Routines_m
