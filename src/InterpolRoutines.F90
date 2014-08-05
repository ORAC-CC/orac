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
!     4th Aug 2014, Greg McGarragh: Orginal version.
!
! Bugs:
!    None known.
!
! $Id: InterpolRoutines.F90 2266 2014-08-04 15:58:20Z gmcgarragh $
!
!---------------------------------------------------------------------

module Interpol_Routines_def

   implicit none

   contains

#include "InterpolSolar.F90"
#include "InterpolSolar_spline.F90"
#include "InterpolThermal.F90"
#include "InterpolThermal_spline.F90"

end module Interpol_Routines_def
