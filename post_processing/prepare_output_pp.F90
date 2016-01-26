!-------------------------------------------------------------------------------
! Name: prepare_output.F90
!
! Purpose:
!
! Description and Algorithm details:
!
! Arguments:
! Name Type In/Out/Both Description
!
! History:
! 2015/09/06, GM: Original version created for the parts of output_routines.F90
!    that were not moved into common/ including the prepare routines.
! 2015/10/07, OS: Renamed to *_pp.F90, as we have to avoid duplicate
!    subroutine names for wrapper.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

module prepare_output_pp

   implicit none

contains

#include "prepare_output_primary_pp.F90"
#include "prepare_output_secondary_pp.F90"

end module prepare_output_pp
