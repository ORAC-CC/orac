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
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

module prepare_output

   implicit none

contains

#include "prepare_primary.F90"
#include "prepare_secondary.F90"

end module prepare_output
