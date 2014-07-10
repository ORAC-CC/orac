!-------------------------------------------------------------------------------
! Name: interpol.F90
!
! Purpose:
! Container for interpolation routines.
!
! Description and Algorithm details:
! None
!
! Arguments:
! None
!
! History:
! 2014/05/23, GM: First version.
!
! $Id$
!
! Bugs:
! None known
!-------------------------------------------------------------------------------

module interpol

implicit none

contains

include 'interpol_bilinear.F90'
include 'interpol_nearest_neighbour.F90'

end module interpol
