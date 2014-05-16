! Name: avhrr2oraclsflag.f90
!
!
! Purpose:
! Map Land/Sea Flag to ORAC values
! 
!
! Description and Algorithm details:
!
!
! Arguments:
! Name Type In/Out/Both Description
!
!
! Local variables:
! Name Type Description
!
!
! History:
!2012/05/16: Matthias Jerg produces draft code which maps the AVHRR swath flags to the ORAC definitions (approximately).
!
! $Id$
!
! Bugs:
!
!none known
!-------------------------------------------------------
!-------------------------------------------------------
subroutine avhrr2oraclsflag(temp)
!-------------------------------------------------------
!-------------------------------------------------------

  use preproc_constants

  implicit none

  integer(kind=lint), intent(inout) :: temp

!!$  AVHRR DEFINITIONS:
!!$                16:      water
!!$            != 16:      various land types

  if(temp .eq. 16) then !water 0
     
     temp=0
     
  else !land 1
     
     temp=1
     
  endif

end subroutine avhrr2oraclsflag
