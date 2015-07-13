!-------------------------------------------------------------------------------
! Name: effective_2way_za.F90
!
! Purpose:
! Return the single-path zenith angle which has the same air-mass as the given
! 2-way path (assuming plane-parallel atmosphere)
!
! Description and Algorithm details:
!
! Arguments:
! Name Type In/Out/Both Description
! ------------------------------------------------------------------------------
! sza  real in          Solar zenith of 2-way path
! lza  real in          View zenith of 2-way path
! amf  real out         The AMF
! za   real out         The effective 2 way angle
!
! History:
! 2012/05/22, CP: Initial version based on R. Siddans idl code
! 2013/12/11, GM: Significant code clean up.
! 2014/08/13, GM: Get d2r from preproc_constants rather than duplicating.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine effective_2way_za(sza,lza,amf,za)

   use preproc_constants

   implicit none

   real, intent(in)  :: sza
   real, intent(in)  :: lza
   real, intent(out) :: amf
   real, intent(out) :: za

   amf=1.0/cos(sza*d2r)+1.0/cos(lza*d2r)

   za=acos(1.0/amf)/d2r

end subroutine effective_2way_za
