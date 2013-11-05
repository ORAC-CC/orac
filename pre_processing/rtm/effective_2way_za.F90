! Name: effective_2wat_za.f90
!
!
! Purpose:
!
! Return the single-path zenith angle which has the same air-mass as the
! given 2-way path (assuming plane-parallel atmosphere)
!
!
! Description and Algorithm details:
!
!
! Arguments:
! Name Type In/Out/Both Description
!	SZA	IN Solar zenith of 2-way path
!	LZA	IN View zenith of 2-way path
!	AMF	Return the AMF
!	ZA	Return the effective 2 way angle
!
! Local variables:
! Name Type Description
!
!
! History:
!2012/05/22: Initial version C. Poulsen based on R. Siddans idl code
!          
!
! $Id$
!
! Bugs:
!
!none known

!TODOS:


subroutine effective_2way_za(sza,lza,amf,za)

  Implicit None

  Real :: sza,lza,amf,za
  real, parameter    :: d2r = 0.017453292   ! Pi/180.0

  amf=1.0/cos(sza*d2r)+1.0/cos(lza*d2r)
  za=acos(1.0/amf)/d2r
  
end subroutine effective_2way_za
