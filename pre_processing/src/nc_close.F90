! Name: nc_close.f90
!
!
! Purpose:
! Open netcdf output files
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
!2011/12/12: Matthias Jerg produces draft code which creates dummy output
!
! $Id$
!
! Bugs:
!
!none known


!-----------------------------------------------
!-----------------------------------------------
SUBROUTINE nc_close( ncid,fname,wo )
!-----------------------------------------------
!-----------------------------------------------

  use netcdf

  IMPLICIT NONE
 
  ! Input
  INTEGER,INTENT(IN) ::  ncid,wo
  CHARACTER(LEN=*) :: fname
  ! Local
  INTEGER ::  ierr

  ierr = NF90_CLOSE(ncid)
  IF (ierr.NE.NF90_NOERR)  stop 'error close'

  IF (wo.EQ.1) THEN
     write(*,*) '***'
     write(*,*) 'Closed data set: ',fname
     write(*,*) '***'
  ENDIF
 
  RETURN

END SUBROUTINE nc_close


 
