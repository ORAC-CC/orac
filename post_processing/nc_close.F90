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
!2012/07/06 MJ extensively overhauls and restructures the code
!2014/06/04 MJ changes routine names to "*_pp" to avoid confusion when building libraries.
!
! $Id$
!
! Bugs:
!
!none known


!-----------------------------------------------
!-----------------------------------------------
SUBROUTINE nc_close_pp( ncid,fname,wo )
!-----------------------------------------------
!-----------------------------------------------

  use netcdf

  IMPLICIT NONE
 
  ! Input
  INTEGER,INTENT(IN) ::  ncid,wo
  CHARACTER(LEN=*),INTENT(IN) :: fname
  ! Local
  INTEGER ::  ierr

  ierr = NF90_CLOSE(ncid)
  write(*,*)'ierr',ierr,NF90_NOERR
  IF (ierr.NE.NF90_NOERR)  stop 'error close'
  write(*,*)'fname',trim(adjustl(fname))
  IF (wo.EQ.1) THEN
     write(*,*) '***'
     write(*,*) 'Closed data set: ',trim(adjustl(fname))
     write(*,*) '***'
  ENDIF
 
  RETURN

END SUBROUTINE nc_close_pp


 
