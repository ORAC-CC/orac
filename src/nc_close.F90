!-----------------------------------------------
!-----------------------------------------------
SUBROUTINE nc_close( ncid,fname,wo,ierr )
!-----------------------------------------------
!-----------------------------------------------

! Description:
!
! Closes netcdf-file.
!
  !-----------------------------------------------------------------------
  ! This software was developed within the ESA Cloud CCI Project
  ! and is based on routines developed during the
  ! ESA DUE GlobVapour Project. 
  ! Copyright 2011, DWD, All Rights Reserved.
  !-----------------------------------------------------------------------
  !
  !
  ! Created on:          05/12/10
  !                      by Matthias Jerg, DWD/KU22
  !                      (matthias.jerg@dwd.de)
  !                      based on code provided by Nadine Schneider (nadine.schneider@dwd.de).

!   
! Modifications Log:        
!
! Applied SPRs:
!
!-----------------------------------------------------------------------
!
! Declarations:
!

! ---------------------------------

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


 
