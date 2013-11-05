
SUBROUTINE nc_info(ncid,ndim,nvar,nattr,wo)

!---------------------------------------------------------------------
! Description:
!
!                      Reads in netcdf-file. Float Format
!
!-----------------------------------------------------------------------
! This software was developed within the ESA DUE GlobVapour Project.
!
! Copyright 2010, DWD, All Rights Reserved.
!-----------------------------------------------------------------------
!
! Unit Name:           nc_dim_info.f90
!
! Created on:          04/08/10
!                      by Nadine Schneider, DWD/KU22
!                      (nadine.schneider@dwd.de)
!
! Last Modified on:    August 04, 2010
!                      / Nadine Schneider, DWD/KU22, nadine.schneider@dwd.de
!
! Modifications Log:
!                     16/02/2012 C. Poulsen added vartypes_pp module
! Applied SPRs:
!
!-----------------------------------------------------------------------
!
! Declarations:
!
!---------------------------------

  USE netcdf

  use  ECP_Constants

  IMPLICIT NONE

!   INCLUDE 'netcdf.inc'

  ! Input
  INTEGER,INTENT(IN) :: ncid,wo
  
  ! Output
  INTEGER :: ndim, nvar, nattr
  
  ! Local
  INTEGER :: ierr

  ! End of header ----------------------------------------------------------



  ierr = 0
  
  ierr = nf90_inquire(ncid,ndim,nvar,nattr)  !Amount of pixels
  IF (ierr.NE.NF90_NOERR) THEN
     stop 'inq all'
  ENDIF
   
  
  RETURN
 
END SUBROUTINE nc_info


SUBROUTINE nc_dim_id(ncid,name,did,wo)

!---------------------------------------------------------------------
! Description:
!
!                      Reads in netcdf-file. Float Format
!
!-----------------------------------------------------------------------
! This software was developed within the ESA DUE GlobVapour Project.
!
! Copyright 2010, DWD, All Rights Reserved.
!-----------------------------------------------------------------------
!
! Unit Name:           nc_dim_info.f90
!
! Created on:          04/08/10
!                      by Nadine Schneider, DWD/KU22
!                      (nadine.schneider@dwd.de)
!
! Last Modified on:    August 04, 2010
!                      / Nadine Schneider, DWD/KU22, nadine.schneider@dwd.de
!
! Modifications Log:
!
! Applied SPRs:
!
!-----------------------------------------------------------------------
!
! Declarations:
!
!---------------------------------

  USE netcdf

  IMPLICIT NONE

!   INCLUDE 'netcdf.inc'

  ! Input
  INTEGER,INTENT(IN) :: ncid,wo
  CHARACTER(LEN=*) :: name

  ! Output
  INTEGER :: did, n

  integer :: diddummy

  ! Local
  INTEGER :: ierr
  character :: dummyname*100

  ! End of header ----------------------------------------------------------

  ierr = 0
 
  ierr = NF90_INQ_DIMID(ncid,name,did)  !Amount of pixels
  IF (ierr.NE.NF90_NOERR) THEN
  write(*,*) ierr,NF90_NOERR
    stop 'error nc_dim_id'
  ENDIF

  
  IF (wo.EQ.1) THEN
     write(*,*) did,': ', did
  ENDIF
    
  RETURN
 
END SUBROUTINE nc_dim_id


SUBROUTINE nc_dim_length(ncid,dname,did,n,wo)

!---------------------------------------------------------------------
! Description:
!
!                      Reads in netcdf-file. Float Format
!
!-----------------------------------------------------------------------
! This software was developed within the ESA DUE GlobVapour Project.
!
! Copyright 2010, DWD, All Rights Reserved.
!-----------------------------------------------------------------------
!
! Unit Name:           nc_dim_info.f90
!
! Created on:          04/08/10
!                      by Nadine Schneider, DWD/KU22
!                      (nadine.schneider@dwd.de)
!
! Last Modified on:    August 04, 2010
!                      / Nadine Schneider, DWD/KU22, nadine.schneider@dwd.de
!
! Modifications Log:
!
! Applied SPRs:
!
!-----------------------------------------------------------------------
!
! Declarations:
!
!---------------------------------

  USE netcdf

!  use preproc_constants
  use  ECP_Constants

  IMPLICIT NONE

!   INCLUDE 'netcdf.inc'

  ! Input
  INTEGER,INTENT(IN) :: ncid,wo
  CHARACTER(LEN=varlength) :: dname

  ! Output
  INTEGER :: did, n

  integer :: diddummy

  ! Local
  INTEGER :: ierr
  character :: dummyname*100

  ! End of header ----------------------------------------------------------

  ierr = 0

  ierr = NF90_INQUIRE_DIMENSION(ncid,did,dname,n)    !searches the amount of pixels
  IF (ierr.NE.NF90_NOERR) THEN
     STOP 'inq dimlen'
  ENDIF

  
  IF (wo.EQ.1) THEN
     write(*,*) TRIM(dname),': ', n
  ENDIF
    
  RETURN
 
END SUBROUTINE nc_dim_length
