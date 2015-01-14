! Name: nc_write_L2.F90
!
!
! Purpose:
! This file contains a collection of subroutines which write L2 data in different representations.
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
!2011/12/19: Matthias Jerg creates initial file.
! 2012/07/06 MJ extensively overhauls and restructures the code
!2014/06/04 MJ changes routine names to "*_pp" to avoid confusion when building libraries.
!2014/12/03 CP added in common_constants should eventually remove vartypes_pp
!
! $Id$
!
! Bugs:
!
!none known


!------------------------------------
!------------------------------------
SUBROUTINE nc_write_L2_float_pp(ncid,var_name,vid,v,ix,nx,jy,ny,wo,ierr)
!------------------------------------
!------------------------------------

! Description:
!
! Writes data in file FLOAT
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
  INTEGER,INTENT(IN) :: ncid, vid, wo
  INTEGER,INTENT(IN) ::  ix, nx, jy,ny
  CHARACTER(LEN=*),INTENT(in) ::  var_name

  INTEGER, PARAMETER :: SINGLE = 4
  INTEGER, PARAMETER :: DOUBLE = 8
  REAL(KIND=SINGLE),DIMENSION(ix:nx,jy:ny),INTENT(in) :: v

! Local
  INTEGER :: ierr, start(2), counter(2),stride(2),i,j

  start(1) = 1
  start(2) = 1
  
  stride=1

  counter(1) = nx-ix+1
  counter(2) = ny-jy+1


  IF (wo.EQ.1) THEN
     write(*,*) ''
     write(*,*) 'wrote variable: ', trim(var_name)
  ENDIF


  ierr = NF90_PUT_VAR(ncid, vid, v(ix:nx,jy:ny), start, counter,stride)

  if (ierr.NE.NF90_NOERR) stop 'err write v'
  


  RETURN
END SUBROUTINE nc_write_L2_float_pp

!------------------------------------
!------------------------------------
SUBROUTINE nc_write_L2_double_pp(ncid,var_name,vid,v,ix,nx,jy,ny,wo,ierr)
!------------------------------------
!------------------------------------
! Description:
!
! Writes data in file DOUBLE
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
  INTEGER,INTENT(IN) :: ncid, vid, wo
  INTEGER,INTENT(IN) ::  ix, nx, jy,ny
  CHARACTER(LEN=*),INTENT(in) ::  var_name

  INTEGER, PARAMETER :: SINGLE = 4
  INTEGER, PARAMETER :: DOUBLE = 8
  REAL(KIND=DOUBLE),DIMENSION(ix:nx,jy:ny),INTENT(in) :: v

! Local
  INTEGER :: ierr, start(2), counter(2),stride(2),i,j

  start(1) = 1
  start(2) = 1
  
  stride=1

  counter(1) = nx-ix+1
  counter(2) = ny-jy+1


  ierr = NF90_PUT_VAR(ncid, vid, v(ix:nx,jy:ny), start, counter,stride)

  if (ierr.NE.NF90_NOERR) stop 'err write v'
  
  IF (wo.EQ.1) THEN
     write(*,*) ''
     write(*,*) 'wrote variable: ', trim(var_name)
  ENDIF

  RETURN
END SUBROUTINE nc_write_L2_double_pp


!------------------------------------------------
!------------------------------------------------
SUBROUTINE nc_write_L2_short_pp(ncid,var_name,vid,v,ix,nx,jy,ny,wo,ierr)
!------------------------------------------------
!------------------------------------------------

! Description:
!
! Writes data in file 16bit integer
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
  
  use vartypes_pp
  use common_constants

  IMPLICIT NONE
 
! Input
  INTEGER,INTENT(IN) :: ncid, vid, wo
  INTEGER,INTENT(IN) ::  ix, nx, jy,ny
  CHARACTER(LEN=*),INTENT(in) ::  var_name

  integer(KIND=sint),DIMENSION(ix:nx,jy:ny),INTENT(in) :: v

! Local
  INTEGER :: ierr, start(2), counter(2),stride(2),i,j

  start(1) = 1
  start(2) = 1
  
  stride=1

  counter(1) = nx-ix+1
  counter(2) = ny-jy+1

 
  IF (wo.EQ.1) THEN
     write(*,*) ''
     write(*,*) 'wrote variable: ', trim(var_name)
  ENDIF

  write(*,*) 'start, counter,stride', start, counter,stride
  write(*,*)'ix,nx,jy,ny',ix,nx,jy,ny
  write(*,*)'ncid',ncid


  ierr = NF90_PUT_VAR(ncid, vid, v(ix:nx,jy:ny), start, counter,stride)

  if (ierr.NE.NF90_NOERR) stop 'err write v'
  
  IF (wo.EQ.1) THEN
     write(*,*) ''
     write(*,*) 'wrote variable: ', trim(var_name)
  ENDIF

!  ierr=1

  RETURN
END SUBROUTINE nc_write_L2_short_pp


!------------------------------------------------
!------------------------------------------------
SUBROUTINE nc_write_L2_long_pp(ncid,var_name,vid,v,ix,nx,jy,ny,wo,ierr)
!------------------------------------------------
!------------------------------------------------

! Description:
!
! Writes data in file 32bit integer
! for other data fomat see: nc_write.f90
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
  
  use vartypes_pp
  use common_constants

  IMPLICIT NONE
 
! Input
  INTEGER,INTENT(IN) :: ncid, vid, wo
  INTEGER,INTENT(IN) ::  ix, nx, jy,ny
  CHARACTER(LEN=*),INTENT(in) ::  var_name

  integer, DIMENSION(ix:nx,jy:ny),INTENT(in) :: v

! Local
  INTEGER :: ierr, start(2), counter(2),stride(2),i,j

  start(1) = 1
  start(2) = 1
  
  stride=1

  counter(1) = nx-ix+1
  counter(2) = ny-jy+1


  ierr = NF90_PUT_VAR(ncid, vid, v(ix:nx,jy:ny), start, counter,stride)

  if (ierr.NE.NF90_NOERR) stop 'err write v'
  
  IF (wo.EQ.1) THEN
     write(*,*) ''
     write(*,*) 'wrote variable: ', trim(var_name)
  ENDIF

  RETURN
END SUBROUTINE nc_write_L2_long_pp

!------------------------------------------------
!------------------------------------------------
SUBROUTINE nc_write_L2_byte_pp(ncid,var_name,vid,v,ix,nx,jy,ny,wo,ierr)
!------------------------------------------------
!------------------------------------------------

! Description:
!
! Writes data in file BYTE (8bit integer)
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
  
  use vartypes_pp
  use common_constants

  IMPLICIT NONE
 
! Input
  INTEGER,INTENT(IN) :: ncid, vid, wo
  INTEGER,INTENT(IN) ::  ix, nx, jy,ny
  CHARACTER(LEN=*),INTENT(in) ::  var_name

  integer(KIND=byte),DIMENSION(ix:nx,jy:ny),INTENT(in) :: v

! Local
  INTEGER :: ierr, start(2), counter(2),stride(2),i,j

  start(1) = 1
  start(2) = 1
  
  stride=1

  counter(1) = nx-ix+1
  counter(2) = ny-jy+1

  ierr = NF90_PUT_VAR(ncid, vid, v(ix:nx,jy:ny), start, counter,stride)

  if (ierr.NE.NF90_NOERR) stop 'err write v'
  
  IF (wo.EQ.1) THEN
     write(*,*) ''
     write(*,*) 'wrote variable: ', trim(var_name)
  ENDIF

  RETURN
END SUBROUTINE nc_write_L2_byte_pp















!!$
!!$!------------------------------------
!!$!------------------------------------
!!$SUBROUTINE nc_write_L2_float(ncid,var_name,vid,v,ix,nx,iyline,wo,ierr)
!!$!------------------------------------
!!$!------------------------------------
!!$
!!$! Description:
!!$!
!!$! Writes data in file FLOAT
!!$!
!!$  !-----------------------------------------------------------------------
!!$  ! This software was developed within the ESA Cloud CCI Project
!!$  ! and is based on routines developed during the
!!$  ! ESA DUE GlobVapour Project. 
!!$  ! Copyright 2011, DWD, All Rights Reserved.
!!$  !-----------------------------------------------------------------------
!!$  !
!!$  !
!!$  ! Created on:          05/12/10
!!$  !                      by Matthias Jerg, DWD/KU22
!!$  !                      (matthias.jerg@dwd.de)
!!$  !                      based on code provided by Nadine Schneider (nadine.schneider@dwd.de).
!!$!   
!!$! Modifications Log:        
!!$!
!!$! Applied SPRs:
!!$!
!!$!-----------------------------------------------------------------------
!!$!
!!$! Declarations:
!!$!
!!$
!!$! ---------------------------------
!!$
!!$  use netcdf
!!$
!!$  use vartypes_pp
!!$  
!!$  IMPLICIT NONE
!!$
!!$
!!$! Input
!!$  INTEGER,INTENT(IN) :: ncid, vid, wo
!!$  INTEGER,INTENT(IN) ::  ix, nx, iyline
!!$  CHARACTER(LEN=*),INTENT(in) ::  var_name
!!$
!!$  INTEGER, PARAMETER :: SINGLE = 4
!!$  INTEGER, PARAMETER :: DOUBLE = 8
!!$  REAL(KIND=SINGLE),DIMENSION(ix:nx),INTENT(in) :: v
!!$
!!$! Local
!!$  INTEGER :: ierr, start(2), counter(2),stride(2),i,j
!!$
!!$  start(1) = 1
!!$  start(2) = iyline
!!$  
!!$  stride=1
!!$
!!$  counter(1) = nx-ix+1
!!$  counter(2) = 1
!!$
!!$  ierr = NF90_PUT_VAR(ncid, vid, v(ix:nx), start, counter,stride)
!!$
!!$  if (ierr.NE.NF90_NOERR) stop 'err write v 1'
!!$  
!!$  IF (wo.EQ.1) THEN
!!$     write(*,*) ''
!!$     write(*,*) 'wrote variable: ', trim(var_name)
!!$  ENDIF
!!$
!!$  RETURN
!!$END SUBROUTINE nc_write_L2_float
!!$
!!$!------------------------------------
!!$!------------------------------------
!!$SUBROUTINE nc_write_L2_double(ncid,var_name,vid,v,ix,nx,iyline,wo,ierr)
!!$!------------------------------------
!!$!------------------------------------
!!$! Description:
!!$!
!!$! Writes data in file DOUBLE
!!$!
!!$  !-----------------------------------------------------------------------
!!$  ! This software was developed within the ESA Cloud CCI Project
!!$  ! and is based on routines developed during the
!!$  ! ESA DUE GlobVapour Project. 
!!$  ! Copyright 2011, DWD, All Rights Reserved.
!!$  !-----------------------------------------------------------------------
!!$  !
!!$  !
!!$  ! Created on:          05/12/10
!!$  !                      by Matthias Jerg, DWD/KU22
!!$  !                      (matthias.jerg@dwd.de)
!!$  !                      based on code provided by Nadine Schneider (nadine.schneider@dwd.de).
!!$!   
!!$! Modifications Log:        
!!$!
!!$! Applied SPRs:
!!$!
!!$!-----------------------------------------------------------------------
!!$!
!!$! Declarations:
!!$!
!!$
!!$! ---------------------------------
!!$
!!$  use netcdf
!!$
!!$  use vartypes_pp
!!$  
!!$  IMPLICIT NONE
!!$
!!$! Input
!!$  INTEGER,INTENT(IN) :: ncid, vid, wo
!!$  INTEGER,INTENT(IN) ::  ix, nx, iyline
!!$  CHARACTER(LEN=*),INTENT(in) ::  var_name
!!$
!!$  INTEGER, PARAMETER :: SINGLE = 4
!!$  INTEGER, PARAMETER :: DOUBLE = 8
!!$  REAL(KIND=DOUBLE),DIMENSION(ix:nx),INTENT(in) :: v
!!$
!!$! Local
!!$  INTEGER :: ierr, start(2), counter(2),stride(2),i,j
!!$
!!$  start(1) = 1
!!$  start(2) = iyline
!!$  
!!$  stride=1
!!$
!!$  counter(1) = nx-ix+1
!!$  counter(2) = 1
!!$
!!$  ierr = NF90_PUT_VAR(ncid, vid, v(ix:nx), start, counter,stride)
!!$
!!$  if (ierr.NE.NF90_NOERR) stop 'err write v 2'
!!$  
!!$  IF (wo.EQ.1) THEN
!!$     write(*,*) ''
!!$     write(*,*) 'wrote variable: ', trim(var_name)
!!$  ENDIF
!!$
!!$  RETURN
!!$END SUBROUTINE nc_write_L2_double
!!$
!!$
!!$!------------------------------------------------
!!$!------------------------------------------------
!!$SUBROUTINE nc_write_L2_short(ncid,var_name,vid,v,ix,nx,iyline,wo,ierr)
!!$!------------------------------------------------
!!$!------------------------------------------------
!!$
!!$! Description:
!!$!
!!$! Writes data in file 16bit integer
!!$!
!!$  !-----------------------------------------------------------------------
!!$  ! This software was developed within the ESA Cloud CCI Project
!!$  ! and is based on routines developed during the
!!$  ! ESA DUE GlobVapour Project. 
!!$  ! Copyright 2011, DWD, All Rights Reserved.
!!$  !-----------------------------------------------------------------------
!!$  !
!!$  !
!!$  ! Created on:          05/12/10
!!$  !                      by Matthias Jerg, DWD/KU22
!!$  !                      (matthias.jerg@dwd.de)
!!$  !                      based on code provided by Nadine Schneider (nadine.schneider@dwd.de).
!!$!   
!!$! Modifications Log:        
!!$!
!!$! Applied SPRs:
!!$!
!!$!-----------------------------------------------------------------------
!!$!
!!$! Declarations:
!!$!
!!$
!!$! ---------------------------------
!!$
!!$  use netcdf
!!$  
!!$  use vartypes_pp
!!$
!!$  IMPLICIT NONE
!!$ 
!!$! Input
!!$  INTEGER,INTENT(IN) :: ncid, vid, wo
!!$  INTEGER,INTENT(IN) ::  ix, nx, iyline
!!$  CHARACTER(LEN=*),INTENT(in) ::  var_name
!!$
!!$  integer(KIND=sint),DIMENSION(ix:nx),INTENT(in) :: v
!!$
!!$! Local
!!$  INTEGER :: ierr, start(2), counter(2),stride(2),i,j
!!$
!!$  start(1) = 1
!!$  start(2) = iyline
!!$  
!!$  stride=1
!!$
!!$  counter(1) = nx-ix+1
!!$  counter(2) = 1
!!$
!!$
!!$  ierr = NF90_PUT_VAR(ncid, vid, v(ix:nx), start, counter,stride)
!!$
!!$  if (ierr.NE.NF90_NOERR) stop 'err write v 3'
!!$  
!!$  IF (wo.EQ.1) THEN
!!$     write(*,*) ''
!!$     write(*,*) 'wrote variable: ', trim(var_name)
!!$  ENDIF
!!$
!!$!  ierr=1
!!$
!!$  RETURN
!!$END SUBROUTINE nc_write_L2_short
!!$
!!$
!!$!------------------------------------------------
!!$!------------------------------------------------
!!$SUBROUTINE nc_write_L2_long(ncid,var_name,vid,v,ix,nx,iyline,wo,ierr)
!!$!------------------------------------------------
!!$!------------------------------------------------
!!$
!!$! Description:
!!$!
!!$! Writes data in file 32bit integer
!!$! for other data fomat see: nc_write.f90
!!$!
!!$  !-----------------------------------------------------------------------
!!$  ! This software was developed within the ESA Cloud CCI Project
!!$  ! and is based on routines developed during the
!!$  ! ESA DUE GlobVapour Project. 
!!$  ! Copyright 2011, DWD, All Rights Reserved.
!!$  !-----------------------------------------------------------------------
!!$  !
!!$  !
!!$  ! Created on:          05/12/10
!!$  !                      by Matthias Jerg, DWD/KU22
!!$  !                      (matthias.jerg@dwd.de)
!!$  !                      based on code provided by Nadine Schneider (nadine.schneider@dwd.de).
!!$!   
!!$! Modifications Log:        
!!$!
!!$! Applied SPRs:
!!$!
!!$!-----------------------------------------------------------------------
!!$!
!!$! Declarations:
!!$!
!!$
!!$! ---------------------------------
!!$
!!$  use netcdf
!!$  
!!$  use vartypes_pp
!!$
!!$  
!!$
!!$  IMPLICIT NONE
!!$ 
!!$! Input
!!$  INTEGER,INTENT(IN) :: ncid, vid, wo
!!$  INTEGER,INTENT(IN) ::  ix, nx, iyline
!!$  CHARACTER(LEN=*),INTENT(in) ::  var_name
!!$
!!$  integer, DIMENSION(ix:nx),INTENT(in) :: v
!!$
!!$! Local
!!$  INTEGER :: ierr, start(2), counter(2),stride(2),i,j
!!$
!!$  start(1) = 1
!!$  start(2) = iyline
!!$  
!!$  stride=1
!!$
!!$  counter(1) = nx-ix+1
!!$  counter(2) = 1
!!$
!!$  ierr = NF90_PUT_VAR(ncid, vid, v(ix:nx), start, counter,stride)
!!$
!!$  if (ierr.NE.NF90_NOERR) stop 'err write v 4'
!!$  
!!$  IF (wo.EQ.1) THEN
!!$     write(*,*) ''
!!$     write(*,*) 'wrote variable: ', trim(var_name)
!!$  ENDIF
!!$
!!$  RETURN
!!$END SUBROUTINE nc_write_L2_long
!!$
!!$!------------------------------------------------
!!$!------------------------------------------------
!!$SUBROUTINE nc_write_L2_byte(ncid,var_name,vid,v,ix,nx,iyline,wo,ierr)
!!$!------------------------------------------------
!!$!------------------------------------------------
!!$
!!$! Description:
!!$!
!!$! Writes data in file BYTE (8bit integer)
!!$!
!!$  !-----------------------------------------------------------------------
!!$  ! This software was developed within the ESA Cloud CCI Project
!!$  ! and is based on routines developed during the
!!$  ! ESA DUE GlobVapour Project. 
!!$  ! Copyright 2011, DWD, All Rights Reserved.
!!$  !-----------------------------------------------------------------------
!!$  !
!!$  !
!!$  ! Created on:          05/12/10
!!$  !                      by Matthias Jerg, DWD/KU22
!!$  !                      (matthias.jerg@dwd.de)
!!$  !                      based on code provided by Nadine Schneider (nadine.schneider@dwd.de).
!!$
!!$!   
!!$! Modifications Log:        
!!$!
!!$! Applied SPRs:
!!$!
!!$!-----------------------------------------------------------------------
!!$!
!!$! Declarations:
!!$!
!!$
!!$! ---------------------------------
!!$
!!$  use netcdf
!!$  
!!$  use vartypes_pp 
!!$
!!$
!!$  IMPLICIT NONE
!!$ 
!!$! Input
!!$  INTEGER,INTENT(IN) :: ncid, vid, wo
!!$  INTEGER,INTENT(IN) ::  ix, nx, iyline
!!$  CHARACTER(LEN=*),INTENT(in) ::  var_name
!!$
!!$  integer(KIND=byte),DIMENSION(ix:nx),INTENT(in) :: v
!!$
!!$! Local
!!$  INTEGER :: ierr, start(2), counter(2),stride(2),i,j
!!$
!!$  start(1) = 1
!!$  start(2) = iyline
!!$  
!!$  stride=1
!!$
!!$  counter(1) = nx-ix+1
!!$  counter(2) = 1
!!$
!!$  ierr = NF90_PUT_VAR(ncid, vid, v(ix:nx), start, counter,stride)
!!$
!!$  if (ierr.NE.NF90_NOERR) stop 'err write v 5'
!!$  
!!$  IF (wo.EQ.1) THEN
!!$     write(*,*) ''
!!$     write(*,*) 'wrote variable: ', trim(var_name)
!!$  ENDIF
!!$
!!$  RETURN
!!$END SUBROUTINE nc_write_L2_byte
