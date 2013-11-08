! Name: nc_read_file.f90
!
!
! Purpose:
! Code contains several netcdf read routines
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
!28/08/2012 M Jerg orginal code
!28/08/2012 C. Poulsen added 4 dimensional array read for new badc files and
!              template header information
!18/03/2013 G Thomas Fixed a couple of error causing optional write statements
!
! $Id$
!
! Bugs:
!
!none k! 
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
SUBROUTINE nc_read_array_float_1d(ncid,n1,cv,v,wo)
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  use netcdf

  use preproc_constants
    
  IMPLICIT NONE
  
  ! Input
  INTEGER,INTENT(IN) :: ncid,n1,wo           ! number of footprints
  CHARACTER(LEN=*) :: cv                     ! variable name
  
  ! Output
  INTEGER, PARAMETER :: SINGLE = 4
  INTEGER, PARAMETER :: DOUBLE = 8
! real(kind=sreal) :: fv,os,sf,vmin,vmax
  CHARACTER(LEN=unitlength) :: unit
  
  ! Local
  INTEGER :: ierr, vid !, i
  INTEGER ::  start(1), counter(1),stride(1)

  REAL,PARAMETER :: miss=-9999.
  real(kind=sreal) :: v(1:n1)
  
  ! End of header ----------------------------------------------------------
  
  !in fortran the inner loop is next to variable
  !e.g. var(n2,n1) n2 is inner loop, n1 is outer loop
  
  start = 1
  counter = n1
  stride=1
  
  ierr = 0
  
  unit=''
  
  IF (wo.EQ.1) THEN
     write(*,*) 'read variable: ', cv
  ENDIF
  
  ierr = NF90_INQ_VARID(ncid, cv, vid)    !get variable ID from variable named cv
  IF (ierr.NE.NF90_NOERR) THEN
     STOP 'error nc_read_file inq v'
  ENDIF
 

!  ierr = NF90_GET_VAR(ncid, vid,v,start,counter,stride)   !read variable from1-n1 and put into field
  ierr = NF90_GET_VAR(ncid,vid,v,start,counter,stride) 
  IF (ierr.NE.NF90_NOERR) THEN
     STOP 'error nc_read_file  get v'
  ENDIF
  IF (wo.EQ.1) THEN
     write(*,*) 'first data value: ', v(1)
  ENDIF
  
  !      write(*,*) minval(v),maxval(v)
  
  ierr = NF90_GET_ATT(ncid, vid, 'units', unit)  !measure range of variable
  IF (wo.EQ.1) THEN
     write(*,*) 'unit: ',TRIM(unit)
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) 'unit: not defined '
     ENDIF
  ENDIF
  
!!$  ierr = NF90_GET_ATT(ncid, vid, '_FillValue', fv)
!!$  IF (wo.EQ.1) THEN
!!$     write(*,*) '_FillVallue: ',fv
!!$  ENDIF
!!$  IF (ierr.NE.NF90_NOERR) THEN
!!$     IF (wo.EQ.1) THEN
!!$        write(*,*) '_FillVallue: not defined '
!!$     ENDIF
!!$     fv=real_fill_value
!!$  ENDIF
!!$
!!$  ierr = NF90_GET_ATT(ncid, vid, 'scale_factor', sf)
!!$  IF (wo.EQ.1) THEN
!!$     write(*,*) 'scale_factor: '; write(*,fmt='(f6.2)') sf
!!$  ENDIF
!!$  IF (ierr.NE.NF90_NOERR) THEN
!!$     IF (wo.EQ.1) THEN
!!$        write(*,*) 'scale_factor: not defined '
!!$     ENDIF
!!$     sf=1.0
!!$  ENDIF
!!$  
!!$  ierr = NF90_GET_ATT(ncid, vid, 'add_offset', os)
!!$  IF (wo.EQ.1) THEN
!!$     write(*,*) 'add_offset: ',os
!!$  ENDIF
!!$  IF (ierr.NE.NF90_NOERR) THEN
!!$     IF (wo.EQ.1) THEN
!!$        write(*,*) 'add_offset: not defined '
!!$     ENDIF
!!$     os=0.00
!!$  ENDIF
!!$
!!$
!!$  ierr = NF90_GET_ATT(ncid, vid, 'valid_min', vmin)
!!$  IF (wo.EQ.1) THEN
!!$     write(*,*) 'valid_min: ',vmin
!!$  ENDIF
!!$  IF (ierr.NE.NF90_NOERR) THEN
!!$     IF (wo.EQ.1) THEN
!!$        write(*,*) 'valid_min: not defined '
!!$     ENDIF
!!$     vmin=-real_fill_value*real_fill_value
!!$  ENDIF
!!$  
!!$  ierr = NF90_GET_ATT(ncid, vid, 'valid_max', vmax)
!!$  IF (wo.EQ.1) THEN
!!$     write(*,*) 'valid_max: ',vmax
!!$  ENDIF
!!$  IF (ierr.NE.NF90_NOERR) THEN
!!$     IF (wo.EQ.1) THEN
!!$        write(*,*) 'valid_max: not defined '
!!$     ENDIF
!!$     vmax=real_fill_value*real_fill_value
!!$  ENDIF
  
!      write(*,*) fv, vmin,vmax,os,sf
 
  
!!$  DO i=1,n1                                            !question: outlier /outliar
!!$     IF ( v(i).EQ.fv .or. (v(i)*sf+os) .lt. vmin .or. (v(i)*sf+os) .gt. vmax ) THEN
!!$        v_out(i)=real_fill_value
!!$     ELSE
!!$        v_out(i)=(v(i)*sf)+os                !scaling and form to real format
!!$     ENDIF
!!$  ENDDO
  
  RETURN
  
END SUBROUTINE nc_read_array_float_1d



!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
SUBROUTINE nc_read_array_int_1d(ncid,n1,cv,v,wo)
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  use netcdf

  use preproc_constants
    
  IMPLICIT NONE
  
  ! Input
  INTEGER,INTENT(IN) :: ncid,n1,wo           ! number of footprints
  CHARACTER(LEN=*) :: cv                     ! variable name
  
  ! Output
  INTEGER, PARAMETER :: SINGLE = 4
  INTEGER, PARAMETER :: DOUBLE = 8
! real(kind=sreal) :: fv,os,sf,vmin,vmax
  CHARACTER(LEN=unitlength) :: unit
  
  ! Local
  INTEGER :: ierr, vid !, i
  INTEGER ::  start(1), counter(1),stride(1)

  REAL,PARAMETER :: miss=-9999.
  integer(kind=lint) :: v(1:n1)
  
  ! End of header ----------------------------------------------------------
  
  !in fortran the inner loop is next to variable
  !e.g. var(n2,n1) n2 is inner loop, n1 is outer loop
  
  start = 1
  counter = n1
  stride=1
  
  ierr = 0
  
  unit=''
  
  IF (wo.EQ.1) THEN
     write(*,*) 'read variable: ', cv
  ENDIF
  
  ierr = NF90_INQ_VARID(ncid, cv, vid)    !get variable ID from variable named cv
  IF (ierr.NE.NF90_NOERR) THEN
     STOP 'error nc_read_file  inq v'
  ENDIF
 

!  ierr = NF90_GET_VAR(ncid, vid,v,start,counter,stride)   !read variable from1-n1 and put into field
  ierr = NF90_GET_VAR(ncid,vid,v,start,counter,stride) 
  IF (ierr.NE.NF90_NOERR) THEN
     STOP 'error nc_read_file  get v'
  ENDIF
  IF (wo.EQ.1) THEN
     write(*,*) 'first data value: ', v(1)
  ENDIF
  
  !      write(*,*) minval(v),maxval(v)
  
  ierr = NF90_GET_ATT(ncid, vid, 'units', unit)  !measure range of variable
  IF (wo.EQ.1) THEN
     write(*,*) 'unit: ',TRIM(unit)
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) 'unit: not defined '
     ENDIF
  ENDIF
  
!!$  ierr = NF90_GET_ATT(ncid, vid, '_FillValue', fv)
!!$  IF (wo.EQ.1) THEN
!!$     write(*,*) '_FillVallue: ',fv
!!$  ENDIF
!!$  IF (ierr.NE.NF90_NOERR) THEN
!!$     IF (wo.EQ.1) THEN
!!$        write(*,*) '_FillVallue: not defined '
!!$     ENDIF
!!$     fv=real_fill_value
!!$  ENDIF
!!$
!!$  ierr = NF90_GET_ATT(ncid, vid, 'scale_factor', sf)
!!$  IF (wo.EQ.1) THEN
!!$     write(*,*) 'scale_factor: '; write(*,fmt='(f6.2)') sf
!!$  ENDIF
!!$  IF (ierr.NE.NF90_NOERR) THEN
!!$     IF (wo.EQ.1) THEN
!!$        write(*,*) 'scale_factor: not defined '
!!$     ENDIF
!!$     sf=1.0
!!$  ENDIF
!!$  
!!$  ierr = NF90_GET_ATT(ncid, vid, 'add_offset', os)
!!$  IF (wo.EQ.1) THEN
!!$     write(*,*) 'add_offset: ',os
!!$  ENDIF
!!$  IF (ierr.NE.NF90_NOERR) THEN
!!$     IF (wo.EQ.1) THEN
!!$        write(*,*) 'add_offset: not defined '
!!$     ENDIF
!!$     os=0.00
!!$  ENDIF
!!$
!!$
!!$  ierr = NF90_GET_ATT(ncid, vid, 'valid_min', vmin)
!!$  IF (wo.EQ.1) THEN
!!$     write(*,*) 'valid_min: ',vmin
!!$  ENDIF
!!$  IF (ierr.NE.NF90_NOERR) THEN
!!$     IF (wo.EQ.1) THEN
!!$        write(*,*) 'valid_min: not defined '
!!$     ENDIF
!!$     vmin=-real_fill_value*real_fill_value
!!$  ENDIF
!!$  
!!$  ierr = NF90_GET_ATT(ncid, vid, 'valid_max', vmax)
!!$  IF (wo.EQ.1) THEN
!!$     write(*,*) 'valid_max: ',vmax
!!$  ENDIF
!!$  IF (ierr.NE.NF90_NOERR) THEN
!!$     IF (wo.EQ.1) THEN
!!$        write(*,*) 'valid_max: not defined '
!!$     ENDIF
!!$     vmax=real_fill_value*real_fill_value
!!$  ENDIF
  
!      write(*,*) fv, vmin,vmax,os,sf

  
!!$  DO i=1,n1                                            !question: outlier /outliar
!!$     IF ( v(i).EQ.fv .or. (v(i)*sf+os) .lt. vmin .or. (v(i)*sf+os) .gt. vmax ) THEN
!!$        v_out(i)=real_fill_value
!!$     ELSE
!!$        v_out(i)=(v(i)*sf)+os                !scaling and form to real format
!!$     ENDIF
!!$  ENDDO
  
  RETURN
  
END SUBROUTINE nc_read_array_int_1d



!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
SUBROUTINE nc_read_array_short_4d(ncid,n1,n2,n3,n4,cv,v_out,wo)
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------


  use netcdf

  use preproc_constants
    
  IMPLICIT NONE
  
  ! Input
  INTEGER,INTENT(IN) :: ncid,n1,n2,n3,n4,wo           ! number of footprints
  CHARACTER(LEN=*) :: cv                     ! variable name
  
  ! Output
  INTEGER, PARAMETER :: SINGLE = 4
  INTEGER, PARAMETER :: DOUBLE = 8
  real(kind=sreal) :: fv,os,sf,vmin,vmax
  CHARACTER(LEN=unitlength) :: unit
  
  ! Local
  INTEGER :: ierr, vid, i,j,k,l
  INTEGER ::  start(4), counter(4),stride(4)

  REAL,PARAMETER :: miss=-9999.
  real(kind=sreal) ,INTENT(out)  :: v_out(1:n1,1:n2,1:n3,1:n4)
  real(kind=sreal)  :: v(1:n4,1:n3,1:n2,1:n1)

  
  ! End of header ----------------------------------------------------------
  
  !in fortran the inner loop is next to variable
  !e.g. var(n2,n1) n2 is inner loop, n1 is outer loop

  start = 1
  counter(1) = n4
  counter(2) = n3
  counter(3) = n2
  counter(4) = n1


  stride=1
  
  ierr = 0
  
  unit=''
  
  IF (wo.EQ.1) THEN
     write(*,*) 'read variable: ', cv
  ENDIF
  
  ierr = NF90_INQ_VARID(ncid, cv, vid)    !get variable ID from variable named cv
  IF (ierr.NE.NF90_NOERR) THEN
  ierr=9
     write(*,*) 'variable  not in file ',vid
  goto 1300

  ENDIF
 

!  ierr = NF90_GET_VAR(ncid, vid,v,start,counter,stride)   !read variable from1-n1 and put into field
 
  ierr = NF90_GET_VAR(ncid,vid,v,start,counter,stride) 
  IF (ierr.NE.NF90_NOERR) THEN
     STOP 'error nc_read_file 4d  get v'
  ENDIF
!  Found to cause array boundary errors. G Thomas March 2013
!  IF (wo.EQ.1) THEN
!     write(*,*) 'first data value: ', v(1,1,1,1)
!  ENDIF
  
 
  
  ierr = NF90_GET_ATT(ncid, vid, 'units', unit)  !measure range of variable
  IF (wo.EQ.1) THEN
     write(*,*) 'unit: ',TRIM(unit)
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) 'unit: not defined '
     ENDIF
  ENDIF
  
  ierr = NF90_GET_ATT(ncid, vid, '_FillValue', fv)
  IF (wo.EQ.1) THEN
     write(*,*) '_FillVallue: ',fv
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) '_FillVallue: not defined '
     ENDIF
     fv=real_fill_value
  ENDIF

  ierr = NF90_GET_ATT(ncid, vid, 'scale_factor', sf)
  IF (wo.EQ.1 .and. ierr.EQ.NF90_NOERR) THEN
     write(*,*) 'scale_factor: '; write(*,fmt='(f6.2)') sf
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) 'scale_factor: not defined '
     ENDIF
     sf=1.0
  ENDIF
  
  ierr = NF90_GET_ATT(ncid, vid, 'add_offset', os)
  IF (wo.EQ.1) THEN
     write(*,*) 'add_offset: ',os
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) 'add_offset: not defined '
     ENDIF
     os=0.00
  ENDIF


  ierr = NF90_GET_ATT(ncid, vid, 'valid_min', vmin)
  IF (wo.EQ.1) THEN
     write(*,*) 'valid_min: ',vmin
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) 'valid_min: not defined '
     ENDIF
     vmin=-real_fill_value*real_fill_value
  ENDIF
  
  ierr = NF90_GET_ATT(ncid, vid, 'valid_max', vmax)
  IF (wo.EQ.1) THEN
     write(*,*) 'valid_max: ',vmax
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) 'valid_max: not defined '
     ENDIF
     vmax=real_fill_value*real_fill_value
  ENDIF
!!$  
!      write(*,*) 'nc read file fv',fv, vmin,vmax,os,sf
fv=real_fill_value
!!$  
  DO i=1,n1                                            !question: outlier /outliar
     DO j=1,n2                                          
        DO k=1,n3                                          
           DO l=1,n4
                                                                                                
!                 IF ( v(l,k,j,i).EQ.fv .or. (v(l,k,j,i)*sf+os) .lt. vmin .or. (v(l,k,j,i)*sf+os) .gt. vmax ) THEN
                 IF ( v(l,k,j,i).EQ.fv ) THEN
                    v_out(i,j,k,l)=real_fill_value
                 ELSE
 
                   v_out(i,j,k,l)=(v(l,k,j,i)*sf)+os                !scaling and offset to real format
                 ENDIF
              ENDDO
          
        enddo
     enddo
  enddo
!!$  
  RETURN

1300 continue
  
END SUBROUTINE nc_read_array_short_4d



!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
SUBROUTINE nc_read_array_short_5d(ncid,n1,n2,n3,n4,n5,cv,v_out,wo)
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  use netcdf

  use preproc_constants
    
  IMPLICIT NONE
  
  ! Input
  INTEGER,INTENT(IN) :: ncid,n1,n2,n3,n4,n5,wo           ! number of footprints
  CHARACTER(LEN=*) :: cv                     ! variable name
  
  ! Output
  INTEGER, PARAMETER :: SINGLE = 4
  INTEGER, PARAMETER :: DOUBLE = 8
  real(kind=sreal) :: fv,os,sf,vmin,vmax
  CHARACTER(LEN=unitlength) :: unit
  
  ! Local
  INTEGER :: ierr, vid, i,j,k,l,m
  INTEGER ::  start(5), counter(5),stride(5)

  REAL,PARAMETER :: miss=-9999.
  real(kind=sreal) :: v_out(1:n1,1:n2,1:n3,1:n4,1:n5)
  integer(kind=stint) :: v(1:n5,1:n4,1:n3,1:n2,1:n1)

  
  ! End of header ----------------------------------------------------------
  
  !in fortran the inner loop is next to variable
  !e.g. var(n2,n1) n2 is inner loop, n1 is outer loop
  
  start = 1
  counter(1) = n5
  counter(2) = n4
  counter(3) = n3
  counter(4) = n2
  counter(5) =  n1

  stride=1
  
  ierr = 0
  
  unit=''
  
  IF (wo.EQ.1) THEN
     write(*,*) 'read variable: ', cv
  ENDIF
  
  ierr = NF90_INQ_VARID(ncid, cv, vid)    !get variable ID from variable named cv
  IF (ierr.NE.NF90_NOERR) THEN
  ierr=9
  goto 1300
     write(*,*) 'variable  not in file ',vid
  ENDIF
 

!  ierr = NF90_GET_VAR(ncid, vid,v,start,counter,stride)   !read variable from1-n1 and put into field
  ierr = NF90_GET_VAR(ncid,vid,v,start,counter,stride) 
  IF (ierr.NE.NF90_NOERR) THEN
     STOP 'error nc_read_file short 5d get v'
  ENDIF
  IF (wo.EQ.1) THEN
     write(*,*) 'first data value: ', v(1,1,1,1,1)
  ENDIF
  
  !      write(*,*) minval(v),maxval(v)
  
  ierr = NF90_GET_ATT(ncid, vid, 'units', unit)  !measure range of variable
  IF (wo.EQ.1) THEN
     write(*,*) 'unit: ',TRIM(unit)
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) 'unit: not defined '
     ENDIF
  ENDIF
  
  ierr = NF90_GET_ATT(ncid, vid, '_FillValue', fv)
  IF (wo.EQ.1) THEN
     write(*,*) '_FillVallue: ',fv
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) '_FillVallue: not defined '
     ENDIF
     fv=real_fill_value
  ENDIF

  ierr = NF90_GET_ATT(ncid, vid, 'scale_factor', sf)
  IF (wo.EQ.1) THEN
     write(*,*) 'scale_factor: '; write(*,fmt='(f6.2)') sf
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) 'scale_factor: not defined '
     ENDIF
     sf=1.0
  ENDIF
  
  ierr = NF90_GET_ATT(ncid, vid, 'add_offset', os)
  IF (wo.EQ.1) THEN
     write(*,*) 'add_offset: ',os
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) 'add_offset: not defined '
     ENDIF
     os=0.00
  ENDIF


  ierr = NF90_GET_ATT(ncid, vid, 'valid_min', vmin)
  IF (wo.EQ.1) THEN
     write(*,*) 'valid_min: ',vmin
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) 'valid_min: not defined '
     ENDIF
     vmin=-real_fill_value*real_fill_value
  ENDIF
  
  ierr = NF90_GET_ATT(ncid, vid, 'valid_max', vmax)
  IF (wo.EQ.1) THEN
     write(*,*) 'valid_max: ',vmax
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) 'valid_max: not defined '
     ENDIF
     vmax=real_fill_value*real_fill_value
  ENDIF
!!$  
!!$!      write(*,*) fv, vmin,vmax,os,sf

!!$  
  DO i=1,n1                                            !question: outlier /outliar
     DO j=1,n2                                          
        DO k=1,n3                                          
           DO l=1,n4
              DO m=1,n5                                                                                    
!                 IF ( v(m,l,k,j,i).EQ.fv .or. (v(m,l,k,j,i)*sf+os) .lt. vmin .or. (v(m,l,k,j,i)*sf+os) .gt. vmax ) THEN
                 IF ( v(m,l,k,j,i).EQ.fv ) THEN
                    v_out(i,j,k,l,m)=real_fill_value
                 ELSE
                    v_out(i,j,k,l,m)=(v(m,l,k,j,i)*sf)+os                !scaling and offset to real format
                 ENDIF
              ENDDO
           enddo
        enddo
     enddo
  enddo
!!$  
  RETURN

1300 continue
  
END SUBROUTINE nc_read_array_short_5d






!!$
!!$
!!$
!!$
!!$
!!$SUBROUTINE nc_read_array_2d_float(ncid,n1,n2,cv,v_out,unit,wo)
!!$
!!$!---------------------------------------------------------------------
!!$! Description:
!!$!
!!$!                      Reads in netcdf-file. Float Format
!!$!
!!$!-----------------------------------------------------------------------
!!$! This software was developed within the ESA DUE GlobVapour Project.
!!$!
!!$! Copyright 2010, DWD, All Rights Reserved.
!!$!-----------------------------------------------------------------------
!!$!
!!$! Unit Name:           nc_readdata_hoaps.f90
!!$!
!!$! Created on:          02/08/10
!!$!                      by Nadine Schneider, DWD/KU22
!!$!                      (nadine.schneider@dwd.de)
!!$!
!!$! Last Modified on:    August 12, 2010
!!$!                      / Nadine Schneider, DWD/KU22, nadine.schneider@dwd.de
!!$!
!!$! Modifications Log:
!!$!
!!$! Applied SPRs:
!!$!
!!$!-----------------------------------------------------------------------
!!$!
!!$! Declarations:
!!$!
!!$!---------------------------------
!!$
!!$  USE netcdf
!!$
!!$  USE vartypes
!!$
!!$   IMPLICIT NONE
!!$
!!$!   INCLUDE 'netcdf.inc'
!!$
!!$      ! Input
!!$      INTEGER,INTENT(IN) :: ncid,n1,n2,wo           ! number of footprints
!!$      CHARACTER(LEN=*) :: cv                     ! variable name
!!$
!!$      ! Output
!!$      INTEGER :: n                            ! Dimension of data
!!$      INTEGER, PARAMETER :: SINGLE = 4
!!$      INTEGER, PARAMETER :: DOUBLE = 8
!!$      REAL(KIND=sreal) :: v_out(1:n1,1:n2)    ! Variable (v): read in field 1-n1
!!$      real(kind=sreal) :: fv,os,sf,vmin,vmax
!!$      CHARACTER(LEN=unitlength) :: unit
!!$
!!$      ! Local
!!$      INTEGER :: ierr, vid, i,j,start(2), counter(2),stride(2)
!!$      REAL,PARAMETER :: miss=-9999.
!!$      real(kind=sreal) :: v(1:n2,1:n1)
!!$
!!$   ! End of header ----------------------------------------------------------
!!$
!!$!in fortran the inner loop is next to variable
!!$!e.g. var(n2,n1) n2 is inner loop, n1 is outer loop
!!$
!!$      start(1) = 1
!!$      start(2) = 1
!!$      counter(1) = n2
!!$      counter(2) = n1
!!$      stride=1
!!$
!!$      ierr = 0
!!$
!!$      unit=''
!!$
!!$      IF (wo.EQ.1) THEN
!!$         write(*,*) 'read variable: ', cv
!!$      ENDIF
!!$
!!$      ierr = NF90_INQ_VARID(ncid, cv, vid)    !get variable ID from variable named cv
!!$      IF (ierr.NE.NF90_NOERR) THEN
!!$          STOP 'inq v'
!!$      ENDIF
!!$
!!$      ierr = NF90_GET_VAR(ncid, vid,v,start,counter,stride)   !read variable from1-n1 and put into field
!!$      IF (ierr.NE.NF90_NOERR) THEN
!!$         STOP 'get v'
!!$      ENDIF
!!$      IF (wo.EQ.1) THEN
!!$         write(*,*) 'first data value: ', v(1,1)
!!$      ENDIF
!!$
!!$!      write(*,*) minval(v),maxval(v)
!!$
!!$      ierr = NF90_GET_ATT(ncid, vid, 'units', unit)  !measure range of variable
!!$      IF (wo.EQ.1) THEN
!!$         write(*,*) 'unit: ',TRIM(unit)
!!$      ENDIF
!!$      IF (ierr.NE.NF90_NOERR) THEN
!!$         IF (wo.EQ.1) THEN
!!$            write(*,*) 'unit: not defined '
!!$         ENDIF
!!$      ENDIF
!!$
!!$      ierr = NF90_GET_ATT(ncid, vid, '_FillValue', fv)
!!$      IF (wo.EQ.1) THEN
!!$         write(*,*) '_FillVallue: ',fv
!!$      ENDIF
!!$      IF (ierr.NE.NF90_NOERR) THEN
!!$         IF (wo.EQ.1) THEN
!!$            write(*,*) '_FillVallue: not defined '
!!$         ENDIF
!!$         fv=real_fill_value
!!$      ENDIF
!!$
!!$
!!$
!!$      ierr = NF90_GET_ATT(ncid, vid, 'scale_factor', sf)
!!$      IF (wo.EQ.1) THEN
!!$         write(*,*) 'scale_factor: '; write(*,fmt='(f6.2)') sf
!!$      ENDIF
!!$      IF (ierr.NE.NF90_NOERR) THEN
!!$         IF (wo.EQ.1) THEN
!!$            write(*,*) 'scale_factor: not defined '
!!$         ENDIF
!!$         sf=1.0
!!$      ENDIF
!!$
!!$      ierr = NF90_GET_ATT(ncid, vid, 'add_offset', os)
!!$      IF (wo.EQ.1) THEN
!!$         write(*,*) 'add_offset: ',os
!!$      ENDIF
!!$      IF (ierr.NE.NF90_NOERR) THEN
!!$         IF (wo.EQ.1) THEN
!!$            write(*,*) 'add_offset: not defined '
!!$         ENDIF
!!$         os=0.00
!!$      ENDIF
!!$
!!$
!!$      ierr = NF90_GET_ATT(ncid, vid, 'valid_min', vmin)
!!$      IF (wo.EQ.1) THEN
!!$         write(*,*) 'valid_min: ',vmin
!!$      ENDIF
!!$      IF (ierr.NE.NF90_NOERR) THEN
!!$         IF (wo.EQ.1) THEN
!!$            write(*,*) 'valid_min: not defined '
!!$         ENDIF
!!$         vmin=-real_fill_value*real_fill_value
!!$      ENDIF
!!$
!!$      ierr = NF90_GET_ATT(ncid, vid, 'valid_max', vmax)
!!$      IF (wo.EQ.1) THEN
!!$         write(*,*) 'valid_max: ',vmax
!!$      ENDIF
!!$      IF (ierr.NE.NF90_NOERR) THEN
!!$         IF (wo.EQ.1) THEN
!!$            write(*,*) 'valid_max: not defined '
!!$         ENDIF
!!$         vmax=real_fill_value*real_fill_value
!!$      ENDIF
!!$
!!$!      write(*,*) fv, vmin,vmax,os,sf
!!$
!!$      DO i=1,n1                                            !question: outlier /outliar
!!$         DO j=1,n2
!!$            IF ( v(j,i).EQ.fv .or. (v(j,i)*sf+os) .lt. vmin .or. (v(j,i)*sf+os) .gt. vmax ) THEN
!!$               v_out(i,j)=real_fill_value
!!$            ELSE
!!$               v_out(i,j)=(v(j,i)*sf)+os                !scaling and form to real format
!!$            ENDIF
!!$         ENDDO
!!$      ENDDO
!!$
!!$     RETURN
!!$
!!$   END SUBROUTINE nc_read_array_2d_float
!!$
!!$
!!$
!!$SUBROUTINE nc_read_array_2d_floatl3d(ncid,n1,n2,cv,v_out,unit,wo)
!!$
!!$!---------------------------------------------------------------------
!!$! Description:
!!$!
!!$!                      Reads in netcdf-file. Float Format
!!$!
!!$!-----------------------------------------------------------------------
!!$! This software was developed within the ESA DUE GlobVapour Project.
!!$!
!!$! Copyright 2010, DWD, All Rights Reserved.
!!$!-----------------------------------------------------------------------
!!$!
!!$! Unit Name:           nc_readdata_hoaps.f90
!!$!
!!$! Created on:          02/08/10
!!$!                      by Nadine Schneider, DWD/KU22
!!$!                      (nadine.schneider@dwd.de)
!!$!
!!$! Last Modified on:    August 12, 2010
!!$!                      / Nadine Schneider, DWD/KU22, nadine.schneider@dwd.de
!!$!
!!$! Modifications Log:
!!$!
!!$! Applied SPRs:
!!$!
!!$!-----------------------------------------------------------------------
!!$!
!!$! Declarations:
!!$!
!!$!---------------------------------
!!$
!!$  USE netcdf
!!$
!!$  USE vartypes
!!$
!!$   IMPLICIT NONE
!!$
!!$!   INCLUDE 'netcdf.inc'
!!$
!!$      ! Input
!!$      INTEGER,INTENT(IN) :: ncid,n1,n2,wo           ! number of footprints
!!$      CHARACTER(LEN=*) :: cv                     ! variable name
!!$
!!$      ! Output
!!$      INTEGER :: n                            ! Dimension of data
!!$      INTEGER, PARAMETER :: SINGLE = 4
!!$      INTEGER, PARAMETER :: DOUBLE = 8
!!$      REAL(KIND=sreal) :: v_out(1:n1,1:n2)    ! Variable (v): read in field 1-n1
!!$      real(kind=sreal) :: fv,os,sf,vmin,vmax
!!$      CHARACTER(LEN=unitlength) :: unit
!!$
!!$      ! Local
!!$      INTEGER :: ierr, vid, i,j,start(2), counter(2),stride(2)
!!$      REAL,PARAMETER :: miss=-9999.
!!$      real(kind=sreal) :: v(1:n1,1:n2)
!!$
!!$   ! End of header ----------------------------------------------------------
!!$
!!$!in fortran the inner loop is next to variable
!!$!e.g. var(n2,n1) n2 is inner loop, n1 is outer loop
!!$
!!$      start(1) = 1
!!$      start(2) = 1
!!$      counter(1) = n1
!!$      counter(2) = n2
!!$      stride=1
!!$
!!$      ierr = 0
!!$
!!$      unit=''
!!$
!!$      IF (wo.EQ.1) THEN
!!$         write(*,*) 'read variable: ', cv
!!$      ENDIF
!!$
!!$      ierr = NF90_INQ_VARID(ncid, cv, vid)    !get variable ID from variable named cv
!!$      IF (ierr.NE.NF90_NOERR) THEN
!!$          STOP 'inq v'
!!$      ENDIF
!!$
!!$      ierr = NF90_GET_VAR(ncid, vid,v,start,counter,stride)   !read variable from1-n1 and put into field
!!$      IF (ierr.NE.NF90_NOERR) THEN
!!$         STOP 'get v'
!!$      ENDIF
!!$      IF (wo.EQ.1) THEN
!!$         write(*,*) 'first data value: ', v(1,1)
!!$      ENDIF
!!$
!!$!      write(*,*) minval(v),maxval(v)
!!$
!!$      ierr = NF90_GET_ATT(ncid, vid, 'units', unit)  !measure range of variable
!!$      IF (wo.EQ.1) THEN
!!$         write(*,*) 'unit: ',TRIM(unit)
!!$      ENDIF
!!$      IF (ierr.NE.NF90_NOERR) THEN
!!$         IF (wo.EQ.1) THEN
!!$            write(*,*) 'unit: not defined '
!!$         ENDIF
!!$      ENDIF
!!$
!!$      ierr = NF90_GET_ATT(ncid, vid, '_FillValue', fv)
!!$      IF (wo.EQ.1) THEN
!!$         write(*,*) '_FillVallue: ',fv
!!$      ENDIF
!!$      IF (ierr.NE.NF90_NOERR) THEN
!!$         IF (wo.EQ.1) THEN
!!$            write(*,*) '_FillVallue: not defined '
!!$         ENDIF
!!$         fv=real_fill_value
!!$      ENDIF
!!$
!!$
!!$
!!$      ierr = NF90_GET_ATT(ncid, vid, 'scale_factor', sf)
!!$      IF (wo.EQ.1) THEN
!!$         write(*,*) 'scale_factor: '; write(*,fmt='(f6.2)') sf
!!$      ENDIF
!!$      IF (ierr.NE.NF90_NOERR) THEN
!!$         IF (wo.EQ.1) THEN
!!$            write(*,*) 'scale_factor: not defined '
!!$         ENDIF
!!$         sf=1.0
!!$      ENDIF
!!$
!!$      ierr = NF90_GET_ATT(ncid, vid, 'add_offset', os)
!!$      IF (wo.EQ.1) THEN
!!$         write(*,*) 'add_offset: ',os
!!$      ENDIF
!!$      IF (ierr.NE.NF90_NOERR) THEN
!!$         IF (wo.EQ.1) THEN
!!$            write(*,*) 'add_offset: not defined '
!!$         ENDIF
!!$         os=0.00
!!$      ENDIF
!!$
!!$
!!$      ierr = NF90_GET_ATT(ncid, vid, 'valid_min', vmin)
!!$      IF (wo.EQ.1) THEN
!!$         write(*,*) 'valid_min: ',vmin
!!$      ENDIF
!!$      IF (ierr.NE.NF90_NOERR) THEN
!!$         IF (wo.EQ.1) THEN
!!$            write(*,*) 'valid_min: not defined '
!!$         ENDIF
!!$         vmin=-real_fill_value*real_fill_value
!!$      ENDIF
!!$
!!$      ierr = NF90_GET_ATT(ncid, vid, 'valid_max', vmax)
!!$      IF (wo.EQ.1) THEN
!!$         write(*,*) 'valid_max: ',vmax
!!$      ENDIF
!!$      IF (ierr.NE.NF90_NOERR) THEN
!!$         IF (wo.EQ.1) THEN
!!$            write(*,*) 'valid_max: not defined '
!!$         ENDIF
!!$         vmax=real_fill_value*real_fill_value
!!$      ENDIF
!!$
!!$!      write(*,*) fv, vmin,vmax,os,sf
!!$
!!$      DO i=1,n1                                            !question: outlier /outliar
!!$         DO j=1,n2
!!$            IF ( v(i,j).EQ.fv .or. (v(i,j)*sf+os) .lt. vmin .or. (v(i,j)*sf+os) .gt. vmax ) THEN
!!$               v_out(i,j)=real_fill_value
!!$            ELSE
!!$               v_out(i,j)=(v(i,j)*sf)+os                !scaling and form to real format
!!$            ENDIF
!!$         ENDDO
!!$      ENDDO
!!$
!!$     RETURN
!!$
!!$   END SUBROUTINE nc_read_array_2d_floatl3d
!!$
!!$
!!$
!!$
!!$SUBROUTINE nc_read_lon_grid_float(ncid,n1,n2,cv,v_out,unit,wo)
!!$
!!$!---------------------------------------------------------------------
!!$! Description:
!!$!
!!$!                      Reads in netcdf-file. Float Format
!!$!
!!$!-----------------------------------------------------------------------
!!$! This software was developed within the ESA DUE GlobVapour Project.
!!$!
!!$! Copyright 2010, DWD, All Rights Reserved.
!!$!-----------------------------------------------------------------------
!!$!
!!$! Unit Name:           nc_readdata_hoaps.f90
!!$!
!!$! Created on:          02/08/10
!!$!                      by Nadine Schneider, DWD/KU22
!!$!                      (nadine.schneider@dwd.de)
!!$!
!!$! Last Modified on:    August 12, 2010
!!$!                      / Nadine Schneider, DWD/KU22, nadine.schneider@dwd.de
!!$!
!!$! Modifications Log:
!!$!
!!$! Applied SPRs:
!!$!
!!$!-----------------------------------------------------------------------
!!$!
!!$! Declarations:
!!$!
!!$!---------------------------------
!!$
!!$  USE netcdf
!!$
!!$  USE vartypes
!!$
!!$   IMPLICIT NONE
!!$
!!$!   INCLUDE 'netcdf.inc'
!!$
!!$      ! Input
!!$      INTEGER,INTENT(IN) :: ncid,n1,n2,wo           ! number of footprints
!!$      CHARACTER(LEN=*) :: cv                     ! variable name
!!$
!!$      ! Output
!!$      INTEGER :: n                            ! Dimension of data
!!$      INTEGER, PARAMETER :: SINGLE = 4
!!$      INTEGER, PARAMETER :: DOUBLE = 8
!!$      REAL(KIND=sreal) :: v_out(1:n1,1:n2)    ! Variable (v): read in field 1-n1
!!$      real(kind=sreal) :: fv,os,sf,vmin,vmax
!!$      CHARACTER(LEN=unitlength) :: unit
!!$
!!$      ! Local
!!$      INTEGER :: ierr, vid, i,j,start(1), counter(1),stride(1)
!!$      REAL,PARAMETER :: miss=-9999.
!!$      real(kind=sreal) :: v(1:n1)
!!$
!!$   ! End of header ----------------------------------------------------------
!!$
!!$!in fortran the inner loop is next to variable
!!$!e.g. var(n2,n1) n2 is inner loop, n1 is outer loop
!!$
!!$      start(1) = 1
!!$      counter(1) = n1
!!$      stride(1)=1
!!$
!!$      ierr = 0
!!$
!!$      unit=''
!!$
!!$      IF (wo.EQ.1) THEN
!!$         write(*,*) 'read variable: ', cv
!!$      ENDIF
!!$
!!$      ierr = NF90_INQ_VARID(ncid, cv, vid)    !get variable ID from variable named cv
!!$      IF (ierr.NE.NF90_NOERR) THEN
!!$          STOP 'inq v'
!!$      ENDIF
!!$
!!$      ierr = NF90_GET_VAR(ncid, vid,v,start,counter,stride)   !read variable from1-n1 and put into field
!!$      IF (ierr.NE.NF90_NOERR) THEN
!!$         STOP 'get v'
!!$      ENDIF
!!$      IF (wo.EQ.1) THEN
!!$         write(*,*) 'first data value: ', v(1)
!!$      ENDIF
!!$
!!$!      write(*,*) minval(v),maxval(v)
!!$
!!$      ierr = NF90_GET_ATT(ncid, vid, 'units', unit)  !measure range of variable
!!$      IF (wo.EQ.1) THEN
!!$         write(*,*) 'unit: ',TRIM(unit)
!!$      ENDIF
!!$      IF (ierr.NE.NF90_NOERR) THEN
!!$         IF (wo.EQ.1) THEN
!!$            write(*,*) 'unit: not defined '
!!$         ENDIF
!!$      ENDIF
!!$
!!$      ierr = NF90_GET_ATT(ncid, vid, '_FillValue', fv)
!!$      IF (wo.EQ.1) THEN
!!$         write(*,*) '_FillVallue: ',fv
!!$      ENDIF
!!$      IF (ierr.NE.NF90_NOERR) THEN
!!$         IF (wo.EQ.1) THEN
!!$            write(*,*) '_FillVallue: not defined '
!!$         ENDIF
!!$      ENDIF
!!$
!!$      ierr = NF90_GET_ATT(ncid, vid, 'scale_factor', sf)
!!$      IF (wo.EQ.1) THEN
!!$         write(*,*) 'scale_factor: '; write(*,fmt='(f6.2)') sf
!!$      ENDIF
!!$      IF (ierr.NE.NF90_NOERR) THEN
!!$         IF (wo.EQ.1) THEN
!!$            write(*,*) 'scale_factor: not defined '
!!$         ENDIF
!!$      ENDIF
!!$
!!$      ierr = NF90_GET_ATT(ncid, vid, 'add_offset', os)
!!$      IF (wo.EQ.1) THEN
!!$         write(*,*) 'add_offset: ',os
!!$      ENDIF
!!$      IF (ierr.NE.NF90_NOERR) THEN
!!$         IF (wo.EQ.1) THEN
!!$            write(*,*) 'add_offset: not defined '
!!$         ENDIF
!!$      ENDIF
!!$
!!$
!!$      ierr = NF90_GET_ATT(ncid, vid, 'valid_min', vmin)
!!$      IF (wo.EQ.1) THEN
!!$         write(*,*) 'valid_min: ',vmin
!!$      ENDIF
!!$      IF (ierr.NE.NF90_NOERR) THEN
!!$         IF (wo.EQ.1) THEN
!!$            write(*,*) 'valid_min: not defined '
!!$         ENDIF
!!$      ENDIF
!!$
!!$      ierr = NF90_GET_ATT(ncid, vid, 'valid_max', vmax)
!!$      IF (wo.EQ.1) THEN
!!$         write(*,*) 'valid_max: ',vmax
!!$      ENDIF
!!$      IF (ierr.NE.NF90_NOERR) THEN
!!$         IF (wo.EQ.1) THEN
!!$            write(*,*) 'valid_max: not defined '
!!$         ENDIF
!!$      ENDIF
!!$
!!$      do i=1,n1
!!$         do j=1,n2
!!$
!!$            v_out(i,j)=v(i)
!!$
!!$         enddo
!!$      enddo
!!$
!!$     RETURN
!!$
!!$   END SUBROUTINE nc_read_lon_grid_float
!!$
!!$
!!$SUBROUTINE nc_read_lat_grid_float(ncid,n1,n2,cv,v_out,unit,wo)
!!$
!!$!---------------------------------------------------------------------
!!$! Description:
!!$!
!!$!                      Reads in netcdf-file. Float Format
!!$!
!!$!-----------------------------------------------------------------------
!!$! This software was developed within the ESA DUE GlobVapour Project.
!!$!
!!$! Copyright 2010, DWD, All Rights Reserved.
!!$!-----------------------------------------------------------------------
!!$!
!!$! Unit Name:           nc_readdata_hoaps.f90
!!$!
!!$! Created on:          02/08/10
!!$!                      by Nadine Schneider, DWD/KU22
!!$!                      (nadine.schneider@dwd.de)
!!$!
!!$! Last Modified on:    August 12, 2010
!!$!                      / Nadine Schneider, DWD/KU22, nadine.schneider@dwd.de
!!$!
!!$! Modifications Log:
!!$!
!!$! Applied SPRs:
!!$!
!!$!-----------------------------------------------------------------------
!!$!
!!$! Declarations:
!!$!
!!$!---------------------------------
!!$
!!$  USE netcdf
!!$
!!$  USE vartypes
!!$
!!$   IMPLICIT NONE
!!$
!!$!   INCLUDE 'netcdf.inc'
!!$
!!$      ! Input
!!$      INTEGER,INTENT(IN) :: ncid,n1,n2,wo           ! number of footprints
!!$      CHARACTER(LEN=*) :: cv                     ! variable name
!!$
!!$      ! Output
!!$      INTEGER :: n                            ! Dimension of data
!!$      INTEGER, PARAMETER :: SINGLE = 4
!!$      INTEGER, PARAMETER :: DOUBLE = 8
!!$      REAL(KIND=sreal) :: v_out(1:n1,1:n2)    ! Variable (v): read in field 1-n1
!!$      real(kind=sreal) :: fv,os,sf,vmin,vmax
!!$      CHARACTER(LEN=unitlength) :: unit
!!$
!!$      ! Local
!!$      INTEGER :: ierr, vid, i,j,start(1), counter(1),stride(1)
!!$      REAL,PARAMETER :: miss=-9999.
!!$      real(kind=sreal) :: v(1:n1)
!!$
!!$   ! End of header ----------------------------------------------------------
!!$
!!$!in fortran the inner loop is next to variable
!!$!e.g. var(n2,n1) n2 is inner loop, n1 is outer loop
!!$
!!$      start(1) = 1
!!$      counter(1) = n2
!!$      stride(1)=1
!!$
!!$      ierr = 0
!!$
!!$      unit=''
!!$
!!$      IF (wo.EQ.1) THEN
!!$         write(*,*) 'read variable: ', cv
!!$      ENDIF
!!$
!!$      ierr = NF90_INQ_VARID(ncid, cv, vid)    !get variable ID from variable named cv
!!$      IF (ierr.NE.NF90_NOERR) THEN
!!$          STOP 'inq v'
!!$      ENDIF
!!$
!!$      ierr = NF90_GET_VAR(ncid, vid,v,start,counter,stride)   !read variable from1-n1 and put into field
!!$      IF (ierr.NE.NF90_NOERR) THEN
!!$         STOP 'get v'
!!$      ENDIF
!!$      IF (wo.EQ.1) THEN
!!$         write(*,*) 'first data value: ', v(1)
!!$      ENDIF
!!$
!!$!      write(*,*) minval(v),maxval(v)
!!$
!!$      ierr = NF90_GET_ATT(ncid, vid, 'units', unit)  !measure range of variable
!!$      IF (wo.EQ.1) THEN
!!$         write(*,*) 'unit: ',TRIM(unit)
!!$      ENDIF
!!$      IF (ierr.NE.NF90_NOERR) THEN
!!$         IF (wo.EQ.1) THEN
!!$            write(*,*) 'unit: not defined '
!!$         ENDIF
!!$      ENDIF
!!$
!!$      ierr = NF90_GET_ATT(ncid, vid, '_FillValue', fv)
!!$      IF (wo.EQ.1) THEN
!!$         write(*,*) '_FillVallue: ',fv
!!$      ENDIF
!!$      IF (ierr.NE.NF90_NOERR) THEN
!!$         IF (wo.EQ.1) THEN
!!$            write(*,*) '_FillVallue: not defined '
!!$         ENDIF
!!$      ENDIF
!!$
!!$      ierr = NF90_GET_ATT(ncid, vid, 'scale_factor', sf)
!!$      IF (wo.EQ.1) THEN
!!$         write(*,*) 'scale_factor: '; write(*,fmt='(f6.2)') sf
!!$      ENDIF
!!$      IF (ierr.NE.NF90_NOERR) THEN
!!$         IF (wo.EQ.1) THEN
!!$            write(*,*) 'scale_factor: not defined '
!!$         ENDIF
!!$      ENDIF
!!$
!!$      ierr = NF90_GET_ATT(ncid, vid, 'add_offset', os)
!!$      IF (wo.EQ.1) THEN
!!$         write(*,*) 'add_offset: ',os
!!$      ENDIF
!!$      IF (ierr.NE.NF90_NOERR) THEN
!!$         IF (wo.EQ.1) THEN
!!$            write(*,*) 'add_offset: not defined '
!!$         ENDIF
!!$      ENDIF
!!$
!!$
!!$      ierr = NF90_GET_ATT(ncid, vid, 'valid_min', vmin)
!!$      IF (wo.EQ.1) THEN
!!$         write(*,*) 'valid_min: ',vmin
!!$      ENDIF
!!$      IF (ierr.NE.NF90_NOERR) THEN
!!$         IF (wo.EQ.1) THEN
!!$            write(*,*) 'valid_min: not defined '
!!$         ENDIF
!!$      ENDIF
!!$
!!$      ierr = NF90_GET_ATT(ncid, vid, 'valid_max', vmax)
!!$      IF (wo.EQ.1) THEN
!!$         write(*,*) 'valid_max: ',vmax
!!$      ENDIF
!!$      IF (ierr.NE.NF90_NOERR) THEN
!!$         IF (wo.EQ.1) THEN
!!$            write(*,*) 'valid_max: not defined '
!!$         ENDIF
!!$      ENDIF
!!$
!!$      do j=1,n2
!!$         do i=1,n1
!!$
!!$            v_out(i,j)=v(j)
!!$
!!$         enddo
!!$      enddo
!!$
!!$     RETURN
!!$
!!$   END SUBROUTINE nc_read_lat_grid_float
!!$
!!$
!!$
!!$
!!$
!!$
!!$!-----------------------------------------------------------------------------
!!$!-----------------------------------------------------------------------------
!!$SUBROUTINE nc_read_array_2d_short(ncid,n1,n2,cv,v_out,unit,wo)
!!$!-----------------------------------------------------------------------------
!!$!-----------------------------------------------------------------------------
!!$
!!$!---------------------------------------------------------------------
!!$! Description:
!!$!
!!$!                      Reads in netcdf-file. Float Format
!!$!
!!$!-----------------------------------------------------------------------
!!$! This software was developed within the ESA DUE GlobVapour Project.
!!$!
!!$! Copyright 2010, DWD, All Rights Reserved.
!!$!-----------------------------------------------------------------------
!!$!
!!$! Unit Name:           nc_readdata_hoaps.f90
!!$!
!!$! Created on:          02/08/10
!!$!                      by Nadine Schneider, DWD/KU22
!!$!                      (nadine.schneider@dwd.de)
!!$!
!!$! Last Modified on:    August 12, 2010
!!$!                      / Nadine Schneider, DWD/KU22, nadine.schneider@dwd.de
!!$!
!!$! Modifications Log:
!!$!
!!$! Applied SPRs:
!!$!
!!$!-----------------------------------------------------------------------
!!$!
!!$! Declarations:
!!$!
!!$!---------------------------------
!!$
!!$  USE netcdf
!!$
!!$  USE vartypes
!!$
!!$   IMPLICIT NONE
!!$
!!$!   INCLUDE 'netcdf.inc'
!!$
!!$      ! Input
!!$      INTEGER,INTENT(IN) :: ncid,n1,n2,wo           ! number of footprints
!!$      CHARACTER(LEN=*) :: cv                     ! variable name
!!$
!!$      ! Output
!!$      INTEGER :: n                            ! Dimension of data
!!$      INTEGER, PARAMETER :: SINGLE = 4
!!$      INTEGER, PARAMETER :: DOUBLE = 8
!!$      REAL(KIND=sreal) :: v_out(1:n1,1:n2)    ! Variable (v): read in field 1-n1
!!$      real(kind=sreal) :: os,sf,vmin,vmax
!!$      CHARACTER(LEN=unitlength) :: unit
!!$
!!$      ! Local
!!$      INTEGER :: ierr, vid, i,j,start(2), counter(2),stride(2)
!!$      REAL,PARAMETER :: miss=-9999.
!!$      integer(kind=stint) :: v(1:n2,1:n1),fv
!!$
!!$   ! End of header ----------------------------------------------------------
!!$
!!$!in fortran the inner loop is next to variable
!!$!e.g. var(n2,n1) n2 is inner loop, n1 is outer loop
!!$
!!$      start(1) = 1
!!$      start(2) = 1
!!$      counter(1) = n2
!!$      counter(2) = n1
!!$      stride=1
!!$
!!$      ierr = 0
!!$
!!$      unit=''
!!$
!!$      IF (wo.EQ.1) THEN
!!$         write(*,*) 'read variable: ', cv
!!$      ENDIF
!!$
!!$      ierr = NF90_INQ_VARID(ncid, cv, vid)    !get variable ID from variable named cv
!!$      IF (ierr.NE.NF90_NOERR) THEN
!!$          STOP 'inq v'
!!$      ENDIF
!!$
!!$      ierr = NF90_GET_VAR(ncid, vid,v,start,counter,stride)   !read variable from1-n1 and put into field
!!$      IF (ierr.NE.NF90_NOERR) THEN
!!$         STOP 'get v'
!!$      ENDIF
!!$      IF (wo.EQ.1) THEN
!!$         write(*,*) 'first data value: ', v(1,1)
!!$      ENDIF
!!$
!!$!      write(*,*) minval(v),maxval(v)
!!$
!!$      ierr = NF90_GET_ATT(ncid, vid, 'units', unit)  !measure range of variable
!!$      IF (wo.EQ.1) THEN
!!$         write(*,*) 'unit: ',TRIM(unit)
!!$      ENDIF
!!$      IF (ierr.NE.NF90_NOERR) THEN
!!$         IF (wo.EQ.1) THEN
!!$            write(*,*) 'unit: not defined '
!!$         ENDIF
!!$      ENDIF
!!$
!!$      ierr = NF90_GET_ATT(ncid, vid, '_FillValue', fv)
!!$      IF (wo.EQ.1) THEN
!!$         write(*,*) '_FillVallue: ',fv
!!$      ENDIF
!!$      IF (ierr.NE.NF90_NOERR) THEN
!!$         IF (wo.EQ.1) THEN
!!$            write(*,*) '_FillVallue: not defined '
!!$         ENDIF
!!$      ENDIF
!!$
!!$      ierr = NF90_GET_ATT(ncid, vid, 'scale_factor', sf)
!!$      IF (wo.EQ.1) THEN
!!$         write(*,*) 'scale_factor: '; write(*,fmt='(f6.2)') sf
!!$      ENDIF
!!$      IF (ierr.NE.NF90_NOERR) THEN
!!$         IF (wo.EQ.1) THEN
!!$            write(*,*) 'scale_factor: not defined '
!!$         ENDIF
!!$      ENDIF
!!$
!!$      ierr = NF90_GET_ATT(ncid, vid, 'add_offset', os)
!!$      IF (wo.EQ.1) THEN
!!$         write(*,*) 'add_offset: ',os
!!$      ENDIF
!!$      IF (ierr.NE.NF90_NOERR) THEN
!!$         IF (wo.EQ.1) THEN
!!$            write(*,*) 'add_offset: not defined '
!!$         ENDIF
!!$      ENDIF
!!$
!!$
!!$      ierr = NF90_GET_ATT(ncid, vid, 'valid_min', vmin)
!!$      IF (wo.EQ.1) THEN
!!$         write(*,*) 'valid_min: ',vmin
!!$      ENDIF
!!$      IF (ierr.NE.NF90_NOERR) THEN
!!$         IF (wo.EQ.1) THEN
!!$            write(*,*) 'valid_min: not defined '
!!$         ENDIF
!!$      ENDIF
!!$
!!$      ierr = NF90_GET_ATT(ncid, vid, 'valid_max', vmax)
!!$      IF (wo.EQ.1) THEN
!!$         write(*,*) 'valid_max: ',vmax
!!$      ENDIF
!!$      IF (ierr.NE.NF90_NOERR) THEN
!!$         IF (wo.EQ.1) THEN
!!$            write(*,*) 'valid_max: not defined '
!!$         ENDIF
!!$      ENDIF
!!$
!!$
!!$      DO i=1,n1                                            !question: outlier /outliar
!!$         DO j=1,n2
!!$            IF ( v(j,i).EQ.fv .or. (real(v(j,i),kind=sreal)*sf+os) .lt. vmin .or. (real(v(j,i),kind=sreal)*sf+os) .gt. vmax ) THEN
!!$               v_out(i,j)=real_fill_value
!!$            ELSE
!!$               v_out(i,j)=(real(v(j,i),kind=sreal)*sf)+os                !scaling and form to real format
!!$            ENDIF
!!$         ENDDO
!!$      ENDDO
!!$
!!$     RETURN
!!$
!!$   END SUBROUTINE nc_read_array_2d_short
!!$
!!$
!!$!-----------------------------------------------------------------------------
!!$!-----------------------------------------------------------------------------
!!$SUBROUTINE nc_read_array_2d_byte(ncid,n1,n2,cv,v_out,unit,wo)
!!$!-----------------------------------------------------------------------------
!!$!-----------------------------------------------------------------------------
!!$
!!$!---------------------------------------------------------------------
!!$! Description:
!!$!
!!$!                      Reads in netcdf-file. Float Format
!!$!
!!$!-----------------------------------------------------------------------
!!$! This software was developed within the ESA DUE GlobVapour Project.
!!$!
!!$! Copyright 2010, DWD, All Rights Reserved.
!!$!-----------------------------------------------------------------------
!!$!
!!$! Unit Name:           nc_readdata_hoaps.f90
!!$!
!!$! Created on:          02/08/10
!!$!                      by Nadine Schneider, DWD/KU22
!!$!                      (nadine.schneider@dwd.de)
!!$!
!!$! Last Modified on:    August 12, 2010
!!$!                      / Nadine Schneider, DWD/KU22, nadine.schneider@dwd.de
!!$!
!!$! Modifications Log:
!!$!
!!$! Applied SPRs:
!!$!
!!$!-----------------------------------------------------------------------
!!$!
!!$! Declarations:
!!$!
!!$!---------------------------------
!!$
!!$  USE netcdf
!!$
!!$  USE vartypes
!!$
!!$   IMPLICIT NONE
!!$
!!$!   INCLUDE 'netcdf.inc'
!!$
!!$      ! Input
!!$      INTEGER,INTENT(IN) :: ncid,n1,n2,wo           ! number of footprints
!!$      CHARACTER(LEN=*) :: cv                     ! variable name
!!$
!!$      ! Output
!!$      INTEGER :: n                            ! Dimension of data
!!$      INTEGER, PARAMETER :: SINGLE = 4
!!$      INTEGER, PARAMETER :: DOUBLE = 8
!!$      REAL(KIND=sreal) :: v_out(1:n1,1:n2)    ! Variable (v): read in field 1-n1
!!$      real(kind=sreal) :: os,sf,vmin,vmax
!!$      CHARACTER(LEN=unitlength) :: unit
!!$
!!$      ! Local
!!$      INTEGER :: ierr, vid, i,j,start(2), counter(2),stride(2)
!!$      REAL,PARAMETER :: miss=-9999.
!!$      integer(kind=sint) :: v(1:n2,1:n1),fv
!!$
!!$   ! End of header ----------------------------------------------------------
!!$
!!$!in fortran the inner loop is next to variable
!!$!e.g. var(n2,n1) n2 is inner loop, n1 is outer loop
!!$
!!$      start(1) = 1
!!$      start(2) = 1
!!$      counter(1) = n2
!!$      counter(2) = n1
!!$      stride=1
!!$
!!$      ierr = 0
!!$
!!$      unit=''
!!$
!!$      IF (wo.EQ.1) THEN
!!$         write(*,*) 'read variable: ', cv
!!$      ENDIF
!!$
!!$      ierr = NF90_INQ_VARID(ncid, cv, vid)    !get variable ID from variable named cv
!!$      IF (ierr.NE.NF90_NOERR) THEN
!!$          STOP 'inq v'
!!$      ENDIF
!!$
!!$      ierr = NF90_GET_VAR(ncid, vid,v,start,counter,stride)   !read variable from1-n1 and put into field
!!$      IF (ierr.NE.NF90_NOERR) THEN
!!$         STOP 'get v'
!!$      ENDIF
!!$      IF (wo.EQ.1) THEN
!!$         write(*,*) 'first data value: ', v(1,1)
!!$      ENDIF
!!$
!!$!      write(*,*) minval(v),maxval(v)
!!$
!!$      ierr = NF90_GET_ATT(ncid, vid, 'units', unit)  !measure range of variable
!!$      IF (wo.EQ.1) THEN
!!$         write(*,*) 'unit: ',TRIM(unit)
!!$      ENDIF
!!$      IF (ierr.NE.NF90_NOERR) THEN
!!$         IF (wo.EQ.1) THEN
!!$            write(*,*) 'unit: not defined '
!!$         ENDIF
!!$      ENDIF
!!$
!!$      ierr = NF90_GET_ATT(ncid, vid, '_FillValue', fv)
!!$      IF (wo.EQ.1) THEN
!!$         write(*,*) '_FillVallue: ',fv
!!$      ENDIF
!!$      IF (ierr.NE.NF90_NOERR) THEN
!!$         IF (wo.EQ.1) THEN
!!$            write(*,*) '_FillVallue: not defined '
!!$         ENDIF
!!$      ENDIF
!!$
!!$      ierr = NF90_GET_ATT(ncid, vid, 'scale_factor', sf)
!!$      IF (wo.EQ.1) THEN
!!$         write(*,*) 'scale_factor: '; write(*,fmt='(f6.2)') sf
!!$      ENDIF
!!$      IF (ierr.NE.NF90_NOERR) THEN
!!$         IF (wo.EQ.1) THEN
!!$            write(*,*) 'scale_factor: not defined '
!!$         ENDIF
!!$      ENDIF
!!$
!!$      ierr = NF90_GET_ATT(ncid, vid, 'add_offset', os)
!!$      IF (wo.EQ.1) THEN
!!$         write(*,*) 'add_offset: ',os
!!$      ENDIF
!!$      IF (ierr.NE.NF90_NOERR) THEN
!!$         IF (wo.EQ.1) THEN
!!$            write(*,*) 'add_offset: not defined '
!!$         ENDIF
!!$      ENDIF
!!$
!!$
!!$      ierr = NF90_GET_ATT(ncid, vid, 'valid_min', vmin)
!!$      IF (wo.EQ.1) THEN
!!$         write(*,*) 'valid_min: ',vmin
!!$      ENDIF
!!$      IF (ierr.NE.NF90_NOERR) THEN
!!$         IF (wo.EQ.1) THEN
!!$            write(*,*) 'valid_min: not defined '
!!$         ENDIF
!!$      ENDIF
!!$
!!$      ierr = NF90_GET_ATT(ncid, vid, 'valid_max', vmax)
!!$      IF (wo.EQ.1) THEN
!!$         write(*,*) 'valid_max: ',vmax
!!$      ENDIF
!!$      IF (ierr.NE.NF90_NOERR) THEN
!!$         IF (wo.EQ.1) THEN
!!$            write(*,*) 'valid_max: not defined '
!!$         ENDIF
!!$      ENDIF
!!$
!!$
!!$      DO i=1,n1                                            !question: outlier /outliar
!!$         DO j=1,n2
!!$            IF ( v(j,i).EQ.fv .or. (real(v(j,i),kind=sreal)*sf+os) .lt. vmin .or. (real(v(j,i),kind=sreal)*sf+os) .gt. vmax ) THEN
!!$               v_out(i,j)=real_fill_value
!!$            ELSE
!!$               v_out(i,j)=(real(v(j,i),kind=sreal)*sf)+os                !scaling and form to real format
!!$            ENDIF
!!$         ENDDO
!!$      ENDDO
!!$
!!$     RETURN
!!$
!!$   END SUBROUTINE nc_read_array_2d_byte
