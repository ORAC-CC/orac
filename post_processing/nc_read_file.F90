! Name: nc_read_file.f90
!
!
! Purpose: File contains subroutines to read netcdf files for various variable types.
! 
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
!2012/02/03, Matthias Jerg: cleans out prototype code to prepare repository upload.
!2012/02/09, Matthias Jerg: adds routines for ORAC.
!2012/07/06, MJ: extensively overhauls and restructures the code
!2014/09/20, CP: bug fix scale factor error
!2014/10/24, OS: further bug fix on scale factor 
!2014/12/03, CP: added in common_constants should eventually remove vartypes_pp
!2015/02/05, OS: changed nint to lint
!2015/02/07, CP: updated to common constants
!
! $Id$
!
! Bugs:
!
!none known

SUBROUTINE nc_read_array_2d_float_orac_pp(ncid,n1,n2,cv,v_out,unit,wo)
  ! Description:
  !
  ! Creates new netcdf-file. (Output file)
  !
  !-----------------------------------------------------------------------
  ! This software was developed within the ESA Cloud CCI Project
  ! and is based on routines developed during the
  ! ESA DUE GlobVapour Project. 
  ! Copyright 2011, DWD, All Rights Reserved.
  !-----------------------------------------------------------------------
  !
  ! Unit Name:           nc_create_global_l2.f90
  !
  ! Created on:          05/12/10
  !                      by Matthias Jerg, DWD/KU22
  !                      (matthias.jerg@dwd.de)
  !                      based on code provided by Nadine Schneider (nadine.schneider@dwd.de).
  !
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

  USE vartypes_pp
  use common_constants

   IMPLICIT NONE

!   INCLUDE 'netcdf.inc'

      ! Input
      INTEGER,INTENT(IN) :: ncid,n1,n2,wo           ! number of footprints
      CHARACTER(LEN=*) :: cv                     ! variable name

      ! Output
      INTEGER :: n                            ! Dimension of data
      INTEGER, PARAMETER :: SINGLE = 4
      INTEGER, PARAMETER :: DOUBLE = 8
      REAL(KIND=sreal) :: v_out(1:n1,1:n2)    ! Variable (v): read in field 1-n1
      real(kind=sreal) :: fv,os,sf,vmin,vmax
      CHARACTER(LEN=unitlength) :: unit

      ! Local
      INTEGER :: ierr, vid, did, i, j,start(2), counter(2),stride(2)
      REAL,PARAMETER :: miss=-9999.
      !MJOLD      real(kind=sreal) :: v(1:n2,1:n1)
      real(kind=sreal) :: v(1:n1,1:n2)

   ! End of header ----------------------------------------------------------

!in fortran the inner loop is next to variable
!e.g. var(n2,n1) n2 is inner loop, n1 is outer loop

      start(1) = 1
      start(2) = 1
      counter(1) = n1
      counter(2) = n2
      
      !MJOLD      counter(1) = n2
      !MJOLD      counter(2) = n1
      stride=1

      ierr = 0

      unit=''

!      IF (wo.EQ.1) THEN
         write(*,*) 'read variable: ', cv
!      ENDIF

      ierr = NF90_INQ_VARID(ncid, cv, vid)    !get variable ID from variable named cv
      IF (ierr.NE.NF90_NOERR) THEN
          STOP 'inq v'
      ENDIF

      ierr = NF90_GET_VAR(ncid, vid,v,start,counter,stride)   !read variable from1-n1 and put into field
      IF (ierr.NE.NF90_NOERR) THEN
         STOP 'get v'
      ENDIF
      IF (wo.EQ.1) THEN
         write(*,*) 'first data value: ', v(1,1)
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
         fv=dreal_fill_value
      ENDIF



      ierr = NF90_GET_ATT(ncid, vid, 'scale_factor', sf)
      IF (wo.EQ.1) THEN
         write(*,*) 'scale_factor: '
!; write(*,fmt='(f6.2)') sf
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
         vmin=-dreal_fill_value*dreal_fill_value
      ENDIF

      ierr = NF90_GET_ATT(ncid, vid, 'valid_max', vmax)
      IF (wo.EQ.1) THEN
         write(*,*) 'valid_max: ',vmax
      ENDIF
      IF (ierr.NE.NF90_NOERR) THEN
         IF (wo.EQ.1) THEN
            write(*,*) 'valid_max: not defined '
         ENDIF
         vmax=dreal_fill_value*dreal_fill_value
      ENDIF

      DO i=1,n1                                            !question: outlier /outliar
         DO j=1,n2
            IF ( v(i,j).EQ.fv .or. v(i,j) .lt. vmin .or. v(i,j) .gt. vmax ) THEN
               v_out(i,j)=dreal_fill_value
            ELSE
               v_out(i,j)=(v(i,j)*sf)+os                !scaling and form to real format
            ENDIF
         ENDDO
      ENDDO

     RETURN

   END SUBROUTINE nc_read_array_2d_float_orac_pp


!-----------------------------------------------------------------------------
!-----------------------------------------------------------------------------
SUBROUTINE nc_read_array_2d_short_orac_pp(ncid,n1,n2,cv,v_out,unit,wo)
!-----------------------------------------------------------------------------
!-----------------------------------------------------------------------------
  ! Description:
  !
  ! Creates new netcdf-file. (Output file)
  !
  !-----------------------------------------------------------------------
  ! This software was developed within the ESA Cloud CCI Project
  ! and is based on routines developed during the
  ! ESA DUE GlobVapour Project. 
  ! Copyright 2011, DWD, All Rights Reserved.
  !-----------------------------------------------------------------------
  !
  ! Unit Name:           nc_create_global_l2.f90
  !
  ! Created on:          05/12/10
  !                      by Matthias Jerg, DWD/KU22
  !                      (matthias.jerg@dwd.de)
  !                      based on code provided by Nadine Schneider (nadine.schneider@dwd.de).
  !
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

  USE vartypes_pp
  use common_constants

   IMPLICIT NONE

!   INCLUDE 'netcdf.inc'

      ! Input
      INTEGER,INTENT(IN) :: ncid,n1,n2,wo           ! number of footprints
      CHARACTER(LEN=*) :: cv                     ! variable name

      ! Output
      INTEGER :: n                            ! Dimension of data
      INTEGER, PARAMETER :: SINGLE = 4
      INTEGER, PARAMETER :: DOUBLE = 8
      REAL(KIND=sreal) :: v_out(1:n1,1:n2)    ! Variable (v): read in field 1-n1
      real(kind=sreal) :: os,sf
      CHARACTER(LEN=unitlength) :: unit

      ! Local
      INTEGER :: ierr, vid, did, i, j,start(2), counter(2),stride(2)
      REAL,PARAMETER :: miss=-9999.
      integer(kind=sint) :: v(1:n1,1:n2),fv,vmin,vmax

   ! End of header ----------------------------------------------------------

!in fortran the inner loop is next to variable
!e.g. var(n2,n1) n2 is inner loop, n1 is outer loop

      start(1) = 1
      start(2) = 1
      counter(1) = n1
      counter(2) = n2
      stride=1

      ierr = 0

      unit=''

      IF (wo.EQ.1) THEN
         write(*,*) 'read variable: ', cv
      ENDIF

      ierr = NF90_INQ_VARID(ncid, cv, vid)    !get variable ID from variable named cv
      IF (ierr.NE.NF90_NOERR) THEN
          STOP 'inq v'
      ENDIF

      ierr = NF90_GET_VAR(ncid, vid,v,start,counter,stride)   !read variable from1-n1 and put into field
      IF (ierr.NE.NF90_NOERR) THEN
         STOP 'get v'
      ENDIF
      IF (wo.EQ.1) THEN
         write(*,*) 'first data value: ', v(1,1)
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
      ENDIF

      ierr = NF90_GET_ATT(ncid, vid, 'scale_factor', sf)
      IF (wo.EQ.1) THEN
         write(*,*) 'scale_factor: '; write(*,fmt='(f6.2)') sf
      ENDIF
      IF (ierr.NE.NF90_NOERR) THEN
         IF (wo.EQ.1) THEN
            write(*,*) 'scale_factor: not defined '
         ENDIF
      ENDIF

      ierr = NF90_GET_ATT(ncid, vid, 'add_offset', os)
      IF (wo.EQ.1) THEN
         write(*,*) 'add_offset: ',os
      ENDIF
      IF (ierr.NE.NF90_NOERR) THEN
         IF (wo.EQ.1) THEN
            write(*,*) 'add_offset: not defined '
         ENDIF
      ENDIF


      ierr = NF90_GET_ATT(ncid, vid, 'valid_min', vmin)
      IF (wo.EQ.1) THEN
         write(*,*) 'valid_min: ',vmin
      ENDIF
      IF (ierr.NE.NF90_NOERR) THEN
         IF (wo.EQ.1) THEN
            write(*,*) 'valid_min: not defined '
         ENDIF
      ENDIF

      ierr = NF90_GET_ATT(ncid, vid, 'valid_max', vmax)
      IF (wo.EQ.1) THEN
         write(*,*) 'valid_max: ',vmax
      ENDIF
      IF (ierr.NE.NF90_NOERR) THEN
         IF (wo.EQ.1) THEN
            write(*,*) 'valid_max: not defined '
         ENDIF
      ENDIF


      DO i=1,n1                                            !question: outlier /outliar
         DO j=1,n2

            IF ( v(i,j).EQ.fv .or. v(i,j) .lt. vmin .or. v(i,j) .gt. vmax ) THEN
               v_out(i,j)=dreal_fill_value
            ELSE
               v_out(i,j)=(real(v(i,j),kind=sreal)*sf)+os                !scaling and form to real format
            ENDIF
         ENDDO
      ENDDO
!!$
!!$      if(trim(adjustl(cv)).eq. 'ref') then 
!!$
!!$         write(*,*) maxval(v),maxval(v_out)
!!$
!!$         pause         
!!$
!!$      endif


     RETURN

   END SUBROUTINE nc_read_array_2d_short_orac_pp




!-----------------------------------------------------------------------------
!-----------------------------------------------------------------------------
   SUBROUTINE nc_read_array_2d_short_to_short_orac_pp(ncid,n1,n2,cv,v_out,unit,wo)
!-----------------------------------------------------------------------------
!-----------------------------------------------------------------------------
  ! Description:
  !
  ! Creates new netcdf-file. (Output file)
  !
  !-----------------------------------------------------------------------
  ! This software was developed within the ESA Cloud CCI Project
  ! and is based on routines developed during the
  ! ESA DUE GlobVapour Project. 
  ! Copyright 2011, DWD, All Rights Reserved.
  !-----------------------------------------------------------------------
  !
  ! Unit Name:           nc_create_global_l2.f90
  !
  ! Created on:          05/12/10
  !                      by Matthias Jerg, DWD/KU22
  !                      (matthias.jerg@dwd.de)
  !                      based on code provided by Nadine Schneider (nadine.schneider@dwd.de).
  !
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

  USE vartypes_pp
  use common_constants

   IMPLICIT NONE

!   INCLUDE 'netcdf.inc'

      ! Input
      INTEGER,INTENT(IN) :: ncid,n1,n2,wo           ! number of footprints
      CHARACTER(LEN=*) :: cv                     ! variable name

      ! Output
      INTEGER :: n                            ! Dimension of data
      INTEGER, PARAMETER :: SINGLE = 4
      INTEGER, PARAMETER :: DOUBLE = 8
      integer(KIND=sint) :: v_out(1:n1,1:n2)    ! Variable (v): read in field 1-n1
      integer(kind=lint) :: os,sf,vmin,vmax
      CHARACTER(LEN=unitlength) :: unit

      ! Local
      INTEGER :: ierr, vid, did, i, j,start(2), counter(2),stride(2)
      REAL,PARAMETER :: miss=-9999.
      integer(kind=sint) :: v(1:n1,1:n2),fv

   ! End of header ----------------------------------------------------------

!in fortran the inner loop is next to variable
!e.g. var(n2,n1) n2 is inner loop, n1 is outer loop

      start(1) = 1
      start(2) = 1
      counter(1) = n1
      counter(2) = n2
      stride=1

      ierr = 0

      unit=''

      IF (wo.EQ.1) THEN
         write(*,*) 'read variable: ', cv
      ENDIF

      ierr = NF90_INQ_VARID(ncid, cv, vid)    !get variable ID from variable named cv
      IF (ierr.NE.NF90_NOERR) THEN
          STOP 'inq v nc_read_array_2d_short_to_short_orac_pp'
      ENDIF

      ierr = NF90_GET_VAR(ncid, vid,v,start,counter,stride)   !read variable from1-n1 and put into field
      IF (ierr.NE.NF90_NOERR) THEN
         STOP 'get v nc_read_array_2d_short_to_short_orac_pp'
      ENDIF
      IF (wo.EQ.1) THEN
         write(*,*) 'first data value: ', v(1,1)
      ENDIF

      write(*,*) minval(v),maxval(v)

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
      ENDIF

      ierr = NF90_GET_ATT(ncid, vid, 'scale_factor', sf)
      IF (wo.EQ.1) THEN
         write(*,*) 'scale_factor: ',sf !; write(*,fmt='(f6.2)') sf
      ENDIF
      IF (ierr.NE.NF90_NOERR) THEN
         IF (wo.EQ.1) THEN
            write(*,*) 'scale_factor: not defined '
         ENDIF
      ENDIF

      ierr = NF90_GET_ATT(ncid, vid, 'add_offset', os)
      IF (wo.EQ.1) THEN
         write(*,*) 'add_offset: ',os
      ENDIF
      IF (ierr.NE.NF90_NOERR) THEN
         IF (wo.EQ.1) THEN
            write(*,*) 'add_offset: not defined '
         ENDIF
      ENDIF


      ierr = NF90_GET_ATT(ncid, vid, 'valid_min', vmin)
      IF (wo.EQ.1) THEN
         write(*,*) 'valid_min: ',vmin
      ENDIF
      IF (ierr.NE.NF90_NOERR) THEN
         IF (wo.EQ.1) THEN
            write(*,*) 'valid_min: not defined '
         ENDIF
      ENDIF

      ierr = NF90_GET_ATT(ncid, vid, 'valid_max', vmax)
      IF (wo.EQ.1) THEN
         write(*,*) 'valid_max: ',vmax
      ENDIF
      IF (ierr.NE.NF90_NOERR) THEN
         IF (wo.EQ.1) THEN
            write(*,*) 'valid_max: not defined '
         ENDIF
      ENDIF

      DO i=1,n1                                            !question: outlier /outliar
         DO j=1,n2
            IF ( v(i,j).EQ.fv .or. v(i,j) .lt. vmin .or. v(i,j) .gt. vmax ) THEN
               v_out(i,j)=fv
            ELSE
               v_out(i,j)=v(i,j)*sf+os             
            ENDIF
         ENDDO
      ENDDO


     RETURN

   END SUBROUTINE nc_read_array_2d_short_to_short_orac_pp


SUBROUTINE nc_read_array_2d_double_orac_pp(ncid,n1,n2,cv,v,wo)
  ! Description:
  !
  ! Creates new netcdf-file. (Output file)
  !
  !-----------------------------------------------------------------------
  ! This software was developed within the ESA Cloud CCI Project
  ! and is based on routines developed during the
  ! ESA DUE GlobVapour Project. 
  ! Copyright 2011, DWD, All Rights Reserved.
  !-----------------------------------------------------------------------
  !
  ! Unit Name:           nc_create_global_l2.f90
  !
  ! Created on:          05/12/10
  !                      by Matthias Jerg, DWD/KU22
  !                      (matthias.jerg@dwd.de)
  !                      based on code provided by Nadine Schneider (nadine.schneider@dwd.de).
  !
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

  USE vartypes_pp
  use common_constants

   IMPLICIT NONE

!   INCLUDE 'netcdf.inc'

      ! Input
      INTEGER,INTENT(IN) :: ncid,n1,n2,wo           ! number of footprints
      CHARACTER(LEN=*) :: cv                     ! variable name

      ! Output
      INTEGER :: n                            ! Dimension of data
      INTEGER, PARAMETER :: SINGLE = 4
      INTEGER, PARAMETER :: DOUBLE = 8
      real(kind=dreal) :: fv,os,sf,vmin,vmax

      ! Local
      INTEGER :: ierr, vid, did, i, j,start(2), counter(2),stride(2)
      REAL,PARAMETER :: miss=-9999.
      !MJOLD      real(kind=sreal) :: v(1:n2,1:n1)
      real(kind=dreal) :: v(1:n1,1:n2)

   ! End of header ----------------------------------------------------------

!in fortran the inner loop is next to variable
!e.g. var(n2,n1) n2 is inner loop, n1 is outer loop

      start(1) = 1
      start(2) = 1
      counter(1) = n1
      counter(2) = n2
      !MJOLD      counter(1) = n2
      !MJOLD      counter(2) = n1
      stride=1

      ierr = 0

      IF (wo.EQ.1) THEN
         write(*,*) 'read variable: ', cv
      ENDIF

      ierr = NF90_INQ_VARID(ncid, cv, vid)    !get variable ID from variable named cv
      IF (ierr.NE.NF90_NOERR) THEN
          STOP 'inq v'
      ENDIF
      !write(*,*) n1,n2
      !pause
      !write(*,*) ncid,vid,v,start,counter,stride
      !pause
      ierr = NF90_GET_VAR(ncid, vid,v,start,counter,stride) 
      IF (ierr.NE.NF90_NOERR) THEN
         STOP 'get v'
      ENDIF


      ierr = NF90_GET_ATT(ncid, vid, '_FillValue', fv)
      IF (wo.EQ.1) THEN
         write(*,*) '_FillVallue: ',fv
      ENDIF
      IF (ierr.NE.NF90_NOERR) THEN
         IF (wo.EQ.1) THEN
            write(*,*) '_FillVallue: not defined '
         ENDIF
         fv=dreal_fill_value
      ENDIF


    END SUBROUTINE nc_read_array_2d_double_orac_pp


!-----------------------------------------------------------------------------
!-----------------------------------------------------------------------------
SUBROUTINE nc_read_array_2d_byte_orac_pp(ncid,n1,n2,cv,v_out,unit,wo)
!-----------------------------------------------------------------------------
!-----------------------------------------------------------------------------
!-----------------------------------------------------------------------------
  ! Description:
  !
  ! Creates new netcdf-file. (Output file)
  !
  !-----------------------------------------------------------------------
  ! This software was developed within the ESA Cloud CCI Project
  ! and is based on routines developed during the
  ! ESA DUE GlobVapour Project. 
  ! Copyright 2011, DWD, All Rights Reserved.
  !-----------------------------------------------------------------------
  !
  ! Unit Name:           nc_create_global_l2.f90
  !
  ! Created on:          05/12/10
  !                      by Matthias Jerg, DWD/KU22
  !                      (matthias.jerg@dwd.de)
  !                      based on code provided by Nadine Schneider (nadine.schneider@dwd.de).
  !
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

  USE vartypes_pp
  use common_constants

   IMPLICIT NONE

!   INCLUDE 'netcdf.inc'

      ! Input
      INTEGER,INTENT(IN) :: ncid,n1,n2,wo           ! number of footprints
      CHARACTER(LEN=*) :: cv                     ! variable name

      ! Output
      INTEGER :: n                            ! Dimension of data
      INTEGER, PARAMETER :: SINGLE = 4
      INTEGER, PARAMETER :: DOUBLE = 8
      integer(KIND=byte) :: v_out(1:n1,1:n2)    ! Variable (v): read in field 1-n1
      integer(kind=byte) :: os,sf,vmin,vmax
      CHARACTER(LEN=unitlength) :: unit

      ! Local
      INTEGER :: ierr, vid, did, i, j,start(2), counter(2),stride(2)
      REAL,PARAMETER :: miss=-9999.
      integer(kind=byte) :: v(1:n1,1:n2),fv

   ! End of header ----------------------------------------------------------

!in fortran the inner loop is next to variable
!e.g. var(n2,n1) n2 is inner loop, n1 is outer loop

      start(1) = 1
      start(2) = 1
      counter(1) = n1
      counter(2) = n2
      stride=1

      ierr = 0

      unit=''

      IF (wo.EQ.1) THEN
         write(*,*) 'read variable: ', cv
      ENDIF

      ierr = NF90_INQ_VARID(ncid, cv, vid)    !get variable ID from variable named cv
      IF (ierr.NE.NF90_NOERR) THEN
          STOP 'inq v'
      ENDIF

      ierr = NF90_GET_VAR(ncid, vid,v,start,counter,stride)   !read variable from1-n1 and put into field
      IF (ierr.NE.NF90_NOERR) THEN
         STOP 'get v'
      ENDIF
      IF (wo.EQ.1) THEN
         write(*,*) 'first data value: ', v(1,1)
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
      ENDIF

      ierr = NF90_GET_ATT(ncid, vid, 'scale_factor', sf)
      IF (wo.EQ.1) THEN
         write(*,*) 'scale_factor: '
!; write(*,fmt='(f6.2)') sf
      ENDIF
      IF (ierr.NE.NF90_NOERR) THEN
         IF (wo.EQ.1) THEN
            write(*,*) 'scale_factor: not defined '
         ENDIF
      ENDIF

      ierr = NF90_GET_ATT(ncid, vid, 'add_offset', os)
      IF (wo.EQ.1) THEN
         write(*,*) 'add_offset: ',os
      ENDIF
      IF (ierr.NE.NF90_NOERR) THEN
         IF (wo.EQ.1) THEN
            write(*,*) 'add_offset: not defined '
         ENDIF
      ENDIF


      ierr = NF90_GET_ATT(ncid, vid, 'valid_min', vmin)
      IF (wo.EQ.1) THEN
         write(*,*) 'valid_min: ',vmin
      ENDIF
      IF (ierr.NE.NF90_NOERR) THEN
         IF (wo.EQ.1) THEN
            write(*,*) 'valid_min: not defined '
         ENDIF
      ENDIF

      ierr = NF90_GET_ATT(ncid, vid, 'valid_max', vmax)
      IF (wo.EQ.1) THEN
         write(*,*) 'valid_max: ',vmax
      ENDIF
      IF (ierr.NE.NF90_NOERR) THEN
         IF (wo.EQ.1) THEN
            write(*,*) 'valid_max: not defined '
         ENDIF
      ENDIF


      DO i=1,n1                                            !question: outlier /outliar
         DO j=1,n2
            IF ( v(i,j).EQ.fv .or. v(i,j) .lt. vmin .or. v(i,j) .gt. vmax ) THEN
               v_out(i,j)=byte_fill_value
            ELSE
               v_out(i,j)=v(i,j)*sf+os                !scaling and form to real format
            ENDIF
         ENDDO
      ENDDO

     RETURN

   END SUBROUTINE nc_read_array_2d_byte_orac_pp
