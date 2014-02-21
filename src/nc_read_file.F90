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
!2012/02/03 Matthias Jerg cleans out prototype code to prepare repository upload.
!2012/02/09 Matthias Jerg adds routines for ORAC.
!2012/07/06 MJ extensively overhauls and restructures the code
!2012/08/21 MJ builds this code collection based on pre- and postprocessing code collections
! $Id$
!
! Bugs:
!
!none known


!-----------------------------------------------------------------------------
!-----------------------------------------------------------------------------
SUBROUTINE nc_read_array_1d_int_to_int_orac(ncid,n1,cv,v,wo)
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

  USE ECP_Constants

   IMPLICIT NONE

!   INCLUDE 'netcdf.inc'

      ! Input
      INTEGER,INTENT(IN) :: ncid,n1,wo           ! number of footprints
      CHARACTER(LEN=*) :: cv                     ! variable name

      ! Output
      INTEGER, PARAMETER :: SINGLE = 4
      INTEGER, PARAMETER :: DOUBLE = 8

      ! Local
      INTEGER :: ierr, vid, start(1), counter(1),stride(1)
      REAL,PARAMETER :: miss=-9999.
      integer(kind=nint) :: v(1:n1),fv

   ! End of header ----------------------------------------------------------

!in fortran the inner loop is next to variable
!e.g. var(n2,n1) n2 is inner loop, n1 is outer loop

      start(1) = 1
      counter(1) = n1
      stride=1

      ierr = 0

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

      ierr = NF90_GET_ATT(ncid, vid, '_FillValue', fv)
      IF (wo.EQ.1) THEN
         write(*,*) '_FillVallue: ',fv
      ENDIF
      IF (ierr.NE.NF90_NOERR) THEN
         IF (wo.EQ.1) THEN
            write(*,*) '_FillVallue: not defined '
         ENDIF
      ENDIF
      

   END SUBROUTINE nc_read_array_1d_int_to_int_orac


!-----------------------------------------------------------------------------
!-----------------------------------------------------------------------------
SUBROUTINE nc_read_array_1d_float_to_float_orac(ncid,n1,cv,v,wo)
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

  USE ECP_Constants

   IMPLICIT NONE

!   INCLUDE 'netcdf.inc'

      ! Input
      INTEGER,INTENT(IN) :: ncid,n1,wo           ! number of footprints
      CHARACTER(LEN=*) :: cv                     ! variable name

      ! Output
      INTEGER, PARAMETER :: SINGLE = 4
      INTEGER, PARAMETER :: DOUBLE = 8

      ! Local
      INTEGER :: ierr, vid, start(1), counter(1),stride(1)
      REAL,PARAMETER :: miss=-9999.
      real(kind=sreal) :: v(1:n1),fv

   ! End of header ----------------------------------------------------------

!in fortran the inner loop is next to variable
!e.g. var(n2,n1) n2 is inner loop, n1 is outer loop

      start(1) = 1
      counter(1) = n1
      stride=1

      ierr = 0

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

      ierr = NF90_GET_ATT(ncid, vid, '_FillValue', fv)
      IF (wo.EQ.1) THEN
         write(*,*) '_FillVallue: ',fv
      ENDIF
      IF (ierr.NE.NF90_NOERR) THEN
         IF (wo.EQ.1) THEN
            write(*,*) '_FillVallue: not defined '
         ENDIF
      ENDIF
      

   END SUBROUTINE nc_read_array_1d_float_to_float_orac


SUBROUTINE nc_read_array_3d_float_orac(ncid,n1,n2,ichan,cv,v,wo)
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

  USE ECP_Constants

   IMPLICIT NONE

!   INCLUDE 'netcdf.inc'

      ! Input
      INTEGER,INTENT(IN) :: ncid,n1,n2,ichan,wo           ! number of footprints
      CHARACTER(LEN=*) :: cv                     ! variable name

      ! Output
      INTEGER, PARAMETER :: SINGLE = 4
      INTEGER, PARAMETER :: DOUBLE = 8
      real(kind=sreal) :: fv
      CHARACTER(LEN=NetcdfUnitLength) :: unit

      ! Local
      INTEGER :: ierr, vid, start(3), counter(3),stride(3)
      REAL,PARAMETER :: miss=-9999.
      real(kind=sreal) :: v(1:n1,1:n2,1)

   ! End of header ----------------------------------------------------------

!in fortran the inner loop is next to variable
!e.g. var(n2,n1) n2 is inner loop, n1 is outer loop

      start(1) = 1
      start(2) = 1
      start(3) = ichan
      counter(1) = n1
      counter(2) = n2
      counter(3) = 1
      !MJOLD      counter(1) = n2
      !MJOLD      counter(2) = n1
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

    END SUBROUTINE nc_read_array_3d_float_orac

SUBROUTINE nc_read_array_1p1_float_orac(ncid,n1,ichan,cv,v,wo)
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

  USE ECP_Constants

   IMPLICIT NONE

!   INCLUDE 'netcdf.inc'

      ! Input
      INTEGER,INTENT(IN) :: ncid,n1,ichan,wo           ! number of footprints
      CHARACTER(LEN=*) :: cv                     ! variable name

      ! Output
      INTEGER, PARAMETER :: SINGLE = 4
      INTEGER, PARAMETER :: DOUBLE = 8
      real(kind=sreal) :: fv
      CHARACTER(LEN=NetcdfUnitLength) :: unit

      ! Local
      INTEGER :: ierr, vid, start(2), counter(2),stride(2)
      REAL,PARAMETER :: miss=-9999.
      real(kind=sreal) :: v(1:n1,1)

   ! End of header ----------------------------------------------------------

!in fortran the inner loop is next to variable
!e.g. var(n2,n1) n2 is inner loop, n1 is outer loop

!trial and error
!!$      start(1) = 1
!!$      start(2) = ichan
!!$      counter(1) = n1
!!$      counter(2) = 1


      start(1) = ichan
      start(2) = 1
      counter(1) = 1
      counter(2) = n1

      !MJOLD      counter(1) = n2
      !MJOLD      counter(2) = n1
      stride=1

      ierr = 0

      unit=''

      IF (wo.EQ.1) THEN
         write(*,*) 'read variable: ', cv
      ENDIF

      ierr = NF90_INQ_VARID(ncid, cv, vid)    !get variable ID from variable named cv
      IF (ierr.NE.NF90_NOERR) THEN
         write(*,*) 'inq v, this is it',start,counter,stride,vid,ncid,'STOP'
          STOP
      ENDIF

      !write(*,*) ichan,n1,ncid,vid
      !write(*,*) start
      !write(*,*) counter
      !write(*,*) stride
     


      ierr = NF90_GET_VAR(ncid, vid,v,start,counter,stride)
      !write(*,*) ierr
     
      !read variable from1-n1 and put into field
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
         fv=real_fill_value
      ENDIF

    END SUBROUTINE nc_read_array_1p1_float_orac



SUBROUTINE nc_read_array_1p2_float_orac(ncid,n1,ichan,ilay,cv,v,wo)
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

  USE ECP_Constants

   IMPLICIT NONE

!   INCLUDE 'netcdf.inc'

      ! Input
      INTEGER,INTENT(IN) :: ncid,n1,ichan,ilay,wo           ! number of footprints
      CHARACTER(LEN=*) :: cv                     ! variable name

      ! Output
      INTEGER, PARAMETER :: SINGLE = 4
      INTEGER, PARAMETER :: DOUBLE = 8
      real(kind=sreal) :: fv
      CHARACTER(LEN=NetcdfUnitLength) :: unit

      ! Local
      INTEGER :: ierr, vid, start(3), counter(3),stride(3)
      REAL,PARAMETER :: miss=-9999.
      real(kind=sreal) :: v(1:n1,1,1)

   ! End of header ----------------------------------------------------------

!in fortran the inner loop is next to variable
!e.g. var(n2,n1) n2 is inner loop, n1 is outer loop



      start(1) = ichan
      start(2) = ilay
      start(3) = 1
      counter(1) = 1
      counter(2) = 1
      counter(3) = n1

      !MJOLD      counter(1) = n2
      !MJOLD      counter(2) = n1
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

      !write(*,*) ichan,n1,ncid,vid
      !write(*,*) start
      !write(*,*) counter
      !write(*,*) stride
     


      ierr = NF90_GET_VAR(ncid, vid,v,start,counter,stride)
      !write(*,*) ierr
     
      !read variable from1-n1 and put into field
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
         fv=real_fill_value
      ENDIF

    END SUBROUTINE nc_read_array_1p2_float_orac





SUBROUTINE nc_read_array_2d_double_orac(ncid,n1,n2,cv,v,wo)
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

  USE ECP_Constants

   IMPLICIT NONE

!   INCLUDE 'netcdf.inc'

      ! Input
      INTEGER,INTENT(IN) :: ncid,n1,n2,wo           ! number of footprints
      CHARACTER(LEN=*) :: cv                     ! variable name

      ! Output
      INTEGER, PARAMETER :: SINGLE = 4
      INTEGER, PARAMETER :: DOUBLE = 8
      real(kind=dreal) :: fv

      ! Local
      INTEGER :: ierr, vid, start(2), counter(2),stride(2)
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
         fv=real_fill_value
      ENDIF


    END SUBROUTINE nc_read_array_2d_double_orac



SUBROUTINE nc_read_array_2d_float_orac(ncid,n1,n2,cv,v,wo)
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

  USE ECP_Constants

   IMPLICIT NONE

!   INCLUDE 'netcdf.inc'

      ! Input
      INTEGER,INTENT(IN) :: ncid,n1,n2,wo           ! number of footprints
      CHARACTER(LEN=*) :: cv                     ! variable name

      ! Output
      INTEGER, PARAMETER :: SINGLE = 4
      INTEGER, PARAMETER :: DOUBLE = 8
      real(kind=sreal) :: fv

      ! Local
      INTEGER :: ierr, vid, start(2), counter(2),stride(2)
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

      IF (wo.EQ.1) THEN
         write(*,*) 'read variable: ', cv
      ENDIF

      ierr = NF90_INQ_VARID(ncid, cv, vid)    !get variable ID from variable named cv
      IF (ierr.NE.NF90_NOERR) THEN
          STOP 'inq v'
      ENDIF

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
         fv=real_fill_value
      ENDIF


    END SUBROUTINE nc_read_array_2d_float_orac

 

!-----------------------------------------------------------------------------
!-----------------------------------------------------------------------------
SUBROUTINE nc_read_array_2d_byte_to_real_orac(ncid,n1,n2,cv,v_out,wo)
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

  USE ECP_Constants

   IMPLICIT NONE

!   INCLUDE 'netcdf.inc'

      ! Input
      INTEGER,INTENT(IN) :: ncid,n1,n2,wo           ! number of footprints
      CHARACTER(LEN=*) :: cv                     ! variable name

      ! Output
      INTEGER, PARAMETER :: SINGLE = 4
      INTEGER, PARAMETER :: DOUBLE = 8
      real(KIND=sreal) :: v_out(1:n1,1:n2)    ! Variable (v): read in field 1-n1

      ! Local
      INTEGER :: ierr, vid, i, j,start(2), counter(2),stride(2)
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

      IF (wo.EQ.1) THEN
         write(*,*) 'read variable: ', cv
      ENDIF

!      write(*,*) ncid, cv,vid
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


      ierr = NF90_GET_ATT(ncid, vid, '_FillValue', fv)
      IF (wo.EQ.1) THEN
         write(*,*) '_FillVallue: ',fv
      ENDIF
      IF (ierr.NE.NF90_NOERR) THEN
         IF (wo.EQ.1) THEN
            write(*,*) '_FillVallue: not defined '
         ENDIF
      ENDIF

      DO i=1,n1                                            !question: outlier /outliar
         DO j=1,n2
            IF ( v(i,j).EQ.fv ) THEN
               v_out(i,j)=real_fill_value
            ELSE
               v_out(i,j)=real(v(i,j),kind=sreal)
            ENDIF
         ENDDO
      ENDDO



   END SUBROUTINE nc_read_array_2d_byte_to_real_orac

!-----------------------------------------------------------------------------
!-----------------------------------------------------------------------------
SUBROUTINE nc_read_array_2d_int_to_real_orac(ncid,n1,n2,cv,v_out,wo)
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

  USE ECP_Constants

   IMPLICIT NONE

!   INCLUDE 'netcdf.inc'

      ! Input
      INTEGER,INTENT(IN) :: ncid,n1,n2,wo           ! number of footprints
      CHARACTER(LEN=*) :: cv                     ! variable name

      ! Output
      INTEGER, PARAMETER :: SINGLE = 4
      INTEGER, PARAMETER :: DOUBLE = 8
      real(KIND=sreal) :: v_out(1:n1,1:n2)    ! Variable (v): read in field 1-n1

      ! Local
      INTEGER :: ierr, vid, i, j,start(2), counter(2),stride(2)
      REAL,PARAMETER :: miss=-9999.
      integer(kind=nint) :: v(1:n1,1:n2),fv

   ! End of header ----------------------------------------------------------

!in fortran the inner loop is next to variable
!e.g. var(n2,n1) n2 is inner loop, n1 is outer loop

      start(1) = 1
      start(2) = 1
      counter(1) = n1
      counter(2) = n2
      stride=1

      ierr = 0

      IF (wo.EQ.1) THEN
         write(*,*) 'read variable: ', cv
      ENDIF

!      write(*,*) ncid, cv,vid
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


      ierr = NF90_GET_ATT(ncid, vid, '_FillValue', fv)
      IF (wo.EQ.1) THEN
         write(*,*) '_FillVallue: ',fv
      ENDIF
      IF (ierr.NE.NF90_NOERR) THEN
         IF (wo.EQ.1) THEN
            write(*,*) '_FillVallue: not defined '
         ENDIF
      ENDIF

      DO i=1,n1                                            !question: outlier /outliar
         DO j=1,n2
            IF ( v(i,j).EQ.fv ) THEN
               v_out(i,j)=real_fill_value
            ELSE
               v_out(i,j)=real(v(i,j),kind=sreal)
            ENDIF
         ENDDO
      ENDDO



    END SUBROUTINE nc_read_array_2d_int_to_real_orac


!-----------------------------------------------------------------------------
!-----------------------------------------------------------------------------
SUBROUTINE nc_read_array_2d_byte_to_byte_orac(ncid,n1,n2,cv,v_out,wo)
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

  USE ECP_Constants

   IMPLICIT NONE

!   INCLUDE 'netcdf.inc'

      ! Input
      INTEGER,INTENT(IN) :: ncid,n1,n2,wo           ! number of footprints
      CHARACTER(LEN=*) :: cv                     ! variable name

      ! Output
      INTEGER, PARAMETER :: SINGLE = 4
      INTEGER, PARAMETER :: DOUBLE = 8
      integer(KIND=byte) :: v_out(1:n1,1:n2)    ! Variable (v): read in field 1-n1

      ! Local
      INTEGER :: ierr, vid, start(2), counter(2),stride(2)
      REAL,PARAMETER :: miss=-9999.
      integer(kind=byte) :: fv

   ! End of header ----------------------------------------------------------

!in fortran the inner loop is next to variable
!e.g. var(n2,n1) n2 is inner loop, n1 is outer loop

      start(1) = 1
      start(2) = 1
      counter(1) = n1
      counter(2) = n2
      stride=1

      ierr = 0

      IF (wo.EQ.1) THEN
         write(*,*) 'read variable: ', cv
      ENDIF

!      write(*,*) ncid, cv,vid
      ierr = NF90_INQ_VARID(ncid, cv, vid)    !get variable ID from variable named cv
      IF (ierr.NE.NF90_NOERR) THEN
          STOP 'inq v'
      ENDIF

      ierr = NF90_GET_VAR(ncid, vid,v_out,start,counter,stride)   !read variable from1-n1 and put into field
      IF (ierr.NE.NF90_NOERR) THEN
         STOP 'get v'
      ENDIF

!      write(*,*) minval(v),maxval(v)


      ierr = NF90_GET_ATT(ncid, vid, '_FillValue', fv)
      IF (wo.EQ.1) THEN
         write(*,*) '_FillVallue: ',fv
      ENDIF
      IF (ierr.NE.NF90_NOERR) THEN
         IF (wo.EQ.1) THEN
            write(*,*) '_FillVallue: not defined '
         ENDIF
      ENDIF




   END SUBROUTINE nc_read_array_2d_byte_to_byte_orac
