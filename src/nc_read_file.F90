! ------------------------------------------------------------------------------
! Name: nc_read_file.F90
!
! Purpose:
! File contains subroutines to read netcdf files for various variable types.
!
! Description and Algorithm details:
!
! Arguments:
! Name Type In/Out/Both Description
!
! Local variables:
! Name Type Description
!
! History:
! 2012/02/03, Matthias Jerg: cleans out prototype code to prepare repository
!    upload.
! 2012/02/09, Matthias Jerg: Adds routines for ORAC.
! 2012/07/06, Matthias Jerg: Extensively overhauls and restructures the code
! 2012/08/21, Matthias Jerg: Builds this code collection based on pre- and
!    postprocessing code collections.
! 2014/04/20, Greg McGarragh: Cleaned up the code.
! 2014/04/20, Greg McGarragh: Added nc_read_array_2d_float_to_float_orac() and
!                                   nc_read_array_2d_float_to_float_orac2().
! 2014/04/25, Greg McGarragh: Cleaned up the code.
!
! $Id$
!
! Bugs:
! none known
! ------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! This software was developed within the ESA Cloud CCI Project and is based on
! routines developed during the ESA DUE GlobVapour Project.
!
! Copyright 2011, DWD, All Rights Reserved.

! Created on: 05/12/10
!             by Matthias Jerg, DWD/KU22 (matthias.jerg@dwd.de)
!             based on code provided by Nadine Schneider (nadine.schneider@dwd.de).
! ------------------------------------------------------------------------------

SUBROUTINE nc_read_array_1d_int_to_int_orac(ncid,n1,cv,v,wo)

   USE netcdf

   USE ECP_Constants

   IMPLICIT NONE

   ! Input
   INTEGER,INTENT(IN) :: ncid,n1,wo ! number of footprints
   CHARACTER(LEN=*),INTENT(IN) :: cv ! variable name

   ! Output
   INTEGER(KIND=NINT),INTENT(OUT) :: v(1:n1) ! the variable read in

   ! Local
   INTEGER :: ierr,vid,start(1),counter(1),stride(1)
   REAL,PARAMETER :: miss=-9999.
   INTEGER(KIND=NINT) :: fv

   ! Fortran is column-major order but NetCDF stores in row-major order
   start(1) = 1
   counter(1) = n1
   stride = 1

   ierr = 0

   IF (wo.EQ.1) THEN
      write(*,*) 'read variable: ', cv
   ENDIF

   ierr = NF90_INQ_VARID(ncid, cv, vid)
   IF (ierr.NE.NF90_NOERR) THEN
       STOP 'inq v'
   ENDIF

   ierr = NF90_GET_VAR(ncid,vid,v,start,counter,stride)
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



SUBROUTINE nc_read_array_1d_float_to_float_orac(ncid,n1,cv,v,wo)

  USE netcdf

  USE ECP_Constants

   IMPLICIT NONE

   ! Input
   INTEGER,INTENT(IN) :: ncid,n1,wo ! number of footprints
   CHARACTER(LEN=*),INTENT(IN) :: cv ! variable name

   ! Output
   REAL(KIND=SREAL),INTENT(OUT) :: v(1:n1) ! the variable read in

   ! Local
   INTEGER :: ierr,vid,start(1),counter(1),stride(1)
   REAL,PARAMETER :: miss=-9999.
   REAL(KIND=SREAL) :: fv

   ! Fortran is column-major order but NetCDF stores in row-major order
   start(1) = 1
   counter(1) = n1
   stride = 1

   ierr = 0

   IF (wo.EQ.1) THEN
      write(*,*) 'read variable: ', cv
   ENDIF

   ierr = NF90_INQ_VARID(ncid, cv, vid)
   IF (ierr.NE.NF90_NOERR) THEN
       STOP 'inq v'
   ENDIF

   ierr = NF90_GET_VAR(ncid,vid,v,start,counter,stride)
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



SUBROUTINE nc_read_array_2d_float_to_float_orac(ncid,n1,n2,cv,v,wo)

  USE netcdf

  USE ECP_Constants

   IMPLICIT NONE

   ! Input
   INTEGER,INTENT(IN) :: ncid,n1,n2,wo ! number of footprints
   CHARACTER(LEN=*),INTENT(IN) :: cv ! variable name

   ! Output
   REAL(KIND=SREAL),INTENT(OUT) :: v(1:n1,1:n2) ! the variable read in

   ! Local
   INTEGER :: ierr,vid,start(2),counter(2),stride(2)
   REAL,PARAMETER :: miss=-9999.
   REAL(KIND=SREAL) :: fv

   ! Fortran is column-major order but NetCDF stores in row-major order
   start = 1

   counter(1) = n2
   counter(2) = n1

   stride = 1

   ierr = 0

   IF (wo.EQ.1) THEN
      write(*,*) 'read variable: ', cv
   ENDIF

   ierr = NF90_INQ_VARID(ncid, cv, vid)
   IF (ierr.NE.NF90_NOERR) THEN
       STOP 'inq v'
   ENDIF

   ierr = NF90_GET_VAR(ncid,vid,v,start,counter,stride)
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

END SUBROUTINE nc_read_array_2d_float_to_float_orac



SUBROUTINE nc_read_array_2d_float_to_float_orac2(ncid,n1,n2,i3,cv,v,wo)

  USE netcdf

  USE ECP_Constants

   IMPLICIT NONE

   ! Input
   INTEGER,INTENT(IN) :: ncid,n1,n2,i3,wo ! number of footprints
   CHARACTER(LEN=*),INTENT(IN) :: cv ! variable name

   ! Output
   REAL(KIND=SREAL),INTENT(OUT) :: v(1:n1,1:n2) ! the variable read in

   ! Local
   INTEGER :: ierr,vid,start(3),counter(3),stride(3)
   REAL,PARAMETER :: miss=-9999.
   REAL(KIND=SREAL) :: fv

   ! Fortran is column-major order but NetCDF stores in row-major order
   start(1) = i3
   start(2) = 1
   start(3) = 1

   counter(1) = 1
   counter(2) = n2
   counter(3) = n1

   stride = 1

   ierr = 0

   IF (wo.EQ.1) THEN
      write(*,*) 'read variable: ', cv
   ENDIF

   ierr = NF90_INQ_VARID(ncid, cv, vid)
   IF (ierr.NE.NF90_NOERR) THEN
       STOP 'inq v'
   ENDIF

   ierr = NF90_GET_VAR(ncid,vid,v,start,counter,stride)
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

END SUBROUTINE nc_read_array_2d_float_to_float_orac2



SUBROUTINE nc_read_array_3d_float_to_float_orac(ncid,n1,n2,n3,cv,v,wo)

  USE netcdf

  USE ECP_Constants

   IMPLICIT NONE

   ! Input
   INTEGER,INTENT(IN) :: ncid,n1,n2,n3,wo ! number of footprints
   CHARACTER(LEN=*),INTENT(IN) :: cv ! variable name

   ! Output
   REAL(KIND=SREAL),INTENT(OUT) :: v(1:n1,1:n2,1:n3) ! the variable read in

   ! Local
   INTEGER :: ierr,vid,start(3),counter(3),stride(3)
   REAL,PARAMETER :: miss=-9999.
   REAL(KIND=SREAL) :: fv

   ! Fortran is column-major order but NetCDF stores in row-major order
   start = 1

   counter(1) = n3
   counter(2) = n2
   counter(3) = n1

   stride = 1

   ierr = 0

   IF (wo.EQ.1) THEN
      write(*,*) 'read variable: ', cv
   ENDIF

   ierr = NF90_INQ_VARID(ncid, cv, vid)
   IF (ierr.NE.NF90_NOERR) THEN
       STOP 'inq v'
   ENDIF

   ierr = NF90_GET_VAR(ncid,vid,v,start,counter,stride)
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

END SUBROUTINE nc_read_array_3d_float_to_float_orac



SUBROUTINE nc_read_array_3d_float_orac(ncid,n1,n2,ichan,cv,v,wo)

  USE netcdf

  USE ECP_Constants

   IMPLICIT NONE

   ! Input
   INTEGER,INTENT(IN) :: ncid,n1,n2,ichan,wo ! number of footprints
   CHARACTER(LEN=*),INTENT(IN) :: cv ! variable name

   ! Output
   REAL(KIND=SREAL),INTENT(OUT) :: v(1:n1,1:n2,1) ! the variable read in

   ! Local
   INTEGER :: ierr,vid,start(3),counter(3),stride(3)
   REAL,PARAMETER :: miss=-9999.
   REAL(KIND=SREAL) :: fv

   ! Fortran is column-major order but NetCDF stores in row-major order
   start(1) = 1
   start(2) = 1
   start(3) = ichan

   counter(1) = n1
   counter(2) = n2
   counter(3) = 1

   stride = 1

   ierr = 0

   IF (wo.EQ.1) THEN
      write(*,*) 'read variable: ', cv
   ENDIF

   ierr = NF90_INQ_VARID(ncid, cv, vid)
   IF (ierr.NE.NF90_NOERR) THEN
       STOP 'inq v'
   ENDIF

   ierr = NF90_GET_VAR(ncid,vid,v,start,counter,stride)
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

  USE netcdf

  USE ECP_Constants

   IMPLICIT NONE

   ! Input
   INTEGER,INTENT(IN) :: ncid,n1,ichan,wo ! number of footprints
   CHARACTER(LEN=*),INTENT(IN) :: cv ! variable name

   ! Output
   REAL(KIND=SREAL),INTENT(OUT) :: v(1:n1,1) ! the variable read in

   ! Local
   INTEGER :: ierr,vid,start(2),counter(2),stride(2)
   REAL,PARAMETER :: miss=-9999.
   REAL(KIND=SREAL) :: fv

   ! Fortran is column-major order but NetCDF stores in row-major order
   start(1) = ichan
   start(2) = 1

   counter(1) = 1
   counter(2) = n1

   stride = 1

   ierr = 0

   IF (wo.EQ.1) THEN
      write(*,*) 'read variable: ', cv
   ENDIF

   ierr = NF90_INQ_VARID(ncid, cv, vid)
   IF (ierr.NE.NF90_NOERR) THEN
      write(*,*) 'inq v, this is it',start,counter,stride,vid,ncid,'STOP'
       STOP
   ENDIF

   ierr = NF90_GET_VAR(ncid,vid,v,start,counter,stride)
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

  USE netcdf

  USE ECP_Constants

   IMPLICIT NONE

   ! Input
   INTEGER,INTENT(IN) :: ncid,n1,ichan,ilay,wo ! number of footprints
   CHARACTER(LEN=*),INTENT(IN) :: cv ! variable name

   ! Output
   REAL(KIND=SREAL),INTENT(OUT) :: v(1:n1,1,1) ! the variable read in

   ! Local
   INTEGER :: ierr,vid,start(3),counter(3),stride(3)
   REAL,PARAMETER :: miss=-9999.
   REAL(KIND=SREAL) :: fv

   ! Fortran is column-major order but NetCDF stores in row-major order
   start(1) = ichan
   start(2) = ilay
   start(3) = 1

   counter(1) = 1
   counter(2) = 1
   counter(3) = n1

   stride = 1

   ierr = 0

   IF (wo.EQ.1) THEN
      write(*,*) 'read variable: ', cv
   ENDIF

   ierr = NF90_INQ_VARID(ncid, cv, vid)
   IF (ierr.NE.NF90_NOERR) THEN
       STOP 'inq v'
   ENDIF

   ierr = NF90_GET_VAR(ncid,vid,v,start,counter,stride)
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

  USE netcdf

  USE ECP_Constants

   IMPLICIT NONE

   ! Input
   INTEGER,INTENT(IN) :: ncid,n1,n2,wo ! number of footprints
   CHARACTER(LEN=*),INTENT(IN) :: cv ! variable name

   ! Output
   REAL(KIND=DREAL),INTENT(OUT) :: v(1:n1,1:n2) ! the variable read in

   ! Local
   INTEGER :: ierr,vid,start(2),counter(2),stride(2)
   REAL,PARAMETER :: miss=-9999.
   REAL(KIND=DREAL) :: fv

   ! Fortran is column-major order but NetCDF stores in row-major order
   start(1) = 1
   start(2) = 1

   counter(1) = n1
   counter(2) = n2

   stride = 1

   ierr = 0

   IF (wo.EQ.1) THEN
      write(*,*) 'read variable: ', cv
   ENDIF

   ierr = NF90_INQ_VARID(ncid, cv, vid)
   IF (ierr.NE.NF90_NOERR) THEN
       STOP 'inq v'
   ENDIF

   ierr = NF90_GET_VAR(ncid,vid,v,start,counter,stride)
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

  USE netcdf

  USE ECP_Constants

   IMPLICIT NONE

   ! Input
   INTEGER,INTENT(IN) :: ncid,n1,n2,wo ! number of footprints
   CHARACTER(LEN=*),INTENT(IN) :: cv ! variable name

   ! Output
   REAL(KIND=SREAL),INTENT(OUT) :: v(1:n1,1:n2) ! the variable read in

   ! Local
   INTEGER :: ierr,vid,start(2),counter(2),stride(2)
   REAL,PARAMETER :: miss=-9999.
   REAL(KIND=SREAL) :: fv

   ! Fortran is column-major order but NetCDF stores in row-major order
   start(1) = 1
   start(2) = 1

   counter(1) = n1
   counter(2) = n2

   stride = 1

   ierr = 0

   IF (wo.EQ.1) THEN
      write(*,*) 'read variable: ', cv
   ENDIF

   ierr = NF90_INQ_VARID(ncid, cv, vid)
   IF (ierr.NE.NF90_NOERR) THEN
       STOP 'inq v'
   ENDIF

   ierr = NF90_GET_VAR(ncid,vid,v,start,counter,stride)
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



SUBROUTINE nc_read_array_2d_byte_to_real_orac(ncid,n1,n2,cv,v_out,wo)

  USE netcdf

  USE ECP_Constants

   IMPLICIT NONE

   ! Input
   INTEGER,INTENT(IN) :: ncid,n1,n2,wo ! number of footprints
   CHARACTER(LEN=*),INTENT(IN) :: cv ! variable name

   ! Output
   REAL(KIND=SREAL),INTENT(OUT) :: v_out(1:n1,1:n2) ! the variable read in

   ! Local
   INTEGER :: ierr,vid,i,j,start(2),counter(2),stride(2)
   REAL,PARAMETER :: miss=-9999.
   INTEGER(KIND=BYTE) :: v(1:n1,1:n2),fv

   ! Fortran is column-major order but NetCDF stores in row-major order
   start(1) = 1
   start(2) = 1

   counter(1) = n1
   counter(2) = n2

   stride = 1

   ierr = 0

   IF (wo.EQ.1) THEN
      write(*,*) 'read variable: ', cv
   ENDIF

   ierr = NF90_INQ_VARID(ncid, cv, vid)
   IF (ierr.NE.NF90_NOERR) THEN
       STOP 'inq v'
   ENDIF

   ierr = NF90_GET_VAR(ncid,vid,v,start,counter,stride)
   IF (ierr.NE.NF90_NOERR) THEN
      STOP 'get v'
   ENDIF
   IF (wo.EQ.1) THEN
      write(*,*) 'first data value: ', v(1,1)
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

   DO i=1,n1
      DO j=1,n2
         IF ( v(i,j).EQ.fv ) THEN
            v_out(i,j)=real_fill_value
         ELSE
            v_out(i,j)=real(v(i,j),kind=sreal)
         ENDIF
      ENDDO
   ENDDO

END SUBROUTINE nc_read_array_2d_byte_to_real_orac



SUBROUTINE nc_read_array_2d_int_to_real_orac(ncid,n1,n2,cv,v_out,wo)

  USE netcdf

  USE ECP_Constants

   IMPLICIT NONE

   ! Input
   INTEGER,INTENT(IN) :: ncid,n1,n2,wo ! number of footprints
   CHARACTER(LEN=*),INTENT(IN) :: cv ! variable name

   ! Output
   REAL(KIND=SREAL),INTENT(OUT) :: v_out(1:n1,1:n2) ! the variable read in

   ! Local
   INTEGER :: ierr,vid,i,j,start(2),counter(2),stride(2)
   REAL,PARAMETER :: miss=-9999.
   INTEGER(KIND=NINT) :: v(1:n1,1:n2),fv

   ! Fortran is column-major order but NetCDF stores in row-major order
   start(1) = 1
   start(2) = 1

   counter(1) = n1
   counter(2) = n2

   stride = 1

   ierr = 0

   IF (wo.EQ.1) THEN
      write(*,*) 'read variable: ', cv
   ENDIF

   ierr = NF90_INQ_VARID(ncid, cv, vid)
   IF (ierr.NE.NF90_NOERR) THEN
       STOP 'inq v'
   ENDIF

   ierr = NF90_GET_VAR(ncid,vid,v,start,counter,stride)
   IF (ierr.NE.NF90_NOERR) THEN
      STOP 'get v'
   ENDIF
   IF (wo.EQ.1) THEN
      write(*,*) 'first data value: ', v(1,1)
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

   DO i=1,n1
      DO j=1,n2
         IF ( v(i,j).EQ.fv ) THEN
            v_out(i,j)=real_fill_value
         ELSE
            v_out(i,j)=real(v(i,j),kind=sreal)
         ENDIF
      ENDDO
   ENDDO

END SUBROUTINE nc_read_array_2d_int_to_real_orac



SUBROUTINE nc_read_array_2d_byte_to_byte_orac(ncid,n1,n2,cv,v_out,wo)

  USE netcdf

  USE ECP_Constants

   IMPLICIT NONE

   ! Input
   INTEGER,INTENT(IN) :: ncid,n1,n2,wo ! number of footprints
   CHARACTER(LEN=*),INTENT(IN) :: cv ! variable name

   ! Output
   INTEGER(KIND=BYTE),INTENT(OUT) :: v_out(1:n1,1:n2) ! the variable read in

   ! Local
   INTEGER :: ierr,vid,start(2),counter(2),stride(2)
   REAL,PARAMETER :: miss=-9999.
   INTEGER(KIND=BYTE) :: fv

   ! Fortran is column-major order but NetCDF stores in row-major order
   start(1) = 1
   start(2) = 1

   counter(1) = n1
   counter(2) = n2

   stride = 1

   ierr = 0

   IF (wo.EQ.1) THEN
      write(*,*) 'read variable: ', cv
   ENDIF

   ierr = NF90_INQ_VARID(ncid, cv, vid)
   IF (ierr.NE.NF90_NOERR) THEN
       STOP 'inq v'
   ENDIF

   ierr = NF90_GET_VAR(ncid,vid,v_out,start,counter,stride)
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

END SUBROUTINE nc_read_array_2d_byte_to_byte_orac
