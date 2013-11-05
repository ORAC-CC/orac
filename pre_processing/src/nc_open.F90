! Name: nc_open.f90
!
!
! Purpose:
! Open a netvdf file for reading.
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
!2011/12/22: Matthias Jerg produces draft code which opens netcdf file.
!
! $Id$
!
! Bugs:
!
!none known


SUBROUTINE nc_open(ncid,fname,ierr,wo)

  USE netcdf
!  use typesizes

  IMPLICIT NONE

!  INCLUDE 'netcdf.inc'

 ! Input
  INTEGER :: wo
  CHARACTER(LEN=*) :: fname

  ! Output
  INTEGER :: ncid, ierr

  ierr = nf90_open(path=trim(adjustl(fname)),mode = nf90_nowrite,ncid = ncid)       !open file
   IF (ierr.NE.NF90_NOERR) THEN
      write(*,*) 'path and file:', fname
      stop 'error open input file'
   ENDIF

   IF (wo.EQ.1) THEN
      write(*,*) '---------------------------------------------'
      write(*,*) '  '
      write(*,*) 'open file: ', trim(fname)
      write(*,*) '  '
      write(*,*) '---------------------------------------------'
   ENDIF

  RETURN
END SUBROUTINE nc_open
