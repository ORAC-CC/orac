!-------------------------------------------------------------------------------
! This software was developed within the ESA Cloud CCI Project and is based on
! routines developed during the ESA DUE GlobVapour Project. Copyright 2011, DWD,
! All Rights Reserved.
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! Name: nc_write.F90
!
! Purpose:
! This file contains a collection of subroutines which write L2 data in
! different representations.
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
! 2011/12/19, Matthias Jerg: Creates initial file.
! 2012/09/15, Matthias Jerg: Specified error more clearly.
! 2012/11/03, MST: Changed the way nf90_put_var is called in nc_write_short.
! 2013/01/23, Caroline Poulsen: Changed selected varname also had to change.
!    write_secondary_inc
! 2013/xx/xx, Matthias Jerg: Implemented MST change.
! 2014/08/04, Greg McGarragh: Cleaned up the code.
!
! $Id: nc_write_L2.F90 2290 2014-08-12 08:24:01Z gmcgarragh $
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine nc_write_float(ncid,var_name,vid,v,ix,nx,jy,ny,wo,ierr)

   use common_constants
   use netcdf

   implicit none

   ! Input
   integer,          intent(in)  :: ncid
   character(len=*), intent(in)  :: var_name
   integer,          intent(in)  :: vid
   integer,          intent(in)  :: ix
   integer,          intent(in)  :: nx
   integer,          intent(in)  :: jy
   integer,          intent(in)  :: ny
   integer,          intent(in)  :: wo
   integer,          intent(out) :: ierr
   real(kind=sreal), dimension(ix:nx,jy:ny), intent(in) :: v

   ! Local
   integer :: start(2), counter(2), stride(2)

   start(1) = 1
   start(2) = 1

   stride = 1

   counter(1) = nx-ix+1
   counter(2) = ny-jy+1

   ierr = nf90_put_var(ncid, vid, v(ix:nx,jy:ny), start, counter, stride)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_var(), var_name = ', var_name
      stop
   end if

   if (wo .eq. 1) then
      write(*,*) 'wrote variable: ', trim(var_name)
   end if

end subroutine nc_write_float


subroutine nc_write_double(ncid,var_name,vid,v,ix,nx,jy,ny,wo,ierr)

   use common_Constants
   use netcdf

   implicit none

   ! Input
   integer,          intent(in)  :: ncid
   character(len=*), intent(in)  :: var_name
   integer,          intent(in)  :: vid
   integer,          intent(in)  :: ix
   integer,          intent(in)  :: nx
   integer,          intent(in)  :: jy
   integer,          intent(in)  :: ny
   integer,          intent(in)  :: wo
   integer,          intent(out) :: ierr
   real(kind=dreal), dimension(ix:nx,jy:ny), intent(in) :: v

   ! Local
   integer :: start(2), counter(2), stride(2)

   start(1) = 1
   start(2) = 1

   stride = 1

   counter(1) = nx-ix+1
   counter(2) = ny-jy+1


   ierr = nf90_put_var(ncid, vid, v(ix:nx,jy:ny), start, counter, stride)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_var(), var_name = ', var_name
      stop
   end if

   if (wo .eq. 1) then
      write(*,*) 'wrote variable: ', trim(var_name)
   end if

end subroutine nc_write_double



subroutine nc_write_short(ncid,var_name,vid,v,ix,nx,jy,ny,wo,ierr)

   use common_constants
   use netcdf

   implicit none

   ! Input
   integer,          intent(in)  :: ncid
   character(len=*), intent(in)  :: var_name
   integer,          intent(in)  :: vid
   integer,          intent(in)  :: ix
   integer,          intent(in)  :: nx
   integer,          intent(in)  :: jy
   integer,          intent(in)  :: ny
   integer,          intent(in)  :: wo
   integer ,         intent(out) :: ierr
   integer(kind=sint), dimension(ix:nx,jy:ny), intent(in) :: v

   ! Local
   integer :: start(2), counter(2), stride(2)

   start(1) = 1
   start(2) = 1

   stride = 1

   counter(1) = nx-ix+1
   counter(2) = ny-jy+1

   ierr = nf90_put_var(ncid, vid, v(ix:nx,jy:ny), start, counter, stride)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_var(), var_name = ', var_name
      stop
   end if

   if (wo .eq. 1) then
      write(*,*) 'wrote variable: ', trim(var_name)
   end if

end subroutine nc_write_short



subroutine nc_write_long(ncid,var_name,vid,v,ix,nx,jy,ny,wo,ierr)

   use common_Constants
   use netcdf

   implicit none

   ! Input
   integer,          intent(in)  :: ncid
   character(len=*), intent(in)  :: var_name
   integer,          intent(in)  :: vid
   integer,          intent(in)  :: ix
   integer,          intent(in)  :: nx
   integer,          intent(in)  :: jy
   integer,          intent(in)  :: ny
   integer,          intent(in)  :: wo
   integer,          intent(out) :: ierr
   integer, dimension(ix:nx,jy:ny), intent(in) :: v

   ! Local
   integer :: start(2), counter(2), stride(2)

   start(1) = 1
   start(2) = 1

   stride = 1

   counter(1) = nx-ix+1
   counter(2) = ny-jy+1

   ierr = nf90_put_var(ncid, vid, v(ix:nx,jy:ny), start, counter, stride)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_var(), var_name = ', var_name
      stop
   end if

   if (wo .eq. 1) then
      write(*,*) 'wrote variable: ', trim(var_name)
   end if

end subroutine nc_write_long


subroutine nc_write_byte(ncid,var_name,vid,v,ix,nx,jy,ny,wo,ierr)

   use common_constants
   use netcdf

   implicit none

   ! Input
   integer,          intent(in)  :: ncid
   character(len=*), intent(in)  :: var_name
   integer,          intent(in)  :: vid
   integer,          intent(in)  :: ix
   integer,          intent(in)  :: nx
   integer,          intent(in)  :: jy
   integer,          intent(in)  :: ny
   integer,          intent(in)  :: wo
   integer,          intent(out) :: ierr
   integer(kind=byte), dimension(ix:nx,jy:ny), intent(in) :: v

   ! Local
   integer :: start(2), counter(2), stride(2)

   start(1) = 1
   start(2) = 1

   stride = 1

   counter(1) = nx-ix+1
   counter(2) = ny-jy+1

   ierr = nf90_put_var(ncid, vid, v(ix:nx,jy:ny), start, counter, stride)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_var(), var_name = ', var_name
      stop
   end if

   if (wo .eq. 1) then
      write(*,*) 'wrote variable: ', trim(var_name)
   end if

end subroutine nc_write_byte
