!-------------------------------------------------------------------------------
! This software was developed within the ESA Cloud CCI Project and is based on
! routines developed during the ESA DUE GlobVapour Project. Copyright 2011, DWD,
! All Rights Reserved.
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! Name: nc_close.F90
!
! Purpose:
! Close a netcdf file.
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
! 2014/08/04, Greg McGarragh: Cleaned up the code.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine nc_close(ncid,fname,wo,ierr)

   use netcdf

   implicit none

   ! Input
   integer,          intent(in)  :: ncid
   character(len=*), intent(in)  :: fname
   integer,          intent(in)  :: wo

   ! Output
   integer,          intent(out) :: ierr

   ierr = nf90_close(ncid)

   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_close(), filename = ', trim(fname)
      stop
   endif

   if (wo.eq.1) then
      write(*,*) 'closed netcdf file = ', trim(fname)
   endif

end subroutine nc_close
