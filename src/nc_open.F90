!-------------------------------------------------------------------------------
! This software was developed within the ESA Cloud CCI Project and is based on
! routines developed during the ESA DUE GlobVapour Project. Copyright 2011, DWD,
! All Rights Reserved.
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! Name: nc_open.F90
!
! Purpose:
! Open a netcdf file for reading.
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
! 2011/12/22, Matthias Jerg: Produces draft code which opens netcdf file.
! 2012/07/06, Matthias Jerg: Extensively overhauls and restructures the code.
! 2014/08/04, Greg McGarragh: Cleaned up the code.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine nc_open(ncid,fname,ierr,wo)

   use netcdf

   implicit none

   ! Input
   character(len=*), intent(in)  :: fname
   integer,          intent(in)  :: wo

   ! Output
   integer,          intent(out) :: ncid
   integer,          intent(out) :: ierr

   ierr = nf90_open(path=trim(adjustl(fname)),mode = nf90_nowrite,ncid = ncid)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_open(), filename = ', trim(fname)
      stop
   endif

   if (wo .eq. 1) then
      write(*,*) 'opened netcdf file = ', trim(fname)
   endif

end subroutine nc_open
