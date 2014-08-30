!-------------------------------------------------------------------------------
! This software was developed within the ESA Cloud CCI Project and is based on
! routines developed during the ESA DUE GlobVapour Project. Copyright 2011, DWD,
! All Rights Reserved.
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! Name: nc_info_dim.F90
!
! Purpose:
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
! 2014/08/04, Greg McGarragh: Cleaned up the code.
!
! $Id: nc_dim_info.F90 2290 2014-08-12 08:24:01Z gmcgarragh $
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine nc_info(ncid,ndim,nvar,nattr,wo)

   use netcdf

   implicit none

   ! Input
   integer, intent(in) :: ncid
   integer, intent(in) :: wo

   ! Output
   integer, intent(out) :: ndim
   integer, intent(out) :: nvar
   integer, intent(out) :: nattr

   ! Local
   integer :: ierr

   ierr = 0

   ierr = nf90_inquire(ncid,ndim,nvar,nattr)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_inquire()'
      stop
   end if

end subroutine nc_info


subroutine nc_dim_id(ncid,name,did,wo)

   use netcdf

   implicit none

   ! Input
   integer,          intent(in)  :: ncid
   character(len=*), intent(in)  :: name
   integer,          intent(in)  :: wo

   ! Output
   integer,          intent(out) :: did

   ! Local
   integer :: ierr

   ierr = 0

   ierr = nf90_inq_dimid(ncid,name,did)
   if (ierr .ne. NF90_NOERR) then
   write(*,*) ierr,NF90_NOERR
      write(*,*) 'ERROR: nf90_inq_dimid()'
      stop
   end if

   if (wo .eq. 1) then
      write(*,*) 'did: ', did
   end if

end subroutine nc_dim_id


subroutine nc_dim_length(ncid,dname,did,n,wo)

   use netcdf

   implicit none

   ! Input
   integer,                      intent(in)  :: ncid
   integer,                      intent(in)  :: wo

   ! Output
   character(len=NF90_MAX_NAME), intent(out) :: dname
   integer,                      intent(in)  :: did
   integer,                      intent(out) :: n

   ! Local
   integer :: ierr

   ierr = 0

   ierr = nf90_inquire_dimension(ncid,did,dname,n)
   if (ierr .ne. NF90_NOERR) then
      stop 'inq dimlen'
   end if

   if (wo .eq. 1) then
      write(*,*) 'n: ', n
   end if

end subroutine nc_dim_length
