!-------------------------------------------------------------------------------
! Name:
!    Read_RTMData_nc
!
! Purpose:
!    Controls the reading of the long- and short-wave RTM data files.
!
! Arguments:
!    Name   Type         In/Out Description
!    Ctrl   struct       Both   Control structure
!    RTM    alloc struct Out    RTM structure
!    status int          Out    Error status
!
! Algorithm:
!    Call ReadLwRTM (reads longwave RTM data)
!    Call ReadSwRTM (reads shortwave RTM data)
!
! Local variables:
!    Name Type Description
!
! History:
!    11th Dec, 2000, Kevin M. Smith:
!       Original version
!    15th Aug 2001, Andy Smith:
!       Removed writes to stdout.
!    23rd Aug 2012, MJ:
!       Used initial file as template for netcdf read.
!     1st Aug 2014, Greg McGarragh:
!       Some cleanup.
!
! Bugs:
!   None known.
!
! $Id$
!
!-------------------------------------------------------------------------------

subroutine Read_RTMData_nc(Ctrl, RTM, status)

   use CTRL_def
   use ECP_Constants
   use RTM_def

   implicit none

   ! Argument declarations

   type(CTRL_t), intent(in)    :: Ctrl
   type(RTM_t),  intent(out)   :: RTM

   integer,      intent(inout) :: status

!  status = 0

   ! Longwave
   call Read_LwRTM_nc(Ctrl, RTM, status)
   write(*,*) 'Reading LW data done, status: ',status

   ! Shortwave
   if (status == 0) &
      call Read_SwRTM_nc(Ctrl, RTM, status)
   write(*,*) 'Reading SW data done, status: ',status

end subroutine Read_RTMData_nc
