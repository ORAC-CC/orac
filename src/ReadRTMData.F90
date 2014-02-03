! Name:
!    Read_RTMData
!
! Purpose:
!    Controls the reading of the long- and short-wave RTM data files. 
!
! Arguments:
!    Name         Type         In/Out   Description
!    Ctrl         struct       Both     Control structure
!    RTM          alloc struct Out      RTM structure
!    status       int          Out      Error status               
!    
! Algorithm:
!    Call ReadLwRTM (reads longwave RTM data)
!    Call ReadSwRTM (reads shortwave RTM data)
!
! Local variables:
! None
!
! History:
!   11th December, 2000, Kevin M. Smith : original version
!   15th Aug 2001, Andy Smith:
!      Removed writes to stdout.
!
! Bugs:
!   None known.
!
! $Id$
!
!------------------------------------------------------------------------------------
subroutine Read_RTMData(Ctrl, RTM, status)

   use CTRL_def
   use RTM_def
   use ECP_Constants

   implicit none

!  Argument declarations

   type(CTRL_t) :: Ctrl
   type(RTM_t)  :: RTM
   
   integer      :: status
   
!   status = 0

!  LONGWAVE SECTION

   call Read_LwRTM(Ctrl, RTM, status)
!   write(*,*) 'LW',status
!  SHORTWAVE SECTION

   if (status == 0) call Read_SwRTM(Ctrl, RTM, status)
!   write(*,*) 'SW',status				
    				    				    
end subroutine Read_RTMData
