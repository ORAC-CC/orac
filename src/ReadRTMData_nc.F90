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
!   20120823 MJ uses initial file as template for netcdf read.
!
! Bugs:
!   None known.
!
!------------------------------------------------------------------------------------
subroutine Read_RTMData_nc(Ctrl, RTM, status)

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
  call Read_LwRTM_nc(Ctrl, RTM, status)
  write(*,*) 'Reading LW data done (status)',status

  !  SHORTWAVE SECTION
  if (status == 0) call Read_SwRTM_nc(Ctrl, RTM, status)
  write(*,*) 'Reading SW data done (status)',status
  
end subroutine Read_RTMData_nc
