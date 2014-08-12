!-------------------------------------------------------------------------------
! Name:
!    sigtrap
!
! Purpose:
!    ECMWF signal trapping part
!
! Arguments:
!    Name Type In/Out/Both Description
!    N/A
!
! Algorithm:
!
! Local variables:
!    Name Type Description
!
! History:
!    13th Jul 2012, Matthias Jerg: Original version
!
! Bugs:
!   None known.
!
! $Id$
!
!-------------------------------------------------------------------------------

integer*4 core_dump_flag,ireturn,signals(1),signal_trap

real a

core_dump_flag=0

signals(1)=0

ireturn=signal_trap(core_dump_flag,signals)

if (ireturn .lt. 0) then
   write(*,*) 'ERROR'
else if (ireturn .eq. 0) then
   write(*,*) 'fpe trapping is not set'
else
   write(*,*) 'fpe trapping mode=',ireturn
end if
