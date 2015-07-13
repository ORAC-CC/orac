!-------------------------------------------------------------------------------
! Name: sigtrap.F90
!
! Purpose:
! ECMWF signal trapping part
!
! Description and Algorithm details:
! Use signal_trap function to fetch error code.
!
! Arguments:
! N/A
!
! History:
! 2012/07/13, MJ: Original version
!
! $Id$
!
! Bugs:
! None known.
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
