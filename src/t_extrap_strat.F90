!-------------------------------------------------------------------------------
! Name: t_extrap_strat.F90
!
! Purpose:
! Extrapolate temperature profile into stratosphere.
! Replace trpopuaes inversion with monotonicall decreasing temperature profile
! with gradient equal to that in the upper layer below the tropopause.
!
! Description and Algorithm details:
! From OCA ATBD
!
! Arguments:
! Name Type In/Out/Both Description
!
! Local variables:
! Name Type Description
!
! History:
! 2014/05/24, C. Poulsen: Original code based on idl find_tp.pro
! 2014/08/04, G. McGarragh: A bit of cleanup.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine t_extrap_strat(SPixel,ztp,index_ztp)

   use SPixel_def

   implicit none

   ! Arguments
   type(SPixel_t), intent(inout) :: SPixel
   real,           intent(in)    :: ztp
   integer,        intent(in)    :: index_ztp

   ! Local variables
   integer :: j,k,test
   real    :: dtdp

   test=0

   if (test .eq. 1) then
      write(*,*)'ztp index_zt ',ztp,index_ztp
      write(*,*),'before strat told=[ $'
      do k=1,SPixel%RTM%LW%Np
         write(*,*)SPixel%RTM%LW%T(k),',$'
      end do
      write(*,*)']'
   end if

   ! Calculate below trpopuse lapse rate

    dtdp=(SPixel%RTM%LW%T(index_ztp+2)-SPixel%RTM%LW%T(index_ztp))/(SPixel%RTM%LW%P(index_ztp+2)-SPixel%RTM%LW%P(index_ztp))

!  write(*,*)'mm',SPixel%RTM%LW%T(index_ztp+2),SPixel%RTM%LW%T(index_ztp),SPixel%RTM%LW%P(index_ztp+2),SPixel%RTM%LW%P(index_ztp)
!  write(*,*)'dtdp',dtdp

   ! Reset temperature above

   do j=index_ztp-1,1,-1
!  do j=SPixel%RTM%LW%NP,index_ztp-1,-1
!     write(*,*)'j',j
      SPixel%RTM%LW%T(j)=SPixel%RTM%LW%T(index_ztp)+dtdp*(SPixel%RTM%LW%p(j)-SPixel%RTM%LW%p(index_ztp))
!     write(*,*)'yy',SPixel%RTM%LW%p(j)-SPixel%RTM%LW%p(index_ztp)
!     write(*,*)'SPixel%RTM%LW%T(index_ztp)',SPixel%RTM%LW%T(index_ztp),SPixel%RTM%LW%T(j)
   end do

   if (test .eq. 1) then
      write(*,*),' after strat tnew=[ $'
      do k=1,SPixel%RTM%LW%Np
         write(*,*)SPixel%RTM%LW%T(k),',$'
      end do
      write(*,*)']'
   end if

end subroutine t_extrap_strat
