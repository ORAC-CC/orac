! Name: t_extrap_strat.F90
!
!
! Purpose:
! extrapolate temperature profile into stratosphere
! replace trpopuaes inversion with monotonicall decreasing temperature profile with gradient equal to that in the upper layer below the tropopause.
!
! Description and Algorithm details:
! from OCA ATBD
!
! Arguments:
! Name Type In/Out/Both Description
!	T0	Temperature profile / K
!	Z0	Corresponding altitudes / km
!
! Local variables:
! Name Type Description
!
!
! History:
! 2014/05/24:Original code C. Poulsen based on idl find_tp.pro
!
!
! $Id$
!
! Bugs:
!
!none known


subroutine t_extrap_strat(SPixel,ztp,index_ztp)

use SPixel_def  

  implicit none
  ! arguments

  type(SPixel_t), intent(inout) :: SPixel 
	integer :: index_ztp,k,test,j
	 real ::  ztp,dtdp

test=0

   if (test .eq. 1) then
	write(*,*)'ztp index_zt ',ztp,index_ztp
         write(*,*),' before strat told=[ $'
         do k=1,SPixel%RTM%LW%Np
            write(*,*)SPixel%RTM%LW%T(k),',$'

         enddo
            write(*,*)']'
      endif
!
!calculate below trpopuse lapse rate
!

dtdp=(SPixel%RTM%LW%T(index_ztp+2)-SPixel%RTM%LW%T(index_ztp))/(SPixel%RTM%LW%P(index_ztp+2)-SPixel%RTM%LW%P(index_ztp))

!write(*,*)'mm',SPixel%RTM%LW%T(index_ztp+2),SPixel%RTM%LW%T(index_ztp),SPixel%RTM%LW%P(index_ztp+2),SPixel%RTM%LW%P(index_ztp)
!write(*,*)'dtdp',dtdp

!
!reset temperature above
!

do j=index_ztp-1,1,-1
!do j=SPixel%RTM%LW%NP,index_ztp-1,-1
!write(*,*)'j',j
SPixel%RTM%LW%T(j)=SPixel%RTM%LW%T(index_ztp)+dtdp*(SPixel%RTM%LW%p(j)-SPixel%RTM%LW%p(index_ztp))
!write(*,*)'yy',SPixel%RTM%LW%p(j)-SPixel%RTM%LW%p(index_ztp)
!write(*,*)'SPixel%RTM%LW%T(index_ztp)',SPixel%RTM%LW%T(index_ztp),SPixel%RTM%LW%T(j)
enddo




if (test .eq. 1) then
         write(*,*),' after strat tnew=[ $'
         do k=1,SPixel%RTM%LW%Np
            write(*,*)SPixel%RTM%LW%T(k),',$'

         enddo
            write(*,*)']'
      endif



end subroutine t_extrap_strat