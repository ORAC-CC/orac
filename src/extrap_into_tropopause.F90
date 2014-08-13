!-------------------------------------------------------------------------------
! Name: extrap_into_tropopause.F90
!
! Purpose:
! Find tropopause given temperature and height profile and remove by extrapolating
! into stratosphere
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
! 2012/05/24, C. Poulsen: Original code, based on idl t_extrap_tp.pro
! 2014/03/14, C. Poulsen: debugged and tested
! 2014/08/04, G. McGarragh: A bit of cleanup.
!
! $Id$
!
! Bugs:
! Not tested, full of bugs
!-------------------------------------------------------------------------------

subroutine extrap_into_tropopause(SPixel)

   use SPixel_def

   implicit none

   ! Arguments
   type(SPixel_t), intent(inout) :: SPixel

   ! Local variables
   real,allocatable :: t(:),z(:),t2(:),t3(:)
   real,allocatable :: t2a(:)
   real             :: lr,ztp,ttp,min_t,maxz,ztp2
   integer          :: nt,zdim,t2adim,i,j,nlevs,index_ztp,k

   index_ztp=-999.0

   nlevs=SPixel%RTM%LW%Np

   allocate(t(nlevs))
   allocate(t2(nlevs))
   allocate(t2a(nlevs))
   allocate(t3(nlevs))
   allocate(z(nlevs))

   do k=1,SPixel%RTM%LW%Np-1
!     write(*,*)'xx',SPixel%RTM%LW%T(k), SPixel%RTM%LW%T(k+1),k,SPixel%RTM%LW%P(k)
      if (SPixel%RTM%LW%T(k+1) .gt. SPixel%RTM%LW%T(k)) then
         if (SPixel%RTM%LW%P(k) .gt. 80.) then
         ! check lapse rate is large enough
         ! write(*,*)'lr',SPixel%RTM%LW%T(k+3)-SPixel%RTM%LW%T(k+2)
            if (SPixel%RTM%LW%T(k+3)-SPixel%RTM%LW%T(k+2) .gt. 2.) then
               index_ztp=k+1
               ztp=SPixel%RTM%LW%P(k+1)
               ! exit out of loop to improve the speed
               exit
            end if ! lapse raet
         end if ! gt 80.0
      end if
   end do

   t=SPixel%RTM%LW%T

!  z=SPixel%RTM%LW%H
!  write(*,*)'SPixel%RTM%LW%H',SPixel%RTM%LW%H

   ! this is atypical value for a moist adiabatic lapse rate (see wikipedia)
!  lr=-5 !lapse rate K/km
!  if (z(1) .le. z(2)) then
!     write(*,*),'z must be in descending order !'
!  end if


   ! find location of tropopause
!  call find_tropopause(t,z,ztp,nlevs,index_ztp)

   if (ztp .eq. -999) then
      write(*,*),'tropopause not found !',ztp
   end if

   ! now extrapolate the tropopause into stratosphere

   if (ztp .ne. -999) then
      call t_extrap_strat(SPixel,ztp,index_ztp)
   end if

   deallocate(t)
   deallocate(t2)
   deallocate(t2a)
   deallocate(t3)
   deallocate(z)

end subroutine  extrap_into_tropopause
