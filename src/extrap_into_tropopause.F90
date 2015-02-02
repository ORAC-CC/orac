!-------------------------------------------------------------------------------
! Name: extrap_into_tropopause.F90
!
! Purpose:
! Find tropopause given temperature and height profile and remove by
! extrapolating into stratosphere
!
! Description and Algorithm details:
!
! Arguments:
! Name   Type   In/Out/Both Description
! SPixel struct Both Summarises the details of the pixel to be processed.
!
! History:
! 2012/05/24, C. Poulsen: Original code, based on idl t_extrap_tp.pro
! 2014/03/14, C. Poulsen: debugged and tested
! 2014/08/04, G. McGarragh: A bit of cleanup.
! 2014/08/20, O. Sus: removed superfluous commata after write statements
! 2015/01/30, A. Povey: Tidying and merging with t_extrap_strat.
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
   real,    parameter :: min_pressure = 80.0
   logical, parameter :: test = .false.
   integer            :: k, index_ztp
   real               :: dtdp

   
   index_ztp = 0

   ! Loop from top of atmosphere down
   do k=2,SPixel%RTM%LW%Np-2
      ! Check that we aren't in the stratosphere
      if (SPixel%RTM%LW%P(k-1) .le. min_pressure) cycle

      ! Search for a negative temperature gradient to identify the tropopause
      if (SPixel%RTM%LW%T(k) .gt. SPixel%RTM%LW%T(k-1)) then
         ! Check lapse rate is large enough
         if (SPixel%RTM%LW%T(k+2)-SPixel%RTM%LW%T(k+1) .gt. 2.) then
            index_ztp=k
            exit
         end if ! lapse rate
      end if
   end do

   if (index_ztp .eq. 0) then
      write(*,*) 'WARNING: extrap_into_tropopause(): Tropopause not found'
   else
      if (test) then
         write(*,*) 'ztp index_zt ',SPixel%RTM%LW%P(index_ztp),index_ztp
         write(*,*) 'before strat told=[ $'
         do k=1,SPixel%RTM%LW%Np
            write(*,*) SPixel%RTM%LW%T(k),',$'
         end do
         write(*,*) ']'
      end if

      ! Calculate below tropopause lapse rate
      dtdp = (SPixel%RTM%LW%T(index_ztp+2) - SPixel%RTM%LW%T(index_ztp)) / &
             (SPixel%RTM%LW%P(index_ztp+2) - SPixel%RTM%LW%P(index_ztp))

      ! Reset temperature above tropopause
      do k=index_ztp-1,1,-1
         SPixel%RTM%LW%T(k) = SPixel%RTM%LW%T(index_ztp) + &
              dtdp * (SPixel%RTM%LW%P(k) - SPixel%RTM%LW%P(index_ztp))
      end do

      if (test) then
         write(*,*) ' after strat tnew=[ $'
         do k=1,SPixel%RTM%LW%Np
            write(*,*) SPixel%RTM%LW%T(k),',$'
         end do
         write(*,*) ']'
      end if
   end if


end subroutine  extrap_into_tropopause
