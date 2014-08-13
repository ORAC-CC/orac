!-------------------------------------------------------------------------------
! Name: call_rtm_ir_rttov.F90
!
! Purpose:
! Copy contents of RTTOV structures into ORAC structures.
!
! Description and Algorithm details:
! 1) Loop over channels and levels, copying contents of transmission and
!    radiance into preproc_lwrtm
!
! Arguments:
! Name           Type   In/Out/Both Description
! ------------------------------------------------------------------------------
! transmission   struct in   RTTOV-derived atmospheric transmission
! radiance       struct in   RTTOV-derived atmospheric radiance
! imager_angles  struct in   Summary of satellite geometry
! channel_info   struct in   Structure summarising the channels to be processed
! preproc_lwrtm  struct both Summary of longwave RTM data
!
! History:
! 2012/05/22, CP: Initial version first version extracted form
!             code by M. Jerg
! 2012/07/04, CP: Removed nviews
! 2012/17/07, CP: Complete rewrite
! 2012/29/07, CP: Fixed many bugs
! 2012/08/01, MJ: Adds rac_up,rac_down
! 2012/08/14, CP: Fixed tbc
! 2013/12/11, GM: Significant code clean up.
! 2014/07/10, AP: Slight tidying. Removed errorstatus as not used.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine call_rtm_ir_rttov(transmission,radiance,imager_angles, &
                             channel_info,preproc_lwrtm)

   use channel_structures
   use imager_structures
   use parkind1, only : jpim
   use preproc_constants
   use preproc_structures
   use rttov_coef_io_mod
   use rttov_types, only : transmission_type, &
                           radiance_type

   implicit none


   type(transmission_type), intent(in)    :: transmission
   type(radiance_type),     intent(in)    :: radiance
   type(imager_angles_s),   intent(in)    :: imager_angles
   type(channel_info_s),    intent(in)    :: channel_info
   type(preproc_lwrtm_s),   intent(inout) :: preproc_lwrtm

   integer :: ii
   integer :: nlevs,p
   integer :: nchans_ir


   nlevs=size(transmission%tau_levels(:,1))

   nchans_ir=size(channel_info%channel_ids_rttov_coef_lw)

   ! extract ir transmissions and upwelling and downwelling radiances
   do ii=1,nchans_ir
      do p=1,nlevs

         preproc_lwrtm%tauac(ii,p)=transmission%tau_levels(p,ii)
         preproc_lwrtm%taubc(ii,p)=transmission%tau_total(ii) / &
              transmission%tau_levels(p,ii)

         ! convert layers information to levels
         if (p .gt. 1) then
            preproc_lwrtm%radbc(ii,p)=(radiance%clear(ii)-radiance%up(p-1,ii))/&
                 preproc_lwrtm%tauac(ii,p)
            preproc_lwrtm%radiance_up(ii,p)=radiance%up(p-1,ii)
            preproc_lwrtm%radiance_down(ii,p)=radiance%down(p-1,ii)
         else if (p .eq. 1) then
            preproc_lwrtm%radbc(ii,p)=(radiance%clear(ii)-radiance%up(p,ii))/ &
                 preproc_lwrtm%tauac(ii,p)
            preproc_lwrtm%radiance_up(ii,p)=radiance%up(p,ii)
            preproc_lwrtm%radiance_down(ii,p)=radiance%down(p,ii)
         end if

      end do
   end do

   if (.false.) then
      ! these are not filled could be removed
      write(*,*) 'tbc(0,*)=[', preproc_lwrtm%taubc(1,:),']'
      write(*,*) 'tac(0,*)=[', preproc_lwrtm%tauac(1,:),']'
      write(*,*) 'rbc(0,*)=[', preproc_lwrtm%radbc(1,:),']'
      write(*,*) 'rad_up(0,*)=[',preproc_lwrtm%radiance_up(1,:),']'
      write(*,*) 'rad_dwn(0,*)=[',preproc_lwrtm%radiance_down(1,:),']'

      write(*,*) 'tbc(1,*)=[', preproc_lwrtm%taubc(2,:),']'
      write(*,*) 'tac(1,*)=[', preproc_lwrtm%tauac(2,:),']'
      write(*,*) 'rbc(1,*)=[', preproc_lwrtm%radbc(2,:),']'
      write(*,*) 'rad_up(1,*)=[',preproc_lwrtm%radiance_up(2,:),']'
      write(*,*) 'rad_dwn(1,*)=[',preproc_lwrtm%radiance_down(2,:),']'

      write(*,*) 'tbc(2,*)=[', preproc_lwrtm%taubc(3,:),']'
      write(*,*) 'tac(2,*)=[', preproc_lwrtm%tauac(3,:),']'
      write(*,*) 'rbc(2,*)=[', preproc_lwrtm%radbc(3,:),']'
      write(*,*) 'rad_up(2,*)=[',preproc_lwrtm%radiance_up(3,:),']'
      write(*,*) 'rad_dwn(2,*)=[',preproc_lwrtm%radiance_down(3,:),']'
   end if

end subroutine call_rtm_ir_rttov
