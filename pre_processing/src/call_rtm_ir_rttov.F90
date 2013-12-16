! Name: call_rtm_ir_rttov.f90
!
!
! Purpose:
!
!
! Description and Algorithm details:
!
!
! Arguments:
! Name Type In/Out/Both Description
!
!
! Local variables:
! Name Type Description
!
!
! History:
! 2012/05/22, C. Poulsen: Initial version first version extracted form
!             code by M. Jerg
! 2012/07/04, C. Poulsen: Removed nviews
! 2012/17/07, C. Poulsen: Complete rewrite
! 2012/29/07, C. Poulsen: Fixed many bugs
! 2012/08/01, M. Jerg: Adds rac_up,rac_down
! 2012/08/14, C. Poulsen: Fixed tbc
! 2013/12/11, G. McGarragh: Significant code clean up.
!
!
! $Id$
!
!
! Bugs:
! none known

subroutine call_rtm_ir_rttov(errorstatus,transmission,radiance,imager_angles, &
                             channel_info,preproc_lwrtm)

   use netcdf

   use preproc_constants
   use preproc_structures
   use channel_structures
   use imager_structures
   use rttov_coef_io_mod
   use parkind1, only : jpim

   use rttov_types, only : transmission_type, &
                           radiance_type

   implicit none


   integer(kind=jpim)      :: errorstatus
   type(transmission_type) :: transmission
   type(radiance_type)     :: radiance
   type(imager_angles_s)   :: imager_angles
   type(channel_info_s)    :: channel_info
   type(preproc_lwrtm_s)   :: preproc_lwrtm

   integer                 :: ii
   integer                 :: nlevs,p
   integer                 :: nchans_ir
   integer                 :: test_write


   nlevs=size(transmission%tau_levels(:,1))

   nchans_ir=size(channel_info%channel_ids_rttov_coef_lw)


  ! extract ir transmissions and upwelling and downwelling radiances
   do ii=1,nchans_ir
      do p=1,nlevs

         preproc_lwrtm%taubc(ii,p)=transmission%tau_total(ii)/transmission%tau_levels(p,ii)
         preproc_lwrtm%tauac(ii,p)=transmission%tau_levels(p,ii)

         ! convert layers information to levels
         if (p .gt. 1) then
            preproc_lwrtm%radbc(ii,p)=(radiance%clear(ii)-radiance%up(p-1,ii))/preproc_lwrtm%tauac(ii,p)
            preproc_lwrtm%radiance_up(ii,p)=radiance%up(p-1,ii)
            preproc_lwrtm%radiance_down(ii,p)=radiance%down(p-1,ii)
         end if
         if (p .eq. 1) then
            preproc_lwrtm%radbc(ii,p)=(radiance%clear(ii)-radiance%up(p,ii))/preproc_lwrtm%tauac(ii,p)
            preproc_lwrtm%radiance_up(ii,p)=radiance%up(p,ii)
            preproc_lwrtm%radiance_down(ii,p)=radiance%down(p,ii)
         end if
      end do

   end do

   test_write=0
   if (test_write .eq. 1) then
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
