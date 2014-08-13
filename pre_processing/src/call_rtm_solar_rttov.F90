!-------------------------------------------------------------------------------
! Name: call_rtm_solar_rttov.F90
!
! Purpose:
! Copy contents of RTTOV structures into ORAC structures.
!
! Description and Algorithm details:
! 1) Loop over channels and levels, copying contents of transmission and
!    radiance into preproc_swrtm
!
! Arguments:
! Name           Type   In/Out/Both Description
! ------------------------------------------------------------------------------
! transmission   struct in   RTTOV-derived atmospheric transmission
! radiance       struct in   RTTOV-derived atmospheric radiance
! imager_angles  struct in   Summary of satellite geometry
! channel_info   struct in   Structure summarising the channels to be processed
! preproc_swrtm  struct both Summary of shortwave RTM data
!
! History:
! 2012/05/22, CP: Initial version first version extracted form
!             code by M. Jerg
! 2012/07/04, CP: remove nview dependance
! 2012/17/07, CP: complete rewrite
! 2012/29/07, CP: algorithm rewrite
! 2012/08/10, CP: algorithm debug
! 2012/08/14, CP: fixed bug in taubc
! 2013/12/11, GM: Significant code clean up.
! 2014/07/10, AP: Slight tidying. Removed errorstatus as not used.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine call_rtm_solar_rttov(transmission,imager_angles, &
                                channel_info,preproc_swrtm,amf,za)

   use channel_structures
   use imager_structures
   use parkind1, only : jpim
   use preproc_constants
   use preproc_structures
   use rttov_coef_io_mod
   use rttov_types, only : transmission_type


   implicit none

   type(transmission_type), intent(in)    :: transmission
   type(imager_angles_s),   intent(in)    :: imager_angles
   type(channel_info_s),    intent(in)    :: channel_info
   type(preproc_swrtm_s),   intent(inout) :: preproc_swrtm

   integer            :: ii
   integer            :: nlevs,p
   integer            :: nchans_solar
   real               :: amf,za
   real(kind=sreal), allocatable :: c_tau_level(:,:)
   real(kind=sreal), allocatable :: new_trans_tau_level(:,:)


   nlevs=size(transmission%tau_levels(:,1))

   nchans_solar=size(channel_info%channel_ids_rttov_coef_sw)

   ! transmission%tau_levels transmittance from each standard pressure level
   ! then multiply by airmass factor, but must convert transmittance to optical
   ! depth first then back again

   ! allocate optical depth
   allocate(c_tau_level(nlevs,nchans_solar))
   ! allocate transmittance
   allocate(new_trans_tau_level(nlevs,nchans_solar))

   ! initialise
   c_tau_level=-999.
   new_trans_tau_level=-999

   ! loop over channels and levels and calculate transmissions
   do ii=1,nchans_solar
      do p=1,nlevs
         c_tau_level(p,ii)=-log(transmission%tau_levels(p,ii))
         c_tau_level(p,ii)=c_tau_level(p,ii)/amf
         new_trans_tau_level(p,ii)=exp(-c_tau_level(p,ii))
      end do
   end do

   do ii=1,nchans_solar
      do p=1,nlevs
         preproc_swrtm%tauac(ii,p)=new_trans_tau_level(p,ii)
         preproc_swrtm%taubc(ii,p)=new_trans_tau_level(nlevs,ii)/ &
              new_trans_tau_level(p,ii)
      end do
   end do

   deallocate(c_tau_level)
   deallocate(new_trans_tau_level)

   if (.false.) then
     write(*,*) 'tbc_stt(0,*)=[',preproc_swrtm%taubc(1,:),']'
     write(*,*) 'tac_stt(0,*)=[',preproc_swrtm%tauac(1,:),']'
     write(*,*) 'tbc_stt(1,*)=[',preproc_swrtm%taubc(2,:),']'
     write(*,*) 'tac_stt(1,*)=[',preproc_swrtm%tauac(2,:),']'
     write(*,*) 'tbc_stt(2,*)=[',preproc_swrtm%taubc(3,:),']'
     write(*,*) 'tac_stt(2,*)=[',preproc_swrtm%tauac(3,:),']'
   end if

end subroutine call_rtm_solar_rttov
