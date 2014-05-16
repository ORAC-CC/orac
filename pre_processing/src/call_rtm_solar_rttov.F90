! Name: call_rtm_solar_rttov.f90
!
!
! Purpose:
!
!
! Description and Algorithm details:

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
! 2012/07/04, C. Poulsen: remove nview dependance
! 2012/17/07, C. Poulsen: complete rewrite
! 2012/29/07, C. Poulsen: algorithm rewrite
! 2012/08/10, C. Poulsen: algorithm debug
! 2012/08/14, C. Poulsen: fixed bug in taubc
! 2013/12/11, G. McGarragh: Significant code clean up.
!
!
! $Id$
!
!
! Bugs:
! none known

subroutine call_rtm_solar_rttov(errorstatus,transmission,imager_angles, &
                                channel_info,preproc_swrtm,amf,za)

   use netcdf

   use preproc_constants
   use preproc_structures
   use channel_structures
   use imager_structures
   use rttov_coef_io_mod
   use parkind1, only : jpim

   use rttov_types, only : transmission_type


   implicit none

   integer(kind=jpim),      intent(in)    :: errorstatus
   type(transmission_type), intent(in)    :: transmission
   type(imager_angles_s),   intent(in)    :: imager_angles
   type(channel_info_s),    intent(in)    :: channel_info
   type(preproc_swrtm_s),   intent(inout) :: preproc_swrtm

   integer            :: ii
   integer            :: nlevs,p
   integer            :: nchans_solar
   integer(kind=jpim) :: test_write
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
   ! allocate transmisttance
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

         preproc_swrtm%taubc(ii,p)=new_trans_tau_level(nlevs,ii)/new_trans_tau_level(p,ii)
         preproc_swrtm%tauac(ii,p)=new_trans_tau_level(p,ii)
      end do
   end do

   deallocate(c_tau_level)
   deallocate(new_trans_tau_level)

   test_write=0
   if (test_write .eq. 1) then
     write(*,*) ' tbc_stt(0,*)=[',preproc_swrtm%taubc(1,:),']'
     write(*,*) ' tac_stt(0,*)=[',preproc_swrtm%tauac(1,:),']'
     write(*,*) ' tbc_stt(1,*)= [',preproc_swrtm%taubc(2,:),']'
     write(*,*) 'tac_stt(1,*)=[ ',preproc_swrtm%tauac(2,:),']'
     write(*,*) ' tbc_stt(2,*)= [',preproc_swrtm%taubc(3,:),']'
     write(*,*) 'tac_stt(2,*)=[ ',preproc_swrtm%tauac(3,:),']'
   endif

end subroutine call_rtm_solar_rttov
