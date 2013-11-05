! Name: call_rtm_solar_rttov.f90
!
!
! Purpose:
!
! call rttov and output the parameters required for ORAC swrtm
!
! Description and Algorithm details taken form call_rttov idl code:

!
! Arguments:
! Name Type In/Out/Both Description
!         errorstatus:
!         chanprof:
!         opts:
!         inonly    profiles: becuse changed for solar calculation
!         coefs(nrttovid):
!         calcemis:
!         emissivity_in:
!         emissivity_out:
!         transmission:
!         radiance:
!         
! Local variables:
! Name Type Description
!
!
! History:
!2012/05/22: Initial version C. Poulsen first version extracted form
!            code by M. Jerg
!2012/07/04 C. Poulsen remove nview dependance          
!2012/17/07 C. Poulsen complete rewrite
!2012/29/07 C. Poulsen algorithm rewrite
!2012/08/10 C. Poulsen algorithm debug
!2012/08/14 C. Poulsen fixed bug in taubc
!
! $Id$
!
! Bugs:
!
!none known

!TODOS:

subroutine call_rtm_solar_rttov(errorstatus,transmission,imager_angles,channel_info,preproc_swrtm,amf,za)

  Use parkind1, Only : jpim, jprb, jplm

  use netcdf
  use preproc_constants
  use preproc_structures
  use imager_structures
  use netcdf_structures
 use channel_structures
  use rttov_coef_io_mod

 Use rttov_types, Only :   &
       & rttov_options,     &
       & rttov_coefs,       &
       & profile_Type,      &
       & transmission_Type, &
       & radiance_Type,     &
       & rttov_chanprof


  Implicit None
 
 ! RTTOV_errorhandling interface
  !====================
  Integer(Kind=jpim) :: Err_Unit         ! Logical error unit (<0 for default)
  Integer(Kind=jpim) :: verbosity_level ! (<0 for default)
  ! RTTOV_setup interface
  !====================
  Integer(Kind=jpim)               :: setup_errorstatus   ! setup return code

!
 ! RTTOV interface
 !====================
  Integer(Kind=jpim)    :: rttov_errorstatus ! rttov error return code
  Integer(Kind=jpim)    :: nchannels,ichan
  Type(transmission_Type)           :: transmission ! transmittances and layer optical depths
  Integer(Kind=jpim) :: alloc_status(20)
  Integer :: nchans_solar
  Integer :: nlevs,iangle,ii,p
  Real(Kind=sreal), Allocatable :: c_tau_level(:,:),new_trans_tau_level(:,:)  
  Integer(Kind=jpim) :: errorstatus,test_write

  type(preproc_geoloc_s) :: preproc_geoloc
  type(preproc_geo_s) :: preproc_geo
  type(preproc_prtm_s) :: preproc_prtm
  type(preproc_lwrtm_s) :: preproc_lwrtm
  type(preproc_swrtm_s) :: preproc_swrtm
  type(imager_angles_s) :: imager_angles
  type(netcdf_info_s) :: netcdf_info
  type(channel_info_s) :: channel_info
  real :: amf,za
  !write(*,*)'in rtm_solar amf',amf
  
  nchans_solar=size(channel_info%channel_ids_rttov_coef_sw)
  nlevs=  size(transmission%tau_levels(:,1))
  
  !transmission%tau_levels transmittance from each standard pressure level
  !multiply by airmass factor
  ! but must convert transmittance to optical depth first then back again
    
  !write(*,*)'nlevs',nlevs
  
  !define optical depth
  allocate(c_tau_level(nlevs,nchans_solar))
  !define transmisttance
  allocate(new_trans_tau_level(nlevs,nchans_solar))
  ! initialise
  c_tau_level=-999.
  new_trans_tau_level=-999
  !
  !loop over channels and levels and calculate transmissions
  !

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
!        ! write(*,*)'taubc tauac',new_trans_tau_level(nlevs+1-p,ii),new_trans_tau_level(p,ii)
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


