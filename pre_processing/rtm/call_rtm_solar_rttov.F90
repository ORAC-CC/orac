! Name: call_rtm_solarrttov.f90
!
!
! Purpose:
!
! call rttov and output the parameters required for ORAC swrtm
!
! Description and Algorithm details:
!
!
! Arguments:
! Name Type In/Out/Both Description
!          errorstatus:
!         chanprof:
!         opts:
!      inonly    profiles: becuse changed for solar calculation
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
!          
!
! $Id$
!
! Bugs:
!
!none known

!TODOS:

subroutine call_rtm_solar_rttov(errorstatus,chanprof,opts,profiles,coefs,calcemis,emissivity_in,emissivity_out,&
     & transmission,radiance,imager_angles,tac,tbc)

  Use parkind1, Only : jpim, jprb, jplm
  Implicit None
  
  Integer(Kind=jpim)    :: rttov_errorstatus ! rttov error return code
  Integer(Kind=jpim)    :: nchannels,ichan
  Type(rttov_chanprof), Allocatable :: chanprof(:)
  Type(profile_Type), Allocatable   :: profiles(:)
  Logical(Kind=jplm), Allocatable   :: calcemis(:)
  Real(Kind=jprb), Allocatable      :: emissivity_in (:)
  Real(Kind=jprb), Allocatable      :: emissivity_out (:)
  Real(Kind=jprb), Allocatable      :: tac(:,:,:),tbc(:,:,:)
  Real(Kind=jprb), Allocatable      :: total_tau_sp_layer(:,:),trans_layer(:,:)
  Type(transmission_Type)           :: transmission ! transmittances and layer optical depths
  Type(radiance_Type)               :: radiance
  Integer(Kind=jpim) :: alloc_status(20)
  Integer :: nchans_solar
  Integer :: nlevs
  
  Integer(Kind=jpim) :: errorstatus
  
  type(preproc_dims_s) :: preproc_dims
  !
  !calculate effective 2 way angle
  !
  write(*,*) 'profiles(1)%zenangle',profiles(1)%zenangle
  pause
  
  call effective_2way_za(profiles(1)%zenangle,profiles(1)%sunzenangle,amf,za)
  profiles(1)%zenangle=za
  
!
  ! call rttov for solar channels
  !
  
  call rttov_direct(errorstatus,chanprof,opts,profiles,coefs,calcemis,emissivity_in,emissivity_out,&
       & transmission,radiance)
  !write(*,*) 'errorstatus ',errorstatus
  Call rttov_errorhandling(Err_unit, verbosity_level)
  !              pause
  tdim=size(transmission%tau_levels)
  write(*,*)'tdim tau_levels, ',tdim
  
  ! vertical tau which will give right transmission for requested geometry
  tau_sp_layer=transmission%tau_levels*1.0/amf)
  ttdim=size(tau_sp_layer)
  write(*,*)'ttdim',ttdim
  pause
  
  nchans_solar=ttdim(1)
  
  !
  !only transmission calculated for sw channels
  !
  nviews=imager_angles%nviews
  
  allocate(total_tau_sp_layer(nchans_solar,nviews))
  allocate(trans_layer(nchans_solar,nviews))
  do ii=1,nchans_solar 
     total_tau_sp_layer(ii,iangle)=sum(tau_sp_layer(:,ii,inagle))
     trans_layer(ii,iangle)=exp(-total_tau_sp_layer(ii,iangle))
  enddo
  
  
  !
  !extract solar transmissions
  !
  
  allocate(tac(nlevs,nchans_solar,nviews))
  allocate(tbc(nlevs,nchans_solar,nviews))
  do  iangle=1,imager_angles%nviews
     do ii=1,nchans_solar  
        do p=1,nlevs 
           tbc(*,ii,iangle)=trans_layer(ii,iangle)/exp(-tau_sp_layer(p,ii,iangle)) ;r.trans/rtt.cm.tr_ac
           tac(p,ii,iangle)=exp(-tau_sp_layer(p,ii,iangle))
        end do
        
     end do
  end do
  
end subroutine call_rtm_solar_rttov


