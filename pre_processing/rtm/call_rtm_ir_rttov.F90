! Name: call_rtm_ir_rttov.f90
!
!
! Purpose:
!
! call rttov and output the parameters required for ORAC lwrtm
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
!         transmission:tac,tbc
!         radiance:rac_up, rac_dwn,rbc_u
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

subroutine call_rtm_ir_rttov(errorstatus,chanprof,opts,profiles,coefs,calcemis,emissivity_in,emissivity_out,&
     & transmission,radiance,imager_angles,tac,tbc,rac_up, rac_dwn,rbc_up,netcdf_info,pixel_counter_lw,pixel_counter_pw)

  use preproc_constants
  
  use netcdf_structures

  Use parkind1, Only : jpim, jprb, jplm
  Implicit None
  
  Integer(Kind=jpim)    :: rttov_errorstatus ! rttov error return code
  Integer(Kind=jpim)    :: nchannels,ichan
  Type(rttov_chanprof), Allocatable :: chanprof(:)
  Type(profile_Type), Allocatable   :: profiles(:)
  Logical(Kind=jplm), Allocatable   :: calcemis(:)
  Real(Kind=jprb), Allocatable      :: emissivity_in (:)
  Real(Kind=jprb), Allocatable      :: emissivity_out (:)
  Real(Kind=jprb), Allocatable      :: tac(:,:,:),tbc(:,:,:),rac_up(:,:,:), rac_dwn(:,:,:),rbc_up(:,:,:)
  Type(transmission_Type)           :: transmission ! transmittances and layer optical depths
  Type(radiance_Type)               :: radiance
  Integer(Kind=jpim) :: alloc_status(20)
  Integer :: nchans_ir
  Integer :: nlevs

  Integer(Kind=jpim) :: errorstatus
  
  type(preproc_dims_s) :: preproc_dims

  type(netcdf_info_s) :: netcdf_info

  integer(kind=lint) ::  pixel_counter_lw,pixel_counter_pw
  real(kind=sreal) :: dummy_real_1d(1),dummy_real_2d(preproc_dims%kdim_pre,1)
  !
  ! call rttov for ir channels
  !

  call rttov_direct(errorstatus,chanprof,opts,profiles,coefs,calcemis,emissivity_in,emissivity_out,&
       & transmission,radiance)
  !write(*,*) 'errorstatus ',errorstatus
  Call rttov_errorhandling(Err_unit, verbosity_level)
  !              pause
  tdim=size(transmission%tau_levels)
  write(*,*)'tdim tau_levels, ',tdim
  nviews=imager_angles%nviews
  
  !allocate(tau_sp_layer(nlevs,nchans_ir,nviews)
  tau_sp_layer=transmission%tau_levels !(1:preproc_dims%nchan_lw,iangle,ilevel)
  ttdim=size(tau_sp_layer)
  write(*,*)'ttdim',ttdim
  write(*,*)'size tau_sp_layer',size(tau_sp_layer)
  pause
  
  nchans_ir=ttdim(1)
  
  
  allocate(tac(nlevs,nchans_ir,nviews))
  allocate(tbc(nlevs,nchans_ir,nviews))
  
  allocate(total_tau_sp_layer(nchans_ir,nviews))
  allocate(ttans_layer(nchans_ir,nviews))
  
  allocate(rac_up(nlevs,nchans_ir,nviews))
  allocate(rac_dwn(nlevs,nchans_ir,nviews))
  allocate(rbc_up(nlevs,nchans_ir,nviews))
  
  
  !extract ir transmissions and upwelling and downwelling radiances
!
  do  iangle=1,imager_angles%nviews
     do ii=1,nchans_ir  
        total_tau_sp_layer(ii,iangle))=sum(tau_sp_layer(:,ii,iangle))
        trans_layer(ii,iangle)=exp(-total_tau_sp_layer(ii,iangle))
        do p=1,nlevs 
           tbc(p,ii,iangle)=trans_layer(ii,iangle)/exp(-tau_sp_layer(p,ii,iangle)) ;r.trans/rtt.cm.tr_ac
           tac(p,ii,iangle)=exp(-tau_sp_layer(p,ii,iangle))
           
           rac_up(p,ii,iangle)=radiance%up(p,ii,iangle)	
           rac_dwn(p,ii,iangle)=radiance%down(p,ii,iangle)
           rbc_up(p,ii,iangle)=radiance%bt(p,ii,iangle)-radiance%up(p,ii,iangle))/tac(p,ii,iangle)
        end do

     end do
  end do

  ! 	UP_AC:	Upwelling radiance from each layer (mW/cm-1/m2/sr)
  !	UP_BC:	Upwelling radiance at each level (mW/cm-1/m2/sr)
  !	DOWN_AC:	Downwelling radiance at each level
  !	TAC:		Transmission from space to each layer}}
  
  
  !write here now the output per pixel
  !further up there needs to be writes for the channels etc.
  !this only serves as example, more output added once RTM section rewrite is done

  !write first the general stuff
  pixel_counter_lw=pixel_counter_lw+1
  pixel_counter_pw=pixel_counter_pw+1
  !1D variables
  netcdf_info%start_1d(1)=pixel_counter_lw
  netcdf_info%counter_1d(1)=1
  netcdf_info%stride_1d(1)=1
    !latitude
  dummy_real_1d(1)=profiles(1)%latitude
  ierr = NF90_PUT_VAR(netcdf_info%ncid_prtm, netcdf_info%latid_pw,dummy_real_1d,&
       & netcdf_info%start_1d, netcdf_info%counter_1d, netcdf_info%stride_1d)
  if (ierr.NE.NF90_NOERR) stop 'err write v'
  
  
  !2D variables
  !pressure profile (at layer centers of preprocessing profile, interfaces for RTTOV)
  netcdf_info%start_2d(1)=1
  netcdf_info%counter_2d(1)=profiles(1)%nlevels-1
  netcdf_info%stride_2d(1)=1                 
  
  netcdf_info%start_2d(2)=pixel_counter_lw
  netcdf_info%counter_2d(2)=1
  netcdf_info%stride_2d(2)=1                 
  
  dummy_real_2d(:,1)=profiles(1)%p(:)
  ierr = NF90_PUT_VAR(netcdf_info%ncid_prtm, netcdf_info%pprofile_lay_id_pw,dummy_real_2d,&
       & netcdf_info%start_2d, netcdf_info%counter_2d, netcdf_info%stride_2d)
  if (ierr.NE.NF90_NOERR) stop 'err write pprof'
  
  


  deallocate(tac)
  deallocate(tbc)
  
  deallocate(total_tau_sp_layer)
  deallocate(ttans_layer)
  
  deallocate(rac_up)
  deallocate(rac_dwn)
  deallocate(rbc_up)


end subroutine call_rtm_ir_rttov


