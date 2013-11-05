! Name: call_rtm_ir_rttov.f90
!
!
! Purpose:
!
! call rttov and output the parameters required for ORAC lwrtm
!
! Description and Algorithm details taken form call_rttov idl code:
!               c_trans_layer=exp(-total(c_tau_layer,2,/cum))
!               tr_ac=c_trans_layer
!		tau_sp_layer=-alog(c_trans_layer)
!		r_bc=(c_rad(*,replicate(0,nz))-c_up)/tr_ac
! followed by fromcreate_lwrtm.pro
! for p=0,nplevs-1 do tbc[j,i,ii,p]=rtt.trans[ir_index[ii]]/rtt.cm.tr_ac[ir_index[ii],p] ;r.trans/rtt.cm.tr_ac
!            tac[j,i,ii,*]=rtt.cm.TR_AC[ir_index[ii],*]
!            rac_up[j,i,ii,*]=rtt.cm.R_UP_AC[ir_index[ii],*]
!            rac_dwn[j,i,ii,*]=rtt.cm.R_DOWN_AC[ir_index[ii],*]
!            rbc_up[j,i,ii,*]=rtt.cm.R_UP_BC[ir_index[ii],*]
! from rttov program
!     	      RADBC(J,ILEV)=(PRAD(J)-BDT(J,ILEV))/TAU(J,ILEV)
!              
!              TAUBC(J,ILEV)= TAU(J,JPLEV+1-ILEV)       
!
!
! Arguments:
! Name Type In/Out/Both Description
!         errorstatus:
!         chanprof:
!         opts:
!         inonly    profiles: because changed for solar calculation
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
!2012/07/04 C. Poulsen removed nviews          
!2012/17/07 C. Poulsen complete rewrite          
!2012/29/07 C. Poulsen fixed many bugs
!2012/08/01 MJ adds rac_up,rac_down
!2012/08/14 C. Poulsen fixed tbc
!
! $Id$
!
! Bugs:
!
!none known

!TODOS:

subroutine call_rtm_ir_rttov(errorstatus,transmission,radiance,imager_angles,channel_info,preproc_lwrtm)

  use netcdf
  use preproc_constants
  use preproc_structures
  use imager_structures
  use netcdf_structures
  use channel_structures
  use rttov_coef_io_mod
 
  Use parkind1, Only : jpim, jprb, jplm

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
 ! RTTOV interface
 !====================

  Integer(Kind=jpim)    :: rttov_errorstatus ! rttov error return code
  Integer(Kind=jpim)    :: nchannels,ichan
  Type(transmission_Type)           :: transmission ! transmittances and layer optical depths
  Type(radiance_Type)               :: radiance
  Integer(Kind=jpim) :: alloc_status(20)
  Integer :: nchans_ir,test_write
  Integer :: nlevs,p
  integer(kind=lint) :: ierr
  Integer(Kind=jpim) :: errorstatus
  Real(Kind=sreal), Allocatable :: c_tau_layer(:,:)
 
  type(preproc_dims_s) :: preproc_dims
  type(preproc_geoloc_s) :: preproc_geoloc
  type(preproc_geo_s) :: preproc_geo
  type(preproc_prtm_s) :: preproc_prtm
  type(preproc_lwrtm_s) :: preproc_lwrtm
  type(imager_angles_s) :: imager_angles
  type(netcdf_info_s) :: netcdf_info
  type(channel_info_s) :: channel_info


  integer :: tdim, ttdim,nviews,iangle,ii,ic
  real :: diff
  
!
!convert radiance up and radiance down and tau_levels to to 60 levels
!

  nlevs=size(transmission%tau_levels(:,1))
  

  nchans_ir=size(channel_info%channel_ids_rttov_coef_lw)
  
  !extract ir transmissions and upwelling and downwelling radiances
  !
  !write(*,*)'nchans_ir',nchans_ir  
  do ii=1,nchans_ir  
     do p=1,nlevs
        
        preproc_lwrtm%taubc(ii,p)=transmission%tau_total(ii)/transmission%tau_levels(p,ii)
        preproc_lwrtm%tauac(ii,p)=transmission%tau_levels(p,ii)



!
!convert layers information to levels
! 
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
        !radbc at the top should be eq to radiance%clear
        
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
  
  ! 	UP_AC:	Upwelling radiance from each layer (mW/cm-1/m2/sr)
  !	UP_BC:	Upwelling radiance at each level (mW/cm-1/m2/sr)
  !	DOWN_AC:	Downwelling radiance at each level
  !	TAC:		Transmission from space to each layer}}
  
end subroutine call_rtm_ir_rttov


