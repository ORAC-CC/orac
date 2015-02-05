! Name: set_struct_pp.f90
!
!
! Purpose: File contains several subroutines to allocate and initialize structures and user defined variable types.
! 
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
! 2012/02/03 Matthias Jerg cleans out prototype code to prepare repository upload.
! 15/02/2012 Modified by C Poulsen to do level 2 post processing
! 7/3/2012  Martin Stengel added missing stemp_ap
! 7/3/2012  C Poulsen cleaned up
! 2012/03/18 Caroline Poulsen modified to add cloud flag
! 2012/06/20 Caroline Poulsen added albedo
! 2012/07/04 Matthias Jerg fixed several data type bugs
! 2012/07/06 MJ extensively overhauls and restructures the code
! 2013/01/17 Matthias Jerg: Adds code to accommodate uncertainties of ctt and cth
! 2014/09/20 change phase from 2 to 1, changed arguments of
!  set_l2_input_struct_2d_secondary added in channels  
! 2014/09/29 CP added in MODIS variable names 
! 2014/10/24 OS: added variables cccot_pre, lusflag, cldtype, cloudmask, DEM
!  (currently deactivated), and nisemask; commented out (de)allocation of variables
!  for water within if condition iphase = 2 (never true for water)
! 2014/11/20 OS: some minor editing
! 2014/11/26 CP: added cloud_albedo
! 2015/02/05 OS: changed nint to lint; added (de)allocation of phase_post 

! $Id$
!
! Bugs:
!
!none known

!-------------------------------------------
!-------------------------------------------
subroutine set_l2_input_struct_2d_primary_ice(iphase,l2_input_2dice_primary,xdim1km,ydim1km)
!-------------------------------------------
!-------------------------------------------

  use vartypes_pp
  use common_constants

  use structures_pp

  implicit none

  integer :: iphase,nchan

  integer(kind=lint) ,INTENT(IN) :: xdim1km,ydim1km
  
  type(l2_input_struct_2d_primary) :: l2_input_2dice_primary
  
  !those do not differ between ice and water, therefore only allocate in ice
  if(iphase .eq. 2) then

     allocate(l2_input_2dice_primary%time(xdim1km,ydim1km))
     l2_input_2dice_primary%time=double_fill_value
     
     allocate(l2_input_2dice_primary%lon(xdim1km,ydim1km))
     l2_input_2dice_primary%lon=real_fill_value
     
     allocate(l2_input_2dice_primary%lat(xdim1km,ydim1km))
     l2_input_2dice_primary%lat=real_fill_value

     allocate(l2_input_2dice_primary%satellite_zenith_view_no1(xdim1km,ydim1km))
     l2_input_2dice_primary%satellite_zenith_view_no1=real_fill_value
     
     
     allocate(l2_input_2dice_primary%solar_zenith_view_no1(xdim1km,ydim1km))
     l2_input_2dice_primary%solar_zenith_view_no1=real_fill_value
     
     allocate(l2_input_2dice_primary%rel_azimuth_view_no1(xdim1km,ydim1km))
     l2_input_2dice_primary%rel_azimuth_view_no1=real_fill_value
     
     allocate(l2_input_2dice_primary%cty(xdim1km,ydim1km))
     l2_input_2dice_primary%cty=real_fill_value
     

     allocate(l2_input_2dice_primary%phase(xdim1km,ydim1km))
     l2_input_2dice_primary%phase=byte_fill_value

     allocate(l2_input_2dice_primary%phase_post(xdim1km,ydim1km))
     l2_input_2dice_primary%phase_post=byte_fill_value     

     allocate(l2_input_2dice_primary%lsflag(xdim1km,ydim1km))
     l2_input_2dice_primary%lsflag=byte_fill_value

     allocate(l2_input_2dice_primary%cc_total(xdim1km,ydim1km))
     l2_input_2dice_primary%cc_total=real_fill_value

     allocate(l2_input_2dice_primary%cccot(xdim1km,ydim1km))
     l2_input_2dice_primary%cccot=real_fill_value

     allocate(l2_input_2dice_primary%cccot_pre(xdim1km,ydim1km))
     l2_input_2dice_primary%cccot_pre=real_fill_value
     
     allocate(l2_input_2dice_primary%cldtype(xdim1km,ydim1km))
     l2_input_2dice_primary%cldtype=byte_fill_value
     
     allocate(l2_input_2dice_primary%illum(xdim1km,ydim1km))
     l2_input_2dice_primary%illum=byte_fill_value
     
     allocate(l2_input_2dice_primary%cldmask(xdim1km,ydim1km))
     l2_input_2dice_primary%cldmask=byte_fill_value

     allocate(l2_input_2dice_primary%lusflag(xdim1km,ydim1km))
     l2_input_2dice_primary%lusflag=byte_fill_value

     !allocate(l2_input_2dice_primary%dem(xdim1km,ydim1km))
     !l2_input_2dice_primary%dem=short_int_fill_value

     allocate(l2_input_2dice_primary%nisemask(xdim1km,ydim1km))
     l2_input_2dice_primary%nisemask=byte_fill_value
     
  endif

  !from here things are different between water and ice

  allocate(l2_input_2dice_primary%ctt(xdim1km,ydim1km))
  l2_input_2dice_primary%ctt=real_fill_value

  allocate(l2_input_2dice_primary%ctt_uncertainty(xdim1km,ydim1km))
  l2_input_2dice_primary%ctt_uncertainty=real_fill_value

  allocate(l2_input_2dice_primary%cc_total_uncertainty(xdim1km,ydim1km))
  l2_input_2dice_primary%cc_total_uncertainty=real_fill_value

  allocate(l2_input_2dice_primary%stemp(xdim1km,ydim1km))
  l2_input_2dice_primary%stemp=real_fill_value

  allocate(l2_input_2dice_primary%stemp_uncertainty(xdim1km,ydim1km))
  l2_input_2dice_primary%stemp_uncertainty=real_fill_value
  
  allocate(l2_input_2dice_primary%cth(xdim1km,ydim1km))
  l2_input_2dice_primary%cth=real_fill_value
  
  allocate(l2_input_2dice_primary%cth_uncertainty(xdim1km,ydim1km))
  l2_input_2dice_primary%cth_uncertainty=real_fill_value
  
  allocate(l2_input_2dice_primary%ctp(xdim1km,ydim1km))
  l2_input_2dice_primary%ctp=real_fill_value
  
  allocate(l2_input_2dice_primary%ctp_uncertainty(xdim1km,ydim1km))
  l2_input_2dice_primary%ctp_uncertainty=real_fill_value
  
  allocate(l2_input_2dice_primary%cct(xdim1km,ydim1km))
  l2_input_2dice_primary%cct=real_fill_value
  
  allocate(l2_input_2dice_primary%cct_uncertainty(xdim1km,ydim1km))
  l2_input_2dice_primary%cct_uncertainty=real_fill_value
  
  allocate(l2_input_2dice_primary%cot(xdim1km,ydim1km))
  l2_input_2dice_primary%cot=real_fill_value
  
  allocate(l2_input_2dice_primary%cot_uncertainty(xdim1km,ydim1km))
  l2_input_2dice_primary%cot_uncertainty=real_fill_value
  
  allocate(l2_input_2dice_primary%ref(xdim1km,ydim1km))
  l2_input_2dice_primary%ref=real_fill_value
  
  allocate(l2_input_2dice_primary%ref_uncertainty(xdim1km,ydim1km))
  l2_input_2dice_primary%ref_uncertainty=real_fill_value

  nchan=2
  allocate(l2_input_2dice_primary%cloud_albedo(xdim1km,ydim1km,nchan))
  l2_input_2dice_primary%cloud_albedo=real_fill_value

  allocate(l2_input_2dice_primary%cwp(xdim1km,ydim1km))
  l2_input_2dice_primary%cwp=real_fill_value

  allocate(l2_input_2dice_primary%cwp_uncertainty(xdim1km,ydim1km))
  l2_input_2dice_primary%cwp_uncertainty=real_fill_value
  
  allocate(l2_input_2dice_primary%costja(xdim1km,ydim1km))
  l2_input_2dice_primary%costja=real_fill_value
  
  allocate(l2_input_2dice_primary%costjm(xdim1km,ydim1km))
  l2_input_2dice_primary%costjm=real_fill_value
    
  allocate(l2_input_2dice_primary%niter(xdim1km,ydim1km))
  l2_input_2dice_primary%niter=byte_fill_value

  allocate(l2_input_2dice_primary%convergence(xdim1km,ydim1km))
  l2_input_2dice_primary%convergence=byte_fill_value
  
  allocate(l2_input_2dice_primary%qcflag(xdim1km,ydim1km))
  l2_input_2dice_primary%qcflag=short_int_fill_value

end subroutine set_l2_input_struct_2d_primary_ice



!-------------------------------------------
!-------------------------------------------
subroutine set_l2_input_struct_2d_primary_wat(iphase,l2_input_2dwat_primary,xdim1km,ydim1km)
!-------------------------------------------
!-------------------------------------------

  use vartypes_pp
  use common_constants
  use structures_pp

  implicit none

  integer :: iphase,nchan

  integer(kind=lint) ,INTENT(IN) :: xdim1km,ydim1km
  
  type(l2_input_struct_2d_primary) :: l2_input_2dwat_primary

  ! THE FOLLOWING IS NEVER EXECUTED FOR WATER, SO WHY NOT DELETE?  
  !those do not differe between ice and water, therefore only allocate in ice
!   if(iphase .eq. 2) then

!      allocate(l2_input_2dwat_primary%time(xdim1km,ydim1km))
!      l2_input_2dwat_primary%time=real_fill_value
     
!      allocate(l2_input_2dwat_primary%lon(xdim1km,ydim1km))
!      l2_input_2dwat_primary%lon=real_fill_value
     
!      allocate(l2_input_2dwat_primary%lat(xdim1km,ydim1km))
!      l2_input_2dwat_primary%lat=real_fill_value

!      allocate(l2_input_2dwat_primary%satellite_zenith_view_no1(xdim1km,ydim1km))
!      l2_input_2dwat_primary%satellite_zenith_view_no1=real_fill_value
     
     
!      allocate(l2_input_2dwat_primary%solar_zenith_view_no1(xdim1km,ydim1km))
!      l2_input_2dwat_primary%solar_zenith_view_no1=real_fill_value
     
!      allocate(l2_input_2dwat_primary%rel_azimuth_view_no1(xdim1km,ydim1km))
!      l2_input_2dwat_primary%rel_azimuth_view_no1=real_fill_value
     
!      allocate(l2_input_2dwat_primary%cty(xdim1km,ydim1km))
!      l2_input_2dwat_primary%cty=real_fill_value
     
!      !allocate(l2_input_2dwat_primary%pchange(xdim1km,ydim1km))
!      !l2_input_2dwat_primary%pchange=byte_fill_value
     
!      allocate(l2_input_2dwat_primary%phase(xdim1km,ydim1km))
!      l2_input_2dwat_primary%phase=byte_fill_value
     
!      allocate(l2_input_2dwat_primary%lsflag(xdim1km,ydim1km))
!      l2_input_2dwat_primary%lsflag=byte_fill_value
     
!      !allocate(l2_input_2dwat_primary%cloudflag(xdim1km,ydim1km))
!      !l2_input_2dwat_primary%cloudflag=byte_fill_value
     
!      allocate(l2_input_2dwat_primary%illum(xdim1km,ydim1km))
!      l2_input_2dwat_primary%illum=byte_fill_value
     
!      !allocate(l2_input_2dwat_primary%scanline_u(xdim1km,ydim1km))
!      !l2_input_2dwat_primary%scanline_u=lint_fill_value
     
!      !allocate(l2_input_2dwat_primary%scanline_v(xdim1km,ydim1km))
!      !l2_input_2dwat_primary%scanline_v=lint_fill_value
     
!   endif



  !from here things are different between water and ice
  

  allocate(l2_input_2dwat_primary%ctt(xdim1km,ydim1km))
  l2_input_2dwat_primary%ctt=real_fill_value

  allocate(l2_input_2dwat_primary%cc_total(xdim1km,ydim1km))
  l2_input_2dwat_primary%cc_total=real_fill_value

  allocate(l2_input_2dwat_primary%cc_total_uncertainty(xdim1km,ydim1km))
  l2_input_2dwat_primary%cc_total_uncertainty=real_fill_value


  allocate(l2_input_2dwat_primary%stemp_uncertainty(xdim1km,ydim1km))
  l2_input_2dwat_primary%stemp_uncertainty=real_fill_value

  allocate(l2_input_2dwat_primary%stemp(xdim1km,ydim1km))
  l2_input_2dwat_primary%stemp=real_fill_value
  
  
  allocate(l2_input_2dwat_primary%ctt_uncertainty(xdim1km,ydim1km))
  l2_input_2dwat_primary%ctt_uncertainty=real_fill_value
  
  allocate(l2_input_2dwat_primary%cth(xdim1km,ydim1km))
  l2_input_2dwat_primary%cth=real_fill_value
  
  allocate(l2_input_2dwat_primary%cth_uncertainty(xdim1km,ydim1km))
  l2_input_2dwat_primary%cth_uncertainty=real_fill_value
  
  allocate(l2_input_2dwat_primary%ctp(xdim1km,ydim1km))
  l2_input_2dwat_primary%ctp=real_fill_value
  
  allocate(l2_input_2dwat_primary%ctp_uncertainty(xdim1km,ydim1km))
  l2_input_2dwat_primary%ctp_uncertainty=real_fill_value
  
  allocate(l2_input_2dwat_primary%cct(xdim1km,ydim1km))
  l2_input_2dwat_primary%cct=real_fill_value
  
  allocate(l2_input_2dwat_primary%cct_uncertainty(xdim1km,ydim1km))
  l2_input_2dwat_primary%cct_uncertainty=real_fill_value
  
  allocate(l2_input_2dwat_primary%cot(xdim1km,ydim1km))
  l2_input_2dwat_primary%cot=real_fill_value
  
  allocate(l2_input_2dwat_primary%cot_uncertainty(xdim1km,ydim1km))
  l2_input_2dwat_primary%cot_uncertainty=real_fill_value
  
  allocate(l2_input_2dwat_primary%ref(xdim1km,ydim1km))
  l2_input_2dwat_primary%ref=real_fill_value
  
  allocate(l2_input_2dwat_primary%ref_uncertainty(xdim1km,ydim1km))
  l2_input_2dwat_primary%ref_uncertainty=real_fill_value

  nchan=2
  allocate(l2_input_2dwat_primary%cloud_albedo(xdim1km,ydim1km,nchan))
  l2_input_2dwat_primary%cloud_albedo=real_fill_value



  allocate(l2_input_2dwat_primary%cwp(xdim1km,ydim1km))
  l2_input_2dwat_primary%cwp=real_fill_value


  allocate(l2_input_2dwat_primary%cwp_uncertainty(xdim1km,ydim1km))
  l2_input_2dwat_primary%cwp_uncertainty=real_fill_value
  

  allocate(l2_input_2dwat_primary%costja(xdim1km,ydim1km))
  l2_input_2dwat_primary%costja=real_fill_value
  
  allocate(l2_input_2dwat_primary%costjm(xdim1km,ydim1km))
  l2_input_2dwat_primary%costjm=real_fill_value
  
  
  allocate(l2_input_2dwat_primary%niter(xdim1km,ydim1km))
  l2_input_2dwat_primary%niter=byte_fill_value

  allocate(l2_input_2dwat_primary%convergence(xdim1km,ydim1km))
  l2_input_2dwat_primary%convergence=byte_fill_value
  
  allocate(l2_input_2dwat_primary%qcflag(xdim1km,ydim1km))
  l2_input_2dwat_primary%qcflag=short_int_fill_value
  
  
  !allocate(l2_input_2dwat_primary%vname(nl2vars_1km))
  !l2_input_2dwat_primary%vname=''  
  
  !allocate(l2_input_2dwat_primary%ename(nl2vars_errors_1km))
  !l2_input_2dwat_primary%ename=''  

end subroutine set_l2_input_struct_2d_primary_wat


!-------------------------------------------
!-------------------------------------------
subroutine set_l2_refl_and_bt(l2_input_2d_refl_bt,xdim1km,ydim1km)
!-------------------------------------------
!-------------------------------------------

  use vartypes_pp
  use common_constants
  use structures_pp

  implicit none

  integer(kind=lint) ,INTENT(IN) :: xdim1km,ydim1km
  
  type(l2_input_struct_2d_refl_bt) :: l2_input_2d_refl_bt
  
  allocate(l2_input_2d_refl_bt%albedo(xdim1km,ydim1km,l2_input_2d_refl_bt%nchannels_sw))
  l2_input_2d_refl_bt%albedo=real_fill_value
  
  allocate(l2_input_2d_refl_bt%reflectance_residual(xdim1km,ydim1km,l2_input_2d_refl_bt%nchannels_sw))
  l2_input_2d_refl_bt%reflectance_residual=real_fill_value

  allocate(l2_input_2d_refl_bt%brightness_temperature_residual(xdim1km,ydim1km,l2_input_2d_refl_bt%nchannels_lw))
  l2_input_2d_refl_bt%brightness_temperature_residual=real_fill_value

  allocate(l2_input_2d_refl_bt%reflectance(xdim1km,ydim1km,l2_input_2d_refl_bt%nchannels_sw))
  l2_input_2d_refl_bt%reflectance=real_fill_value

  allocate(l2_input_2d_refl_bt%brightness_temperature(xdim1km,ydim1km,l2_input_2d_refl_bt%nchannels_lw))
  l2_input_2d_refl_bt%brightness_temperature=real_fill_value

end subroutine set_l2_refl_and_bt



!-------------------------------------------
!-------------------------------------------
subroutine unset_l2_refl_and_bt(l2_input_2d_refl_bt)
!-------------------------------------------
!-------------------------------------------

  use vartypes_pp
  use common_constants
  use structures_pp

  implicit none

  type(l2_input_struct_2d_refl_bt) :: l2_input_2d_refl_bt
  

  deallocate(l2_input_2d_refl_bt%albedo)
  deallocate(l2_input_2d_refl_bt%reflectance_residual)
  deallocate(l2_input_2d_refl_bt%brightness_temperature_residual)
  deallocate(l2_input_2d_refl_bt%reflectance)
  deallocate(l2_input_2d_refl_bt%brightness_temperature)

end subroutine unset_l2_refl_and_bt





!----------------------------------------------------------
!----------------------------------------------------------
subroutine set_l2_input_struct_2d_secondary(l2_input_2d_secondary,xdim1km,ydim1km,nl2vars_1km, &
     nl2vars_errors_1km,n_val_plus_error,n_oe_features)
  !----------------------------------------------------------
  !----------------------------------------------------------

  use vartypes_pp
  use common_constants
  use structures_pp

  implicit none

  integer(kind=lint) ,INTENT(IN) :: xdim1km,ydim1km

  integer(kind=lint),INTENT(IN)  :: nl2vars_1km,nl2vars_errors_1km,n_val_plus_error, n_oe_features

  !  integer(kind=lint) :: nl2vars_1km,nl2vars_errors_1km,n_val_plus_error, n_oe_features

  type(l2_input_struct_2d_secondary) :: l2_input_2d_secondary


  !
  !  now do secondary file
  !
  allocate(l2_input_2d_secondary%cot_ap(xdim1km,ydim1km))
  l2_input_2d_secondary%cot_ap=real_fill_value


  allocate(l2_input_2d_secondary%cot_fg(xdim1km,ydim1km))
  l2_input_2d_secondary%cot_fg=real_fill_value


  allocate(l2_input_2d_secondary%ref_ap(xdim1km,ydim1km))
  l2_input_2d_secondary%ref_ap=real_fill_value



  allocate(l2_input_2d_secondary%ref_fg(xdim1km,ydim1km))
  l2_input_2d_secondary%ref_fg=real_fill_value



  allocate(l2_input_2d_secondary%ctp_ap(xdim1km,ydim1km))
  l2_input_2d_secondary%ctp_ap=real_fill_value

  allocate(l2_input_2d_secondary%ctp_fg(xdim1km,ydim1km))
  l2_input_2d_secondary%ctp_fg=real_fill_value



  allocate(l2_input_2d_secondary%albedo_in_channel_no_1(xdim1km,ydim1km))
  l2_input_2d_secondary%albedo_in_channel_no_1=real_fill_value

  allocate(l2_input_2d_secondary%albedo_in_channel_no_2(xdim1km,ydim1km))
  l2_input_2d_secondary%albedo_in_channel_no_2=real_fill_value

  allocate(l2_input_2d_secondary%albedo_in_channel_no_3(xdim1km,ydim1km))
  l2_input_2d_secondary%albedo_in_channel_no_3=real_fill_value




  allocate(l2_input_2d_secondary%reflectance_residual_in_channel_no_1(xdim1km,ydim1km))
  l2_input_2d_secondary%reflectance_residual_in_channel_no_1=real_fill_value

  allocate(l2_input_2d_secondary%reflectance_residual_in_channel_no_2(xdim1km,ydim1km))
  l2_input_2d_secondary%reflectance_residual_in_channel_no_2=real_fill_value

  allocate(l2_input_2d_secondary%reflectance_residual_in_channel_no_3(xdim1km,ydim1km))
  l2_input_2d_secondary%reflectance_residual_in_channel_no_3=real_fill_value

  allocate(l2_input_2d_secondary%brightness_temperature_residual_in_channel_no_4(xdim1km,ydim1km))
  l2_input_2d_secondary%brightness_temperature_residual_in_channel_no_4=real_fill_value

  allocate(l2_input_2d_secondary%brightness_temperature_residual_in_channel_no_5(xdim1km,ydim1km))
  l2_input_2d_secondary%brightness_temperature_residual_in_channel_no_5=real_fill_value

  allocate(l2_input_2d_secondary%brightness_temperature_residual_in_channel_no_6(xdim1km,ydim1km))              
  l2_input_2d_secondary%brightness_temperature_residual_in_channel_no_6=real_fill_value

  allocate(l2_input_2d_secondary%brightness_temperature_residual_in_channel_no_7(xdim1km,ydim1km))              
  l2_input_2d_secondary%brightness_temperature_residual_in_channel_no_7=real_fill_value

  allocate(l2_input_2d_secondary%brightness_temperature_residual_in_channel_no_20(xdim1km,ydim1km))
  l2_input_2d_secondary%brightness_temperature_residual_in_channel_no_20=real_fill_value  

  allocate(l2_input_2d_secondary%brightness_temperature_residual_in_channel_no_31(xdim1km,ydim1km))             
  l2_input_2d_secondary%brightness_temperature_residual_in_channel_no_31=real_fill_value

  allocate(l2_input_2d_secondary%brightness_temperature_residual_in_channel_no_32(xdim1km,ydim1km))             
  l2_input_2d_secondary%brightness_temperature_residual_in_channel_no_32=real_fill_value

  allocate(l2_input_2d_secondary%reflectance_in_channel_no_1(xdim1km,ydim1km))
  l2_input_2d_secondary%reflectance_in_channel_no_1=real_fill_value

  allocate(l2_input_2d_secondary%reflectance_in_channel_no_2(xdim1km,ydim1km))
  l2_input_2d_secondary%reflectance_in_channel_no_2=real_fill_value

  allocate(l2_input_2d_secondary%reflectance_in_channel_no_3(xdim1km,ydim1km))
  l2_input_2d_secondary%reflectance_in_channel_no_3=real_fill_value

  allocate(l2_input_2d_secondary%brightness_temperature_in_channel_no_4(xdim1km,ydim1km))
  l2_input_2d_secondary%brightness_temperature_in_channel_no_4=real_fill_value

  allocate(l2_input_2d_secondary%brightness_temperature_in_channel_no_5(xdim1km,ydim1km))
  l2_input_2d_secondary%brightness_temperature_in_channel_no_5=real_fill_value

  allocate(l2_input_2d_secondary%brightness_temperature_in_channel_no_6(xdim1km,ydim1km))
  l2_input_2d_secondary%brightness_temperature_in_channel_no_6=real_fill_value  

  allocate(l2_input_2d_secondary%brightness_temperature_in_channel_no_7(xdim1km,ydim1km))
  l2_input_2d_secondary%brightness_temperature_in_channel_no_7=real_fill_value

  allocate(l2_input_2d_secondary%brightness_temperature_in_channel_no_20(xdim1km,ydim1km))
  l2_input_2d_secondary%brightness_temperature_in_channel_no_20=real_fill_value          

  allocate(l2_input_2d_secondary%brightness_temperature_in_channel_no_31(xdim1km,ydim1km)) 
  l2_input_2d_secondary%brightness_temperature_in_channel_no_31=real_fill_value

  allocate(l2_input_2d_secondary%brightness_temperature_in_channel_no_32(xdim1km,ydim1km))
  l2_input_2d_secondary%brightness_temperature_in_channel_no_32=real_fill_value

  write(*,*) 'sec filed'     

  allocate(l2_input_2d_secondary%stemp_fg(xdim1km,ydim1km))
  l2_input_2d_secondary%stemp_fg=real_fill_value

  allocate(l2_input_2d_secondary%stemp_ap(xdim1km,ydim1km))
  l2_input_2d_secondary%stemp_ap=real_fill_value

  allocate(l2_input_2d_secondary%firstguess_reflectance_in_channel_no_1(xdim1km,ydim1km))
  l2_input_2d_secondary%firstguess_reflectance_in_channel_no_1=real_fill_value

  allocate(l2_input_2d_secondary%firstguess_reflectance_in_channel_no_2(xdim1km,ydim1km))
  l2_input_2d_secondary%firstguess_reflectance_in_channel_no_2=real_fill_value

  allocate(l2_input_2d_secondary%firstguess_reflectance_in_channel_no_3(xdim1km,ydim1km))
  l2_input_2d_secondary%firstguess_reflectance_in_channel_no_3=real_fill_value

  allocate(l2_input_2d_secondary%firstguess_brightness_temperature_in_channel_no_4(xdim1km,ydim1km))
  l2_input_2d_secondary%firstguess_brightness_temperature_in_channel_no_4=real_fill_value

  allocate(l2_input_2d_secondary%firstguess_brightness_temperature_in_channel_no_5(xdim1km,ydim1km))
  l2_input_2d_secondary%firstguess_brightness_temperature_in_channel_no_5=real_fill_value

  allocate(l2_input_2d_secondary%firstguess_brightness_temperature_in_channel_no_6(xdim1km,ydim1km))
  l2_input_2d_secondary%firstguess_brightness_temperature_in_channel_no_6=real_fill_value

  allocate(l2_input_2d_secondary%firstguess_brightness_temperature_in_channel_no_7(xdim1km,ydim1km))             
  l2_input_2d_secondary%firstguess_brightness_temperature_in_channel_no_7=real_fill_value

  allocate(l2_input_2d_secondary%firstguess_brightness_temperature_in_channel_no_20(xdim1km,ydim1km))           
  l2_input_2d_secondary%firstguess_brightness_temperature_in_channel_no_20=real_fill_value

  allocate(l2_input_2d_secondary%firstguess_brightness_temperature_in_channel_no_31(xdim1km,ydim1km))
  l2_input_2d_secondary%firstguess_brightness_temperature_in_channel_no_31=real_fill_value

  allocate(l2_input_2d_secondary%firstguess_brightness_temperature_in_channel_no_32(xdim1km,ydim1km))            
  l2_input_2d_secondary%firstguess_brightness_temperature_in_channel_no_32=real_fill_value

  write(*,*) 'sec filee'            


end subroutine set_l2_input_struct_2d_secondary



!-------------------------------------------
!-------------------------------------------
subroutine unset_l2_input_struct_2d_primary_ice(iphase,l2_input_2dice_primary)
  !-------------------------------------------
  !-------------------------------------------

  use vartypes_pp
  use common_constants
  use structures_pp

  implicit none

  integer :: iphase

  type(l2_input_struct_2d_primary) :: l2_input_2dice_primary

  if(iphase .eq. 2 ) then
     deallocate(l2_input_2dice_primary%time)
     deallocate(l2_input_2dice_primary%lon)
     deallocate(l2_input_2dice_primary%lat)
     deallocate(l2_input_2dice_primary%phase)
     deallocate(l2_input_2dice_primary%phase_post)
     !deallocate(l2_input_2dice_primary%pchange)

     deallocate(l2_input_2dice_primary%satellite_zenith_view_no1)
     deallocate(l2_input_2dice_primary%solar_zenith_view_no1)
     deallocate(l2_input_2dice_primary%rel_azimuth_view_no1)

     deallocate(l2_input_2dice_primary%lsflag)
     deallocate(l2_input_2dice_primary%cldtype)
     deallocate(l2_input_2dice_primary%illum)

     deallocate(l2_input_2dice_primary%cc_total)
     deallocate(l2_input_2dice_primary%cccot)
     deallocate(l2_input_2dice_primary%cccot_pre)
     !deallocate(l2_input_2dice_primary%scanline_u)
     !deallocate(l2_input_2dice_primary%scanline_v)

     deallocate(l2_input_2dice_primary%cty)

     deallocate(l2_input_2dice_primary%cldmask)
     deallocate(l2_input_2dice_primary%lusflag)
     !deallocate(l2_input_2dice_primary%dem)
     deallocate(l2_input_2dice_primary%nisemask)

  endif


  deallocate(l2_input_2dice_primary%ctp)
  deallocate(l2_input_2dice_primary%ctp_uncertainty)
  deallocate(l2_input_2dice_primary%cth)
  deallocate(l2_input_2dice_primary%cth_uncertainty)
  deallocate(l2_input_2dice_primary%ctt)
  deallocate(l2_input_2dice_primary%ctt_uncertainty)

  deallocate(l2_input_2dice_primary%cc_total_uncertainty)

  deallocate(l2_input_2dice_primary%stemp)
  deallocate(l2_input_2dice_primary%stemp_uncertainty)
  deallocate(l2_input_2dice_primary%cct)
  deallocate(l2_input_2dice_primary%cct_uncertainty)
  deallocate(l2_input_2dice_primary%cot)
  deallocate(l2_input_2dice_primary%cot_uncertainty)
  deallocate(l2_input_2dice_primary%ref)
  deallocate(l2_input_2dice_primary%ref_uncertainty)
  deallocate(l2_input_2dice_primary%cwp)
  deallocate(l2_input_2dice_primary%cloud_albedo)
  deallocate(l2_input_2dice_primary%cwp_uncertainty)
  deallocate(l2_input_2dice_primary%costja)
  deallocate(l2_input_2dice_primary%costjm)
  deallocate(l2_input_2dice_primary%convergence)

  deallocate(l2_input_2dice_primary%niter)

  deallocate(l2_input_2dice_primary%qcflag)
  !deallocate(l2_input_2dice_primary%vname)
  !deallocate(l2_input_2dice_primary%ename)

end subroutine unset_l2_input_struct_2d_primary_ice




!-------------------------------------------
!-------------------------------------------
subroutine unset_l2_input_struct_2d_primary_wat(iphase,l2_input_2dwat_primary)
!-------------------------------------------
!-------------------------------------------
  
  use vartypes_pp
    use common_constants
  use structures_pp
  
  implicit none
  
  integer :: iphase

  type(l2_input_struct_2d_primary) :: l2_input_2dwat_primary
  
!   if(iphase .eq. 2 ) then
!      deallocate(l2_input_2dwat_primary%time)
!      deallocate(l2_input_2dwat_primary%lon)
!      deallocate(l2_input_2dwat_primary%lat)
!      deallocate(l2_input_2dwat_primary%phase)
!      !deallocate(l2_input_2dwat_primary%pchange)
     
!      deallocate(l2_input_2dwat_primary%satellite_zenith_view_no1)
!      deallocate(l2_input_2dwat_primary%solar_zenith_view_no1)
!      deallocate(l2_input_2dwat_primary%rel_azimuth_view_no1)

!      deallocate(l2_input_2dwat_primary%lsflag)
!      !deallocate(l2_input_2dwat_primary%cloudflag)
!      deallocate(l2_input_2dwat_primary%illum)
!      !deallocate(l2_input_2dwat_primary%scanline_u)
!      !deallocate(l2_input_2dwat_primary%scanline_v)

!      deallocate(l2_input_2dwat_primary%cty)
     
!   endif


  deallocate(l2_input_2dwat_primary%ctp)
  deallocate(l2_input_2dwat_primary%ctp_uncertainty)
  deallocate(l2_input_2dwat_primary%cth)
  deallocate(l2_input_2dwat_primary%cth_uncertainty)
  deallocate(l2_input_2dwat_primary%ctt)
  deallocate(l2_input_2dwat_primary%ctt_uncertainty)
  deallocate(l2_input_2dwat_primary%cc_total)
  deallocate(l2_input_2dwat_primary%cc_total_uncertainty)
  
  deallocate(l2_input_2dwat_primary%stemp)
  deallocate(l2_input_2dwat_primary%stemp_uncertainty)
  deallocate(l2_input_2dwat_primary%cct)
  deallocate(l2_input_2dwat_primary%cct_uncertainty)
  deallocate(l2_input_2dwat_primary%cot)
  deallocate(l2_input_2dwat_primary%cot_uncertainty)
  deallocate(l2_input_2dwat_primary%ref)
  deallocate(l2_input_2dwat_primary%ref_uncertainty)
  deallocate(l2_input_2dwat_primary%cloud_albedo)
  deallocate(l2_input_2dwat_primary%cwp)
  deallocate(l2_input_2dwat_primary%cwp_uncertainty)
  deallocate(l2_input_2dwat_primary%costja)
  deallocate(l2_input_2dwat_primary%costjm)
  deallocate(l2_input_2dwat_primary%convergence)
  
  deallocate(l2_input_2dwat_primary%niter)

  deallocate(l2_input_2dwat_primary%qcflag)
  !deallocate(l2_input_2dwat_primary%vname)
  !deallocate(l2_input_2dwat_primary%ename)

end subroutine unset_l2_input_struct_2d_primary_wat

!-------------------------------------------
!-------------------------------------------
subroutine unset_l2_input_struct_2d_secondary(l2_input_2d_secondary)
!-------------------------------------------
!-------------------------------------------
  
  use vartypes_pp
    use common_constants
  use structures_pp
  
  implicit none
  
  type(l2_input_struct_2d_secondary) :: l2_input_2d_secondary

!
!secondary variables
!

  deallocate(l2_input_2d_secondary%ctp_ap)
  deallocate(l2_input_2d_secondary%ctp_fg)
  deallocate(l2_input_2d_secondary%ref_ap)
  deallocate(l2_input_2d_secondary%ref_fg)
  deallocate(l2_input_2d_secondary%cot_ap)
  deallocate(l2_input_2d_secondary%cot_fg)
  deallocate(l2_input_2d_secondary%stemp_ap)
  deallocate(l2_input_2d_secondary%stemp_fg)
  deallocate(l2_input_2d_secondary%albedo_in_channel_no_1)
  deallocate(l2_input_2d_secondary%albedo_in_channel_no_2)
  deallocate(l2_input_2d_secondary%albedo_in_channel_no_3)
  
  deallocate(l2_input_2d_secondary%reflectance_residual_in_channel_no_1)
  deallocate(l2_input_2d_secondary%reflectance_residual_in_channel_no_2)
  deallocate(l2_input_2d_secondary%reflectance_residual_in_channel_no_3)
  deallocate(l2_input_2d_secondary%brightness_temperature_residual_in_channel_no_4)
  deallocate(l2_input_2d_secondary%brightness_temperature_residual_in_channel_no_5)
  deallocate(l2_input_2d_secondary%reflectance_in_channel_no_1)
  deallocate(l2_input_2d_secondary%reflectance_in_channel_no_2)
  deallocate(l2_input_2d_secondary%reflectance_in_channel_no_3)
  deallocate(l2_input_2d_secondary%brightness_temperature_in_channel_no_4)
  deallocate(l2_input_2d_secondary%brightness_temperature_in_channel_no_5)
  deallocate(l2_input_2d_secondary%brightness_temperature_in_channel_no_6)
  
  deallocate(l2_input_2d_secondary%firstguess_reflectance_in_channel_no_1)
  deallocate(l2_input_2d_secondary%firstguess_reflectance_in_channel_no_2)
  deallocate(l2_input_2d_secondary%firstguess_reflectance_in_channel_no_3)
  deallocate(l2_input_2d_secondary%firstguess_brightness_temperature_in_channel_no_4)
  deallocate(l2_input_2d_secondary%firstguess_brightness_temperature_in_channel_no_5)
  deallocate(l2_input_2d_secondary%firstguess_brightness_temperature_in_channel_no_6)
  
  !deallocate(l2_input_2d_secondary%covariance_matrix_element_11)
  !deallocate(l2_input_2d_secondary%covariance_matrix_element_12)
  !deallocate(l2_input_2d_secondary%covariance_matrix_element_13)
  !deallocate(l2_input_2d_secondary%covariance_matrix_element_14)
  !deallocate(l2_input_2d_secondary%covariance_matrix_element_15)
  !deallocate(l2_input_2d_secondary%covariance_matrix_element_21)
  !deallocate(l2_input_2d_secondary%covariance_matrix_element_22)
  !deallocate(l2_input_2d_secondary%covariance_matrix_element_23)
  !deallocate(l2_input_2d_secondary%covariance_matrix_element_24)
  !deallocate(l2_input_2d_secondary%covariance_matrix_element_25)
  !deallocate(l2_input_2d_secondary%covariance_matrix_element_31)
  !deallocate(l2_input_2d_secondary%covariance_matrix_element_32)
  !deallocate(l2_input_2d_secondary%covariance_matrix_element_33)
  !deallocate(l2_input_2d_secondary%covariance_matrix_element_34)
  !deallocate(l2_input_2d_secondary%covariance_matrix_element_35)
  !deallocate(l2_input_2d_secondary%covariance_matrix_element_41)
  !deallocate(l2_input_2d_secondary%covariance_matrix_element_42)
  !deallocate(l2_input_2d_secondary%covariance_matrix_element_43)
  !deallocate(l2_input_2d_secondary%covariance_matrix_element_44)
  !deallocate(l2_input_2d_secondary%covariance_matrix_element_45)
  !deallocate(l2_input_2d_secondary%covariance_matrix_element_51)
  !deallocate(l2_input_2d_secondary%covariance_matrix_element_52)
  !deallocate(l2_input_2d_secondary%covariance_matrix_element_53)
  !deallocate(l2_input_2d_secondary%covariance_matrix_element_54)
!deallocate(l2_input_2d_secondary%covariance_matrix_element_55)
  
end subroutine unset_l2_input_struct_2d_secondary



