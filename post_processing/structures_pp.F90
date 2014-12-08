! Name: structures_pp.f90
!
!
! Purpose: F90 Module file which declares user defined variable type structures.
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
!2012/02/03 Matthias Jerg cleans out prototype code to prepare repository upload.
! 2012/03/06 Caroline Poulsen modified to produce post processed files
! 2012/03/18 Caroline Poulsen modified to add cloud flag
! 2012/06/20 Caroline Poulsen modified to add albedo
! 2012/07/06 MJ extensively overhauls and restructures the code
! 2014/09/20 CP adds in extra channel variables
! 2014/09/29 CP adds in variable names for MODIS
! 2014/10/24 OS: added variables cccot_pre, lusflag, cldtype, cloudmask, DEM, and nisemask
! 2014/12/02 CP adds in cloud_albedo

! $Id$
!
! Bugs:
!
!none known

module structures_pp

  use vartypes_pp

  implicit none

  type l2_input_struct_2d_primary

     real(kind=dreal), dimension(:,:), pointer :: time
     real(kind=sreal), dimension(:,:), pointer :: satellite_zenith_view_no1
     real(kind=sreal), dimension(:,:), pointer :: solar_zenith_view_no1
     real(kind=sreal), dimension(:,:), pointer :: rel_azimuth_view_no1
     real(kind=sreal), dimension(:,:), pointer :: lon, lat
     real(kind=sreal), dimension(:,:), pointer ::  ctt, cth, ctp, cct
     real(kind=sreal), dimension(:,:), pointer ::  stemp,stemp_uncertainty
     real(kind=sreal), dimension(:,:), pointer ::  costja,costjm
     real(kind=sreal), dimension(:,:), pointer ::  ctp_uncertainty,&
          & cct_uncertainty,ctt_uncertainty,cth_uncertainty
     real(kind=sreal), dimension(:,:), pointer ::  cot, ref, cty, cwp&
          &,cc_total,cc_total_uncertainty,cccot,cccot_pre
     real(kind=sreal), dimension(:,:,:), pointer ::  cloud_albedo

     real(kind=sreal), dimension(:,:), pointer ::  cot_uncertainty,&
          & ref_uncertainty, cwp_uncertainty
     
     integer(kind=sint), dimension(:,:), pointer :: qcflag
     integer(kind=byte), dimension(:,:), pointer :: niter
     integer(kind=byte), dimension(:,:), pointer :: convergence
     integer(kind=byte), dimension(:,:), pointer :: lsflag
     integer(kind=byte), dimension(:,:), pointer :: cldtype
     integer(kind=byte), dimension(:,:), pointer :: illum
     integer(kind=byte), dimension(:,:), pointer :: phase
     integer(kind=byte), dimension(:,:), pointer :: pchange
     integer(kind=byte), dimension(:,:), pointer :: cldmask
     integer(kind=byte), dimension(:,:), pointer :: lusflag
     integer(kind=sint), dimension(:,:), pointer :: dem
     integer(kind=byte), dimension(:,:), pointer :: nisemask
     !integer(kind=nint), dimension(:,:), pointer :: scanline_u
     !integer(kind=nint), dimension(:,:), pointer :: scanline_v

  end type l2_input_struct_2d_primary


  type l2_input_struct_2d_refl_bt

     integer(kind=nint) :: nchannels,nchannels_sw,nchannels_lw

     real(kind=sreal), dimension(:,:,:), pointer :: albedo
     real(kind=sreal), dimension(:,:,:), pointer :: reflectance_residual
     real(kind=sreal), dimension(:,:,:), pointer :: brightness_temperature_residual
     real(kind=sreal), dimension(:,:,:), pointer :: reflectance
     real(kind=sreal), dimension(:,:,:), pointer :: brightness_temperature
    
     !character(len=varlength), dimension(:), pointer :: vname, ename

  end type l2_input_struct_2d_refl_bt



  type l2_input_struct_2d_secondary

     real(kind=sreal), dimension(:,:), pointer :: cot_ap,cot_fg,ref_ap,ref_fg,ctp_ap,ctp_fg,stemp_ap,stemp_fg
     
     real(kind=sreal), dimension(:,:), pointer ::albedo_in_channel_no_1,albedo_in_channel_no_2
     real(kind=sreal), dimension(:,:), pointer ::albedo_in_channel_no_3
     real(kind=sreal), dimension(:,:), pointer ::reflectance_residual_in_channel_no_1,reflectance_residual_in_channel_no_2
     real(kind=sreal), dimension(:,:), pointer ::reflectance_residual_in_channel_no_3
     real(kind=sreal), dimension(:,:), pointer ::brightness_temperature_residual_in_channel_no_4
     real(kind=sreal), dimension(:,:), pointer ::brightness_temperature_residual_in_channel_no_5
     real(kind=sreal), dimension(:,:), pointer ::brightness_temperature_residual_in_channel_no_6
     real(kind=sreal), dimension(:,:), pointer ::brightness_temperature_residual_in_channel_no_7
     real(kind=sreal), dimension(:,:), pointer ::brightness_temperature_residual_in_channel_no_20
     real(kind=sreal), dimension(:,:), pointer ::brightness_temperature_residual_in_channel_no_31
     real(kind=sreal), dimension(:,:), pointer ::brightness_temperature_residual_in_channel_no_32
     real(kind=sreal), dimension(:,:), pointer ::reflectance_in_channel_no_1,reflectance_in_channel_no_2
     real(kind=sreal), dimension(:,:), pointer ::reflectance_in_channel_no_3
     real(kind=sreal), dimension(:,:), pointer ::brightness_temperature_in_channel_no_5,brightness_temperature_in_channel_no_7
     real(kind=sreal), dimension(:,:), pointer ::brightness_temperature_in_channel_no_4,brightness_temperature_in_channel_no_6
     real(kind=sreal), dimension(:,:), pointer ::brightness_temperature_in_channel_no_20,brightness_temperature_in_channel_no_31,brightness_temperature_in_channel_no_32

     real(kind=sreal), dimension(:,:), pointer ::firstguess_reflectance_in_channel_no_1,firstguess_reflectance_in_channel_no_2
     real(kind=sreal), dimension(:,:), pointer :: firstguess_reflectance_in_channel_no_3
     real(kind=sreal), dimension(:,:), pointer ::firstguess_brightness_temperature_in_channel_no_4
     real(kind=sreal), dimension(:,:), pointer ::firstguess_brightness_temperature_in_channel_no_5
    real(kind=sreal), dimension(:,:), pointer ::firstguess_brightness_temperature_in_channel_no_6
    real(kind=sreal), dimension(:,:), pointer ::firstguess_brightness_temperature_in_channel_no_7
     real(kind=sreal), dimension(:,:), pointer ::firstguess_brightness_temperature_in_channel_no_20
    real(kind=sreal), dimension(:,:), pointer ::firstguess_brightness_temperature_in_channel_no_31
    real(kind=sreal), dimension(:,:), pointer ::firstguess_brightness_temperature_in_channel_no_32
     real(kind=sreal), dimension(:,:), pointer :: covariance_matrix_element_11, covariance_matrix_element_12
     real(kind=sreal), dimension(:,:), pointer :: covariance_matrix_element_13
     real(kind=sreal), dimension(:,:), pointer :: covariance_matrix_element_14, covariance_matrix_element_15
     real(kind=sreal), dimension(:,:), pointer :: covariance_matrix_element_21
     real(kind=sreal), dimension(:,:), pointer :: covariance_matrix_element_22, covariance_matrix_element_23
     real(kind=sreal), dimension(:,:), pointer :: covariance_matrix_element_24
     real(kind=sreal), dimension(:,:), pointer :: covariance_matrix_element_25, covariance_matrix_element_31
     real(kind=sreal), dimension(:,:), pointer ::  covariance_matrix_element_32
     real(kind=sreal), dimension(:,:), pointer :: covariance_matrix_element_33, covariance_matrix_element_34
     real(kind=sreal), dimension(:,:), pointer ::  covariance_matrix_element_35
     real(kind=sreal), dimension(:,:), pointer :: covariance_matrix_element_41, covariance_matrix_element_42
     real(kind=sreal), dimension(:,:), pointer ::  covariance_matrix_element_43
     real(kind=sreal), dimension(:,:), pointer :: covariance_matrix_element_44
     real(kind=sreal), dimension(:,:), pointer :: covariance_matrix_element_45, covariance_matrix_element_51
     real(kind=sreal), dimension(:,:), pointer ::  covariance_matrix_element_52
     real(kind=sreal), dimension(:,:), pointer :: covariance_matrix_element_53, covariance_matrix_element_54
     real(kind=sreal), dimension(:,:), pointer ::  covariance_matrix_element_55
    
     !character(len=varlength), dimension(:), pointer :: vname, ename

  end type l2_input_struct_2d_secondary

end module structures_pp
