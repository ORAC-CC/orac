! Name:
!    scanline_structure
!
! 2012/07/06 MJ extensively overhauls and restructures the code
! 2013/01/17 Matthias Jerg: Adds code to accommodate uncertainties of ctt and cth
! 2013/03/12 CP changed 32767 to 999
! 2014/10/24 OS: added variables cccot_pre, lusflag, cldtype, cloudmask, DEM, and nisemask
! 2014/11/20 OS: added Pavolonis cloud phase related variables
! 2014/11/26 CP: added Pavolonis cloud_albedo

!---------------------------------------------------------------------

module scanline_structure

  use vartypes_pp
  use common_constants

  implicit none

  type spixel_scanline_primary_output

     integer :: vidlat, vidlon,vidtime

     integer,dimension(:), pointer :: vidsat_zen,vidsol_zen,vidrel_azi,vidcloud_albedo

     integer :: vidcot,vidref, vidctp,vidcct,vidstemp, vidcth,vidcccot,vidcccot_pre
     integer :: vidctt, vidcwp
     integer :: vidcoterror,vidreferror, vidctperror,vidccterror,vidctherror,vidctterror
     integer :: vidstemperror,vidcwperror

     integer :: vidconvergence,vidniter, vidlsflag,vidphase,vidphase_pavolonis

     integer :: vidcostja,vidcostjm, vidqcflag, vidillum

     integer :: vidradiance1
     integer :: vidcldtype,vidcldmask,vidlusflag,viddem,vidnisemask

     real(kind=dreal) :: time_scale,time_offset,time_vmin,time_vmax,time_fv
     real :: lat_scale,lat_offset,lat_vmin,lat_vmax
     real :: lon_scale,lon_offset,lon_vmin,lon_vmax
     real :: real_fill_value_lat_lon=-999.0
     integer(kind=sint) :: int_fill_value=-999
     integer(kind=byte) :: byte_fill_value=-127
     real(kind=sreal) :: real_fill_value=-999.0

     real :: sat_scale,sat_offset,sat_vmin,sat_vmax
     real :: sol_scale,sol_offset,sol_vmin,sol_vmax
     real :: azi_scale,azi_offset,azi_vmin,azi_vmax

     real :: cot_scale,cot_offset
     integer(kind=sint) :: cot_vmin,cot_vmax

     real :: cot_error_scale,cot_error_offset
     integer(kind=sint) :: cot_error_vmin,cot_error_vmax

     real :: ref_scale,ref_offset
     integer(kind=sint) :: ref_vmin,ref_vmax

     real :: ref_error_scale,ref_error_offset
     integer(kind=sint) :: ref_error_vmin,ref_error_vmax

     real :: ctp_scale,ctp_offset
     integer(kind=sint) :: ctp_vmin,ctp_vmax

     real :: ctp_error_scale,ctp_error_offset
     integer(kind=sint) :: ctp_error_vmin,ctp_error_vmax

     real :: cccot_scale,cccot_offset
     integer(kind=sint) :: cccot_vmin,cccot_vmax

     real :: cccot_pre_scale,cccot_pre_offset
     integer(kind=sint) :: cccot_pre_vmin,cccot_pre_vmax

     real :: cct_scale,cct_offset
     integer(kind=sint) :: cct_vmin,cct_vmax

     real :: cct_error_scale,cct_error_offset
     integer(kind=sint) :: cct_error_vmin,cct_error_vmax

     real :: stemp_scale, stemp_offset
     integer(kind=sint) ::  stemp_vmin, stemp_vmax

     real :: stemp_error_scale, stemp_error_offset
     integer(kind=sint) ::  stemp_error_vmin, stemp_error_vmax

     real :: albedo_scale, albedo_offset
     integer(kind=sint) ::  albedo_vmin, albedo_vmax

     real :: cth_scale, cth_offset
     integer(kind=sint) ::  cth_vmin, cth_vmax

     real :: cth_error_scale,cth_error_offset
     integer(kind=sint) :: cth_error_vmin,cth_error_vmax

     real :: ctt_scale, ctt_offset
     integer(kind=sint) ::  ctt_vmin, ctt_vmax

     real :: ctt_error_scale,ctt_error_offset
     integer(kind=sint) :: ctt_error_vmin,ctt_error_vmax

     real :: cloud_albedo_scale,cloud_albedo_offset
     integer(kind=sint) :: cloud_albedo_vmin,cloud_albedo_vmax

     real :: cwp_scale,cwp_offset
     integer(kind=sint) :: cwp_vmin,cwp_vmax

     real :: cwp_error_scale,cwp_error_offset
     integer(kind=sint) :: cwp_error_vmin,cwp_error_vmax

     real(kind=sreal) :: costja_scale, costja_offset
     real(kind=sreal) ::  costja_vmin, costja_vmax

     real(kind=sreal) :: costjm_scale, costjm_offset
     real(kind=sreal) ::  costjm_vmin, costjm_vmax

     integer(kind=byte) :: con_scale, con_offset, con_vmin, con_vmax
     integer(kind=byte) :: ls_scale, ls_offset, ls_vmin, ls_vmax

     integer(kind=byte) :: niter_scale, niter_offset, niter_vmin, niter_vmax
     integer(kind=byte) :: phase_scale, phase_offset, phase_vmin, phase_vmax
     integer(kind=byte) :: phase_pavolonis_scale, phase_pavolonis_offset, phase_pavolonis_vmin, phase_pavolonis_vmax

     integer(kind=byte) :: illum_scale, illum_offset, illum_vmin, illum_vmax

     integer(kind=sint) :: qc_scale,qc_offset,qc_vmin,qc_vmax

     integer(kind=byte) :: cldtype_scale, cldtype_offset, cldtype_vmin,&
          & cldtype_vmax

     integer(kind=byte) :: cldmask_scale, cldmask_offset, cldmask_vmin,&
          & cldmask_vmax

     integer(kind=byte) :: lusflag_scale, lusflag_offset, lusflag_vmin,&
          & lusflag_vmax

     integer(kind=byte) :: lsflag_scale, lsflag_offset, lsflag_vmin,&
          & lsflag_vmax

     integer(kind=sint) :: dem_scale, dem_offset, dem_vmin, dem_vmax

     integer(kind=byte) :: nisemask_scale, nisemask_offset, nisemask_vmin,&
          & nisemask_vmax


     real(kind=dreal),  dimension(:,:), pointer :: time

     real,  dimension(:,:), pointer :: lon
     real,  dimension(:,:), pointer :: lat

     real,  dimension(:,:), pointer :: sat_zen
     real,  dimension(:,:), pointer :: sol_zen
     real,  dimension(:,:), pointer :: rel_azi

     !     integer(kind=byte), dimension(:), pointer :: cloudflag

     integer(kind=sint), dimension(:,:), pointer :: cct,cct_error

     integer(kind=sint), dimension(:,:), pointer :: cccot, cccot_pre

     integer(kind=sint), dimension(:,:), pointer :: cot,cot_error
     integer(kind=sint), dimension(:,:), pointer :: ref,ref_error

     integer(kind=sint), dimension(:,:), pointer :: ctp,ctp_error

     integer(kind=sint), dimension(:,:), pointer :: stemp,stemp_error

     integer(kind=sint), dimension(:,:), pointer :: albedo

     integer(kind=sint), dimension(:,:), pointer :: ctt,ctt_error
     integer(kind=sint), dimension(:,:), pointer :: cth,cth_error
     integer(kind=sint), dimension(:,:), pointer :: cwp,cwp_error
     integer(kind=sint), dimension(:,:,:), pointer :: cloud_albedo	

     integer(kind=byte), dimension(:,:), pointer :: convergence

     integer(kind=byte), dimension(:,:), pointer :: niter
     integer(kind=byte), dimension(:,:), pointer :: phase
     integer(kind=byte), dimension(:,:), pointer :: phase_pavolonis

     real(kind=sreal), dimension(:,:), pointer :: costja
     real(kind=sreal), dimension(:,:), pointer :: costjm

     integer(kind=byte), dimension(:,:), pointer :: lsflag,lusflag

     integer(kind=sint),  dimension(:,:), pointer :: qcflag

     integer(kind=byte), dimension(:,:), pointer :: illum

     integer(kind=byte), dimension(:,:), pointer :: cldtype, cldmask,&
          & nisemask

     integer(kind=sint), dimension(:,:), pointer :: dem

  end type spixel_scanline_primary_output


  type spixel_scanline_output_rt

     integer, dimension(:),pointer :: vidres
     integer, dimension(:),pointer :: vidchans	

     integer :: lint_fill_value=-999
     integer(kind=lint) :: int_fill_value=-999
     integer(kind=byte) :: byte_fill_value=-127

     real, dimension(:), pointer :: res_scale,res_offset
     real, dimension(:), pointer :: chans_scale,chans_offset	
     integer(kind=sint), dimension(:), pointer :: res_vmin,res_vmax
     integer(kind=sint), dimension(:), pointer :: chans_vmin,chans_vmax

     integer(kind=sint), dimension(:,:,:), pointer :: residuals
     integer(kind=sint), dimension(:,:,:), pointer :: channels


  end type spixel_scanline_output_rt


  !MJOBS
!!$
!!$!  Define a type for the Short Wave RTM parameters
!!$
!!$     
!!$    type spixel_scanline_primary_output
!!$
!!$        integer :: vidlat, vidlon,vidtime
!!$
!!$       integer :: vidsat_zen,vidsol_zen,vidrel_azi
!!$
!!$       integer :: vidcot,vidref, vidctp,vidcct,vidstemp, vidcth
!!$       integer :: vidctt, vidcwp
!!$       integer :: vidcoterror,vidreferror, vidctperror,vidccterror
!!$       integer :: vidstemperror,vidcwperror
!!$
!!$       integer :: vidconvergence,vidniter, vidlsflag,vidpchange, vidcloudflag
!!$
!!$       integer :: vidcostja,vidcostjm, vidqcflag, vidillum
!!$
!!$       integer :: vidradiance1
!!$
!!$       real(kind=dreal) :: time_scale,time_offset,time_vmin,time_vmax,time_fv
!!$       real :: lat_scale,lat_offset,lat_vmin,lat_vmax
!!$       real :: lon_scale,lon_offset,lon_vmin,lon_vmax
!!$       real :: real_fill_value_lat_lon=-999.0
!!$       integer(kind=lint) :: int_fill_value=-999
!!$       integer(kind=byte) :: byte_fill_value=-127
!!$
!!$       real :: sat_scale,sat_offset,sat_vmin,sat_vmax
!!$       real :: sol_scale,sol_offset,sol_vmin,sol_vmax
!!$       real :: azi_scale,azi_offset,azi_vmin,azi_vmax
!!$
!!$       real :: cot_scale,cot_offset
!!$       integer(kind=sint) :: cot_vmin,cot_vmax
!!$       
!!$       real :: cot_error_scale,cot_error_offset
!!$       integer(kind=sint) :: cot_error_vmin,cot_error_vmax
!!$
!!$       real :: ref_scale,ref_offset
!!$       integer(kind=sint) :: ref_vmin,ref_vmax
!!$
!!$       real :: ref_error_scale,ref_error_offset
!!$       integer(kind=sint) :: ref_error_vmin,ref_error_vmax
!!$
!!$       real :: ctp_scale,ctp_offset
!!$       integer(kind=sint) :: ctp_vmin,ctp_vmax
!!$
!!$       real :: ctp_error_scale,ctp_error_offset
!!$       integer(kind=sint) :: ctp_error_vmin,ctp_error_vmax
!!$
!!$       real :: cct_scale,cct_offset
!!$       integer(kind=sint) :: cct_vmin,cct_vmax
!!$
!!$       real :: cct_error_scale,cct_error_offset
!!$       integer(kind=sint) :: cct_error_vmin,cct_error_vmax
!!$
!!$       real :: stemp_scale, stemp_offset
!!$       integer(kind=sint) ::  stemp_vmin, stemp_vmax
!!$
!!$       real :: stemp_error_scale, stemp_error_offset
!!$       integer(kind=sint) ::  stemp_error_vmin, stemp_error_vmax
!!$
!!$       real :: albedo_scale, albedo_offset
!!$       integer(kind=sint) ::  albedo_vmin, albedo_vmax
!!$
!!$       
!!$       real :: cth_scale, cth_offset
!!$       integer(kind=sint) ::  cth_vmin, cth_vmax
!!$
!!$       real :: ctt_scale, ctt_offset
!!$       integer(kind=sint) ::  ctt_vmin, ctt_vmax
!!$
!!$       real :: cwp_scale,cwp_offset
!!$       integer(kind=sint) :: cwp_vmin,cwp_vmax
!!$
!!$       real :: cwp_error_scale,cwp_error_offset
!!$       integer(kind=sint) :: cwp_error_vmin,cwp_error_vmax
!!$
!!$       real :: costja_scale, costja_offset
!!$       integer(kind=sint) ::  costja_vmin, costja_vmax
!!$
!!$       real :: costjm_scale, costjm_offset
!!$       integer(kind=sint) ::  costjm_vmin, costjm_vmax
!!$
!!$       integer(kind=byte) :: con_scale, con_offset, con_vmin, con_vmax
!!$       integer(kind=byte) :: ls_scale, ls_offset, ls_vmin, ls_vmax
!!$       integer(kind=byte) :: cloud_scale, cloud_offset, cloud_vmin, cloud_vmax
!!$
!!$       integer(kind=byte) :: niter_scale, niter_offset, niter_vmin, niter_vmax
!!$       integer(kind=byte) :: pchange_scale, pchange_offset, pchange_vmin, pchange_vmax
!!$
!!$       integer(kind=byte) :: illum_scale, illum_offset, illum_vmin, illum_vmax
!!$
!!$       integer(kind=sint) :: qc_scale,qc_offset,qc_vmin,qc_vmax
!!$
!!$       real(kind=dreal),  dimension(:), pointer :: time
!!$
!!$       real,  dimension(:), pointer :: lon
!!$       real,  dimension(:), pointer :: lat
!!$
!!$       real,  dimension(:), pointer :: sat_zen
!!$       real,  dimension(:), pointer :: sol_zen
!!$       real,  dimension(:), pointer :: rel_azi
!!$
!!$       integer(kind=sint), dimension(:), pointer :: cct,cct_error
!!$
!!$       integer(kind=sint), dimension(:), pointer :: cot,cot_error
!!$       integer(kind=sint), dimension(:), pointer :: ref,ref_error
!!$
!!$       integer(kind=sint), dimension(:), pointer :: ctp,ctp_error
!!$
!!$       integer(kind=sint), dimension(:), pointer :: stemp,stemp_error
!!$
!!$       integer(kind=sint), dimension(:,:), pointer :: albedo
!!$
!!$       integer(kind=sint), dimension(:), pointer :: ctt
!!$       integer(kind=sint), dimension(:), pointer :: cth
!!$     integer(kind=sint), dimension(:), pointer :: cwp,cwp_error
!!$
!!$       integer(kind=byte), dimension(:), pointer :: convergence
!!$
!!$       integer(kind=byte), dimension(:), pointer :: niter
!!$       integer(kind=byte), dimension(:), pointer :: pchange
!!$       integer(kind=byte), dimension(:), pointer :: phase
!!$
!!$       integer(kind=sint), dimension(:), pointer :: costja
!!$       integer(kind=sint), dimension(:), pointer :: costjm
!!$
!!$       integer(kind=byte), dimension(:), pointer :: lsflag
!!$       integer(kind=byte), dimension(:), pointer :: cloudflag
!!$
!!$       integer(kind=sint),  dimension(:), pointer :: qcflag
!!$
!!$       integer(kind=byte), dimension(:), pointer :: illum
!!$
!!$    end type spixel_scanline_primary_output
!!$
!!$    type spixel_scanline_secondary_output
!!$
!!$       integer :: vidscanline_u, vidscanline_v
!!$
!!$       integer :: vidcotap,vidcotfg
!!$       integer :: vidrefap,vidreffg
!!$       integer :: vidctpap,vidctpfg
!!$      integer :: vidstempfg
!!$
!!$       integer, dimension(:,:), pointer :: vidcovar
!!$
!!$       integer, dimension(:),pointer :: vidres
!!$       integer, dimension(:),pointer :: vidchans	
!!$       integer, dimension(:),pointer :: vidy0
!!$
!!$       integer :: lint_fill_value=-999
!!$       integer(kind=lint) :: int_fill_value=-999
!!$       integer(kind=byte) :: byte_fill_value=-127
!!$
!!$       real :: scanline_u_scale,scanline_u_offset
!!$       real :: scanline_v_scale,scanline_v_offset
!!$       integer :: scanline_u_vmin,scanline_u_vmax
!!$       integer :: scanline_v_vmin,scanline_v_vmax
!!$      
!!$       integer, dimension(:), pointer :: scanline_u, scanline_v
!!$
!!$
!!$       real :: cot_ap_scale,cot_ap_offset
!!$       real :: cot_fg_scale,cot_fg_offset
!!$       integer(kind=sint) :: cot_ap_vmin,cot_ap_vmax
!!$       integer(kind=sint) :: cot_fg_vmin,cot_fg_vmax
!!$
!!$       integer(kind=sint), dimension(:), pointer :: cot_ap,cot_fg
!!$
!!$       real :: ref_ap_scale,ref_ap_offset
!!$       real :: ref_fg_scale,ref_fg_offset
!!$       integer(kind=sint) :: ref_ap_vmin,ref_ap_vmax
!!$       integer(kind=sint) :: ref_fg_vmin,ref_fg_vmax
!!$
!!$       integer(kind=sint), dimension(:), pointer :: ref_ap,ref_fg
!!$
!!$
!!$       real :: ctp_ap_scale,ctp_ap_offset
!!$       real :: ctp_fg_scale,ctp_fg_offset
!!$       integer(kind=sint) :: ctp_ap_vmin,ctp_ap_vmax
!!$       integer(kind=sint) :: ctp_fg_vmin,ctp_fg_vmax
!!$
!!$       integer(kind=sint), dimension(:), pointer :: ctp_ap,ctp_fg
!!$
!!$
!!$       real :: stemp_fg_offset,stemp_fg_scale
!!$             integer(kind=sint) :: stemp_fg_vmin,stemp_fg_vmax
!!$          integer(kind=sint), dimension(:), pointer :: stemp_fg,stemp_ap
!!$
!!$       real, dimension(:), pointer :: res_scale,res_offset
!!$       real, dimension(:), pointer :: chans_scale,chans_offset	
!!$       integer(kind=sint), dimension(:), pointer :: res_vmin,res_vmax
!!$       integer(kind=sint), dimension(:), pointer :: chans_vmin,chans_vmax
!!$
!!$
!!$      real, dimension(:), pointer :: y0_scale,y0_offset	
!!$        integer(kind=sint), dimension(:), pointer :: y0_vmin,y0_vmax
!!$
!!$
!!$       integer(kind=sint), dimension(:,:), pointer :: residuals
!!$       integer(kind=sint), dimension(:,:), pointer :: y0
!!$      integer(kind=sint), dimension(:,:), pointer :: channels
!!$
!!$       real(kind=sreal), dimension(:,:,:), pointer :: covariance
!!$
!!$    end type spixel_scanline_secondary_output
!!$
!!$
!!$    type spixel_scanline_input
!!$
!!$       integer, dimension(:),pointer :: vidinput,viderror
!!$
!!$       integer(kind=lint) :: int_fill_value=-999
!!$
!!$       real, dimension(:), pointer :: input_scale,input_offset
!!$       integer(kind=sint), dimension(:), pointer :: input_vmin,input_vmax
!!$
!!$       real, dimension(:), pointer :: error_scale,error_offset
!!$       integer(kind=sint), dimension(:), pointer :: error_vmin,error_vmax
!!$
!!$       integer(kind=sint), dimension(:,:), pointer :: input,error
!!$       
!!$    end type spixel_scanline_input
!!$

end module Scanline_structure
   
