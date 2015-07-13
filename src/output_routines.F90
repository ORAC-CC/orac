!-------------------------------------------------------------------------------
! Name: output_routines.F90
!
! Purpose:
! Structure defintion for output_data and module for output routines.
!
! History:
! 2014/06/13, GM: Original version, output_data_*
!    structures taken from SPixel module.
! 2014/09/16, GM: Added output_utils.F90.
! 2014/10/24, OS: added variables cldtype, cloudmask,
!    cccot_pre, lusflags, dem, and nisemask
! 2014/11/25, AP: Move scaling/offset definitions here.
! 2014/12/01, OS: new cldtype_vmax = 9
! 2014/12/01, CP: added cloud albedo
! 2015/03/05, OS: added values to nisemask scale, offset, vmin,
!    vmax; set cth_vmin to -0.01
! 2015/03/19, OS: cth_vmin set to 0
! 2015/04/22, OS: cth_vmin set to -1000 m, i.e. -1 km
! 2015/07/03, OS: Added cldmask_error variables
! 2015/07/04, CP: Added corrected cth
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

module output_routines

   use ECP_Constants

   implicit none

   type output_data_primary
      ! Variable IDs for data fields in primary output file
      integer                       :: vid_time
      integer                       :: vid_lat, vid_lon
      integer,dimension(:), pointer :: vid_sol_zen, vid_sat_zen, vid_rel_azi
      integer                       :: vid_cot, vid_coterror
      integer                       :: vid_ref, vid_referror
      integer                       :: vid_ctp, vid_ctperror
      integer                       :: vid_cct, vid_ccterror
      integer                       :: vid_stemp, vid_stemperror
      integer                       :: vid_cth, vid_ctherror
      integer                       :: vid_cth_corrected, vid_cth_correctederror
      integer                       :: vid_ctt, vid_ctterror
      integer                       :: vid_cwp, vid_cwperror
      integer                       :: vid_convergence
      integer                       :: vid_niter
      integer                       :: vid_phase

      integer,dimension(:), pointer :: vid_cloud_albedo

      integer                       :: vid_costja
      integer                       :: vid_costjm

      integer                       :: vid_lsflag
      integer                       :: vid_qcflag
      integer                       :: vid_illum
      integer                       :: vid_cldtype
      integer                       :: vid_cldmask,vid_cldmaskerror
      integer                       :: vid_cccot_pre
      integer                       :: vid_lusflag
      integer                       :: vid_dem
      integer                       :: vid_nisemask

      ! Scale, offset, valid min/max for output fields
      real(kind=dreal)              :: time_scale  = 1.0
      real(kind=dreal)              :: time_offset = 0.0
      real(kind=dreal)              :: time_vmin   = 0.0
      real(kind=dreal)              :: time_vmax   = 1.0e10

      real(kind=sreal)              :: lat_scale  = 1.0
      real(kind=sreal)              :: lat_offset = 0.0
      real(kind=sreal)              :: lat_vmin   = -90.0
      real(kind=sreal)              :: lat_vmax   = 90.0
      real(kind=sreal)              :: lon_scale  = 1.0
      real(kind=sreal)              :: lon_offset = 0.0
      real(kind=sreal)              :: lon_vmin   = -180.0
      real(kind=sreal)              :: lon_vmax   = 180.0

      real(kind=sreal)              :: sol_scale  = 1.0
      real(kind=sreal)              :: sol_offset = 0.0
      real(kind=sreal)              :: sol_vmin   = -180.0
      real(kind=sreal)              :: sol_vmax   = 180.0
      real(kind=sreal)              :: sat_scale  = 1.0
      real(kind=sreal)              :: sat_offset = 0.0
      real(kind=sreal)              :: sat_vmin   = -180.0
      real(kind=sreal)              :: sat_vmax   = 180.0
      real(kind=sreal)              :: azi_scale  = 1.0
      real(kind=sreal)              :: azi_offset = 0.0
      real(kind=sreal)              :: azi_vmin   = -180.0
      real(kind=sreal)              :: azi_vmax   = 180.0

      real(kind=sreal)              :: cot_scale        = 0.01
      real(kind=sreal)              :: cot_offset       = 0.0
      integer(kind=sint)            :: cot_vmin         = 0
      integer(kind=sint)            :: cot_vmax         = 32000
      real(kind=sreal)              :: cot_error_scale  = 0.01
      real(kind=sreal)              :: cot_error_offset = 0.0
      integer(kind=sint)            :: cot_error_vmin   = 0
      integer(kind=sint)            :: cot_error_vmax   = 32000

      real(kind=sreal)              :: ref_scale        = 0.01
      real(kind=sreal)              :: ref_offset       = 0.0
      integer(kind=sint)            :: ref_vmin         = 0
      integer(kind=sint)            :: ref_vmax         = 20000
      real(kind=sreal)              :: ref_error_scale  = 0.01
      real(kind=sreal)              :: ref_error_offset = 0.0
      integer(kind=sint)            :: ref_error_vmin   = 0
      integer(kind=sint)            :: ref_error_vmax   = 20000

      real(kind=sreal)              :: ctp_scale        = 0.1
      real(kind=sreal)              :: ctp_offset       = 0.0
      integer(kind=sint)            :: ctp_vmin         = 0
      integer(kind=sint)            :: ctp_vmax         = 12000
      real(kind=sreal)              :: ctp_error_scale  = 0.1
      real(kind=sreal)              :: ctp_error_offset = 0.0
      integer(kind=sint)            :: ctp_error_vmin   = 0
      integer(kind=sint)            :: ctp_error_vmax   = 12000

      real(kind=sreal)              :: cct_scale        = 0.01
      real(kind=sreal)              :: cct_offset       = 0.0
      integer(kind=sint)            :: cct_vmin         = 0
      integer(kind=sint)            :: cct_vmax         = 100
      real                          :: cct_error_scale  = 0.01
      real                          :: cct_error_offset = 0.0
      integer(kind=sint)            :: cct_error_vmin   = 0
      integer(kind=sint)            :: cct_error_vmax   = 10000

!     real(kind=sreal)              :: albedo_scale,albedo_offset
!     integer(kind=sint)            :: albedo_vmin,albedo_vmax
!     real(kind=sreal)              :: albedo_error_scale,albedo_error_offset
!     integer(kind=sint)            :: albedo_error_vmin,albedo_error_vmax

      real(kind=sreal)              :: stemp_scale        = 0.01
      real(kind=sreal)              :: stemp_offset       = 100.0
      integer(kind=sint)            :: stemp_vmin         = 0
      integer(kind=sint)            :: stemp_vmax         = 32000
      real(kind=sreal)              :: stemp_error_scale  = 0.01
      real(kind=sreal)              :: stemp_error_offset = 0.0
      integer(kind=sint)            :: stemp_error_vmin   = 0
      integer(kind=sint)            :: stemp_error_vmax   = 32000

      real(kind=sreal)              :: cth_scale        = 0.01
      real(kind=sreal)              :: cth_offset       = 0.0
      integer(kind=sint)            :: cth_vmin         = -1000
      integer(kind=sint)            :: cth_vmax         = 2000
      real(kind=sreal)              :: cth_error_scale  = 0.01
      real(kind=sreal)              :: cth_error_offset = 0.0
      integer(kind=sint)            :: cth_error_vmin   = 0
      integer(kind=sint)            :: cth_error_vmax   = 2000

      real(kind=sreal)              :: ctt_scale        = 0.01
      real(kind=sreal)              :: ctt_offset       = 100.0
      integer(kind=sint)            :: ctt_vmin         = 0
      integer(kind=sint)            :: ctt_vmax         = 32000
      real                          :: ctt_error_scale  = 0.01
      real                          :: ctt_error_offset = 0.0
      integer(kind=sint)            :: ctt_error_vmin   = 0
      integer(kind=sint)            :: ctt_error_vmax   = 32000

      real(kind=sreal)              :: cwp_scale        = 1.0
      real(kind=sreal)              :: cwp_offset       = 0.0
      integer(kind=sint)            :: cwp_vmin         = 0
      integer(kind=sint)            :: cwp_vmax         = 32000
      real                          :: cwp_error_scale  = 1.0
      real                          :: cwp_error_offset = 0.0
      integer(kind=sint)            :: cwp_error_vmin   = 0
      integer(kind=sint)            :: cwp_error_vmax   = 32000

      integer(kind=byte)            :: convergence_scale  = 1
      integer(kind=byte)            :: convergence_offset = 0
      integer(kind=byte)            :: convergence_vmin   = 0
      integer(kind=byte)            :: convergence_vmax   = 1

      integer(kind=byte)            :: niter_scale  = 1
      integer(kind=byte)            :: niter_offset = 0
      integer(kind=byte)            :: niter_vmin   = 0
      integer(kind=byte)            :: niter_vmax   ! Assigned by ReadDriver

      integer(kind=byte)            :: phase_scale  = 1
      integer(kind=byte)            :: phase_offset = 0
      integer(kind=byte)            :: phase_vmin   = 0
      integer(kind=byte)            :: phase_vmax   = 2

      real(kind=sreal)              :: costja_scale  = 1.0
      real(kind=sreal)              :: costja_offset = 0.0
      real(kind=sreal)              :: costja_vmin   = 0.0
      real(kind=sreal)              :: costja_vmax   = 100000.0

      real(kind=sreal)              :: costjm_scale  = 1.0
      real(kind=sreal)              :: costjm_offset = 0.0
      real(kind=sreal)              :: costjm_vmin   = 0.0
      real(kind=sreal)              :: costjm_vmax   = 100000.0

      integer(kind=byte)            :: lsflag_scale  = 1
      integer(kind=byte)            :: lsflag_offset = 0
      integer(kind=byte)            :: lsflag_vmin   = 0
      integer(kind=byte)            :: lsflag_vmax   = 6

      integer(kind=sint)            :: qcflag_scale  = 1
      integer(kind=sint)            :: qcflag_offset = 0
      integer(kind=sint)            :: qcflag_vmin   = 0
      integer(kind=sint)            :: qcflag_vmax   = 254

      integer(kind=byte)            :: illum_scale  = 1
      integer(kind=byte)            :: illum_offset = 0
      integer(kind=byte)            :: illum_vmin   = 1
      integer(kind=byte)            :: illum_vmax   = 12

      integer(kind=byte)            :: cldtype_scale  = 1
      integer(kind=byte)            :: cldtype_offset = 0
      integer(kind=byte)            :: cldtype_vmin   = 0
      integer(kind=byte)            :: cldtype_vmax   = 9

      integer(kind=byte)            :: cldmask_scale  = 1
      integer(kind=byte)            :: cldmask_offset = 0
      integer(kind=byte)            :: cldmask_vmin   = 0
      integer(kind=byte)            :: cldmask_vmax   = 1

      real(kind=sreal)              :: cccot_pre_scale  = 1.0
      real(kind=sreal)              :: cccot_pre_offset = 0.0
      real(kind=sreal)              :: cccot_pre_vmin   = -1.0
      real(kind=sreal)              :: cccot_pre_vmax   = 2.0

      integer(kind=byte)            :: lusflag_scale  = 1
      integer(kind=byte)            :: lusflag_offset = 0
      integer(kind=byte)            :: lusflag_vmin   = 1
      integer(kind=byte)            :: lusflag_vmax   = 24

      integer(kind=sint)            :: dem_scale  = 1
      integer(kind=sint)            :: dem_offset = 0
      integer(kind=sint)            :: dem_vmin   = 0
      integer(kind=sint)            :: dem_vmax   = 10000

      integer(kind=byte)            :: nisemask_scale  = 1
      integer(kind=byte)            :: nisemask_offset = 0
      integer(kind=byte)            :: nisemask_vmin   = 0
      integer(kind=byte)            :: nisemask_vmax   = 1

      real(kind=sreal),   dimension(:),     pointer :: cloud_albedo_scale,cloud_albedo_offset
      integer(kind=sint), dimension(:),     pointer :: cloud_albedo_vmin,cloud_albedo_vmax


      real(kind=dreal),   dimension(:,:),   pointer :: time

      real(kind=sreal),   dimension(:,:),   pointer :: lat
      real(kind=sreal),   dimension(:,:),   pointer :: lon

      real(kind=sreal),   dimension(:,:,:), pointer :: sol_zen
      real(kind=sreal),   dimension(:,:,:), pointer :: sat_zen
      real(kind=sreal),   dimension(:,:,:), pointer :: rel_azi

      integer(kind=sint), dimension(:,:),   pointer :: cot
      integer(kind=sint), dimension(:,:),   pointer :: cot_error

      integer(kind=sint), dimension(:,:),   pointer :: ref
      integer(kind=sint), dimension(:,:),   pointer :: ref_error

      integer(kind=sint), dimension(:,:),   pointer :: ctp
      integer(kind=sint), dimension(:,:),   pointer :: ctp_error

      integer(kind=sint), dimension(:,:),   pointer :: cct
      integer(kind=sint), dimension(:,:),   pointer :: cct_error

!     integer(kind=sint), dimension(:,:),   pointer :: albedo
!     integer(kind=sint), dimension(:,:),   pointer :: albedo_error

      integer(kind=sint), dimension(:,:),   pointer :: stemp
      integer(kind=sint), dimension(:,:),   pointer :: stemp_error

      integer(kind=sint), dimension(:,:),   pointer :: ctt
      integer(kind=sint), dimension(:,:),   pointer :: ctt_error

      integer(kind=sint), dimension(:,:),   pointer :: cth
      integer(kind=sint), dimension(:,:),   pointer :: cth_error

      integer(kind=sint), dimension(:,:),   pointer :: cth_corrected
      integer(kind=sint), dimension(:,:),   pointer :: cth_corrected_error

      integer(kind=sint), dimension(:,:),   pointer :: cwp
      integer(kind=sint), dimension(:,:),   pointer :: cwp_error

      integer(kind=byte), dimension(:,:),   pointer :: convergence

      integer(kind=byte), dimension(:,:),   pointer :: niter

      integer(kind=byte), dimension(:,:),   pointer :: phase

      real(kind=sreal),   dimension(:,:),   pointer :: costja

      real(kind=sreal),   dimension(:,:),   pointer :: costjm

      integer(kind=byte), dimension(:,:),   pointer :: lsflag

      integer(kind=sint), dimension(:,:),   pointer :: qcflag

      integer(kind=byte), dimension(:,:),   pointer :: illum

      integer(kind=byte), dimension(:,:),   pointer :: cldtype

      integer(kind=byte), dimension(:,:),   pointer :: cldmask

      real(kind=sreal),   dimension(:,:),   pointer :: cldmask_error

      real(kind=sreal),   dimension(:,:),   pointer :: cccot_pre

      integer(kind=byte), dimension(:,:),   pointer :: lusflag

      integer(kind=sint), dimension(:,:),   pointer :: dem

      integer(kind=byte), dimension(:,:),   pointer :: nisemask

      integer(kind=sint), dimension(:,:,:),  pointer :: cloud_albedo

   end type output_data_primary


   type output_data_secondary
      integer                          :: vid_scanline_u
      integer                          :: vid_scanline_v

      integer                          :: vid_cot_ap,vid_cot_fg
      integer                          :: vid_ref_ap,vid_ref_fg
      integer                          :: vid_ctp_ap,vid_ctp_fg
      integer                          :: vid_stemp_ap,vid_stemp_fg

      integer, dimension(:),   pointer :: vid_albedo

      integer, dimension(:),   pointer :: vid_channels

      integer, dimension(:),   pointer :: vid_y0

      integer, dimension(:),   pointer :: vid_residuals

      integer                          :: vid_ds

      integer, dimension(:,:), pointer :: vid_covariance


      integer(kind=lint)               :: scanline_u_scale,scanline_u_offset
      integer(kind=lint)               :: scanline_u_vmin,scanline_u_vmax

      integer(kind=lint)               :: scanline_v_scale,scanline_v_offset
      integer(kind=lint)               :: scanline_v_vmin,scanline_v_vmax

      real(kind=sreal)                 :: cot_ap_scale,cot_ap_offset
      integer(kind=sint)               :: cot_ap_vmin,cot_ap_vmax

      real(kind=sreal)                 :: cot_fg_scale,cot_fg_offset
      integer(kind=sint)               :: cot_fg_vmin,cot_fg_vmax

      real(kind=sreal)                 :: ref_ap_scale,ref_ap_offset
      integer(kind=sint)               :: ref_ap_vmin,ref_ap_vmax

      real(kind=sreal)                 :: ref_fg_scale,ref_fg_offset
      integer(kind=sint)               :: ref_fg_vmin,ref_fg_vmax

      real(kind=sreal)                 :: ctp_ap_scale,ctp_ap_offset
      integer(kind=sint)               :: ctp_ap_vmin,ctp_ap_vmax

      real(kind=sreal)                 :: ctp_fg_scale,ctp_fg_offset
      integer(kind=sint)               :: ctp_fg_vmin,ctp_fg_vmax

      real(kind=sreal)                 :: stemp_ap_offset,stemp_ap_scale
      integer(kind=sint)               :: stemp_ap_vmin,stemp_ap_vmax

      real(kind=sreal)                 :: stemp_fg_offset,stemp_fg_scale
      integer(kind=sint)               :: stemp_fg_vmin,stemp_fg_vmax

      real(kind=sreal),   dimension(:), pointer :: albedo_scale,albedo_offset
      integer(kind=sint), dimension(:), pointer :: albedo_vmin,albedo_vmax

      real(kind=sreal),   dimension(:), pointer :: channels_scale,channels_offset
      integer(kind=sint), dimension(:), pointer :: channels_vmin,channels_vmax

      real(kind=sreal),   dimension(:), pointer :: y0_scale,y0_offset
      integer(kind=sint), dimension(:), pointer :: y0_vmin,y0_vmax

      real(kind=sreal),   dimension(:), pointer :: residuals_scale,residuals_offset
      integer(kind=sint), dimension(:), pointer :: residuals_vmin,residuals_vmax

      real(kind=sreal)                          :: ds_offset, ds_scale
      integer(kind=sint)                        :: ds_vmin, ds_vmax

      real(kind=sreal)                          :: covariance_offset, covariance_scale
      integer(kind=sint)                        :: covariance_vmin, covariance_vmax


      integer(kind=lint), dimension(:,:),     pointer :: scanline_u, scanline_v

      integer(kind=sint), dimension(:,:),     pointer :: cot_ap,cot_fg
      integer(kind=sint), dimension(:,:),     pointer :: ref_ap,ref_fg
      integer(kind=sint), dimension(:,:),     pointer :: ctp_ap,ctp_fg
      integer(kind=sint), dimension(:,:),     pointer :: stemp_ap,stemp_fg

      integer(kind=sint), dimension(:,:,:),   pointer :: albedo
      integer(kind=sint), dimension(:,:,:),   pointer :: channels
      integer(kind=sint), dimension(:,:,:),   pointer :: y0
      integer(kind=sint), dimension(:,:,:),   pointer :: residuals

      integer(kind=sint), dimension(:,:),     pointer :: ds

      real(kind=sreal),   dimension(:,:,:,:), pointer :: covariance

   end type output_data_secondary

contains

include 'alloc_output_data.F90'
include 'dealloc_output_data.F90'

include 'output_utils.F90'

include 'def_vars_primary.F90'
include 'def_vars_secondary.F90'

include 'prepare_primary.F90'
include 'prepare_secondary.F90'

include 'write_primary.F90'
include 'write_secondary.F90'

end module output_routines
