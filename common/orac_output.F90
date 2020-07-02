!-------------------------------------------------------------------------------
! Name: orac_output.F90
!
! Purpose:
! Structure defintion for output_data and module for output routines.
!
! History:
! 2014/06/13, GM: Original version, output_data_* structures taken from SPixel
!    module.
! 2014/09/16, GM: Added output_utils.F90.
! 2014/10/24, OS: added variables cldtype, cloudmask, cccot_pre, lusflags, dem,
!    and nisemask
! 2014/11/25, AP: Move scaling/offset definitions here.
! 2014/12/01, OS: new cldtype_vmax = 9
! 2014/12/01, CP: added cloud albedo
! 2015/03/05, OS: added values to nisemask scale, offset, vmin, vmax; set
!    cth_vmin to -0.01
! 2015/03/19, OS: cth_vmin set to 0
! 2015/04/22, OS: cth_vmin set to -1000 m, i.e. -1 km
! 2015/07/03, OS: Added cldmask_error variables
! 2015/07/04, CP: Added corrected cth
! 2015/07/31, AP: Add string_description_of_state().
! 2015/09/06, GM: Move into common/ from src/ minus the prepare routines and
!    string_description_of_state().  Also, changes related to sharing with
!    post_processing/.
! 2015/09/07, GM: Add cldmask_uncertainty.
! 2015/10/22, GM: Add cloud albedo uncertainty.
! 2015/12/28, AP: Add output fields for aerosol retrievals.
! 2015/12/30, AP: Move declarations of scale/offset/vmin/vmax to here from def_
!    routines. Have all albedo fields use the same values.
! 2016/01/06, AP: Wrap do_* flags into output_flags structure.
! 2015/01/07, AP: Make QCFlag long to accomodate longer state vectors.
! 2016/01/27, GM: Add cee and cee_uncertainty.
! 2016/01/28, GM: Add ctp and ctt corrected and corrected_uncertianty.
! 2016/03/02, AP: Homogenisation of I/O modules.
! 2016/04/28, AP: Add multiple views.
! 2016/07/08, GM: Add fields for cloud layer 2.
! 2016/07/19, AP: Reduce rho and swansea_s to only contain terms that were
!    retrieved. This is indicated by the rho|ss_terms array (and Nrho|Nss).
! 2016/07/20, WJ: Change offset for stemp output to 100. to support values
!    greater than 320K for instruments other than ATSR2/AATSR.
! 2017/01/09, CP: Changed phase range to include ML cloud type.
! 2017/05/17, OS: Added variables for ann phase.
! 2017/07/05, AP: Add channels_used, variables_retrieved. New QC.
! 2018/01/19, GT: Removed QCFlag scale and offset values, as these should only
!    be used for packed floating point data (not straight integers like QCFlag).
! 2018/06/08, SP: Add satellite azimuth angle to output.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

module orac_output_m

   use orac_indexing_m

   implicit none

   type output_data_primary_t
      ! Variable IDs for data fields in primary output file
      integer          :: vid_aot550
      integer          :: vid_aot550_uncertainty
      integer          :: vid_aot870
      integer          :: vid_aot870_uncertainty
      integer          :: vid_aer
      integer          :: vid_aer_uncertainty

      integer, pointer :: vid_rho(:)
      integer, pointer :: vid_rho_uncertainty(:)

      integer, pointer :: vid_swansea_s(:)
      integer, pointer :: vid_swansea_s_uncertainty(:)
      integer, pointer :: vid_swansea_p(:)
      integer, pointer :: vid_swansea_p_uncertainty(:)
      integer, pointer :: vid_diffuse_frac(:)
      integer, pointer :: vid_diffuse_frac_uncertainty(:)

      integer          :: vid_cot
      integer          :: vid_cot_uncertainty
      integer          :: vid_cer
      integer          :: vid_cer_uncertainty
      integer          :: vid_ctp
      integer          :: vid_ctp_uncertainty
      integer          :: vid_ctp_corrected
      integer          :: vid_ctp_corrected_uncertainty
      integer          :: vid_cc_total
      integer          :: vid_cc_total_uncertainty

      integer          :: vid_cot2
      integer          :: vid_cot2_uncertainty
      integer          :: vid_cer2
      integer          :: vid_cer2_uncertainty
      integer          :: vid_ctp2
      integer          :: vid_ctp2_uncertainty
      integer          :: vid_cc_total2
      integer          :: vid_cc_total2_uncertainty

      integer          :: vid_stemp
      integer          :: vid_stemp_uncertainty

      integer          :: vid_cth
      integer          :: vid_cth_uncertainty
      integer          :: vid_cth_corrected
      integer          :: vid_cth_corrected_uncertainty
      integer          :: vid_ctt
      integer          :: vid_ctt_uncertainty
      integer          :: vid_ctt_corrected
      integer          :: vid_ctt_corrected_uncertainty
      integer          :: vid_cwp
      integer          :: vid_cwp_uncertainty

      integer          :: vid_cth2
      integer          :: vid_cth2_uncertainty
      integer          :: vid_ctt2
      integer          :: vid_ctt2_uncertainty
      integer          :: vid_cwp2
      integer          :: vid_cwp2_uncertainty

      integer, pointer :: vid_cloud_albedo(:)
      integer, pointer :: vid_cloud_albedo_uncertainty(:)
      integer, pointer :: vid_cee(:)
      integer, pointer :: vid_cee_uncertainty(:)

      integer          :: vid_cccot_pre

      integer          :: vid_time
      integer          :: vid_lat
      integer          :: vid_lon
      integer, pointer :: vid_sol_zen(:)
      integer, pointer :: vid_sat_zen(:)
      integer, pointer :: vid_rel_azi(:)
      integer, pointer :: vid_sat_azi(:)

      integer          :: vid_niter
      integer          :: vid_costja
      integer          :: vid_costjm
      integer          :: vid_qcflag
      integer          :: vid_channels_used
      integer          :: vid_variables_retrieved

      integer          :: vid_lsflag
      integer          :: vid_lusflag
      integer          :: vid_dem

      integer          :: vid_illum

      integer          :: vid_cldtype
      integer          :: vid_cldmask
      integer          :: vid_cldmask_uncertainty

      integer          :: vid_ann_phase
      integer          :: vid_ann_phase_uncertainty
      integer          :: vid_cphcot

      integer          :: vid_phase

      integer          :: vid_phase_pavolonis

      integer          :: vid_y_id
      integer          :: vid_view_id
      integer          :: vid_rho_flags
      integer          :: vid_ch_is

      ! Scale, offset, valid min/max for output fields
      real(sreal)   :: aot550_scale              = 0.001
      real(sreal)   :: aot550_offset             = 0.0
      integer(sint) :: aot550_vmin               = 0
      integer(sint) :: aot550_vmax               = 32000
      real(sreal)   :: aot550_uncertainty_scale  = 0.001
      real(sreal)   :: aot550_uncertainty_offset = 0.0
      integer(sint) :: aot550_uncertainty_vmin   = 0
      integer(sint) :: aot550_uncertainty_vmax   = 32000

      real(sreal)   :: aot870_scale              = 0.001
      real(sreal)   :: aot870_offset             = 0.0
      integer(sint) :: aot870_vmin               = 0
      integer(sint) :: aot870_vmax               = 32000
      real(sreal)   :: aot870_uncertainty_scale  = 0.001
      real(sreal)   :: aot870_uncertainty_offset = 0.0
      integer(sint) :: aot870_uncertainty_vmin   = 0
      integer(sint) :: aot870_uncertainty_vmax   = 32000

      real(sreal)   :: aer_scale              = 0.001
      real(sreal)   :: aer_offset             = 0.0
      integer(sint) :: aer_vmin               = 0
      integer(sint) :: aer_vmax               = 32000
      real(sreal)   :: aer_uncertainty_scale  = 0.001
      real(sreal)   :: aer_uncertainty_offset = 0.0
      integer(sint) :: aer_uncertainty_vmin   = 0
      integer(sint) :: aer_uncertainty_vmax   = 32000

      real(sreal)   :: rho_scale              = 0.0001
      real(sreal)   :: rho_offset             = 0.0
      integer(sint) :: rho_vmin               = 0
      integer(sint) :: rho_vmax               = 32000
      real(sreal)   :: rho_uncertainty_scale  = 0.0001
      real(sreal)   :: rho_uncertainty_offset = 0.0
      integer(sint) :: rho_uncertainty_vmin   = 0
      integer(sint) :: rho_uncertainty_vmax   = 32000

      real(sreal)   :: swansea_s_scale              = 0.0001
      real(sreal)   :: swansea_s_offset             = 0.0
      integer(sint) :: swansea_s_vmin               = 0
      integer(sint) :: swansea_s_vmax               = 32000
      real(sreal)   :: swansea_s_uncertainty_scale  = 0.0001
      real(sreal)   :: swansea_s_uncertainty_offset = 0.0
      integer(sint) :: swansea_s_uncertainty_vmin   = 0
      integer(sint) :: swansea_s_uncertainty_vmax   = 32000

      real(sreal)   :: swansea_p_scale              = 0.0001
      real(sreal)   :: swansea_p_offset             = 0.0
      integer(sint) :: swansea_p_vmin               = 0
      integer(sint) :: swansea_p_vmax               = 32000
      real(sreal)   :: swansea_p_uncertainty_scale  = 0.0001
      real(sreal)   :: swansea_p_uncertainty_offset = 0.0
      integer(sint) :: swansea_p_uncertainty_vmin   = 0
      integer(sint) :: swansea_p_uncertainty_vmax   = 32000

      real(sreal)   :: diffuse_frac_scale              = 0.0001
      real(sreal)   :: diffuse_frac_offset             = 0.0
      integer(sint) :: diffuse_frac_vmin               = 0
      integer(sint) :: diffuse_frac_vmax               = 10000
      real(sreal)   :: diffuse_frac_uncertainty_scale  = 0.0001
      real(sreal)   :: diffuse_frac_uncertainty_offset = 0.0
      integer(sint) :: diffuse_frac_uncertainty_vmin   = 0
      integer(sint) :: diffuse_frac_uncertainty_vmax   = 10000

      real(sreal)   :: cot_scale              = 0.01
      real(sreal)   :: cot_offset             = 0.0
      integer(sint) :: cot_vmin               = 0
      integer(sint) :: cot_vmax               = 32000
      real(sreal)   :: cot_uncertainty_scale  = 0.01
      real(sreal)   :: cot_uncertainty_offset = 0.0
      integer(sint) :: cot_uncertainty_vmin   = 0
      integer(sint) :: cot_uncertainty_vmax   = 32000

      real(sreal)   :: cer_scale              = 0.01
      real(sreal)   :: cer_offset             = 0.0
      integer(sint) :: cer_vmin               = 0
      integer(sint) :: cer_vmax               = 20000
      real(sreal)   :: cer_uncertainty_scale  = 0.01
      real(sreal)   :: cer_uncertainty_offset = 0.0
      integer(sint) :: cer_uncertainty_vmin   = 0
      integer(sint) :: cer_uncertainty_vmax   = 20000

      real(sreal)   :: ctp_scale              = 0.1
      real(sreal)   :: ctp_offset             = 0.0
      integer(sint) :: ctp_vmin               = 0
      integer(sint) :: ctp_vmax               = 12000
      real(sreal)   :: ctp_uncertainty_scale  = 0.1
      real(sreal)   :: ctp_uncertainty_offset = 0.0
      integer(sint) :: ctp_uncertainty_vmin   = 0
      integer(sint) :: ctp_uncertainty_vmax   = 12000

      real(sreal)   :: cc_total_scale              = 0.01
      real(sreal)   :: cc_total_offset             = 0.0
      integer(sint) :: cc_total_vmin               = 0
      integer(sint) :: cc_total_vmax               = 100
      real(sreal)   :: cc_total_uncertainty_scale  = 0.01
      real(sreal)   :: cc_total_uncertainty_offset = 0.0
      integer(sint) :: cc_total_uncertainty_vmin   = 0
      integer(sint) :: cc_total_uncertainty_vmax   = 10000

      real(sreal)   :: stemp_scale              = 0.01
      real(sreal)   :: stemp_offset             = 100.0
      integer(sint) :: stemp_vmin               = 0
      integer(sint) :: stemp_vmax               = 32000
      real(sreal)   :: stemp_uncertainty_scale  = 0.01
      real(sreal)   :: stemp_uncertainty_offset = 0.0
      integer(sint) :: stemp_uncertainty_vmin   = 0
      integer(sint) :: stemp_uncertainty_vmax   = 32000

      real(sreal)   :: cth_scale              = 0.01
      real(sreal)   :: cth_offset             = 0.0
      integer(sint) :: cth_vmin               = -1000
      integer(sint) :: cth_vmax               = 3500
      real(sreal)   :: cth_uncertainty_scale  = 0.01
      real(sreal)   :: cth_uncertainty_offset = 0.0
      integer(sint) :: cth_uncertainty_vmin   = 0
      integer(sint) :: cth_uncertainty_vmax   = 2000

      real(sreal)   :: ctt_scale              = 0.01
      real(sreal)   :: ctt_offset             = 0.0
      integer(sint) :: ctt_vmin               = 0
      integer(sint) :: ctt_vmax               = 32000
      real(sreal)   :: ctt_uncertainty_scale  = 0.01
      real(sreal)   :: ctt_uncertainty_offset = 0.0
      integer(sint) :: ctt_uncertainty_vmin   = 0
      integer(sint) :: ctt_uncertainty_vmax   = 32000

      real(sreal)   :: cwp_scale              = 1.0
      real(sreal)   :: cwp_offset             = 0.0
      integer(sint) :: cwp_vmin               = 0
      integer(sint) :: cwp_vmax               = 32000
      real(sreal)   :: cwp_uncertainty_scale  = 1.0
      real(sreal)   :: cwp_uncertainty_offset = 0.0
      integer(sint) :: cwp_uncertainty_vmin   = 0
      integer(sint) :: cwp_uncertainty_vmax   = 32000

      real(sreal)   :: cloud_albedo_scale              = 0.0001
      real(sreal)   :: cloud_albedo_offset             = 0.0
      integer(sint) :: cloud_albedo_vmin               = 0
      integer(sint) :: cloud_albedo_vmax               = 11000
      real(sreal)   :: cloud_albedo_uncertainty_scale  = 0.0001
      real(sreal)   :: cloud_albedo_uncertainty_offset = 0.0
      integer(sint) :: cloud_albedo_uncertainty_vmin   = 0
      integer(sint) :: cloud_albedo_uncertainty_vmax   = 11000

      real(sreal)   :: cee_scale              = 0.0001
      real(sreal)   :: cee_offset             = 0.0
      integer(sint) :: cee_vmin               = 0
      integer(sint) :: cee_vmax               = 11000
      real(sreal)   :: cee_uncertainty_scale  = 0.0001
      real(sreal)   :: cee_uncertainty_offset = 0.0
      integer(sint) :: cee_uncertainty_vmin   = 0
      integer(sint) :: cee_uncertainty_vmax   = 11000

      real(sreal)   :: cccot_pre_scale  = 0.001
      real(sreal)   :: cccot_pre_offset = 0.0
      integer(sint) :: cccot_pre_vmin   = -1000.0
      integer(sint) :: cccot_pre_vmax   = 2000.0

      real(sreal)   :: cphcot_scale  = 0.001
      real(sreal)   :: cphcot_offset = 0.0
      integer(sint) :: cphcot_vmin   = -1000.0
      integer(sint) :: cphcot_vmax   = 2000.0

      real(dreal)   :: time_scale  = 1.0
      real(dreal)   :: time_offset = 0.0
      real(dreal)   :: time_vmin   = 0.0
      real(dreal)   :: time_vmax   = 1.0e10

      real(sreal)   :: lat_scale  = 1.0
      real(sreal)   :: lat_offset = 0.0
      real(sreal)   :: lat_vmin   = -90.0
      real(sreal)   :: lat_vmax   = 90.0
      real(sreal)   :: lon_scale  = 1.0
      real(sreal)   :: lon_offset = 0.0
      real(sreal)   :: lon_vmin   = -180.0
      real(sreal)   :: lon_vmax   = 180.0

      real(sreal)   :: sol_scale  = 1.0
      real(sreal)   :: sol_offset = 0.0
      real(sreal)   :: sol_vmin   = -180.0
      real(sreal)   :: sol_vmax   = 180.0
      real(sreal)   :: sat_scale  = 1.0
      real(sreal)   :: sat_offset = 0.0
      real(sreal)   :: sat_vmin   = -180.0
      real(sreal)   :: sat_vmax   = 180.0
      real(sreal)   :: azi_scale  = 1.0
      real(sreal)   :: azi_offset = 0.0
      real(sreal)   :: azi_vmin   = -180.0
      real(sreal)   :: azi_vmax   = 180.0

      integer(byte) :: niter_scale  = 1
      integer(byte) :: niter_offset = 0
      integer(byte) :: niter_vmin   = 0
      integer(byte) :: niter_vmax   = 100

      real(sreal)   :: costja_scale  = 1.0
      real(sreal)   :: costja_offset = 0.0
      real(sreal)   :: costja_vmin   = 0.0
      real(sreal)   :: costja_vmax   = 100000.0
      real(sreal)   :: costjm_scale  = 1.0
      real(sreal)   :: costjm_offset = 0.0
      real(sreal)   :: costjm_vmin   = 0.0
      real(sreal)   :: costjm_vmax   = 100000.0

!      integer(lint) :: qcflag_scale  = 1
!      integer(lint) :: qcflag_offset = 0
      integer(lint) :: qcflag_vmin   = 0
      integer(lint) :: qcflag_vmax   = 32767

      integer(dint) :: channels_used_scale  = 1
      integer(dint) :: channels_used_offset = 0
      integer(dint) :: channels_used_vmin   = 0
      integer(dint) :: channels_used_vmax   = 137438953470_dint

      integer(dint) :: variables_retrieved_scale  = 1
      integer(dint) :: variables_retrieved_offset = 0
      integer(dint) :: variables_retrieved_vmin   = 0
      integer(dint) :: variables_retrieved_vmax   = 137438953470_dint

      integer(byte) :: lsflag_scale  = 1
      integer(byte) :: lsflag_offset = 0
      integer(byte) :: lsflag_vmin   = 0
      integer(byte) :: lsflag_vmax   = 6

      integer(byte) :: lusflag_scale  = 1
      integer(byte) :: lusflag_offset = 0
      integer(byte) :: lusflag_vmin   = 1
      integer(byte) :: lusflag_vmax   = 24

      integer(sint) :: dem_scale  = 1
      integer(sint) :: dem_offset = 0
      integer(sint) :: dem_vmin   = 0
      integer(sint) :: dem_vmax   = 10000

      integer(byte) :: illum_scale  = 1
      integer(byte) :: illum_offset = 0
      integer(byte) :: illum_vmin   = 1
      integer(byte) :: illum_vmax   = 3

      integer(byte) :: cldtype_scale  = 1
      integer(byte) :: cldtype_offset = 0
      integer(byte) :: cldtype_vmin   = 0
      integer(byte) :: cldtype_vmax   = 9

      integer(byte) :: cldmask_scale  = 1
      integer(byte) :: cldmask_offset = 0
      integer(byte) :: cldmask_vmin   = 0
      integer(byte) :: cldmask_vmax   = 1
      real(sreal)   :: cldmask_uncertainty_scale  = 0.01
      real(sreal)   :: cldmask_uncertainty_offset = 0.0
      integer(sint) :: cldmask_uncertainty_vmin   = 0
      integer(sint) :: cldmask_uncertainty_vmax   = 10000

      integer(byte) :: ann_phase_scale  = 1
      integer(byte) :: ann_phase_offset = 0
      integer(byte) :: ann_phase_vmin   = 0
      integer(byte) :: ann_phase_vmax   = 2
      real(sreal)   :: ann_phase_uncertainty_scale  = 0.01
      real(sreal)   :: ann_phase_uncertainty_offset = 0.0
      integer(sint) :: ann_phase_uncertainty_vmin   = 0
      integer(sint) :: ann_phase_uncertainty_vmax   = 10000

      integer(byte) :: phase_scale  = 1
      integer(byte) :: phase_offset = 0
      integer(byte) :: phase_vmin   = 0
      integer(byte) :: phase_vmax   = 127

      integer(byte) :: phase_pavolonis_scale  = 1
      integer(byte) :: phase_pavolonis_offset = 0
      integer(byte) :: phase_pavolonis_vmin   = 0
      integer(byte) :: phase_pavolonis_vmax   = 2

      integer(byte) :: y_id_scale  = 1
      integer(byte) :: y_id_offset = 0
      integer(byte) :: y_id_vmin   = 1
      integer(byte) :: y_id_vmax   = MaxNumMeas

      integer(byte) :: view_id_scale  = 1
      integer(byte) :: view_id_offset = 0
      integer(byte) :: view_id_vmin   = 1
      integer(byte) :: view_id_vmax   = MaxNumViews

      integer(byte) :: ch_is_scale  = 1
      integer(byte) :: ch_is_offset = 0
      integer(byte) :: ch_is_vmin   = 0
      integer(byte) :: ch_is_vmax   = 3

      integer(byte) :: rho_flags_scale  = 1
      integer(byte) :: rho_flags_offset = 0
      integer(byte) :: rho_flags_vmin   = 0
      integer(byte) :: rho_flags_vmax   = 127

      ! Arrays to store output fields
      integer(sint), pointer :: aot550(:,:)
      integer(sint), pointer :: aot550_uncertainty(:,:)
      integer(sint), pointer :: aot870(:,:)
      integer(sint), pointer :: aot870_uncertainty(:,:)
      integer(sint), pointer :: aer(:,:)
      integer(sint), pointer :: aer_uncertainty(:,:)

      integer(sint), pointer :: rho(:,:,:)
      integer(sint), pointer :: rho_uncertainty(:,:,:)

      integer(sint), pointer :: swansea_s(:,:,:)
      integer(sint), pointer :: swansea_s_uncertainty(:,:,:)
      integer(sint), pointer :: swansea_p(:,:,:)
      integer(sint), pointer :: swansea_p_uncertainty(:,:,:)
      integer(sint), pointer :: diffuse_frac(:,:,:)
      integer(sint), pointer :: diffuse_frac_uncertainty(:,:,:)

      integer(sint), pointer :: cot(:,:)
      integer(sint), pointer :: cot_uncertainty(:,:)
      integer(sint), pointer :: cer(:,:)
      integer(sint), pointer :: cer_uncertainty(:,:)
      integer(sint), pointer :: ctp(:,:)
      integer(sint), pointer :: ctp_uncertainty(:,:)
      integer(sint), pointer :: ctp_corrected(:,:)
      integer(sint), pointer :: ctp_corrected_uncertainty(:,:)
      integer(sint), pointer :: cc_total(:,:)
      integer(sint), pointer :: cc_total_uncertainty(:,:)

      integer(sint), pointer :: cot2(:,:)
      integer(sint), pointer :: cot2_uncertainty(:,:)
      integer(sint), pointer :: cer2(:,:)
      integer(sint), pointer :: cer2_uncertainty(:,:)
      integer(sint), pointer :: ctp2(:,:)
      integer(sint), pointer :: ctp2_uncertainty(:,:)
      integer(sint), pointer :: cc_total2(:,:)
      integer(sint), pointer :: cc_total2_uncertainty(:,:)

      integer(sint), pointer :: stemp(:,:)
      integer(sint), pointer :: stemp_uncertainty(:,:)

      integer(sint), pointer :: cth(:,:)
      integer(sint), pointer :: cth_uncertainty(:,:)
      integer(sint), pointer :: cth_corrected(:,:)
      integer(sint), pointer :: cth_corrected_uncertainty(:,:)
      integer(sint), pointer :: ctt(:,:)
      integer(sint), pointer :: ctt_uncertainty(:,:)
      integer(sint), pointer :: ctt_corrected(:,:)
      integer(sint), pointer :: ctt_corrected_uncertainty(:,:)
      integer(sint), pointer :: cwp(:,:)
      integer(sint), pointer :: cwp_uncertainty(:,:)

      integer(sint), pointer :: cth2(:,:)
      integer(sint), pointer :: cth2_uncertainty(:,:)
      integer(sint), pointer :: ctt2(:,:)
      integer(sint), pointer :: ctt2_uncertainty(:,:)
      integer(sint), pointer :: cwp2(:,:)
      integer(sint), pointer :: cwp2_uncertainty(:,:)

      integer(sint), pointer :: cloud_albedo(:,:,:)
      integer(sint), pointer :: cloud_albedo_uncertainty(:,:,:)
      integer(sint), pointer :: cee(:,:,:)
      integer(sint), pointer :: cee_uncertainty(:,:,:)

      integer(sint), pointer :: cccot_pre(:,:,:)

      real(dreal),   pointer :: time(:,:)
      real(sreal),   pointer :: lat(:,:)
      real(sreal),   pointer :: lon(:,:)
      real(sreal),   pointer :: sol_zen(:,:,:)
      real(sreal),   pointer :: sat_zen(:,:,:)
      real(sreal),   pointer :: rel_azi(:,:,:)
      real(sreal),   pointer :: sat_azi(:,:,:)

      integer(byte), pointer :: niter(:,:)
      real(sreal),   pointer :: costja(:,:)
      real(sreal),   pointer :: costjm(:,:)
      integer(lint), pointer :: qcflag(:,:)
      integer(dint), pointer :: channels_used(:,:)
      integer(dint), pointer :: variables_retrieved(:,:)
      character(len=attribute_length_long) :: qc_flag_masks
      character(len=attribute_length_long) :: qc_flag_meanings
      character(len=attribute_length_long) :: ch_flag_masks
      character(len=attribute_length_long) :: ch_flag_meanings
      character(len=attribute_length_long) :: vr_flag_masks
      character(len=attribute_length_long) :: vr_flag_meanings

      integer(byte), pointer :: lsflag(:,:)
      integer(byte), pointer :: lusflag(:,:)
      integer(sint), pointer :: dem(:,:)

      integer(byte), pointer :: illum(:,:)

      integer(byte), pointer :: cldtype(:,:,:)
      integer(byte), pointer :: cldmask(:,:,:)
      integer(sint), pointer :: cldmask_uncertainty(:,:,:)

      integer(byte), pointer :: ann_phase(:,:,:)
      integer(sint), pointer :: ann_phase_uncertainty(:,:,:)
      integer(sint), pointer :: cphcot(:,:,:)

      integer(byte), pointer :: phase(:,:)
      integer(byte), pointer :: phase_pavolonis(:,:)

      integer(byte), pointer :: y_id(:)
      integer(byte), pointer :: view_id(:)
      integer(byte), pointer :: ch_is(:)
      integer(byte), pointer :: rho_flags(:)
   end type output_data_primary_t


   type output_data_secondary_t
      ! Variable IDs for data fields in primary output file
      integer          :: vid_aot550_ap
      integer          :: vid_aot550_fg
      integer          :: vid_aer_ap
      integer          :: vid_aer_fg

      integer, pointer :: vid_rho_ap(:)
      integer, pointer :: vid_rho_fg(:)
      integer, pointer :: vid_swansea_s_ap(:)
      integer, pointer :: vid_swansea_s_fg(:)
      integer, pointer :: vid_swansea_p_ap(:)
      integer, pointer :: vid_swansea_p_fg(:)

      integer          :: vid_cot_ap
      integer          :: vid_cot_fg
      integer          :: vid_cer_ap
      integer          :: vid_cer_fg
      integer          :: vid_ctp_ap
      integer          :: vid_ctp_fg
      integer          :: vid_cot2_ap
      integer          :: vid_cot2_fg
      integer          :: vid_cer2_ap
      integer          :: vid_cer2_fg
      integer          :: vid_ctp2_ap
      integer          :: vid_ctp2_fg
      integer          :: vid_stemp_ap
      integer          :: vid_stemp_fg

      integer, pointer :: vid_albedo(:)

      integer          :: vid_scanline_u
      integer          :: vid_scanline_v

      integer, pointer :: vid_channels(:)
      integer, pointer :: vid_y0(:)
      integer, pointer :: vid_residuals(:)

      integer          :: vid_ds
      integer, pointer :: vid_covariance(:,:)

      ! Scale, offset, valid min/max for output fields
      real(sreal)    :: aot550_ap_scale  = 0.001
      real(sreal)    :: aot550_ap_offset = 0.0
      integer(sint)  :: aot550_ap_vmin   = 0
      integer(sint)  :: aot550_ap_vmax   = 32000
      real(sreal)    :: aot550_fg_scale  = 0.001
      real(sreal)    :: aot550_fg_offset = 0.0
      integer(sint)  :: aot550_fg_vmin   = 0
      integer(sint)  :: aot550_fg_vmax   = 32000

      real(sreal)    :: aer_ap_scale  = 0.001
      real(sreal)    :: aer_ap_offset = 0.0
      integer(sint)  :: aer_ap_vmin   = 0
      integer(sint)  :: aer_ap_vmax   = 32000
      real(sreal)    :: aer_fg_scale  = 0.001
      real(sreal)    :: aer_fg_offset = 0.0
      integer(sint)  :: aer_fg_vmin   = 0
      integer(sint)  :: aer_fg_vmax   = 32000

      real(sreal)    :: rho_ap_scale  = 0.000
      real(sreal)    :: rho_ap_offset = 0.0
      integer(sint)  :: rho_ap_vmin   = 0
      integer(sint)  :: rho_ap_vmax   = 32000
      real(sreal)    :: rho_fg_scale  = 0.001
      real(sreal)    :: rho_fg_offset = 0.0
      integer(sint)  :: rho_fg_vmin   = 0
      integer(sint)  :: rho_fg_vmax   = 32000

      real(sreal)    :: swansea_s_ap_scale  = 0.0001
      real(sreal)    :: swansea_s_ap_offset = 0.0
      integer(sint)  :: swansea_s_ap_vmin   = 0
      integer(sint)  :: swansea_s_ap_vmax   = 32000
      real(sreal)    :: swansea_s_fg_scale  = 0.0001
      real(sreal)    :: swansea_s_fg_offset = 0.0
      integer(sint)  :: swansea_s_fg_vmin   = 0
      integer(sint)  :: swansea_s_fg_vmax   = 32000

      real(sreal)    :: swansea_p_ap_scale  = 0.0001
      real(sreal)    :: swansea_p_ap_offset = 0.0
      integer(sint)  :: swansea_p_ap_vmin   = 0
      integer(sint)  :: swansea_p_ap_vmax   = 32000
      real(sreal)    :: swansea_p_fg_scale  = 0.0001
      real(sreal)    :: swansea_p_fg_offset = 0.0
      integer(sint)  :: swansea_p_fg_vmin   = 0
      integer(sint)  :: swansea_p_fg_vmax   = 32000

      real(sreal)    :: cot_ap_scale  = 0.01
      real(sreal)    :: cot_ap_offset = 0.0
      integer(sint)  :: cot_ap_vmin   = 0
      integer(sint)  :: cot_ap_vmax   = 32000
      real(sreal)    :: cot_fg_scale  = 0.01
      real(sreal)    :: cot_fg_offset = 0.0
      integer(sint)  :: cot_fg_vmin   = 0
      integer(sint)  :: cot_fg_vmax   = 32000

      real(sreal)    :: cer_ap_scale  = 0.01
      real(sreal)    :: cer_ap_offset = 0.0
      integer(sint)  :: cer_ap_vmin   = 0
      integer(sint)  :: cer_ap_vmax   = 20000
      real(sreal)    :: cer_fg_scale  = 0.01
      real(sreal)    :: cer_fg_offset = 0.0
      integer(sint)  :: cer_fg_vmin   = 0
      integer(sint)  :: cer_fg_vmax   = 20000

      real(sreal)    :: ctp_ap_scale  = 0.1
      real(sreal)    :: ctp_ap_offset = 0.0
      integer(sint)  :: ctp_ap_vmin   = 500
      integer(sint)  :: ctp_ap_vmax   = 12000
      real(sreal)    :: ctp_fg_scale  = 0.1
      real(sreal)    :: ctp_fg_offset = 0.0
      integer(sint)  :: ctp_fg_vmin   = 500
      integer(sint)  :: ctp_fg_vmax   = 12000

      real(sreal)    :: stemp_ap_scale  = 0.01
      real(sreal)    :: stemp_ap_offset = 100.0
      integer(sint)  :: stemp_ap_vmin   = 0
      integer(sint)  :: stemp_ap_vmax   = 32000
      real(sreal)    :: stemp_fg_scale  = 0.01
      real(sreal)    :: stemp_fg_offset = 100.0
      integer(sint)  :: stemp_fg_vmin   = 0
      integer(sint)  :: stemp_fg_vmax   = 32000

      real(sreal)    :: albedo_scale  = 0.0001
      real(sreal)    :: albedo_offset = 0.0
      integer(sint)  :: albedo_vmin   = 0
      integer(sint)  :: albedo_vmax   = 10000

      integer(lint)  :: scanline_u_scale  = 1
      integer(lint)  :: scanline_u_offset = 0
      integer(lint)  :: scanline_u_vmin   = 1
      integer(lint)  :: scanline_u_vmax
      integer(lint)  :: scanline_v_scale  = 1
      integer(lint)  :: scanline_v_offset = 0
      integer(lint)  :: scanline_v_vmin   = 1
      integer(lint)  :: scanline_v_vmax

      real(sreal),   pointer :: channels_scale(:)
      real(sreal),   pointer :: channels_offset(:)
      integer(sint), pointer :: channels_vmin(:)
      integer(sint), pointer :: channels_vmax(:)

      real(sreal),   pointer :: y0_scale(:)
      real(sreal),   pointer :: y0_offset(:)
      integer(sint), pointer :: y0_vmin(:)
      integer(sint), pointer :: y0_vmax(:)

      real(sreal),   pointer :: residuals_scale(:)
      real(sreal),   pointer :: residuals_offset(:)
      integer(sint), pointer :: residuals_vmin(:)
      integer(sint), pointer :: residuals_vmax(:)

      real(sreal)    :: ds_scale  = 0.001
      real(sreal)    :: ds_offset = 0.0
      integer(sint)  :: ds_vmin   = 0
      integer(sint)  :: ds_vmax   = 10000

      real(sreal)    :: covariance_scale  = 0.001
      real(sreal)    :: covariance_offset = 0.0
      integer(sint)  :: covariance_vmax   = -32000
      integer(sint)  :: covariance_vmin   = 32000

      ! Arrays to store output fields
      integer(sint), pointer :: aot550_ap(:,:)
      integer(sint), pointer :: aot550_fg(:,:)
      integer(sint), pointer :: aer_ap(:,:)
      integer(sint), pointer :: aer_fg(:,:)

      integer(sint), pointer :: rho_ap(:,:,:)
      integer(sint), pointer :: rho_fg(:,:,:)

      integer(sint), pointer :: swansea_s_ap(:,:,:)
      integer(sint), pointer :: swansea_s_fg(:,:,:)
      integer(sint), pointer :: swansea_p_ap(:,:,:)
      integer(sint), pointer :: swansea_p_fg(:,:,:)

      integer(sint), pointer :: cot_ap(:,:)
      integer(sint), pointer :: cot_fg(:,:)
      integer(sint), pointer :: cer_ap(:,:)
      integer(sint), pointer :: cer_fg(:,:)
      integer(sint), pointer :: ctp_ap(:,:)
      integer(sint), pointer :: ctp_fg(:,:)
      integer(sint), pointer :: cot2_ap(:,:)
      integer(sint), pointer :: cot2_fg(:,:)
      integer(sint), pointer :: cer2_ap(:,:)
      integer(sint), pointer :: cer2_fg(:,:)
      integer(sint), pointer :: ctp2_ap(:,:)
      integer(sint), pointer :: ctp2_fg(:,:)
      integer(sint), pointer :: stemp_fg(:,:)
      integer(sint), pointer :: stemp_ap(:,:)

      integer(sint), pointer :: albedo(:,:,:)

      integer(sint), pointer :: channels(:,:,:)
      integer(sint), pointer :: y0(:,:,:)
      integer(sint), pointer :: residuals(:,:,:)

      integer(sint), pointer :: ds(:,:)

      integer(lint), pointer :: scanline_u(:,:)
      integer(lint), pointer :: scanline_v(:,:)

      real(sreal),   pointer :: covariance(:,:,:,:)
   end type output_data_secondary_t

contains


#include "alloc_output_data.F90"
#include "dealloc_output_data.F90"

#include "def_output_primary.F90"
#include "def_output_secondary.F90"

#include "write_output_primary.F90"
#include "write_output_secondary.F90"


end module orac_output_m
