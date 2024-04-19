!-------------------------------------------------------------------------------
! Name: alloc_output_data.F90
!
! Purpose:
! The file contains a collection of two subroutines which allocate the array
! parts of the output variable types, stored within module output_routines.
!
! History:
! 2011/12/19, MJ: creates initial file.
! 2012/01/05, CP: added in channel information
! 2012/01/06, CP: added in cwp
! 2012/01/15, CP: added in chan definitions
! 2012/01/28, CP: added in albedo
! 2012/07/08, CP: fixed memory access error
! 2013/01/17, MJ: Adds code to accommodate uncertainties of ctt and cth.
! 2013/01/23, CP: Changed illum from byte to int
! 2013/10/02, CP/GT: Added allocation statement for DOFS
! 2014/01/01, GM: Fixed the range in NY for initializations. Plus, no need for
!    explicit indexing in these cases anyway.
! 2014/05/27, GM: Some cleanup.
! 2014/10/24, OS: added allocation of cldtype, cldmask, cccot_pre, lusflag, dem,
!    nisemask
! 2014/12/01, CP: Added in cloud albedo
! 2015/07/01, CP: Added corrected cth
! 2015/09/06, GM: Move into common/ from src/ and changes related to sharing
!    with post_processing/.
! 2015/09/07, GM: Add cldmask_uncertainty.
! 2015/10/22, GM: Add cloud albedo uncertainty.
! 2015/12/28, AP: Add output fields for aerosol retrievals.
! 2015/12/30, AP: Move declarations of scale/offset/vmin/vmax here from def_
!    routines for fields that could be BTs or reflectances. Have all albedo
!    fields use the same values.
! 2016/01/06, AP: Wrap do_* flags into output_flags structure. Pass logical
!    array to identify thermal channels rather than dealing with Ch_Is.
! 2016/01/27, GM: Add cee and cee_uncertainty.
! 2016/01/28, GM: Add ctp and ctt corrected and corrected_uncertainty.
! 2016/03/02, AP: Homogenisation of I/O modules.
! 2016/04/28, AP: Add multiple views.
! 2016/07/08, GM: Add fields for cloud layer 2.
! 2017/05/17, OS: Added ann phase variables.
! 2017/07/05, AP: Add channels_used, variables_retrieved. New QC.
! 2018/06/08, SP: Add satellite azimuth angle to output.
! 2023/10/10, GT: Added optional measurement uncertainties to secondary output
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! Name: alloc_output_data_primary
!
! Purpose:
! Allocate storage for primary output file.
!
! Description and Algorithm details:
! 1) Allocate all arrays, writing fill values to them.
!
! Arguments:
! Name        Type   In/Out/Both Description
! ------------------------------------------------------------------------------
! ind         struct In          Channel indexing information
! MaxIter     int    In          Maximum number of allowed iterations
! output_data struct Both        Structure of arrays to be allocated
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------
subroutine alloc_output_data_primary(ind, MaxIter, data)

   implicit none

   type(common_indices_t),      intent(in)    :: ind
   integer,                     intent(in)    :: MaxIter
   type(output_data_primary_t), intent(inout) :: data

   if (ind%flags%do_aerosol) then
      allocate(data%aot550(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%aot550 = sint_fill_value
      allocate(data%aot550_uncertainty(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%aot550_uncertainty = sint_fill_value

      allocate(data%aot870(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%aot870 = sint_fill_value
      allocate(data%aot870_uncertainty(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%aot870_uncertainty = sint_fill_value

      allocate(data%aer(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%aer = sint_fill_value
      allocate(data%aer_uncertainty(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%aer_uncertainty = sint_fill_value
   else
      nullify(data%aot550)
      nullify(data%aot550_uncertainty)
      nullify(data%aot870)
      nullify(data%aot870_uncertainty)
      nullify(data%aer)
      nullify(data%aer_uncertainty)
   end if

   if (ind%flags%do_rho) then
      allocate(data%vid_rho(ind%Nrho))
      data%vid_rho = 0
      allocate(data%vid_rho_uncertainty(ind%Nrho))
      data%vid_rho_uncertainty = 0

      allocate(data%rho(ind%X0:ind%X1, ind%Y0:ind%Y1, ind%Nrho))
      data%rho = sint_fill_value
      allocate(data%rho_uncertainty(ind%X0:ind%X1, ind%Y0:ind%Y1, ind%Nrho))
      data%rho_uncertainty = sint_fill_value
   else
      nullify(data%vid_rho)
      nullify(data%vid_rho_uncertainty)
      nullify(data%rho)
      nullify(data%rho_uncertainty)
   end if

   if (ind%flags%do_swansea) then
      allocate(data%vid_swansea_s(ind%Nss))
      data%vid_swansea_s = 0
      allocate(data%vid_swansea_s_uncertainty(ind%Nss))
      data%vid_swansea_s_uncertainty = 0

      allocate(data%vid_swansea_p(ind%NViews))
      data%vid_swansea_p = 0
      allocate(data%vid_swansea_p_uncertainty(ind%NViews))
      data%vid_swansea_p_uncertainty = 0

      allocate(data%vid_diffuse_frac(ind%Nss))
      data%vid_diffuse_frac = 0
      allocate(data%vid_diffuse_frac_uncertainty(ind%Nss))
      data%vid_diffuse_frac_uncertainty = 0

      allocate(data%swansea_s(ind%X0:ind%X1, ind%Y0:ind%Y1, ind%Nss))
      data%swansea_s = sint_fill_value
      allocate(data%swansea_s_uncertainty(ind%X0:ind%X1, ind%Y0:ind%Y1, ind%Nss))
      data%swansea_s_uncertainty = sint_fill_value

      allocate(data%swansea_p(ind%X0:ind%X1, ind%Y0:ind%Y1, ind%NViews))
      data%swansea_p = sint_fill_value
      allocate(data%swansea_p_uncertainty(ind%X0:ind%X1, ind%Y0:ind%Y1, &
           ind%NViews))
      data%swansea_p_uncertainty = sint_fill_value

      allocate(data%diffuse_frac(ind%X0:ind%X1, ind%Y0:ind%Y1, ind%Nss))
      data%diffuse_frac = sint_fill_value
      allocate(data%diffuse_frac_uncertainty(ind%X0:ind%X1, ind%Y0:ind%Y1, &
           ind%Nss))
      data%diffuse_frac_uncertainty = sint_fill_value
   else
      nullify(data%vid_swansea_s)
      nullify(data%vid_swansea_s_uncertainty)
      nullify(data%vid_swansea_p)
      nullify(data%vid_swansea_p_uncertainty)
      nullify(data%vid_diffuse_frac)
      nullify(data%vid_diffuse_frac_uncertainty)
      nullify(data%swansea_s)
      nullify(data%swansea_s_uncertainty)
      nullify(data%swansea_p)
      nullify(data%swansea_p_uncertainty)
      nullify(data%diffuse_frac)
      nullify(data%diffuse_frac_uncertainty)
   end if

   if (ind%flags%do_cloud) then
      allocate(data%vid_cloud_albedo(ind%Nalb))
      data%vid_cloud_albedo = 0
      allocate(data%vid_cloud_albedo_uncertainty(ind%Nalb))
      data%vid_cloud_albedo_uncertainty = 0

      allocate(data%vid_cee(ind%Ncee))
      data%vid_cee = 0
      allocate(data%vid_cee_uncertainty(ind%Ncee))
      data%vid_cee_uncertainty = 0

      allocate(data%cot(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%cot = sint_fill_value
      allocate(data%cot_uncertainty(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%cot_uncertainty = sint_fill_value

      allocate(data%cer(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%cer = sint_fill_value
      allocate(data%cer_uncertainty(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%cer_uncertainty = sint_fill_value

      allocate(data%ctp(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%ctp = sint_fill_value
      allocate(data%ctp_uncertainty(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%ctp_uncertainty = sint_fill_value

      allocate(data%ctp_corrected(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%ctp_corrected = sint_fill_value
      allocate(data%ctp_corrected_uncertainty(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%ctp_corrected_uncertainty = sint_fill_value

      allocate(data%cc_total(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%cc_total = sint_fill_value
      allocate(data%cc_total_uncertainty(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%cc_total_uncertainty = sint_fill_value

      allocate(data%stemp(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%stemp = sint_fill_value
      allocate(data%stemp_uncertainty(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%stemp_uncertainty = sint_fill_value

      allocate(data%cth(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%cth = sint_fill_value
      allocate(data%cth_uncertainty(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%cth_uncertainty = sint_fill_value

      allocate(data%cth_corrected(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%cth_corrected = sint_fill_value
      allocate(data%cth_corrected_uncertainty(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%cth_corrected_uncertainty = sint_fill_value

      allocate(data%ctt(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%ctt = sint_fill_value
      allocate(data%ctt_uncertainty(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%ctt_uncertainty = sint_fill_value

      allocate(data%ctt_corrected(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%ctt_corrected = sint_fill_value
      allocate(data%ctt_corrected_uncertainty(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%ctt_corrected_uncertainty = sint_fill_value

      allocate(data%cwp(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%cwp = sint_fill_value
      allocate(data%cwp_uncertainty(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%cwp_uncertainty = sint_fill_value

      allocate(data%cloud_albedo(ind%X0:ind%X1, ind%Y0:ind%Y1, ind%NSolar))
      data%cloud_albedo = sint_fill_value
      allocate(data%cloud_albedo_uncertainty(ind%X0:ind%X1, ind%Y0:ind%Y1, &
           ind%NSolar))
      data%cloud_albedo_uncertainty = sint_fill_value

      allocate(data%cee(ind%X0:ind%X1, ind%Y0:ind%Y1, ind%NThermal))
      data%cee = sint_fill_value
      allocate(data%cee_uncertainty(ind%X0:ind%X1, ind%Y0:ind%Y1, ind%NThermal))
      data%cee_uncertainty = sint_fill_value

      allocate(data%cccot_pre(ind%X0:ind%X1, ind%Y0:ind%Y1, ind%NViews))
      data%cccot_pre = sint_fill_value

   else
      nullify(data%vid_cloud_albedo)
      nullify(data%vid_cloud_albedo_uncertainty)
      nullify(data%vid_cee)
      nullify(data%vid_cee_uncertainty)
      nullify(data%cot)
      nullify(data%cot_uncertainty)
      nullify(data%cer)
      nullify(data%cer_uncertainty)
      nullify(data%ctp)
      nullify(data%ctp_uncertainty)
      nullify(data%ctp_corrected)
      nullify(data%ctp_corrected_uncertainty)
      nullify(data%cc_total)
      nullify(data%cc_total_uncertainty)
      nullify(data%stemp)
      nullify(data%stemp_uncertainty)
      nullify(data%cth)
      nullify(data%cth_uncertainty)
      nullify(data%cth_corrected)
      nullify(data%cth_corrected_uncertainty)
      nullify(data%ctt)
      nullify(data%ctt_uncertainty)
      nullify(data%ctt_corrected)
      nullify(data%ctt_corrected_uncertainty)
      nullify(data%cwp)
      nullify(data%cwp_uncertainty)
      nullify(data%cloud_albedo)
      nullify(data%cloud_albedo_uncertainty)
      nullify(data%cee)
      nullify(data%cee_uncertainty)
      nullify(data%cphcot)
      nullify(data%cccot_pre)
   end if

   if (ind%flags%do_cloud_layer_2) then
      allocate(data%cot2(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%cot2 = sint_fill_value
      allocate(data%cot2_uncertainty(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%cot2_uncertainty = sint_fill_value

      allocate(data%cer2(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%cer2 = sint_fill_value
      allocate(data%cer2_uncertainty(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%cer2_uncertainty = sint_fill_value

      allocate(data%ctp2(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%ctp2 = sint_fill_value
      allocate(data%ctp2_uncertainty(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%ctp2_uncertainty = sint_fill_value

      allocate(data%cc_total2(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%cc_total2 = sint_fill_value
      allocate(data%cc_total2_uncertainty(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%cc_total2_uncertainty = sint_fill_value

      allocate(data%cth2(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%cth2 = sint_fill_value
      allocate(data%cth2_uncertainty(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%cth2_uncertainty = sint_fill_value

      allocate(data%ctt2(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%ctt2 = sint_fill_value
      allocate(data%ctt2_uncertainty(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%ctt2_uncertainty = sint_fill_value

      allocate(data%cwp2(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%cwp2 = sint_fill_value
      allocate(data%cwp2_uncertainty(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%cwp2_uncertainty = sint_fill_value
   else
      nullify(data%cot2)
      nullify(data%cot2_uncertainty)
      nullify(data%cer2)
      nullify(data%cer2_uncertainty)
      nullify(data%ctp2)
      nullify(data%ctp2_uncertainty)
      nullify(data%cc_total2)
      nullify(data%cc_total2_uncertainty)
      nullify(data%cth2)
      nullify(data%cth2_uncertainty)
      nullify(data%ctt2)
      nullify(data%ctt2_uncertainty)
      nullify(data%cwp2)
      nullify(data%cwp2_uncertainty)
   end if

   if (ind%flags%do_ann_phase) then
      allocate(data%ann_phase(ind%X0:ind%X1, ind%Y0:ind%Y1, ind%NViews))
      data%ann_phase = byte_fill_value
      allocate(data%cphcot(ind%X0:ind%X1, ind%Y0:ind%Y1, ind%NViews))
      data%cphcot = sint_fill_value
   else
      nullify(data%ann_phase)
      nullify(data%cphcot)
   end if
   if (ind%flags%do_ann_phase_uncertainty) then
      allocate(data%ann_phase_uncertainty(ind%X0:ind%X1, ind%Y0:ind%Y1, &
           ind%NViews))
      data%ann_phase_uncertainty = sint_fill_value
   else
      nullify(data%ann_phase_uncertainty)
   end if

   allocate(data%niter(ind%X0:ind%X1, ind%Y0:ind%Y1))
   data%niter = byte_fill_value

   allocate(data%costja(ind%X0:ind%X1, ind%Y0:ind%Y1))
   data%costja = sreal_fill_value
   allocate(data%costjm(ind%X0:ind%X1, ind%Y0:ind%Y1))
   data%costjm = sreal_fill_value

   allocate(data%qcflag(ind%X0:ind%X1, ind%Y0:ind%Y1))
   data%qcflag = sint_fill_value
   allocate(data%channels_used(ind%X0:ind%X1, ind%Y0:ind%Y1))
   data%channels_used = 0
   allocate(data%variables_retrieved(ind%X0:ind%X1, ind%Y0:ind%Y1))
   data%variables_retrieved = 0

   allocate(data%time(ind%X0:ind%X1, ind%Y0:ind%Y1))
   data%time = dreal_fill_value

   allocate(data%lat(ind%X0:ind%X1, ind%Y0:ind%Y1))
   data%lat = sreal_fill_value
   allocate(data%lon(ind%X0:ind%X1, ind%Y0:ind%Y1))
   data%lon = sreal_fill_value

   allocate(data%vid_sol_zen(ind%NViews))
   data%vid_sol_zen = 0
   allocate(data%vid_sat_zen(ind%NViews))
   data%vid_sat_zen = 0
   allocate(data%vid_rel_azi(ind%NViews))
   data%vid_rel_azi = 0
   allocate(data%vid_sat_azi(ind%NViews))
   data%vid_sat_azi = 0
   allocate(data%sol_zen(ind%X0:ind%X1, ind%Y0:ind%Y1, ind%NViews))
   data%sol_zen = sreal_fill_value
   allocate(data%sat_zen(ind%X0:ind%X1, ind%Y0:ind%Y1, ind%NViews))
   data%sat_zen = sreal_fill_value
   allocate(data%rel_azi(ind%X0:ind%X1, ind%Y0:ind%Y1, ind%NViews))
   data%rel_azi = sreal_fill_value
   allocate(data%sat_azi(ind%X0:ind%X1, ind%Y0:ind%Y1, ind%NViews))
   data%sat_azi = sreal_fill_value

   allocate(data%lsflag(ind%X0:ind%X1, ind%Y0:ind%Y1))
   data%lsflag = byte_fill_value
   allocate(data%lusflag(ind%X0:ind%X1, ind%Y0:ind%Y1))
   data%lusflag = byte_fill_value
   allocate(data%dem(ind%X0:ind%X1, ind%Y0:ind%Y1))
   data%dem = sint_fill_value

   allocate(data%illum(ind%X0:ind%X1, ind%Y0:ind%Y1))
   data%illum = byte_fill_value

   allocate(data%cldtype(ind%X0:ind%X1, ind%Y0:ind%Y1, ind%NViews))
   data%cldtype = byte_fill_value

   if (ind%flags%do_cldmask) then
      allocate(data%cldmask(ind%X0:ind%X1, ind%Y0:ind%Y1, ind%NViews))
      data%cldmask = byte_fill_value
   else
      nullify(data%cldmask)
   end if
   if (ind%flags%do_cldmask_uncertainty) then
      allocate(data%cldmask_uncertainty(ind%X0:ind%X1, ind%Y0:ind%Y1, &
           ind%NViews))
      data%cldmask_uncertainty = sint_fill_value
   else
      nullify(data%cldmask_uncertainty)
   end if

   if (ind%flags%do_phase) then
      allocate(data%phase(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%phase = byte_fill_value
   else
      nullify(data%phase)
   end if
   if (ind%flags%do_phase_pavolonis) then
      allocate(data%phase_pavolonis(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%phase_pavolonis = byte_fill_value
   else
      nullify(data%phase_pavolonis)
   end if

   if (ind%flags%do_indexing) then
      allocate(data%y_id(ind%Ny))
      data%y_id = byte_fill_value
      allocate(data%view_id(ind%NViews))
      data%view_id = byte_fill_value
      allocate(data%ch_is(ind%Ny))
      data%ch_is = byte_fill_value
      allocate(data%rho_flags(ind%Ny))
      data%rho_flags = byte_fill_value
   else
      nullify(data%y_id)
      nullify(data%view_id)
      nullify(data%ch_is)
   end if

   ! Set scale/offset/vmin/vmax dynamically as required
   data%niter_vmax = MaxIter

end subroutine alloc_output_data_primary


!-------------------------------------------------------------------------------
! Name: alloc_output_data_secondary
!
! Purpose:
! Allocate storage for primary output file.
!
! Description and Algorithm details:
! 1) Allocate all arrays, writing fill values to them.
!
! Arguments:
! Name        Type   In/Out/Both Description
! ------------------------------------------------------------------------------
! ind         struct In          Channel indexing information
! output_data struct Both        Structure of arrays to be allocated
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------
subroutine alloc_output_data_secondary(ind, data)

   implicit none

   type(common_indices_t),         intent(in)    :: ind
   type(output_data_secondary_t),  intent(inout) :: data

   integer :: i

   if (ind%flags%do_aerosol) then
      allocate(data%aot550_ap(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%aot550_ap = sint_fill_value
      allocate(data%aot550_fg(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%aot550_fg = sint_fill_value

      allocate(data%aer_ap(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%aer_ap = sint_fill_value
      allocate(data%aer_fg(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%aer_fg = sint_fill_value
   else
      nullify(data%aot550_ap)
      nullify(data%aot550_fg)
      nullify(data%aer_ap)
      nullify(data%aer_fg)
   end if

   if (ind%flags%do_rho) then
      allocate(data%vid_rho_ap(ind%Nrho))
      data%vid_rho_ap = 0
      allocate(data%vid_rho_fg(ind%Nrho))
      data%vid_rho_fg = 0

      allocate(data%rho_ap(ind%X0:ind%X1, ind%Y0:ind%Y1, ind%Nrho))
      data%rho_ap = sint_fill_value
      allocate(data%rho_fg(ind%X0:ind%X1, ind%Y0:ind%Y1, ind%Nrho))
      data%rho_fg = sint_fill_value
   else
      nullify(data%vid_rho_ap)
      nullify(data%vid_rho_fg)
      nullify(data%rho_ap)
      nullify(data%rho_fg)
   end if

   if (ind%flags%do_swansea) then
      allocate(data%vid_swansea_s_ap(ind%Nss))
      data%vid_swansea_s_ap = 0
      allocate(data%vid_swansea_s_fg(ind%Nss))
      data%vid_swansea_s_fg = 0

      allocate(data%vid_swansea_p_ap(ind%NViews))
      data%vid_swansea_p_ap = 0
      allocate(data%vid_swansea_p_fg(ind%NViews))
      data%vid_swansea_p_fg = 0

      allocate(data%swansea_s_ap(ind%X0:ind%X1, ind%Y0:ind%Y1, ind%Nss))
      data%swansea_s_ap = sint_fill_value
      allocate(data%swansea_s_fg(ind%X0:ind%X1, ind%Y0:ind%Y1, ind%Nss))
      data%swansea_s_fg = sint_fill_value

      allocate(data%swansea_p_ap(ind%X0:ind%X1, ind%Y0:ind%Y1, ind%NViews))
      data%swansea_p_ap = sint_fill_value
      allocate(data%swansea_p_fg(ind%X0:ind%X1, ind%Y0:ind%Y1, ind%NViews))
      data%swansea_p_fg = sint_fill_value
   else
      nullify(data%vid_swansea_s_ap)
      nullify(data%vid_swansea_s_fg)
      nullify(data%vid_swansea_p_ap)
      nullify(data%vid_swansea_p_fg)
      nullify(data%swansea_s_ap)
      nullify(data%swansea_s_fg)
      nullify(data%swansea_p_ap)
      nullify(data%swansea_p_fg)
   end if

   if (ind%flags%do_cloud) then
      allocate(data%cot_ap(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%cot_ap = sint_fill_value
      allocate(data%cot_fg(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%cot_fg = sint_fill_value

      allocate(data%cer_ap(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%cer_ap = sint_fill_value
      allocate(data%cer_fg(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%cer_fg = sint_fill_value

      allocate(data%ctp_ap(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%ctp_ap = sint_fill_value
      allocate(data%ctp_fg(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%ctp_fg = sint_fill_value

      allocate(data%stemp_fg(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%stemp_fg = sint_fill_value
      allocate(data%stemp_ap(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%stemp_ap = sint_fill_value

      allocate(data%vid_albedo(ind%NSolar))
      data%vid_albedo = 0
      allocate(data%albedo(ind%X0:ind%X1, ind%Y0:ind%Y1, ind%NSolar))
      data%albedo = sint_fill_value
   else
      nullify(data%cot_ap)
      nullify(data%cot_fg)
      nullify(data%cer_ap)
      nullify(data%cer_fg)
      nullify(data%ctp_ap)
      nullify(data%ctp_fg)
      nullify(data%stemp_fg)
      nullify(data%stemp_ap)
      nullify(data%vid_albedo)
      nullify(data%albedo)
   end if

   if (ind%flags%do_cloud_layer_2) then
      allocate(data%cot2_ap(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%cot2_ap = sint_fill_value
      allocate(data%cot2_fg(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%cot2_fg = sint_fill_value

      allocate(data%cer2_ap(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%cer2_ap = sint_fill_value
      allocate(data%cer2_fg(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%cer2_fg = sint_fill_value

      allocate(data%ctp2_ap(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%ctp2_ap = sint_fill_value
      allocate(data%ctp2_fg(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%ctp2_fg = sint_fill_value
   else
      nullify(data%cot2_ap)
      nullify(data%cot2_fg)
      nullify(data%cer2_ap)
      nullify(data%cer2_fg)
      nullify(data%ctp2_ap)
      nullify(data%ctp2_fg)
   end if

   allocate(data%scanline_u(ind%X0:ind%X1, ind%Y0:ind%Y1))
   data%scanline_u = lint_fill_value
   allocate(data%scanline_v(ind%X0:ind%X1, ind%Y0:ind%Y1))
   data%scanline_v = lint_fill_value

   allocate(data%vid_channels(ind%Ny))
   data%vid_channels = 0
   allocate(data%channels_scale(ind%Ny))
   data%channels_scale = sreal_fill_value
   allocate(data%channels_offset(ind%Ny))
   data%channels_offset = sreal_fill_value
   allocate(data%channels_vmin(ind%Ny))
   data%channels_vmin = sint_fill_value
   allocate(data%channels_vmax(ind%Ny))
   data%channels_vmax = sint_fill_value
   allocate(data%channels(ind%X0:ind%X1, ind%Y0:ind%Y1, ind%Ny))
   data%channels = sint_fill_value

   if (ind%flags%do_meas_error) then
      allocate(data%vid_Sy(ind%Ny))
      data%vid_Sy = 0
      allocate(data%Sy_scale(ind%Ny))
      data%Sy_scale = sreal_fill_value
      allocate(data%Sy_offset(ind%Ny))
      data%Sy_offset = sreal_fill_value
      allocate(data%Sy_vmin(ind%Ny))
      data%Sy_vmin = sint_fill_value
      allocate(data%Sy_vmax(ind%Ny))
      data%Sy_vmax = sint_fill_value
      allocate(data%Sy(ind%X0:ind%X1, ind%Y0:ind%Y1, ind%Ny))
      data%Sy = sint_fill_value
   else
      nullify(data%vid_Sy)
      nullify(data%Sy_scale)
      nullify(data%Sy_offset)
      nullify(data%Sy_vmin)
      nullify(data%Sy_vmax)
      nullify(data%Sy)
   end if

   allocate(data%vid_y0(ind%Ny))
   data%vid_y0 = 0
   allocate(data%y0_scale(ind%Ny))
   data%y0_scale = sreal_fill_value
   allocate(data%y0_offset(ind%Ny))
   data%y0_offset = sreal_fill_value
   allocate(data%y0_vmin(ind%Ny))
   data%y0_vmin = sint_fill_value
   allocate(data%y0_vmax(ind%Ny))
   data%y0_vmax = sint_fill_value
   allocate(data%y0(ind%X0:ind%X1, ind%Y0:ind%Y1, ind%Ny))
   data%y0 = sint_fill_value

   allocate(data%vid_residuals(ind%Ny))
   data%vid_residuals = 0
   allocate(data%residuals_scale(ind%Ny))
   data%residuals_scale = sreal_fill_value
   allocate(data%residuals_offset(ind%Ny))
   data%residuals_offset = sreal_fill_value
   allocate(data%residuals_vmin(ind%Ny))
   data%residuals_vmin = sint_fill_value
   allocate(data%residuals_vmax(ind%Ny))
   data%residuals_vmax = sint_fill_value
   allocate(data%residuals(ind%X0:ind%X1, ind%Y0:ind%Y1, ind%Ny))
   data%residuals = sint_fill_value

   allocate(data%ds(ind%X0:ind%X1, ind%Y0:ind%Y1))
   data%ds = sint_fill_value

   if (ind%flags%do_covariance) then
      allocate(data%vid_covariance(ind%Nx, ind%Nx))
      data%vid_covariance = 0

      allocate(data%covariance(ind%X0:ind%X1, ind%Y0:ind%Y1, ind%Nx, ind%Nx))
      data%covariance = sreal_fill_value
   else
      nullify(data%vid_covariance)
      nullify(data%covariance)
   end if


   ! Set scale/offset/vmin/vmax dynamically as required
   data%scanline_u_vmax = ind%Y1 - ind%Y0 + 1
   data%scanline_v_vmax = ind%X1 - ind%X0 + 1

   do i = 1, ind%Ny
      if (btest(ind%Ch_Is(i), ThermalBit)) then
         data%channels_scale(i) = 0.01
         data%channels_offset(i) = 100.0
         data%channels_vmin(i) = 0
         data%channels_vmax(i) = 32000

         if (ind%flags%do_meas_error) then
            data%Sy_scale(i) = 0.001
            data%Sy_offset(i) = 32.0
            data%Sy_vmin(i) = -32000
            data%Sy_vmax(i) = 32000
         end if

         data%y0_scale(i) = 0.01
         data%y0_offset(i) = 100.0
         data%y0_vmin(i) = 0
         data%y0_vmax(i) = 32000

         data%residuals_scale(i) = 0.01
         data%residuals_offset(i) = 100.0
         data%residuals_vmin(i) = -32000
         data%residuals_vmax(i) = 32000
      else
         data%channels_scale(i) = 0.0001
         data%channels_offset(i) = 0.0
         data%channels_vmin(i) = 0
         data%channels_vmax(i) = 10000

         if (ind%flags%do_meas_error) then
            data%Sy_scale(i) = 0.00001 ! Max uncertainty of 0.32 in reflectance
            data%Sy_offset(i) = 0.0
            data%Sy_vmin(i) = 0
            data%Sy_vmax(i) = 32765
         end if

         data%y0_scale(i) = 0.0001
         data%y0_offset(i) = 0.0
         data%y0_vmin(i) = 0
         data%y0_vmax(i) = 10000

         data%residuals_scale(i) = 0.0001
         data%residuals_offset(i) = 0.0
         data%residuals_vmin(i) = -10000
         data%residuals_vmax(i) = 10000
      end if
   end do

end subroutine alloc_output_data_secondary
