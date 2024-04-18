!-------------------------------------------------------------------------------
! Name: alloc_input_data.F90
!
! Purpose:
! File contains several subroutines to allocate and initialize structures and
! user defined variable types.
!
! Description and Algorithm details:
!
! Arguments:
! Name Type In/Out/Both Description
!
! History:
! 2012/02/03, MJ: Cleans out prototype code to prepare repository upload.
! 15/02/2012, CP: To do level 2 post processing
! 07/03/2012, MS: Added missing stemp_ap
! 07/03/2012, CP: Cleaned up
! 2012/03/18, CP: Modified to add cloud flag
! 2012/06/20, CP: Added albedo
! 2012/07/04, MJ: Fixed several data type bugs
! 2012/07/06, MJ: Extensively overhauls and restructures the code
! 2013/01/17, MJ: Adds code to accommodate uncertainties of ctt and cth
! 2014/09/20, change phase from 2 to 1, changed arguments of
!    alloc_input_data_secondary added in channels
! 2014/09/29, CP: Added in MODIS variable names
! 2014/10/24, OS: Added variables cccot_pre, lusflag, cldtype, cloudmask, DEM
!    (currently deactivated), and nisemask; commented out (de)allocation of
!    variables for water within if condition iphase = 2 (never true for water)
! 2014/11/20, OS: Some minor editing
! 2014/11/26, CP: Added cloud_albedo
! 2015/01/26, CP: Added multi layer cloud IR only
! 2015/07/16, GM: Major cleanup.
! 2015/09/07, GM: Add cldmask_uncertainty.
! 2015/10/22, GM: Add cloud albedo uncertainty.
! 2016/01/27, GM: Add cee and cee_uncertainty.
! 2016/01/28, GM: Add ctp and ctt corrected and corrected_uncertianty.
! 2016/03/02, AP: Homogenisation of I/O modules.
! 2016/04/28, AP: Add multiple views.
! 2016/06/13, SP: Updates for bayesian selection without huge memory usage.
! 2017/01/09, CP: ML additions.
! 2017/06/22, OS: Added phase variables.
! 2017/07/05, AP: Add channels_used, variables_retrieved. New QC.
! 2018/06/08, SP: Add satellite azimuth angle to output.
! 2023/10/10, GT: Added measurement uncertainties to secondary data
! 2023/11/21, GT: Added alloc_input_data_classify() subroutine.
! 2024/03/07, GT: Removed unused alloc_input_data_only_cost() subroutine.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine alloc_input_data_classify(ind, data, read_cost, read_ctt)

   implicit none

   type(input_indices_t),        intent(in)    :: ind
   type(input_data_primary_t),   intent(inout) :: data
   logical,                      intent(in)    :: read_cost
   logical,                      intent(in)    :: read_ctt

   if (read_cost) then
      allocate(data%costja(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%costja = sreal_fill_value
      allocate(data%costjm(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%costjm = sreal_fill_value
   else
      nullify(data%costja)
      nullify(data%costjm)
   end if

   if (read_ctt) then
      allocate(data%ctt(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%costja = sreal_fill_value
   else
      nullify(data%ctt)
   end if

   nullify(data%aot550)
   nullify(data%aot550_uncertainty)
   nullify(data%aot870)
   nullify(data%aot870_uncertainty)
   nullify(data%aer)
   nullify(data%aer_uncertainty)
   nullify(data%rho)
   nullify(data%rho_uncertainty)
   nullify(data%swansea_s)
   nullify(data%swansea_s_uncertainty)
   nullify(data%swansea_p)
   nullify(data%swansea_p_uncertainty)
   nullify(data%diffuse_frac)
   nullify(data%diffuse_frac_uncertainty)
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
   ! CTT already dealt with above..
   nullify(data%ctt_uncertainty)
   nullify(data%ctt_corrected)
   nullify(data%ctt_corrected_uncertainty)
   nullify(data%cwp)
   nullify(data%cwp_uncertainty)
   nullify(data%cloud_albedo)
   nullify(data%cloud_albedo_uncertainty)
   nullify(data%cee)
   nullify(data%cee_uncertainty)
   nullify(data%cot2)
   nullify(data%cot2_uncertainty)
   nullify(data%cer2)
   nullify(data%cer2_uncertainty)
   nullify(data%ctp2)
   nullify(data%ctp2_uncertainty)
   nullify(data%cth2)
   nullify(data%cth2_uncertainty)
   nullify(data%ctt2)
   nullify(data%ctt2_uncertainty)
   nullify(data%cwp2)
   nullify(data%cwp2_uncertainty)

   nullify(data%cccot_pre)
   nullify(data%cldmask)
   nullify(data%cldmask_uncertainty)
   nullify(data%ann_phase)
   nullify(data%cphcot)
   nullify(data%ann_phase_uncertainty)
   nullify(data%phase)
   nullify(data%phase_pavolonis)

   nullify(data%time)
   nullify(data%lat)
   nullify(data%lon)
   nullify(data%sol_zen)
   nullify(data%sat_zen)
   nullify(data%rel_azi)
   nullify(data%sat_azi)
   nullify(data%niter)
   nullify(data%qcflag)
   nullify(data%channels_used)
   nullify(data%variables_retrieved)
   nullify(data%lsflag)
   nullify(data%lusflag)
   nullify(data%dem)
   nullify(data%illum)
   nullify(data%cldtype)

end subroutine alloc_input_data_classify


subroutine alloc_input_data_primary_common(ind, data)

   implicit none

   type(input_indices_t),      intent(in)    :: ind
   type(input_data_primary_t), intent(inout) :: data

   if (ind%flags%do_aerosol) then
      allocate(data%aot550(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%aot550 = sreal_fill_value
      allocate(data%aot550_uncertainty(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%aot550_uncertainty = sreal_fill_value

      allocate(data%aot870(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%aot870 = sreal_fill_value
      allocate(data%aot870_uncertainty(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%aot870_uncertainty = sreal_fill_value

      allocate(data%aer(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%aer = sreal_fill_value
      allocate(data%aer_uncertainty(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%aer_uncertainty = sreal_fill_value
   else
      nullify(data%aot550)
      nullify(data%aot550_uncertainty)
      nullify(data%aot870)
      nullify(data%aot870_uncertainty)
      nullify(data%aer)
      nullify(data%aer_uncertainty)
   end if

   if (ind%flags%do_rho) then
      allocate(data%rho(ind%X0:ind%X1, ind%Y0:ind%Y1, ind%Nrho))
      data%rho = sreal_fill_value
      allocate(data%rho_uncertainty(ind%X0:ind%X1, ind%Y0:ind%Y1, ind%Nrho))
      data%rho_uncertainty = sreal_fill_value
   else
      nullify(data%rho)
      nullify(data%rho_uncertainty)
   end if

   if (ind%flags%do_swansea) then
      allocate(data%swansea_s(ind%X0:ind%X1, ind%Y0:ind%Y1, ind%Nss))
      data%swansea_s = sreal_fill_value
      allocate(data%swansea_s_uncertainty(ind%X0:ind%X1, ind%Y0:ind%Y1, ind%Nss))
      data%swansea_s_uncertainty = sreal_fill_value

      allocate(data%swansea_p(ind%X0:ind%X1, ind%Y0:ind%Y1, ind%NViews))
      data%swansea_p = sreal_fill_value
      allocate(data%swansea_p_uncertainty(ind%X0:ind%X1, ind%Y0:ind%Y1, &
           ind%NViews))
      data%swansea_p_uncertainty = sreal_fill_value

      allocate(data%diffuse_frac(ind%X0:ind%X1, ind%Y0:ind%Y1, ind%NSolar))
      data%diffuse_frac = sreal_fill_value
      allocate(data%diffuse_frac_uncertainty(ind%X0:ind%X1, ind%Y0:ind%Y1, &
           ind%Nss))
      data%diffuse_frac_uncertainty = sreal_fill_value
   else
      nullify(data%swansea_s)
      nullify(data%swansea_s_uncertainty)
      nullify(data%swansea_p)
      nullify(data%swansea_p_uncertainty)
      nullify(data%diffuse_frac)
      nullify(data%diffuse_frac_uncertainty)
   end if

   if (ind%flags%do_cloud) then
      allocate(data%cot(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%cot = sreal_fill_value
      allocate(data%cot_uncertainty(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%cot_uncertainty = sreal_fill_value

      allocate(data%cer(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%cer = sreal_fill_value
      allocate(data%cer_uncertainty(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%cer_uncertainty = sreal_fill_value

      allocate(data%ctp(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%ctp = sreal_fill_value
      allocate(data%ctp_uncertainty(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%ctp_uncertainty = sreal_fill_value

      allocate(data%ctp_corrected(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%ctp_corrected = sreal_fill_value
      allocate(data%ctp_corrected_uncertainty(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%ctp_corrected_uncertainty = sreal_fill_value

      allocate(data%cc_total(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%cc_total = sreal_fill_value
      allocate(data%cc_total_uncertainty(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%cc_total_uncertainty = sreal_fill_value

      allocate(data%stemp(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%stemp = sreal_fill_value
      allocate(data%stemp_uncertainty(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%stemp_uncertainty = sreal_fill_value

      allocate(data%cth(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%cth = sreal_fill_value
      allocate(data%cth_uncertainty(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%cth_uncertainty = sreal_fill_value

      allocate(data%cth_corrected(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%cth_corrected = sreal_fill_value
      allocate(data%cth_corrected_uncertainty(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%cth_corrected_uncertainty = sreal_fill_value

      allocate(data%ctt(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%ctt = sreal_fill_value
      allocate(data%ctt_uncertainty(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%ctt_uncertainty = sreal_fill_value

      allocate(data%ctt_corrected(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%ctt_corrected = sreal_fill_value
      allocate(data%ctt_corrected_uncertainty(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%ctt_corrected_uncertainty = sreal_fill_value

      allocate(data%cwp(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%cwp = sreal_fill_value
      allocate(data%cwp_uncertainty(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%cwp_uncertainty = sreal_fill_value

      allocate(data%cloud_albedo(ind%X0:ind%X1, ind%Y0:ind%Y1, ind%NSolar))
      data%cloud_albedo = sreal_fill_value
      allocate(data%cloud_albedo_uncertainty(ind%X0:ind%X1, ind%Y0:ind%Y1, &
           ind%NSolar))
      data%cloud_albedo_uncertainty = sreal_fill_value

      allocate(data%cee(ind%X0:ind%X1, ind%Y0:ind%Y1, ind%NThermal))
      data%cee = sreal_fill_value
      allocate(data%cee_uncertainty(ind%X0:ind%X1, ind%Y0:ind%Y1, &
           ind%NThermal))
      data%cee_uncertainty = sreal_fill_value
   else
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
   end if

   if (ind%flags%do_cloud_layer_2) then
      allocate(data%cot2(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%cot2 = sreal_fill_value
      allocate(data%cot2_uncertainty(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%cot2_uncertainty = sreal_fill_value

      allocate(data%cer2(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%cer2 = sreal_fill_value
      allocate(data%cer2_uncertainty(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%cer2_uncertainty = sreal_fill_value

      allocate(data%ctp2(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%ctp2 = sreal_fill_value
      allocate(data%ctp2_uncertainty(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%ctp2_uncertainty = sreal_fill_value

      allocate(data%cth2(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%cth2 = sreal_fill_value
      allocate(data%cth2_uncertainty(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%cth2_uncertainty = sreal_fill_value

      allocate(data%ctt2(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%ctt2 = sreal_fill_value
      allocate(data%ctt2_uncertainty(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%ctt2_uncertainty = sreal_fill_value

      allocate(data%cwp2(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%cwp2 = sreal_fill_value
      allocate(data%cwp2_uncertainty(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%cwp2_uncertainty = sreal_fill_value
   else
      nullify(data%cot2)
      nullify(data%cot2_uncertainty)
      nullify(data%cer2)
      nullify(data%cer2_uncertainty)
      nullify(data%ctp2)
      nullify(data%ctp2_uncertainty)
      nullify(data%cth2)
      nullify(data%cth2_uncertainty)
      nullify(data%ctt2)
      nullify(data%ctt2_uncertainty)
      nullify(data%cwp2)
      nullify(data%cwp2_uncertainty)
   end if

   allocate(data%niter(ind%X0:ind%X1, ind%Y0:ind%Y1))
   data%niter = byte_fill_value
   if (.not. associated(data%costja)) then
      allocate(data%costja(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%costja = sreal_fill_value
   end if
   if (.not. associated(data%costjm)) then
      allocate(data%costjm(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%costjm = sreal_fill_value
   end if

   allocate(data%qcflag(ind%X0:ind%X1, ind%Y0:ind%Y1))
   data%qcflag = sint_fill_value

   allocate(data%channels_used(ind%X0:ind%X1, ind%Y0:ind%Y1))
   data%channels_used = 0

   allocate(data%variables_retrieved(ind%X0:ind%X1, ind%Y0:ind%Y1))
   data%variables_retrieved = 0

end subroutine alloc_input_data_primary_common


subroutine alloc_input_data_primary_all(ind, data)

   implicit none

   type(input_indices_t),      intent(in)    :: ind
   type(input_data_primary_t), intent(inout) :: data

   call alloc_input_data_primary_common(ind, data)

   if (ind%flags%do_cloud) then
      allocate(data%cccot_pre(ind%X0:ind%X1, ind%Y0:ind%Y1, ind%NViews))
      data%cccot_pre = sreal_fill_value
   else
      nullify(data%cccot_pre)
   end if

   allocate(data%time(ind%X0:ind%X1, ind%Y0:ind%Y1))
   data%time = dreal_fill_value

   allocate(data%lat(ind%X0:ind%X1, ind%Y0:ind%Y1))
   data%lat = sreal_fill_value
   allocate(data%lon(ind%X0:ind%X1, ind%Y0:ind%Y1))
   data%lon = sreal_fill_value

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
      data%cldmask_uncertainty = sreal_fill_value
   else
      nullify(data%cldmask_uncertainty)
   end if

   if (ind%flags%do_ann_phase) then
      allocate(data%ann_phase(ind%X0:ind%X1, ind%Y0:ind%Y1, ind%NViews))
      data%ann_phase = byte_fill_value
      allocate(data%cphcot(ind%X0:ind%X1, ind%Y0:ind%Y1, ind%NViews))
      data%cphcot = sreal_fill_value
   else
      nullify(data%ann_phase)
      nullify(data%cphcot)
   end if

   if (ind%flags%do_ann_phase_uncertainty) then
      allocate(data%ann_phase_uncertainty(ind%X0:ind%X1, ind%Y0:ind%Y1, &
           ind%NViews))
      data%ann_phase_uncertainty = sreal_fill_value
   else
      nullify(data%ann_phase_uncertainty)
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

end subroutine alloc_input_data_primary_all


subroutine alloc_input_data_primary_class(ind, data)

   implicit none

   type(input_indices_t),      intent(in)    :: ind
   type(input_data_primary_t), intent(inout) :: data

   call alloc_input_data_primary_common(ind, data)

   nullify(data%cccot_pre)
   nullify(data%cldmask)
   nullify(data%cldmask_uncertainty)
   nullify(data%ann_phase)
   nullify(data%cphcot)
   nullify(data%ann_phase_uncertainty)
   nullify(data%phase)
   nullify(data%phase_pavolonis)

   nullify(data%time)
   nullify(data%lat)
   nullify(data%lon)
   nullify(data%sol_zen)
   nullify(data%sat_zen)
   nullify(data%rel_azi)
   nullify(data%sat_azi)
   nullify(data%lsflag)
   nullify(data%lusflag)
   nullify(data%dem)
   nullify(data%illum)
   nullify(data%cldtype)

end subroutine alloc_input_data_primary_class


subroutine alloc_input_data_secondary_common(ind, data)

   implicit none

   type(input_indices_t),        intent(in)    :: ind
   type(input_data_secondary_t), intent(inout) :: data

   if (ind%flags%do_aerosol) then
      allocate(data%aot550_ap(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%aot550_ap = sreal_fill_value
      allocate(data%aot550_fg(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%aot550_fg = sreal_fill_value

      allocate(data%aer_ap(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%aer_ap = sreal_fill_value
      allocate(data%aer_fg(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%aer_fg = sreal_fill_value
   else
      nullify(data%aot550_ap)
      nullify(data%aot550_fg)
      nullify(data%aer_ap)
      nullify(data%aer_fg)
   end if

   if (ind%flags%do_rho) then
      allocate(data%rho_ap(ind%X0:ind%X1, ind%Y0:ind%Y1, ind%Nrho))
      data%rho_ap = sreal_fill_value
      allocate(data%rho_fg(ind%X0:ind%X1, ind%Y0:ind%Y1, ind%Nrho))
      data%rho_fg = sreal_fill_value
   else
      nullify(data%rho_ap)
      nullify(data%rho_fg)
   end if

   if (ind%flags%do_swansea) then
      allocate(data%swansea_s_ap(ind%X0:ind%X1, ind%Y0:ind%Y1, ind%Nss))
      data%swansea_s_ap = sreal_fill_value
      allocate(data%swansea_s_fg(ind%X0:ind%X1, ind%Y0:ind%Y1, ind%Nss))
      data%swansea_s_fg = sreal_fill_value

      allocate(data%swansea_p_ap(ind%X0:ind%X1, ind%Y0:ind%Y1, ind%NViews))
      data%swansea_p_ap = sreal_fill_value
      allocate(data%swansea_p_fg(ind%X0:ind%X1, ind%Y0:ind%Y1, ind%NViews))
      data%swansea_p_fg = sreal_fill_value
   else
      nullify(data%swansea_s_ap)
      nullify(data%swansea_s_fg)
      nullify(data%swansea_p_ap)
      nullify(data%swansea_p_fg)
   end if

   if (ind%flags%do_cloud) then
      allocate(data%cot_ap(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%cot_ap = sreal_fill_value
      allocate(data%cot_fg(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%cot_fg = sreal_fill_value

      allocate(data%cer_ap(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%cer_ap = sreal_fill_value
      allocate(data%cer_fg(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%cer_fg = sreal_fill_value

      allocate(data%ctp_ap(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%ctp_ap = sreal_fill_value
      allocate(data%ctp_fg(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%ctp_fg = sreal_fill_value

      allocate(data%stemp_fg(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%stemp_fg = sreal_fill_value
      allocate(data%stemp_ap(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%stemp_ap = sreal_fill_value
   else
      nullify(data%cot_ap)
      nullify(data%cot_fg)
      nullify(data%cer_ap)
      nullify(data%cer_fg)
      nullify(data%ctp_ap)
      nullify(data%ctp_fg)
      nullify(data%stemp_fg)
      nullify(data%stemp_ap)
   end if

   if (ind%flags%do_cloud_layer_2) then
      allocate(data%cot2_ap(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%cot2_ap = sreal_fill_value
      allocate(data%cot2_fg(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%cot2_fg = sreal_fill_value

      allocate(data%cer2_ap(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%cer2_ap = sreal_fill_value
      allocate(data%cer2_fg(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%cer2_fg = sreal_fill_value

      allocate(data%ctp2_ap(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%ctp2_ap = sreal_fill_value
      allocate(data%ctp2_fg(ind%X0:ind%X1, ind%Y0:ind%Y1))
      data%ctp2_fg = sreal_fill_value

   else
      nullify(data%cot2_ap)
      nullify(data%cot2_fg)
      nullify(data%cer2_ap)
      nullify(data%cer2_fg)
      nullify(data%ctp2_ap)
      nullify(data%ctp2_fg)
   end if

   if (ind%flags%do_meas_error) then
      allocate(data%Sy(ind%X0:ind%X1, ind%Y0:ind%Y1, ind%Ny))
      data%Sy = sreal_fill_value
   else
      nullify(data%Sy)
   end if

   allocate(data%y0(ind%X0:ind%X1, ind%Y0:ind%Y1, ind%Ny))
   data%y0 = sreal_fill_value

   allocate(data%residuals(ind%X0:ind%X1, ind%Y0:ind%Y1, ind%Ny))
   data%residuals = sreal_fill_value

   allocate(data%ds(ind%X0:ind%X1, ind%Y0:ind%Y1))
   data%ds = sreal_fill_value

end subroutine alloc_input_data_secondary_common


subroutine alloc_input_data_secondary_all(ind, data)

   implicit none

   type(input_indices_t),        intent(in)    :: ind
   type(input_data_secondary_t), intent(inout) :: data

   call alloc_input_data_secondary_common(ind, data)

   if (ind%flags%do_cloud) then
      allocate(data%albedo(ind%X0:ind%X1, ind%Y0:ind%Y1, ind%NSolar))
      data%albedo = sreal_fill_value
   else
      nullify(data%albedo)
   end if

   allocate(data%channels(ind%X0:ind%X1, ind%Y0:ind%Y1, ind%Ny))
   data%channels = sreal_fill_value

end subroutine alloc_input_data_secondary_all


subroutine alloc_input_data_secondary_class(ind, data)

   implicit none


   type(input_indices_t),        intent(in)    :: ind
   type(input_data_secondary_t), intent(inout) :: data

   call alloc_input_data_secondary_common(ind, data)

   nullify(data%channels)
   nullify(data%albedo)

end subroutine alloc_input_data_secondary_class
