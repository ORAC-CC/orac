!-------------------------------------------------------------------------------
! Name: postproc_utils.F90
!
! Purpose:
!
! History:
! 2015/09/14, GM: Original version
! 2015/09/26, GM: Add init_class_specific_inputs().
! 2015/10/22, GM: Add cloud albedo uncertainty.
! 2016/01/27, GM: Add cee and cee_uncertainty.
! 2016/01/28, GM: Add ctp and ctt corrected and corrected_uncertianty.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

module postproc_utils

   use common_constants

   implicit none

contains

subroutine copy_class_specific_inputs(i, j, indexing, primary2, primary1, &
                                      secondary2, secondary1, do_secondary)

   use orac_input
   use postproc_constants

   implicit none

   integer,                    intent(in)    :: i, j
   type(input_indices),        intent(in)    :: indexing
   type(input_data_primary),   intent(inout) :: primary2
   type(input_data_primary),   intent(in)    :: primary1
   type(input_data_secondary), intent(inout) :: secondary2
   type(input_data_secondary), intent(in)    :: secondary1
   logical,                    intent(in)    :: do_secondary

   ! primary file
if (indexing%flags%do_aerosol) then
   primary2%aot550(i,j)                = primary1%aot550(i,j)
   primary2%aot550_uncertainty(i,j)    = primary1%aot550_uncertainty(i,j)
   primary2%aot870(i,j)                = primary1%aot870(i,j)
   primary2%aot870_uncertainty(i,j)    = primary1%aot870_uncertainty(i,j)
   primary2%aer(i,j)                   = primary1%aer(i,j)
   primary2%aer_uncertainty(i,j)       = primary1%aer_uncertainty(i,j)
end if

if (indexing%flags%do_rho) then
   primary2%rho(i,j,indexing%ysolar_loop_to_main_index,:) &
                                       = primary1%rho(i,j,:,:)
   primary2%rho_uncertainty(i,j,indexing%ysolar_loop_to_main_index,:) &
                                       = primary1%rho_uncertainty(i,j,:,:)
end if

if (indexing%flags%do_swansea) then
   primary2%swansea_s(i,j,indexing%ysolar_loop_to_main_index) &
                                       = primary1%swansea_s(i,j,:)
   primary2%swansea_s_uncertainty(i,j,indexing%ysolar_loop_to_main_index) &
                                       = primary1%swansea_s_uncertainty(i,j,:)
   primary2%swansea_p(i,j,indexing%view_loop_to_main_index) &
                                       = primary1%swansea_p(i,j,:)
   primary2%swansea_p_uncertainty(i,j,indexing%view_loop_to_main_index) &
                                       = primary1%swansea_p_uncertainty(i,j,:)
   primary2%diffuse_frac(i,j,indexing%ysolar_loop_to_main_index) &
                                       = primary1%diffuse_frac(i,j,:)
   primary2%diffuse_frac_uncertainty(i,j,indexing%ysolar_loop_to_main_index) &
                                       = primary1%diffuse_frac_uncertainty(i,j,:)
end if

if (indexing%flags%do_cloud) then
   primary2%cot(i,j)                   = primary1%cot(i,j)
   primary2%cot_uncertainty(i,j)       = primary1%cot_uncertainty(i,j)
   primary2%cer(i,j)                   = primary1%cer(i,j)
   primary2%cer_uncertainty(i,j)       = primary1%cer_uncertainty(i,j)

   primary2%ctp(i,j)                   = primary1%ctp(i,j)
   primary2%ctp_uncertainty(i,j)       = primary1%ctp_uncertainty(i,j)
   primary2%ctp_corrected(i,j)         = primary1%ctp_corrected(i,j)
   primary2%ctp_corrected_uncertainty(i,j) &
                                       = primary1%ctp_corrected_uncertainty(i,j)

!  primary2%cc_total(i,j)              = primary1%cc_total(i,j)
!  primary2%cc_total_uncertainty(i,j)  = primary1%cc_total_uncertainty(i,j)

   primary2%stemp(i,j)                 = primary1%stemp(i,j)
   primary2%stemp_uncertainty(i,j)     = primary1%stemp_uncertainty(i,j)

   primary2%cth(i,j)                   = primary1%cth(i,j)
   primary2%cth_uncertainty(i,j)       = primary1%cth_uncertainty(i,j)

   primary2%cth_corrected(i,j)         = primary1%cth_corrected(i,j)
   primary2%cth_corrected_uncertainty(i,j) &
                                       = primary1%cth_corrected_uncertainty(i,j)

   primary2%ctt(i,j)                   = primary1%ctt(i,j)
   primary2%ctt_uncertainty(i,j)       = primary1%ctt_uncertainty(i,j)
   primary2%ctt_corrected(i,j)         = primary1%ctt_corrected(i,j)
   primary2%ctt_corrected_uncertainty(i,j) &
                                       = primary1%ctt_corrected_uncertainty(i,j)

   primary2%cwp(i,j)                   = primary1%cwp(i,j)
   primary2%cwp_uncertainty(i,j)       = primary1%cwp_uncertainty(i,j)

   primary2%cloud_albedo(i,j,indexing%ysolar_loop_to_main_index)       &
                                       = primary1%cloud_albedo(i,j,:)
   primary2%cloud_albedo_uncertainty(i,j,indexing%ysolar_loop_to_main_index) &
                                       = primary1%cloud_albedo_uncertainty(i,j,:)

   primary2%cee(i,j,indexing%ythermal_loop_to_main_index) &
                                       = primary1%cee(i,j,:)
   primary2%cee_uncertainty(i,j,indexing%ythermal_loop_to_main_index) &
                                       = primary1%cee_uncertainty(i,j,:)
end if

   primary2%convergence(i,j)           = primary1%convergence(i,j)

   primary2%niter(i,j)                 = primary1%niter(i,j)

   primary2%costja(i,j)                = primary1%costja(i,j)
   primary2%costjm(i,j)                = primary1%costjm(i,j)

   primary2%qcflag(i,j)                = primary1%qcflag(i,j)

   ! secondary file
if (do_secondary) then
   if (indexing%flags%do_aerosol) then
      secondary2%aot550_ap(i,j)        = secondary1%aot550_ap(i,j)
      secondary2%aot550_fg(i,j)        = secondary1%aot550_fg(i,j)
   end if

   if (indexing%flags%do_rho) then
      secondary2%rho_ap(i,j,indexing%ysolar_loop_to_main_index,:) &
                                       = secondary1%rho_ap(i,j,:,:)
      secondary2%rho_fg(i,j,indexing%ysolar_loop_to_main_index,:) &
                                       = secondary1%rho_fg(i,j,:,:)
   end if

   if (indexing%flags%do_swansea) then
      secondary2%swansea_s_ap(i,j,indexing%ysolar_loop_to_main_index) &
                                       = secondary1%swansea_s_ap(i,j,:)
      secondary2%swansea_s_fg(i,j,indexing%ysolar_loop_to_main_index) &
                                       = secondary1%swansea_s_fg(i,j,:)
      secondary2%swansea_p_ap(i,j,indexing%view_loop_to_main_index) &
                                       = secondary1%swansea_p_ap(i,j,:)
      secondary2%swansea_p_fg(i,j,indexing%view_loop_to_main_index) &
                                       = secondary1%swansea_p_fg(i,j,:)
   end if

   if (indexing%flags%do_cloud) then
      secondary2%cot_ap(i,j)           = secondary1%cot_ap(i,j)
      secondary2%cot_fg(i,j)           = secondary1%cot_fg(i,j)

      secondary2%cer_ap(i,j)           = secondary1%cer_ap(i,j)
      secondary2%cer_fg(i,j)           = secondary1%cer_fg(i,j)

      secondary2%ctp_ap(i,j)           = secondary1%ctp_ap(i,j)
      secondary2%ctp_fg(i,j)           = secondary1%ctp_fg(i,j)

      secondary2%stemp_ap(i,j)         = secondary1%stemp_ap(i,j)
      secondary2%stemp_fg(i,j)         = secondary1%stemp_fg(i,j)
   end if

   secondary2%y0(i,j,indexing%loop_to_main_index) &
                                       = secondary1%y0(i,j,:)

   secondary2%residuals(i,j,indexing%loop_to_main_index) &
                                       = secondary1%residuals(i,j,:)

   secondary2%ds(i,j)                  = secondary1%ds(i,j)
end if

end subroutine copy_class_specific_inputs

end module postproc_utils
