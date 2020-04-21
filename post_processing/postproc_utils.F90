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
! 2016/07/19, AP: Reduce rho and swansea_s to only contain terms that were
!    retrieved. This is indicated by the rho|ss_terms array (and Nrho|Nss).
! 2017/01/09, CP: ML additions.
! 2017/10/05, GM: Add subroutine get_use_ann_phase().
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

module postproc_utils_m

   implicit none

contains

subroutine get_use_ann_phase(in_files, n_in_files, use_ann_phase, ml_flag, verbose)

   use orac_ncdf_m

   implicit none

   character(len=*), intent(in)  :: in_files(:)
   integer,          intent(in)  :: n_in_files
   logical,          intent(out) :: use_ann_phase
   logical,          intent(in)  :: ml_flag
   logical,          intent(in)  :: verbose

   integer :: i
   integer :: ncid, ierr
   integer :: ann_phase_used, ann_phase_used2

   do i = 1, n_in_files
      call ncdf_open(ncid, in_files(i), 'get_use_ann_phase()')

      ierr = nf90_get_att(ncid, NF90_GLOBAL, 'ANN_phase_used', ann_phase_used2)
      if (ierr /= NF90_NOERR) then
         write(*,*) 'ERROR: get_use_ann_phase(), ', trim(nf90_strerror(ierr))
         stop error_stop_code
      end if

      if (i == 1) then
          ann_phase_used  = ann_phase_used2
      else if (.not. (ml_flag .and. i == 3) .and. &
               ann_phase_used /= ann_phase_used2) then
         write(*,*) 'ERROR: ANN_phase_used global attribute must be the '// &
                    'same for all input files'
         stop error_stop_code
      end if

      call ncdf_close(ncid, 'get_use_ann_phase()')
   end do

   if (ann_phase_used == 1) then
      use_ann_phase = .true.
   else
      use_ann_phase = .false.
   end if

end subroutine get_use_ann_phase


subroutine copy_class_specific_inputs(i, j, indexing, primary2, primary1, &
                                      secondary2, secondary1, do_secondary)

   use orac_input_m
   use postproc_constants_m

   implicit none

   integer,                      intent(in)    :: i, j
   type(input_indices_t),        intent(in)    :: indexing
   type(input_data_primary_t),   intent(inout) :: primary2
   type(input_data_primary_t),   intent(in)    :: primary1
   type(input_data_secondary_t), intent(inout) :: secondary2
   type(input_data_secondary_t), intent(in)    :: secondary1
   logical,                      intent(in)    :: do_secondary

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
   primary2%rho(i,j,indexing%rho_loop_to_rho_main_index) &
                                       = primary1%rho(i,j,:)
   primary2%rho_uncertainty(i,j,indexing%rho_loop_to_rho_main_index) &
                                       = primary1%rho_uncertainty(i,j,:)
end if

if (indexing%flags%do_swansea) then
   primary2%swansea_s(i,j,indexing%ss_loop_to_ss_main_index) &
                                       = primary1%swansea_s(i,j,:)
   primary2%swansea_s_uncertainty(i,j,indexing%ss_loop_to_ss_main_index) &
                                       = primary1%swansea_s_uncertainty(i,j,:)
   primary2%swansea_p(i,j,indexing%view_loop_to_main_index) &
                                       = primary1%swansea_p(i,j,:)
   primary2%swansea_p_uncertainty(i,j,indexing%view_loop_to_main_index) &
                                       = primary1%swansea_p_uncertainty(i,j,:)
   primary2%diffuse_frac(i,j,indexing%ss_loop_to_ss_main_index) &
                                       = primary1%diffuse_frac(i,j,:)
   primary2%diffuse_frac_uncertainty(i,j,indexing%ss_loop_to_ss_main_index) &
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

   primary2%cloud_albedo(i,j,indexing%alb_loop_to_alb_main_index)       &
                                       = primary1%cloud_albedo(i,j,:)
   primary2%cloud_albedo_uncertainty(i,j,indexing%alb_loop_to_alb_main_index) &
                                       = primary1%cloud_albedo_uncertainty(i,j,:)

   primary2%cee(i,j,indexing%cee_loop_to_cee_main_index) &
                                       = primary1%cee(i,j,:)
   primary2%cee_uncertainty(i,j,indexing%cee_loop_to_cee_main_index) &
                                       = primary1%cee_uncertainty(i,j,:)
end if

if (indexing%flags%do_cloud_layer_2) then
   primary2%cot2(i,j)                   = primary1%cot2(i,j)
   primary2%cot2_uncertainty(i,j)       = primary1%cot2_uncertainty(i,j)
   primary2%cer2(i,j)                   = primary1%cer2(i,j)
   primary2%cer2_uncertainty(i,j)       = primary1%cer2_uncertainty(i,j)
   primary2%ctp2(i,j)                   = primary1%ctp2(i,j)
   primary2%ctp2_uncertainty(i,j)       = primary1%ctp2_uncertainty(i,j)
   primary2%cth2(i,j)                   = primary1%cth2(i,j)
   primary2%cth2_uncertainty(i,j)       = primary1%cth2_uncertainty(i,j)
   primary2%ctt2(i,j)                   = primary1%ctt2(i,j)
   primary2%ctt2_uncertainty(i,j)       = primary1%ctt2_uncertainty(i,j)
   primary2%cwp2(i,j)                   = primary1%cwp2(i,j)
   primary2%cwp2_uncertainty(i,j)       = primary1%cwp2_uncertainty(i,j)
end if

   primary2%niter(i,j)                 = primary1%niter(i,j)

   primary2%costja(i,j)                = primary1%costja(i,j)
   primary2%costjm(i,j)                = primary1%costjm(i,j)

   primary2%qcflag(i,j)                = primary1%qcflag(i,j)
   primary2%channels_used(i,j)         = primary1%channels_used(i,j)
   primary2%variables_retrieved(i,j)   = primary1%variables_retrieved(i,j)

   ! secondary file

if (do_secondary) then
   if (indexing%flags%do_aerosol) then
      secondary2%aot550_ap(i,j)        = secondary1%aot550_ap(i,j)
      secondary2%aot550_fg(i,j)        = secondary1%aot550_fg(i,j)
   end if

   if (indexing%flags%do_rho) then
      secondary2%rho_ap(i,j,indexing%rho_loop_to_rho_main_index) &
                                       = secondary1%rho_ap(i,j,:)
      secondary2%rho_fg(i,j,indexing%rho_loop_to_rho_main_index) &
                                       = secondary1%rho_fg(i,j,:)
   end if

   if (indexing%flags%do_swansea) then
      secondary2%swansea_s_ap(i,j,indexing%ss_loop_to_ss_main_index) &
                                       = secondary1%swansea_s_ap(i,j,:)
      secondary2%swansea_s_fg(i,j,indexing%ss_loop_to_ss_main_index) &
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

   if (indexing%flags%do_cloud_layer_2) then
      secondary2%cot2_ap(i,j)          = secondary1%cot2_ap(i,j)
      secondary2%cot2_fg(i,j)          = secondary1%cot2_fg(i,j)

      secondary2%cer2_ap(i,j)          = secondary1%cer2_ap(i,j)
      secondary2%cer2_fg(i,j)          = secondary1%cer2_fg(i,j)

      secondary2%ctp2_ap(i,j)          = secondary1%ctp2_ap(i,j)
      secondary2%ctp2_fg(i,j)          = secondary1%ctp2_fg(i,j)

   end if

   secondary2%y0(i,j,indexing%loop_to_main_index) &
                                       = secondary1%y0(i,j,:)

   secondary2%residuals(i,j,indexing%loop_to_main_index) &
                                       = secondary1%residuals(i,j,:)

   secondary2%ds(i,j)                  = secondary1%ds(i,j)
end if

end subroutine copy_class_specific_inputs

end module postproc_utils_m
