!-------------------------------------------------------------------------------
! Name: dealloc_output_data.F90
!
! Purpose:
! The file contains a collection of subroutines which deallocates the
! array parts of the output variable types, stored within module output_routines.
!
! History:
! 2011/12/19, MJ: Creates initial file.
! 2012/01/05, CP: Added in channel information
! 2012/01/06, CP: Added in cwp
! 2012/01/15, CP: Added in chan definitions
! 2012/01/28, CP: Added in albedo
! 2012/08/21, MJ: Adds time variable
! 2013/12/16, GM: Add deallocation of output_data%ctt_error,
!    output_data%cth_error, and output_data_sec%ds and a bit cleanup.
! 2014/05/27, GM: Some cleanup.
! 2014/10/24, OS: Added deallocation of cldtype, cldmask, cccot_pre, lusflag,
!    dem, nisemask
! 2014/12/01, CP: Added in cloud albedo
! 2015/07/01, CP: Added in corrected cth
! 2015/09/06, GM: Move into common/ from src/ and changes related to sharing
!    with post_processing/.
! 2015/09/07, GM: Add cldmask_uncertainty.
! 2015/10/22, GM: Add cloud albedo uncertainty.
! 2015/12/28, AP: Add output fields for aerosol retrievals.
! 2015/12/30, AP: Have all albedo fields use the same values.
! 2016/01/06, AP: Wrap do_* flags into output_flags structure.
! 2016/01/27, GM: Add cee and cee_uncertainty.
! 2016/01/28, GM: Add ctp and ctt corrected and corrected_uncertianty.
! 2016/03/02, AP: Homogenisation of I/O modules.
! 2016/07/08, GM: Add fields for cloud layer 2.
! 2017/05/17, OS: Added ann phase variables.
! 2017/07/05, AP: Add channels_used, variables_retrieved. New QC.
! 2018/06/08, SP: Add satellite azimuth angle to output.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! Name: dealloc_output_data_primary
!
! Purpose:
! Deallocate storage for primary output file.
!
! Description and Algorithm details:
! 1) Deallocate all arrays.
!
! Arguments:
! Name        Type   In/Out/Both Description
! ------------------------------------------------------------------------------
! data        struct Both        Contents of secondard output file.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------
subroutine dealloc_output_data_primary(data)

   implicit none

   type(output_data_primary_t), intent(inout) :: data

   if (associated(data%aot550))           deallocate(data%aot550)
   if (associated(data%aot550_uncertainty)) &
                                          deallocate(data%aot550_uncertainty)
   if (associated(data%aot870))           deallocate(data%aot870)
   if (associated(data%aot870_uncertainty)) &
                                          deallocate(data%aot870_uncertainty)
   if (associated(data%aer))              deallocate(data%aer)
   if (associated(data%aer_uncertainty))  deallocate(data%aer_uncertainty)

   if (associated(data%vid_rho))          deallocate(data%vid_rho)
   if (associated(data%vid_rho_uncertainty)) &
                                          deallocate(data%vid_rho_uncertainty)
   if (associated(data%rho))              deallocate(data%rho)
   if (associated(data%rho_uncertainty))  deallocate(data%rho_uncertainty)

   if (associated(data%vid_swansea_s))    deallocate(data%vid_swansea_s)
   if (associated(data%vid_swansea_s_uncertainty)) &
                                          deallocate(data%vid_swansea_s_uncertainty)
   if (associated(data%vid_swansea_p))    deallocate(data%vid_swansea_p)
   if (associated(data%vid_swansea_p_uncertainty)) &
                                          deallocate(data%vid_swansea_p_uncertainty)
   if (associated(data%vid_diffuse_frac)) deallocate(data%vid_diffuse_frac)
   if (associated(data%vid_diffuse_frac_uncertainty)) &
                                          deallocate(data%vid_diffuse_frac_uncertainty)
   if (associated(data%swansea_s))        deallocate(data%swansea_s)
   if (associated(data%swansea_s_uncertainty)) &
                                          deallocate(data%swansea_s_uncertainty)
   if (associated(data%swansea_p))        deallocate(data%swansea_p)
   if (associated(data%swansea_p_uncertainty)) &
                                          deallocate(data%swansea_p_uncertainty)
   if (associated(data%diffuse_frac))     deallocate(data%diffuse_frac)
   if (associated(data%diffuse_frac_uncertainty)) &
                                          deallocate(data%diffuse_frac_uncertainty)

   if (associated(data%vid_cloud_albedo)) deallocate(data%vid_cloud_albedo)
   if (associated(data%vid_cloud_albedo_uncertainty)) &
                                          deallocate(data%vid_cloud_albedo_uncertainty)
   if (associated(data%vid_cee))          deallocate(data%vid_cee)
   if (associated(data%vid_cee_uncertainty)) &
                                          deallocate(data%vid_cee_uncertainty)

   if (associated(data%cot))              deallocate(data%cot)
   if (associated(data%cot_uncertainty))  deallocate(data%cot_uncertainty)
   if (associated(data%cer))              deallocate(data%cer)
   if (associated(data%cer_uncertainty))  deallocate(data%cer_uncertainty)
   if (associated(data%ctp))              deallocate(data%ctp)
   if (associated(data%ctp_uncertainty))  deallocate(data%ctp_uncertainty)
   if (associated(data%ctp_corrected))    deallocate(data%ctp_corrected)
   if (associated(data%ctp_corrected_uncertainty)) &
                                          deallocate(data%ctp_corrected_uncertainty)
   if (associated(data%cc_total))         deallocate(data%cc_total)
   if (associated(data%cc_total_uncertainty)) &
                                          deallocate(data%cc_total_uncertainty)
   if (associated(data%stemp))            deallocate(data%stemp)
   if (associated(data%stemp_uncertainty)) &
                                          deallocate(data%stemp_uncertainty)
   if (associated(data%cth))              deallocate(data%cth)
   if (associated(data%cth_uncertainty))  deallocate(data%cth_uncertainty)
   if (associated(data%cth_corrected))    deallocate(data%cth_corrected)
   if (associated(data%cth_corrected_uncertainty)) &
                                          deallocate(data%cth_corrected_uncertainty)
   if (associated(data%ctt))              deallocate(data%ctt)
   if (associated(data%ctt_uncertainty))  deallocate(data%ctt_uncertainty)
   if (associated(data%ctt_corrected))    deallocate(data%ctt_corrected)
   if (associated(data%ctt_corrected_uncertainty)) &
                                          deallocate(data%ctt_corrected_uncertainty)
   if (associated(data%cwp))              deallocate(data%cwp)
   if (associated(data%cwp_uncertainty))  deallocate(data%cwp_uncertainty)

   if (associated(data%cot2))             deallocate(data%cot2)
   if (associated(data%cot2_uncertainty)) deallocate(data%cot2_uncertainty)
   if (associated(data%cer2))             deallocate(data%cer2)
   if (associated(data%cer2_uncertainty)) deallocate(data%cer2_uncertainty)
   if (associated(data%ctp2))             deallocate(data%ctp2)
   if (associated(data%ctp2_uncertainty)) deallocate(data%ctp2_uncertainty)
   if (associated(data%cc_total2))        deallocate(data%cc_total2)
   if (associated(data%cc_total2_uncertainty)) &
                                          deallocate(data%cc_total2_uncertainty)
   if (associated(data%cth2))             deallocate(data%cth2)
   if (associated(data%cth2_uncertainty)) deallocate(data%cth2_uncertainty)
   if (associated(data%ctt2))             deallocate(data%ctt2)
   if (associated(data%ctt2_uncertainty)) deallocate(data%ctt2_uncertainty)
   if (associated(data%cwp2))             deallocate(data%cwp2)
   if (associated(data%cwp2_uncertainty)) deallocate(data%cwp2_uncertainty)

   if (associated(data%cloud_albedo))     deallocate(data%cloud_albedo)
   if (associated(data%cloud_albedo_uncertainty)) &
                                          deallocate(data%cloud_albedo_uncertainty)
   if (associated(data%cee))              deallocate(data%cee)
   if (associated(data%cee_uncertainty))  deallocate(data%cee_uncertainty)

   if (associated(data%cccot_pre))        deallocate(data%cccot_pre)

   if (associated(data%ann_phase))        deallocate(data%ann_phase)
   if (associated(data%ann_phase_uncertainty)) &
                                          deallocate(data%ann_phase_uncertainty)
   if (associated(data%cphcot))           deallocate(data%cphcot)

   deallocate(data%niter)
   deallocate(data%costja)
   deallocate(data%costjm)
   deallocate(data%qcflag)
   deallocate(data%channels_used)
   deallocate(data%variables_retrieved)

   deallocate(data%time)
   deallocate(data%lat)
   deallocate(data%lon)

   deallocate(data%vid_sol_zen)
   deallocate(data%vid_sat_zen)
   deallocate(data%vid_rel_azi)
   deallocate(data%vid_sat_azi)
   deallocate(data%sol_zen)
   deallocate(data%sat_zen)
   deallocate(data%rel_azi)
   deallocate(data%sat_azi)

   deallocate(data%lsflag)
   deallocate(data%lusflag)
   deallocate(data%dem)

   deallocate(data%illum)
   deallocate(data%cldtype)

   if (associated(data%cldmask))          deallocate(data%cldmask)
   if (associated(data%cldmask_uncertainty)) &
                                          deallocate(data%cldmask_uncertainty)

   if (associated(data%phase))            deallocate(data%phase)
   if (associated(data%phase_pavolonis))  deallocate(data%phase_pavolonis)

   if (associated(data%y_id))             deallocate(data%y_id)
   if (associated(data%view_id))          deallocate(data%view_id)
   if (associated(data%ch_is))            deallocate(data%ch_is)
   if (associated(data%rho_flags))        deallocate(data%rho_flags)

end subroutine dealloc_output_data_primary


!-------------------------------------------------------------------------------
! Name: dealloc_output_data_secondary
!
! Purpose:
! Deallocate storage for secondary output file.
!
! Description and Algorithm details:
! 1) Deallocate all arrays.
!
! Arguments:
! Name        Type   In/Out/Both Description
! ------------------------------------------------------------------------------
! data        struct Both        Contents of secondard output file.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------
subroutine dealloc_output_data_secondary(data)

   implicit none

   type(output_data_secondary_t), intent(inout) :: data

   if (associated(data%aot550_ap))        deallocate(data%aot550_ap)
   if (associated(data%aot550_fg))        deallocate(data%aot550_fg)
   if (associated(data%aer_ap))           deallocate(data%aer_ap)
   if (associated(data%aer_fg))           deallocate(data%aer_fg)

   if (associated(data%vid_rho_ap))       deallocate(data%vid_rho_ap)
   if (associated(data%vid_rho_fg))       deallocate(data%vid_rho_fg)
   if (associated(data%rho_ap))           deallocate(data%rho_ap)
   if (associated(data%rho_fg))           deallocate(data%rho_fg)

   if (associated(data%vid_swansea_s_ap)) deallocate(data%vid_swansea_s_ap)
   if (associated(data%vid_swansea_s_fg)) deallocate(data%vid_swansea_s_fg)
   if (associated(data%vid_swansea_p_ap)) deallocate(data%vid_swansea_p_ap)
   if (associated(data%vid_swansea_p_fg)) deallocate(data%vid_swansea_p_fg)
   if (associated(data%swansea_s_ap))     deallocate(data%swansea_s_ap)
   if (associated(data%swansea_s_fg))     deallocate(data%swansea_s_fg)
   if (associated(data%swansea_p_ap))     deallocate(data%swansea_p_ap)
   if (associated(data%swansea_p_fg))     deallocate(data%swansea_p_fg)

   if (associated(data%cot_ap))           deallocate(data%cot_ap)
   if (associated(data%cot_fg))           deallocate(data%cot_fg)
   if (associated(data%cer_ap))           deallocate(data%cer_ap)
   if (associated(data%cer_fg))           deallocate(data%cer_fg)
   if (associated(data%ctp_ap))           deallocate(data%ctp_ap)
   if (associated(data%ctp_fg))           deallocate(data%ctp_fg)
   if (associated(data%stemp_ap))         deallocate(data%stemp_ap)
   if (associated(data%stemp_fg))         deallocate(data%stemp_fg)

   if (associated(data%cot2_ap))          deallocate(data%cot2_ap)
   if (associated(data%cot2_fg))          deallocate(data%cot2_fg)
   if (associated(data%cer2_ap))          deallocate(data%cer2_ap)
   if (associated(data%cer2_fg))          deallocate(data%cer2_fg)
   if (associated(data%ctp2_ap))          deallocate(data%ctp2_ap)
   if (associated(data%ctp2_fg))          deallocate(data%ctp2_fg)

   if (associated(data%vid_albedo))       deallocate(data%vid_albedo)
   if (associated(data%albedo))           deallocate(data%albedo)

   deallocate(data%scanline_u)
   deallocate(data%scanline_v)

   deallocate(data%vid_channels)
   deallocate(data%channels_scale)
   deallocate(data%channels_offset)
   deallocate(data%channels_vmin)
   deallocate(data%channels_vmax)
   deallocate(data%channels)

   if (associated(data%Sy)) then
      deallocate(data%Sy)
      deallocate(data%vid_Sy)
      deallocate(data%Sy_scale)
      deallocate(data%Sy_offset)
      deallocate(data%Sy_vmin)
      deallocate(data%Sy_vmax)
   end if

   deallocate(data%vid_y0)
   deallocate(data%y0_scale)
   deallocate(data%y0_offset)
   deallocate(data%y0_vmin)
   deallocate(data%y0_vmax)
   deallocate(data%y0)

   deallocate(data%vid_residuals)
   deallocate(data%residuals_scale)
   deallocate(data%residuals_offset)
   deallocate(data%residuals_vmin)
   deallocate(data%residuals_vmax)
   deallocate(data%residuals)

   deallocate(data%ds)

   if (associated(data%vid_covariance)) deallocate(data%vid_covariance)
   if (associated(data%covariance))     deallocate(data%covariance)

end subroutine dealloc_output_data_secondary
