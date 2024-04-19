!-------------------------------------------------------------------------------
! Name: dealloc_input_data.F90
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
! 2012/02/15, CP: To do level 2 post processing
! 2012/03/07, MS: Added missing stemp_ap
! 2012/03/07, CP: Cleaned up
! 2012/03/18, CP: Modified to add cloud flag
! 2012/06/20, CP: Added albedo
! 2012/07/04, MJ: Fixed several data type bugs
! 2012/07/06, MJ: Extensively overhauls and restructures the code
! 2013/01/17, MJ: Adds code to accommodate uncertainties of ctt and cth
! 2014/09/20, change phase from 2 to 1, changed arguments of
!    set_input_data_secondary added in channels
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
! 2017/01/09, CP: ML additions.
! 2017/06/22, OS: Added phase variables.
! 2017/07/05, AP: Add channels_used, variables_retrieved. New QC.
! 2018/06/08, SP: Add satellite azimuth angle to output.
! 2023/10/10, GT: Added measurement uncertainties to secondary data
! 2023/11/21, GT: Added dealloc_input_data_primary_classify subroutine.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine dealloc_input_data_primary_common(data)

   implicit none

   type(input_data_primary_t), intent(inout) :: data

   if (associated(data%aot550))        deallocate(data%aot550)
   if (associated(data%aot550_uncertainty)) &
                                       deallocate(data%aot550_uncertainty)
   if (associated(data%aot870))        deallocate(data%aot870)
   if (associated(data%aot870_uncertainty)) &
                                       deallocate(data%aot870_uncertainty)
   if (associated(data%aer))           deallocate(data%aer)
   if (associated(data%aer_uncertainty)) &
                                       deallocate(data%aer_uncertainty)

   if (associated(data%rho))           deallocate(data%rho)
   if (associated(data%rho_uncertainty)) &
                                       deallocate(data%rho_uncertainty)

   if (associated(data%swansea_s))     deallocate(data%swansea_s)
   if (associated(data%swansea_s_uncertainty)) &
                                       deallocate(data%swansea_s_uncertainty)
   if (associated(data%swansea_p))     deallocate(data%swansea_p)
   if (associated(data%swansea_p_uncertainty)) &
                                       deallocate(data%swansea_p_uncertainty)
   if (associated(data%diffuse_frac))  deallocate(data%diffuse_frac)
   if (associated(data%diffuse_frac_uncertainty)) &
                                       deallocate(data%diffuse_frac_uncertainty)

   if (associated(data%cot))           deallocate(data%cot)
   if (associated(data%cot_uncertainty)) &
                                       deallocate(data%cot_uncertainty)
   if (associated(data%cer))           deallocate(data%cer)
   if (associated(data%cer_uncertainty)) &
                                       deallocate(data%cer_uncertainty)
   if (associated(data%ctp))           deallocate(data%ctp)
   if (associated(data%ctp_uncertainty)) &
                                       deallocate(data%ctp_uncertainty)
   if (associated(data%ctp_corrected)) deallocate(data%ctp_corrected)
   if (associated(data%ctp_corrected_uncertainty)) &
                                       deallocate(data%ctp_corrected_uncertainty)
   if (associated(data%cc_total))      deallocate(data%cc_total)
   if (associated(data%cc_total_uncertainty)) &
                                       deallocate(data%cc_total_uncertainty)
   if (associated(data%stemp))         deallocate(data%stemp)
   if (associated(data%stemp_uncertainty)) &
                                       deallocate(data%stemp_uncertainty)
   if (associated(data%cth))           deallocate(data%cth)
   if (associated(data%cth_uncertainty)) &
                                       deallocate(data%cth_uncertainty)
   if (associated(data%cth_corrected)) deallocate(data%cth_corrected)
   if (associated(data%cth_corrected_uncertainty)) &
                                       deallocate(data%cth_corrected_uncertainty)
   if (associated(data%ctt))           deallocate(data%ctt)
   if (associated(data%ctt_uncertainty)) &
                                       deallocate(data%ctt_uncertainty)
   if (associated(data%ctt_corrected)) deallocate(data%ctt_corrected)
   if (associated(data%ctt_corrected_uncertainty)) &
                                       deallocate(data%ctt_corrected_uncertainty)
   if (associated(data%cwp))           deallocate(data%cwp)
   if (associated(data%cwp_uncertainty)) &
                                       deallocate(data%cwp_uncertainty)
   if (associated(data%cloud_albedo))  deallocate(data%cloud_albedo)
   if (associated(data%cloud_albedo_uncertainty)) &
                                       deallocate(data%cloud_albedo_uncertainty)
   if (associated(data%cee))           deallocate(data%cee)
   if (associated(data%cee_uncertainty)) &
                                       deallocate(data%cee_uncertainty)

   if (associated(data%cot2))          deallocate(data%cot2)
   if (associated(data%cot2_uncertainty)) &
                                       deallocate(data%cot2_uncertainty)
   if (associated(data%cer2))          deallocate(data%cer2)
   if (associated(data%cer2_uncertainty)) &
                                       deallocate(data%cer2_uncertainty)
   if (associated(data%ctp2))          deallocate(data%ctp2)
   if (associated(data%ctp2_uncertainty)) &
                                       deallocate(data%ctp2_uncertainty)
   if (associated(data%cth2))          deallocate(data%cth2)
   if (associated(data%cth2_uncertainty)) &
                                       deallocate(data%cth2_uncertainty)
   if (associated(data%ctt2))          deallocate(data%ctt2)
   if (associated(data%ctt2_uncertainty)) &
                                       deallocate(data%ctt2_uncertainty)
   if (associated(data%cwp2))          deallocate(data%cwp2)
   if (associated(data%cwp2_uncertainty)) &
                                       deallocate(data%cwp2_uncertainty)

   if (associated(data%niter))         deallocate(data%niter)
   if (associated(data%costja))        deallocate(data%costja)
   if (associated(data%costjm))        deallocate(data%costjm)
   if (associated(data%qcflag))        deallocate(data%qcflag)
   if (associated(data%channels_used)) deallocate(data%channels_used)
   if (associated(data%variables_retrieved)) &
                                       deallocate(data%variables_retrieved)

end subroutine dealloc_input_data_primary_common


subroutine dealloc_input_data_primary_all(data)

   implicit none

   type(input_data_primary_t), intent(inout) :: data

   call dealloc_input_data_primary_common(data)

   if (associated(data%cccot_pre)) deallocate(data%cccot_pre)

   deallocate(data%time)
   deallocate(data%lat)
   deallocate(data%lon)
   deallocate(data%sol_zen)
   deallocate(data%sat_zen)
   deallocate(data%rel_azi)
   deallocate(data%sat_azi)

   deallocate(data%lsflag)
   deallocate(data%lusflag)
   deallocate(data%dem)

   deallocate(data%illum)

   deallocate(data%cldtype)
   if (associated(data%cldmask))       deallocate(data%cldmask)
   if (associated(data%cldmask_uncertainty)) &
                                       deallocate(data%cldmask_uncertainty)

   if (associated(data%phase))         deallocate(data%phase)
   if (associated(data%phase_pavolonis)) &
                                       deallocate(data%phase_pavolonis)

   if (associated(data%ann_phase))     deallocate(data%ann_phase)
   if (associated(data%ann_phase_uncertainty)) &
                                       deallocate(data%ann_phase_uncertainty)
   if (associated(data%cphcot))        deallocate(data%cphcot)

end subroutine dealloc_input_data_primary_all


subroutine dealloc_input_data_primary_class(data)

   implicit none

   type(input_data_primary_t), intent(inout) :: data

   call dealloc_input_data_primary_common(data)

end subroutine dealloc_input_data_primary_class


subroutine dealloc_input_data_primary_classify(data)

   implicit none

   type(input_data_primary_t), intent(inout) :: data

   call dealloc_input_data_primary_common(data)

end subroutine dealloc_input_data_primary_classify



subroutine dealloc_input_data_secondary_common(data)

   implicit none

   type(input_data_secondary_t), intent(inout) :: data

   if (associated(data%aot550_ap))     deallocate(data%aot550_ap)
   if (associated(data%aot550_fg))     deallocate(data%aot550_fg)
   if (associated(data%aer_ap))        deallocate(data%aer_ap)
   if (associated(data%aer_fg))        deallocate(data%aer_fg)

   if (associated(data%rho_ap))        deallocate(data%rho_ap)
   if (associated(data%rho_fg))        deallocate(data%rho_fg)

   if (associated(data%swansea_s_ap))  deallocate(data%swansea_s_ap)
   if (associated(data%swansea_s_fg))  deallocate(data%swansea_s_fg)
   if (associated(data%swansea_p_ap))  deallocate(data%swansea_p_ap)
   if (associated(data%swansea_p_fg))  deallocate(data%swansea_p_fg)

   if (associated(data%cot_ap))        deallocate(data%cot_ap)
   if (associated(data%cot_fg))        deallocate(data%cot_fg)
   if (associated(data%cer_ap))        deallocate(data%cer_ap)
   if (associated(data%cer_fg))        deallocate(data%cer_fg)
   if (associated(data%ctp_ap))        deallocate(data%ctp_ap)
   if (associated(data%ctp_fg))        deallocate(data%ctp_fg)
   if (associated(data%stemp_ap))      deallocate(data%stemp_ap)
   if (associated(data%stemp_fg))      deallocate(data%stemp_fg)


   if (associated(data%cot2_ap))       deallocate(data%cot2_ap)
   if (associated(data%cot2_fg))       deallocate(data%cot2_fg)
   if (associated(data%cer2_ap))       deallocate(data%cer2_ap)
   if (associated(data%cer2_fg))       deallocate(data%cer2_fg)
   if (associated(data%ctp2_ap))       deallocate(data%ctp2_ap)
   if (associated(data%ctp2_fg))       deallocate(data%ctp2_fg)

   if (associated(data%Sy))            deallocate(data%Sy)
   if (associated(data%y0))            deallocate(data%y0)
   if (associated(data%residuals))     deallocate(data%residuals)
   if (associated(data%ds))            deallocate(data%ds)

end subroutine dealloc_input_data_secondary_common


subroutine dealloc_input_data_secondary_all(data)

   implicit none

   type(input_data_secondary_t), intent(inout) :: data

   call dealloc_input_data_secondary_common(data)

   if (associated(data%albedo))       deallocate(data%albedo)

   deallocate(data%channels)

end subroutine dealloc_input_data_secondary_all


subroutine dealloc_input_data_secondary_class(data)

   implicit none

   type(input_data_secondary_t), intent(inout) :: data

   call dealloc_input_data_secondary_common(data)

end subroutine dealloc_input_data_secondary_class
