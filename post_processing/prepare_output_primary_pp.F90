!-------------------------------------------------------------------------------
! Name: prepare_output_primary.F90
!
! Purpose:
! Map internal representation of variables to output representation by applying
! scale and offset where necessary.
!
! Description and Algorithm details:
!
! Arguments:
! Name Type In/Out/Both Description
!
! History:
! 2011/12/19, MJ: Creates initial output for main ooutput variables.
! 2012/01/06, MJ: Added in cwp
! 2012/01/16, CP: Bug fix: changed how offset applied
! 2012/07/06, MJ: Extensively overhauls and restructures the code
! 2012/07/17, MJ: Fixes bug in CWP write.
! 2012/07/31, MJ: Fixes bug in CTY write.
! 2013/01/17, MJ: Adds code to accommodate uncertainties of ctt and cth
! 2014/01/22, MJ: Fixes FP overflow with COT.
! 2014/07/08, CP: Added more illumination options
! 2014/10/24, OS: Added variables cccot_pre, lusflag, cldtype, cloudmask, DEM
!    (currently deactivated), and nisemask
! 2014/11/20, OS: Added Pavolonis cloud phase variable, which is here classified
!    through Pavolonis cloud types
! 2014/11/20, CP: Added cloud albedo
! 2015/02/07, CP: Changed to common constants
! 2015/07/16, GM: Major cleanup.
! 2015/10/07, Oliver Sus: renamed to *_pp.F90, as we have to avoid duplicate
!    subroutine names for wrapper
! 2015/10/22, GM: Add cloud albedo uncertainty.
! 2015/11/17, OS: Including switched types when estimating phase.
! 2015/11/27, CP: Added additional cloud type prob clear
! 2016/01/27, GM: Add cee and cee_uncertainty.
! 2016/01/28, GM: Add ctp and ctt corrected and corrected_uncertianty.
! 2016/02/10, GM: Remove redundant checks for fill_value.
! 2016/03/04, AP: Tidy prepare_*_packed_float.
! 2017/01/09, CP: ML additions.
! 2017/07/05, AP: Add channels_used, variables_retrieved. New QC.
! 2018/06/08, SP: Add satellite azimuth angle to output.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine prepare_output_primary_pp(i, j, indexing, input_data, output_data, &
     output_optical_props_at_night)


   use constants_cloud_typing_pavolonis_m
   use orac_input_m
   use orac_ncdf_m
   use orac_output_m
   use postproc_constants_m

   implicit none


   integer,                     intent(in)    :: i, j
   type(common_indices_t),      intent(in)    :: indexing
   type(input_data_primary_t),  intent(in)    :: input_data
   type(output_data_primary_t), intent(inout) :: output_data
   logical,                     intent(in)    :: output_optical_props_at_night

   integer :: k, l, i_rho

   !----------------------------------------------------------------------------
   ! time
   !----------------------------------------------------------------------------
   output_data%time(i,j)=input_data%time(i,j)

   !----------------------------------------------------------------------------
   ! lat, lon
   !----------------------------------------------------------------------------
   call prepare_float_packed_float( &
        input_data%lat(i,j), output_data%lat(i,j), &
        output_data%lat_scale, output_data%lat_offset, &
        output_data%lat_vmin, output_data%lat_vmax, &
        sreal_fill_value, sreal_fill_value)
   call prepare_float_packed_float( &
        input_data%lon(i,j), output_data%lon(i,j), &
        output_data%lon_scale, output_data%lon_offset, &
        output_data%lon_vmin, output_data%lon_vmax, &
        sreal_fill_value, sreal_fill_value)

   !----------------------------------------------------------------------------
   ! sol_zen, sat_zen, rel_azi
   !----------------------------------------------------------------------------
   do k = 1, indexing%NViews
      call prepare_float_packed_float( &
           input_data%sat_zen(i,j,k), output_data%sat_zen(i,j,k), &
           output_data%sol_scale, output_data%sol_offset, &
           output_data%sol_vmin, output_data%sol_vmax, &
           sreal_fill_value, sreal_fill_value)
      call prepare_float_packed_float( &
           input_data%sol_zen(i,j,k), output_data%sol_zen(i,j,k), &
           output_data%sat_scale, output_data%sat_offset, &
           output_data%sat_vmin, output_data%sat_vmax, &
           sreal_fill_value, sreal_fill_value)
      call prepare_float_packed_float( &
           input_data%rel_azi(i,j,k), output_data%rel_azi(i,j,k), &
           output_data%azi_scale, output_data%azi_offset, &
           output_data%azi_vmin, output_data%azi_vmax, &
           sreal_fill_value, sreal_fill_value)
      call prepare_float_packed_float( &
           input_data%sat_azi(i,j,k), output_data%sat_azi(i,j,k), &
           output_data%azi_scale, output_data%azi_offset, &
           output_data%azi_vmin, output_data%azi_vmax, &
           sreal_fill_value, sreal_fill_value)
   end do

   if (indexing%flags%do_aerosol) then
      !-------------------------------------------------------------------------
      ! aot, aot_uncertainty
      !-------------------------------------------------------------------------
      call prepare_short_packed_float( &
           input_data%aot550(i,j), output_data%aot550(i,j), &
           output_data%aot550_scale, output_data%aot550_offset, &
           output_data%aot550_vmin, output_data%aot550_vmax, &
           sreal_fill_value, output_data%aot550_vmax)

      call prepare_short_packed_float( &
           input_data%aot550_uncertainty(i,j), &
           output_data%aot550_uncertainty(i,j), &
           output_data%aot550_uncertainty_scale, &
           output_data%aot550_uncertainty_offset, &
           output_data%aot550_uncertainty_vmin, &
           output_data%aot550_uncertainty_vmax, &
           sreal_fill_value, output_data%aot550_uncertainty_vmax)

      call prepare_short_packed_float( &
           input_data%aot870(i,j), output_data%aot870(i,j), &
           output_data%aot870_scale, output_data%aot870_offset, &
           output_data%aot870_vmin, output_data%aot870_vmax, &
           sreal_fill_value, output_data%aot870_vmax)

      call prepare_short_packed_float( &
           input_data%aot870_uncertainty(i,j), &
           output_data%aot870_uncertainty(i,j), &
           output_data%aot870_uncertainty_scale, &
           output_data%aot870_uncertainty_offset, &
           output_data%aot870_uncertainty_vmin, &
           output_data%aot870_uncertainty_vmax, &
           sreal_fill_value, output_data%aot870_uncertainty_vmax)

      !-------------------------------------------------------------------------
      ! aer, aer_uncertainty
      !-------------------------------------------------------------------------
      call prepare_short_packed_float( &
           input_data%aer(i,j), output_data%aer(i,j), &
           output_data%aer_scale, output_data%aer_offset, &
           output_data%aer_vmin, output_data%aer_vmax, &
           sreal_fill_value, output_data%aer_vmax)

      call prepare_short_packed_float( &
           input_data%aer_uncertainty(i,j), &
           output_data%aer_uncertainty(i,j), &
           output_data%aer_uncertainty_scale, &
           output_data%aer_uncertainty_offset, &
           output_data%aer_uncertainty_vmin, &
           output_data%aer_uncertainty_vmax, &
           sreal_fill_value, output_data%aer_uncertainty_vmax)
   end if

   if (indexing%flags%do_rho) then
      !-------------------------------------------------------------------------
      ! rho, rho_uncertainty
      !-------------------------------------------------------------------------
      i_rho = 0
      do k = 1, indexing%NSolar
         do l = 1, MaxRho_XX
            if (indexing%rho_terms(k,l)) then
               i_rho = i_rho + 1

               call prepare_short_packed_float( &
                    input_data%rho(i,j,i_rho), output_data%rho(i,j,i_rho), &
                    output_data%rho_scale, output_data%rho_offset, &
                    output_data%rho_vmin, output_data%rho_vmax, &
                    sreal_fill_value, output_data%rho_vmax)

               call prepare_short_packed_float( &
                    input_data%rho_uncertainty(i,j,i_rho), &
                    output_data%rho_uncertainty(i,j,i_rho), &
                    output_data%rho_uncertainty_scale, &
                    output_data%rho_uncertainty_offset, &
                    output_data%rho_uncertainty_vmin, &
                    output_data%rho_uncertainty_vmax, &
                    sreal_fill_value, output_data%rho_uncertainty_vmax)
            end if
         end do
      end do
   end if

   if (indexing%flags%do_swansea) then
      i_rho = 0
      do k = 1, indexing%NSolar
         if (indexing%ss_terms(k)) then
            i_rho = i_rho + 1

            !-------------------------------------------------------------------
            ! swansea_s, swansea_s_uncertainty
            !-------------------------------------------------------------------
            call prepare_short_packed_float( &
                 input_data%swansea_s(i,j,i_rho),&
                 output_data%swansea_s(i,j,i_rho), &
                 output_data%swansea_s_scale, &
                 output_data%swansea_s_offset, &
                 output_data%swansea_s_vmin, &
                 output_data%swansea_s_vmax, &
                 sreal_fill_value, output_data%swansea_s_vmax)

            call prepare_short_packed_float( &
                 input_data%swansea_s_uncertainty(i,j,i_rho), &
                 output_data%swansea_s_uncertainty(i,j,i_rho), &
                 output_data%swansea_s_uncertainty_scale, &
                 output_data%swansea_s_uncertainty_offset, &
                 output_data%swansea_s_uncertainty_vmin, &
                 output_data%swansea_s_uncertainty_vmax, &
                 sreal_fill_value, output_data%swansea_s_uncertainty_vmax)

            !-------------------------------------------------------------------
            ! diffuse_frac, diffuse_frac_uncertainty
            !-------------------------------------------------------------------
            call prepare_short_packed_float( &
                 input_data%diffuse_frac(i,j,i_rho), &
                 output_data%diffuse_frac(i,j,i_rho), &
                 output_data%diffuse_frac_scale, &
                 output_data%diffuse_frac_offset, &
                 output_data%diffuse_frac_vmin, &
                 output_data%diffuse_frac_vmax, &
                 sreal_fill_value, output_data%diffuse_frac_vmax)

            call prepare_short_packed_float( &
                 input_data%diffuse_frac_uncertainty(i,j,i_rho), &
                 output_data%diffuse_frac_uncertainty(i,j,i_rho), &
                 output_data%diffuse_frac_uncertainty_scale, &
                 output_data%diffuse_frac_uncertainty_offset, &
                 output_data%diffuse_frac_uncertainty_vmin, &
                 output_data%diffuse_frac_uncertainty_vmax, &
                 sreal_fill_value, output_data%diffuse_frac_uncertainty_vmax)
         end if
      end do

      !-------------------------------------------------------------------------
      ! swansea_p, swansea_p_uncertainty
      !-------------------------------------------------------------------------
      do k = 1, indexing%NViews
         call prepare_short_packed_float( &
              input_data%swansea_p(i,j,k), output_data%swansea_p(i,j,k), &
              output_data%swansea_p_scale, output_data%swansea_p_offset, &
              output_data%swansea_p_vmin, output_data%swansea_p_vmax, &
              sreal_fill_value, output_data%swansea_p_vmax)

         call prepare_short_packed_float( &
              input_data%swansea_p_uncertainty(i,j,k), &
              output_data%swansea_p_uncertainty(i,j,k), &
              output_data%swansea_p_uncertainty_scale, &
              output_data%swansea_p_uncertainty_offset, &
              output_data%swansea_p_uncertainty_vmin, &
              output_data%swansea_p_uncertainty_vmax, &
              sreal_fill_value, output_data%swansea_p_uncertainty_vmax)
      end do
   end if

   if (indexing%flags%do_cloud) then
      !-------------------------------------------------------------------------
      ! cot, cot_uncertainty
      !-------------------------------------------------------------------------
      if (input_data%illum(i,j) .eq. 1_byte .or. &
           output_optical_props_at_night) then

         call prepare_short_packed_float( &
              input_data%cot(i,j), output_data%cot(i,j), &
              output_data%cot_scale, output_data%cot_offset, &
              output_data%cot_vmin, output_data%cot_vmax, &
              sreal_fill_value, output_data%cot_vmax)

         call prepare_short_packed_float( &
              input_data%cot_uncertainty(i,j), &
              output_data%cot_uncertainty(i,j), &
              output_data%cot_uncertainty_scale, &
              output_data%cot_uncertainty_offset, &
              output_data%cot_uncertainty_vmin, &
              output_data%cot_uncertainty_vmax, &
              sreal_fill_value, output_data%cot_uncertainty_vmax)

         !-----------------------------------------------------------------------
         ! cer, cer_uncertainty
         !-----------------------------------------------------------------------
         call prepare_short_packed_float( &
              input_data%cer(i,j), output_data%cer(i,j), &
              output_data%cer_scale, output_data%cer_offset, &
              output_data%cer_vmin, output_data%cer_vmax, &
              sreal_fill_value, output_data%cer_vmax)

         call prepare_short_packed_float( &
              input_data%cer_uncertainty(i,j), &
              output_data%cer_uncertainty(i,j), &
              output_data%cer_uncertainty_scale, &
              output_data%cer_uncertainty_offset, &
              output_data%cer_uncertainty_vmin, &
              output_data%cer_uncertainty_vmax, &
              sreal_fill_value, output_data%cer_uncertainty_vmax)

         !----------------------------------------------------------------------
         ! cwp, cwp_uncertainty
         !----------------------------------------------------------------------
         call prepare_short_packed_float( &
              input_data%cwp(i,j), output_data%cwp(i,j), &
              output_data%cwp_scale, output_data%cwp_offset, &
              output_data%cwp_vmin, output_data%cwp_vmax, &
              sreal_fill_value, output_data%cwp_vmax)

         call prepare_short_packed_float( &
              input_data%cwp_uncertainty(i,j), &
              output_data%cwp_uncertainty(i,j), &
              output_data%cwp_uncertainty_scale, &
              output_data%cwp_uncertainty_offset, &
              output_data%cwp_uncertainty_vmin, &
              output_data%cwp_uncertainty_vmax, &
              sreal_fill_value, output_data%cwp_uncertainty_vmax)
      end if !day


      !-------------------------------------------------------------------------
      ! ctp, ctp_uncertainty
      !-------------------------------------------------------------------------
      call prepare_short_packed_float( &
           input_data%ctp(i,j), output_data%ctp(i,j), &
           output_data%ctp_scale, output_data%ctp_offset, &
           output_data%ctp_vmin, output_data%ctp_vmax, &
           sreal_fill_value, output_data%ctp_vmax)

      call prepare_short_packed_float( &
           input_data%ctp_uncertainty(i,j), &
           output_data%ctp_uncertainty(i,j), &
           output_data%ctp_uncertainty_scale, &
           output_data%ctp_uncertainty_offset, &
           output_data%ctp_uncertainty_vmin, &
           output_data%ctp_uncertainty_vmax, &
           sreal_fill_value, output_data%ctp_uncertainty_vmax)

      !-------------------------------------------------------------------------
      ! ctp_corrected, ctp_corrected_uncertainty
      !-------------------------------------------------------------------------
      call prepare_short_packed_float( &
           input_data%ctp_corrected(i,j), output_data%ctp_corrected(i,j), &
           output_data%ctp_scale, output_data%ctp_offset, &
           output_data%ctp_vmin, output_data%ctp_vmax, &
           sreal_fill_value, output_data%ctp_vmax)

      call prepare_short_packed_float( &
           input_data%ctp_corrected_uncertainty(i,j), &
           output_data%ctp_corrected_uncertainty(i,j), &
           output_data%ctp_uncertainty_scale, &
           output_data%ctp_uncertainty_offset, &
           output_data%ctp_uncertainty_vmin, &
           output_data%ctp_uncertainty_vmax, &
           sreal_fill_value, output_data%ctp_uncertainty_vmax)

      !-------------------------------------------------------------------------
      ! cc_total, cc_total_uncertainty
      !-------------------------------------------------------------------------
      call prepare_short_packed_float( &
           input_data%cc_total(i,j), output_data%cc_total(i,j), &
           output_data%cc_total_scale, output_data%cc_total_offset, &
           output_data%cc_total_vmin, output_data%cc_total_vmax, &
           sreal_fill_value, output_data%cc_total_vmax)

      call prepare_short_packed_float( &
           input_data%cc_total_uncertainty(i,j), &
           output_data%cc_total_uncertainty(i,j), &
           output_data%cc_total_uncertainty_scale, &
           output_data%cc_total_uncertainty_offset, &
           output_data%cc_total_uncertainty_vmin, &
           output_data%cc_total_uncertainty_vmax, &
           sreal_fill_value, output_data%cc_total_uncertainty_vmax)

      !-------------------------------------------------------------------------
      ! stemp, stemp_uncertainty
      !-------------------------------------------------------------------------
      call prepare_short_packed_float( &
           input_data%stemp(i,j), output_data%stemp(i,j), &
           output_data%stemp_scale, output_data%stemp_offset, &
           output_data%stemp_vmin, output_data%stemp_vmax, &
           sreal_fill_value, output_data%stemp_vmax)

      call prepare_short_packed_float( &
           input_data%stemp_uncertainty(i,j), &
           output_data%stemp_uncertainty(i,j), &
           output_data%stemp_uncertainty_scale, &
           output_data%stemp_uncertainty_offset, &
           output_data%stemp_uncertainty_vmin, &
           output_data%stemp_uncertainty_vmax, &
           sreal_fill_value, output_data%stemp_uncertainty_vmax)

      !-------------------------------------------------------------------------
      ! cth, cth_uncertainty
      !-------------------------------------------------------------------------
      call prepare_short_packed_float( &
           input_data%cth(i,j), output_data%cth(i,j), &
           output_data%cth_scale, output_data%cth_offset, &
           output_data%cth_vmin, output_data%cth_vmax, &
           sreal_fill_value, output_data%cth_vmax)

      call prepare_short_packed_float( &
           input_data%cth_uncertainty(i,j), &
           output_data%cth_uncertainty(i,j), &
           output_data%cth_uncertainty_scale, &
           output_data%cth_uncertainty_offset, &
           output_data%cth_uncertainty_vmin, &
           output_data%cth_uncertainty_vmax, &
           sreal_fill_value, output_data%cth_uncertainty_vmax)

      !-------------------------------------------------------------------------
      ! cth_corrected, cth_corrected_uncertainty
      !-------------------------------------------------------------------------
      call prepare_short_packed_float( &
           input_data%cth_corrected(i,j), output_data%cth_corrected(i,j), &
           output_data%cth_scale, output_data%cth_offset, &
           output_data%cth_vmin, output_data%cth_vmax, &
           sreal_fill_value, output_data%cth_vmax)

      call prepare_short_packed_float( &
           input_data%cth_corrected_uncertainty(i,j), &
           output_data%cth_corrected_uncertainty(i,j), &
           output_data%cth_uncertainty_scale, &
           output_data%cth_uncertainty_offset, &
           output_data%cth_uncertainty_vmin, &
           output_data%cth_uncertainty_vmax, &
           sreal_fill_value, output_data%cth_uncertainty_vmax)

      !-------------------------------------------------------------------------
      ! ctt, ctt_uncertainty
      !-------------------------------------------------------------------------
      call prepare_short_packed_float( &
           input_data%ctt(i,j), output_data%ctt(i,j), &
           output_data%ctt_scale, output_data%ctt_offset, &
           output_data%ctt_vmin, output_data%ctt_vmax, &
           sreal_fill_value, output_data%ctt_vmax)

      call prepare_short_packed_float( &
           input_data%ctt_uncertainty(i,j), &
           output_data%ctt_uncertainty(i,j), &
           output_data%ctt_uncertainty_scale, &
           output_data%ctt_uncertainty_offset, &
           output_data%ctt_uncertainty_vmin, &
           output_data%ctt_uncertainty_vmax, &
           sreal_fill_value, output_data%ctt_uncertainty_vmax)

      !-------------------------------------------------------------------------
      ! ctt_corrected, ctt_corrected_uncertainty
      !-------------------------------------------------------------------------
      call prepare_short_packed_float( &
           input_data%ctt_corrected(i,j), output_data%ctt_corrected(i,j), &
           output_data%ctt_scale, output_data%ctt_offset, &
           output_data%ctt_vmin, output_data%ctt_vmax, &
           sreal_fill_value, output_data%ctt_vmax)

      call prepare_short_packed_float( &
           input_data%ctt_corrected_uncertainty(i,j), &
           output_data%ctt_corrected_uncertainty(i,j), &
           output_data%ctt_uncertainty_scale, &
           output_data%ctt_uncertainty_offset, &
           output_data%ctt_uncertainty_vmin, &
           output_data%ctt_uncertainty_vmax, &
           sreal_fill_value, output_data%ctt_uncertainty_vmax)

      !-------------------------------------------------------------------------
      ! cloud_albedo
      !-------------------------------------------------------------------------
      do k = 1, indexing%NSolar
         call prepare_short_packed_float( &
              input_data%cloud_albedo(i,j,k), output_data%cloud_albedo(i,j,k), &
              output_data%cloud_albedo_scale, output_data%cloud_albedo_offset, &
              output_data%cloud_albedo_vmin, output_data%cloud_albedo_vmax, &
              sreal_fill_value, output_data%cloud_albedo_vmax)

         call prepare_short_packed_float( &
              input_data%cloud_albedo_uncertainty(i,j,k), &
              output_data%cloud_albedo_uncertainty(i,j,k), &
              output_data%cloud_albedo_uncertainty_scale, &
              output_data%cloud_albedo_uncertainty_offset, &
              output_data%cloud_albedo_uncertainty_vmin, &
              output_data%cloud_albedo_uncertainty_vmax, &
              sreal_fill_value, output_data%cloud_albedo_uncertainty_vmax)
      end do

      !-------------------------------------------------------------------------
      ! cee
      !-------------------------------------------------------------------------
      do k = 1, indexing%NThermal
         call prepare_short_packed_float( &
              input_data%cee(i,j,k), output_data%cee(i,j,k), &
              output_data%cee_scale, output_data%cee_offset, &
              output_data%cee_vmin, output_data%cee_vmax, &
              sreal_fill_value, output_data%cee_vmax)

         call prepare_short_packed_float( &
              input_data%cee_uncertainty(i,j,k), &
              output_data%cee_uncertainty(i,j,k), &
              output_data%cee_uncertainty_scale, &
              output_data%cee_uncertainty_offset, &
              output_data%cee_uncertainty_vmin, &
              output_data%cee_uncertainty_vmax, &
              sreal_fill_value, output_data%cee_uncertainty_vmax)
      end do

      !-------------------------------------------------------------------------
      ! cccot_pre
      !-------------------------------------------------------------------------
      do k = 1, indexing%NViews
         call prepare_short_packed_float( &
              input_data%cccot_pre(i,j,k), output_data%cccot_pre(i,j,k), &
              output_data%cccot_pre_scale, output_data%cccot_pre_offset, &
              output_data%cccot_pre_vmin, output_data%cccot_pre_vmax, &
              sreal_fill_value, sint_fill_value)
      end do
   end if


   if (indexing%flags%do_cloud_layer_2) then

      if (input_data%illum(i,j) .eq. 1_byte .or. &
           output_optical_props_at_night) then
         !----------------------------------------------------------------------
         ! cot2, cot2_uncertainty
         !----------------------------------------------------------------------

         call prepare_short_packed_float( &
              input_data%cot2(i,j), output_data%cot2(i,j), &
              output_data%cot_scale, output_data%cot_offset, &
              output_data%cot_vmin, output_data%cot_vmax, &
              sreal_fill_value, output_data%cot_vmax)

         call prepare_short_packed_float( &
              input_data%cot2_uncertainty(i,j), &
              output_data%cot2_uncertainty(i,j), &
              output_data%cot_uncertainty_scale, &
              output_data%cot_uncertainty_offset, &
              output_data%cot_uncertainty_vmin, &
              output_data%cot_uncertainty_vmax, &
              sreal_fill_value, output_data%cot_uncertainty_vmax)

         !-----------------------------------------------------------------------
         ! cer2, cer2_uncertainty
         !-----------------------------------------------------------------------
         call prepare_short_packed_float( &
              input_data%cer2(i,j), output_data%cer2(i,j), &
              output_data%cer_scale, output_data%cer_offset, &
              output_data%cer_vmin, output_data%cer_vmax, &
              sreal_fill_value, output_data%cer_vmax)

         call prepare_short_packed_float( &
              input_data%cer2_uncertainty(i,j), &
              output_data%cer2_uncertainty(i,j), &
              output_data%cer_uncertainty_scale, &
              output_data%cer_uncertainty_offset, &
              output_data%cer_uncertainty_vmin, &
              output_data%cer_uncertainty_vmax, &
              sreal_fill_value, output_data%cer_uncertainty_vmax)

         !----------------------------------------------------------------------
         ! cwp2, cwp2_uncertainty
         !----------------------------------------------------------------------
         call prepare_short_packed_float( &
              input_data%cwp2(i,j), output_data%cwp2(i,j), &
              output_data%cwp_scale, output_data%cwp_offset, &
              output_data%cwp_vmin, output_data%cwp_vmax, &
              sreal_fill_value, output_data%cwp_vmax)

         call prepare_short_packed_float( &
              input_data%cwp2_uncertainty(i,j), &
              output_data%cwp2_uncertainty(i,j), &
              output_data%cwp_uncertainty_scale, &
              output_data%cwp_uncertainty_offset, &
              output_data%cwp_uncertainty_vmin, &
              output_data%cwp_uncertainty_vmax, &
              sreal_fill_value, output_data%cwp_uncertainty_vmax)
      end if

      !-------------------------------------------------------------------------
      ! ctp2, ctp2_uncertainty
      !-------------------------------------------------------------------------
      call prepare_short_packed_float( &
           input_data%ctp2(i,j), output_data%ctp2(i,j), &
           output_data%ctp_scale, output_data%ctp_offset, &
           output_data%ctp_vmin, output_data%ctp_vmax, &
           sreal_fill_value, output_data%ctp_vmax)

      call prepare_short_packed_float( &
           input_data%ctp2_uncertainty(i,j), &
           output_data%ctp2_uncertainty(i,j), &
           output_data%ctp_uncertainty_scale, &
           output_data%ctp_uncertainty_offset, &
           output_data%ctp_uncertainty_vmin, &
           output_data%ctp_uncertainty_vmax, &
           sreal_fill_value, output_data%ctp_uncertainty_vmax)

      !-------------------------------------------------------------------------
      ! cth2, cth2_uncertainty
      !-------------------------------------------------------------------------
      call prepare_short_packed_float( &
           input_data%cth2(i,j), output_data%cth2(i,j), &
           output_data%cth_scale, output_data%cth_offset, &
           output_data%cth_vmin, output_data%cth_vmax, &
           sreal_fill_value, output_data%cth_vmax)

      call prepare_short_packed_float( &
           input_data%cth2_uncertainty(i,j), &
           output_data%cth2_uncertainty(i,j), &
           output_data%cth_uncertainty_scale, &
           output_data%cth_uncertainty_offset, &
           output_data%cth_uncertainty_vmin, &
           output_data%cth_uncertainty_vmax, &
           sreal_fill_value, output_data%cth_uncertainty_vmax)

      !-------------------------------------------------------------------------
      ! ctt2, ctt2_uncertainty
      !-------------------------------------------------------------------------
      call prepare_short_packed_float( &
           input_data%ctt2(i,j), output_data%ctt2(i,j), &
           output_data%ctt_scale, output_data%ctt_offset, &
           output_data%ctt_vmin, output_data%ctt_vmax, &
           sreal_fill_value, output_data%ctt_vmax)

      call prepare_short_packed_float( &
           input_data%ctt2_uncertainty(i,j), &
           output_data%ctt2_uncertainty(i,j), &
           output_data%ctt_uncertainty_scale, &
           output_data%ctt_uncertainty_offset, &
           output_data%ctt_uncertainty_vmin, &
           output_data%ctt_uncertainty_vmax, &
           sreal_fill_value, output_data%ctt_uncertainty_vmax)
   end if

   !----------------------------------------------------------------------------
   ! ANN phase
   !----------------------------------------------------------------------------
   if (indexing%flags%do_ann_phase) then
      do k = 1, indexing%NViews
         call prepare_short_packed_float( &
              input_data%cphcot(i,j,k), output_data%cphcot(i,j,k), &
              output_data%cphcot_scale, output_data%cphcot_offset, &
              output_data%cphcot_vmin, output_data%cphcot_vmax, &
              sreal_fill_value, sint_fill_value)
         call prepare_short_packed_float( &
              input_data%ann_phase_uncertainty(i,j,k), &
              output_data%ann_phase_uncertainty(i,j,k), &
              output_data%ann_phase_uncertainty_scale, &
              output_data%ann_phase_uncertainty_offset, &
              output_data%ann_phase_uncertainty_vmin, &
              output_data%ann_phase_uncertainty_vmax, &
              sreal_fill_value, sint_fill_value)
      end do

      output_data%ann_phase(i,j,:)=input_data%ann_phase(i,j,:)

   end if

   !----------------------------------------------------------------------------
   ! niter
   !----------------------------------------------------------------------------
   output_data%niter(i,j)=input_data%niter(i,j)

   !----------------------------------------------------------------------------
   ! costja
   !----------------------------------------------------------------------------
   call prepare_float_packed_float( &
        input_data%costja(i,j), output_data%costja(i,j), &
        output_data%costja_scale, output_data%costja_offset, &
        output_data%costja_vmin, output_data%costja_vmax, &
        sreal_fill_value, sreal_fill_value)

   !----------------------------------------------------------------------------
   ! costjm
   !----------------------------------------------------------------------------
   call prepare_float_packed_float( &
        input_data%costjm(i,j), output_data%costjm(i,j), &
        output_data%costjm_scale, output_data%costjm_offset, &
        output_data%costjm_vmin, output_data%costjm_vmax, &
        sreal_fill_value, sreal_fill_value)

   !----------------------------------------------------------------------------
   ! qcflag, channels_used, variables_retrieved
   !----------------------------------------------------------------------------
   output_data%qcflag(i,j) = input_data%QCFlag(i,j)
   output_data%channels_used(i,j) = input_data%channels_used(i,j)
   output_data%variables_retrieved(i,j) = input_data%variables_retrieved(i,j)

   !----------------------------------------------------------------------------
   ! lsflag
   !----------------------------------------------------------------------------
   output_data%lsflag(i,j)=input_data%LSFlag(i,j)

   !----------------------------------------------------------------------------
   ! lusflag
   !----------------------------------------------------------------------------
   output_data%lusflag(i,j)=input_data%lusflag(i,j)

   !----------------------------------------------------------------------------
   ! dem
   !----------------------------------------------------------------------------
   output_data%dem(i,j)=input_data%dem(i,j)

   !----------------------------------------------------------------------------
   ! illum
   !----------------------------------------------------------------------------
   output_data%illum(i,j)=input_data%illum(i,j)

   !----------------------------------------------------------------------------
   ! cldtype
   !----------------------------------------------------------------------------
   output_data%cldtype(i,j,:)=input_data%cldtype(i,j,:)

   !----------------------------------------------------------------------------
   ! cldmask
   !----------------------------------------------------------------------------
   if (indexing%flags%do_cldmask) then
      output_data%cldmask(i,j,:)=input_data%cldmask(i,j,:)
   end if
   if (indexing%flags%do_cldmask_uncertainty) then
      !-------------------------------------------------------------------------
      ! cldmask_uncertainty
      !-------------------------------------------------------------------------
      do k = 1, indexing%NViews
         call prepare_short_packed_float( &
              input_data%cldmask_uncertainty(i,j,k), &
              output_data%cldmask_uncertainty(i,j,k), &
              output_data%cldmask_uncertainty_scale, &
              output_data%cldmask_uncertainty_offset, &
              output_data%cldmask_uncertainty_vmin, &
              output_data%cldmask_uncertainty_vmax, &
              sreal_fill_value, sint_fill_value)
      end do
   end if

   !----------------------------------------------------------------------------
   ! phase
   !----------------------------------------------------------------------------
   if (indexing%flags%do_phase) then
      output_data%phase(i,j) = input_data%phase(i,j)
   end if

   !----------------------------------------------------------------------------
   ! phase_pavolonis
   !----------------------------------------------------------------------------
   if (indexing%flags%do_phase_pavolonis) then
      select case (input_data%cldtype(i,j,1))
      case(CLEAR_TYPE, &
           PROB_CLEAR_TYPE)
         output_data%phase_pavolonis(i,j) = IPhaseClU
      case(SWITCHED_TO_WATER_TYPE, &
           FOG_TYPE, &
           WATER_TYPE, &
           SUPERCOOLED_TYPE)
         output_data%phase_pavolonis(i,j) = IPhaseWat
      case(SWITCHED_TO_ICE_TYPE, &
           OPAQUE_ICE_TYPE, &
           CIRRUS_TYPE, &
           OVERLAP_TYPE, &
           PROB_OPAQUE_ICE_TYPE)
         output_data%phase_pavolonis(i,j) = IPhaseIce
      case default
         output_data%phase_pavolonis(i,j) = byte_fill_value
      end select
   end if

end subroutine prepare_output_primary_pp
