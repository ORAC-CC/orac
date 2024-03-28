!-------------------------------------------------------------------------------
! Name: prepare_output_primary.F90
!
! Purpose:
! Map internal representation of variables to output representation by applying
! scale and offset where necessary.
!
! Description and Algorithm details:
! Call prepare_short_packed_float many times.
!
! Arguments:
! Name        Type    In/Out/Both Description
! ------------------------------------------------------------------------------
! Ctrl        struct  In          Control structure
! i           int     In          Across-track pixel to output
! j           int     In          Along-track pixel to output
! MSI_Data    struct  In          Imager data structure
! SPixel      struct  In          Retrieval pixel structure
! RTM_Pc      array of structs    RTM_Pc structs for each layer
!                     In
! Diag        struct  In          Diagnostic structure
! output_data struct  Both        Results structure
!
! History:
! 2011/12/19, MJ: Creates initial version
! 2012/01/06, MJ: Added in cwp
! 2012/01/16, CP: Bug fix, changed how offset applied
! 2012/06/16, CP: Change illum arry size
! 2012/07/17, MJ: Fixes bug in CWP write.
! 2012/08/10, CP: Fixed bug in illum read 2d array instead of 3d
! 2012/11/03, MST: Converted height to km
! 2012/11/03, CP: Changed log cot to COT using 10*
! 2013/01/17, MJ: Adds code to accommodate uncertainties of ctt and cth
! 2013/xx/xx, MJ: Make various modifications to define range of variables more
!    thoroughly.
! 2014/06/13, GM: Put the code into a subroutine.
! 2014/06/13, GM: Cleaned up the code.
! 2014/08/31, GM: Update to use general routines in the current module.
! 2014/09/17, GM: Fixed handling of missing values in some cases.
! 2014/10/24, OS: Added variables cldtype, cldmask, cccot_pre, lusflags, dem,
!    and nisemask.
! 2014/11/15, CP: Added cloud albedo.
! 2014/11/25, AP: Fixed bug in writing cth|ctt_uncertainty.
! 2014/01/30, AP: Replace YSeg0 with Y0 as superpixeling removed.
! 2015/03/11, GM: Fixed an indexing bug in writing the cloud albedo.
! 2015/03/19, OS: CTH now .ge. 0
! 2015/03/19, OS: undid previous change in file; CTH is allowed to be negative
!    again
! 2015/07/03, OS: Added cldmask_uncertainty data
! 2015/07/04, CP: Added corrected cth
! 2015/07/31, AP: Remove convergence argument.
! 2015/09/07, GM: Change COT uncertainty output from log10(COT) space to COT
!    space.
! 2015/09/07, GM: Add cldmask_uncertainty.
! 2015/10/22, GM: Add cloud albedo uncertainty.
! 2015/11/16, GM: Fixed calculation of temp_short_ctp_error.
! 2015/11/16, CP: Made AATSR times consistent with AVHRR and MODIS.
! 2015/11/26, GM: Fixed AATSR time offset relative to Julian day.
! 2015/11/26, GM: AATSR time offset is now applied in the preprocessor.
! 2015/12/10, GM: Fixed conversion of geopotential height to geometric height:
!    10.0 was being used instead of g_wmo (gravitational acceleration).
! 2015/12/28, AP: Add output fields for aerosol retrievals.
! 2016/01/06, AP: Wrap do_* flags into output_flags structure.
! 2016/01/27, GM: Add cloud emissivity and cloud emissivity uncertainty.
! 2016/01/28, GM: Add ctp and ctt corrected and corrected_uncertianty.
! 2016/03/04, AP: Tidy prepare_*_packed_float.
! 2016/04/28, AP: Add multiple views.
! 2016/05/03, AP: Add AOT at a second wavelength.
! 2016/07/27, GM: Add output fields for the multilayer retrieval.
! 2017/06/21, OS: Added ANN phase variables.
! 2017/07/05, AP: Add channels_used, variables_retrieved. New QC.
! 2018/06/08, SP: Add satellite azimuth angle to output.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine prepare_output_primary(Ctrl, i, j, MSI_Data, SPixel, RTM_Pc, Diag, &
                                  output_data)

   use Ctrl_m
   use Data_m
   use Diag_m
   use orac_ncdf_m
   use orac_output_m
   use RTM_Pc_m
   use SPixel_m

   implicit none

   type(Ctrl_t),                intent(in)    :: Ctrl
   integer,                     intent(in)    :: i, j
   type(Data_t),                intent(in)    :: MSI_Data
   type(SPixel_t),              intent(in)    :: SPixel
   type(RTM_Pc_t),              intent(in)    :: RTM_Pc(:)
   type(Diag_t),                intent(in)    :: Diag
   type(output_data_primary_t), intent(inout) :: output_data

   integer            :: k, kk, l, i_rho
   integer(kind=sint) :: temp_short_ctp_uncertainty
   real(kind=sreal)   :: temp_real, temp_real_ctp_uncertainty


   !----------------------------------------------------------------------------
   ! time
   !----------------------------------------------------------------------------
   output_data%time(i,j) = MSI_Data%time(SPixel%Loc%X0, SPixel%Loc%Y0)


   !----------------------------------------------------------------------------
   ! lat, lon
   !----------------------------------------------------------------------------
   call prepare_float_packed_float( &
        MSI_Data%Location%Lat(SPixel%Loc%X0, SPixel%Loc%Y0), &
        output_data%lat(i,j), output_data%lat_scale, output_data%lat_offset, &
        output_data%lat_vmin, output_data%lat_vmax, &
        sreal_fill_value, sreal_fill_value)
   call prepare_float_packed_float( &
        MSI_Data%Location%Lon(SPixel%Loc%X0, SPixel%Loc%Y0), &
        output_data%lon(i,j), output_data%lon_scale, output_data%lon_offset, &
        output_data%lon_vmin, output_data%lon_vmax, &
        sreal_fill_value, sreal_fill_value)

   !----------------------------------------------------------------------------
   ! sol_zen, sat_zen, rel_azi
   !----------------------------------------------------------------------------
   do k = 1, Ctrl%Ind%NViews
      call prepare_float_packed_float( &
           MSI_Data%Geometry%Sol(SPixel%Loc%X0, SPixel%Loc%Y0, k), &
           output_data%sol_zen(i,j,k), &
           output_data%sol_scale, output_data%sol_offset, &
           output_data%sol_vmin, output_data%sol_vmax, &
           sreal_fill_value, sreal_fill_value)
      call prepare_float_packed_float( &
           MSI_Data%Geometry%Sat(SPixel%Loc%X0, SPixel%Loc%Y0, k), &
           output_data%sat_zen(i,j,k), &
           output_data%sat_scale, output_data%sat_offset, &
           output_data%sat_vmin, output_data%sat_vmax, &
           sreal_fill_value, sreal_fill_value)
      call prepare_float_packed_float( &
           MSI_Data%Geometry%Azi(SPixel%Loc%X0, SPixel%Loc%Y0, k), &
           output_data%rel_azi(i,j,k), &
           output_data%azi_scale, output_data%azi_offset, &
           output_data%azi_vmin, output_data%azi_vmax, &
           sreal_fill_value, sreal_fill_value)
      call prepare_float_packed_float( &
           MSI_Data%Geometry%Saz(SPixel%Loc%X0, SPixel%Loc%Y0, k), &
           output_data%sat_azi(i,j,k), &
           output_data%azi_scale, output_data%azi_offset, &
           output_data%azi_vmin, output_data%azi_vmax, &
           sreal_fill_value, sreal_fill_value)
   end do

if (Ctrl%Ind%flags%do_aerosol) then
   !----------------------------------------------------------------------------
   ! aot, aot_uncertainty
   !----------------------------------------------------------------------------
   call prepare_short_packed_float( &
        SPixel%Xn(ITau), output_data%aot550(i,j), &
        output_data%aot550_scale, output_data%aot550_offset, &
        output_data%aot550_vmin, output_data%aot550_vmax, &
        MissingXn, output_data%aot550_vmax, &
        control=SPixel%Xn(ITau))

   temp_real = sqrt(SPixel%Sn(ITau,ITau))
   call prepare_short_packed_float( &
        temp_real, output_data%aot550_uncertainty(i,j), &
        output_data%aot550_uncertainty_scale, &
        output_data%aot550_uncertainty_offset, &
        output_data%aot550_uncertainty_vmin, &
        output_data%aot550_uncertainty_vmax, &
        MissingSn, output_data%aot550_uncertainty_vmax, &
        control=SPixel%Sn(ITau,ITau))

   call prepare_short_packed_float( &
        Diag%aot870, output_data%aot870(i,j), &
        output_data%aot870_scale, output_data%aot870_offset, &
        output_data%aot870_vmin, output_data%aot870_vmax, &
        MissingXn, output_data%aot870_vmax, &
        control=SPixel%Xn(ITau))

   temp_real = sqrt(Diag%aot870_uncertainty)
   call prepare_short_packed_float( &
        temp_real, output_data%aot870_uncertainty(i,j), &
        output_data%aot870_uncertainty_scale, &
        output_data%aot870_uncertainty_offset, &
        output_data%aot870_uncertainty_vmin, &
        output_data%aot870_uncertainty_vmax, &
        MissingSn, output_data%aot870_uncertainty_vmax, &
        control=SPixel%Sn(ITau,ITau))

   !----------------------------------------------------------------------------
   ! aer, aer_uncertainty
   !----------------------------------------------------------------------------
   call prepare_short_packed_float( &
        SPixel%Xn(IRe), output_data%aer(i,j), &
        output_data%aer_scale, output_data%aer_offset, &
        output_data%aer_vmin, output_data%aer_vmax, &
        MissingXn, output_data%aer_vmax, &
        control=SPixel%Xn(IRe))

   temp_real = sqrt(SPixel%Sn(IRe,IRe))
   call prepare_short_packed_float( &
        temp_real, output_data%aer_uncertainty(i,j), &
        output_data%aer_uncertainty_scale, output_data%aer_uncertainty_offset, &
        output_data%aer_uncertainty_vmin, output_data%aer_uncertainty_vmax, &
        MissingSn, output_data%aer_uncertainty_vmax, &
        control=SPixel%Sn(IRe,IRe))
end if

if (Ctrl%Ind%flags%do_rho) then
   !----------------------------------------------------------------------------
   ! rho, rho_uncertainty
   !----------------------------------------------------------------------------
   do k = 1, SPixel%Ind%NSolar
      kk = SPixel%spixel_y_solar_to_ctrl_y_solar_index(k)

      do l = 1, MaxRho_XX
         i_rho = SPixel%spixel_y_solar_to_rho_terms(k,l)

         if (i_rho >= 0) then
            call prepare_short_packed_float( &
                 SPixel%Xn(IRs(kk,l)), output_data%rho(i,j,i_rho), &
                 output_data%rho_scale, output_data%rho_offset, &
                 output_data%rho_vmin, output_data%rho_vmax, &
                 MissingXn, output_data%rho_vmax)

            temp_real = sqrt(SPixel%Sn(IRs(kk,l),IRs(kk,l)))
            call prepare_short_packed_float( &
                 temp_real, output_data%rho_uncertainty(i,j,i_rho), &
                 output_data%rho_uncertainty_scale, &
                 output_data%rho_uncertainty_offset, &
                 output_data%rho_uncertainty_vmin, &
                 output_data%rho_uncertainty_vmax, &
                 MissingSn, output_data%rho_uncertainty_vmax, &
                 control=SPixel%Sn(IRs(kk,l),IRs(kk,l)))
         end if
      end do
   end do
end if

if (Ctrl%Ind%flags%do_swansea) then
   do k = 1, SPixel%Ind%NSolar
      kk = SPixel%spixel_y_solar_to_ctrl_y_solar_index(k)
      i_rho = SPixel%spixel_y_solar_to_ss_terms(k)

      if (i_rho >= 0) then
   !----------------------------------------------------------------------------
   ! swansea_s, swansea_s_uncertainty
   !----------------------------------------------------------------------------
         call prepare_short_packed_float( &
              SPixel%Xn(ISS(kk)), output_data%swansea_s(i,j,i_rho), &
              output_data%swansea_s_scale, &
              output_data%swansea_s_offset, &
              output_data%swansea_s_vmin, &
              output_data%swansea_s_vmax, &
              MissingXn, output_data%swansea_s_vmax)

         temp_real = sqrt(SPixel%Sn(ISS(kk),ISS(kk)))
         call prepare_short_packed_float( &
              temp_real, output_data%swansea_s_uncertainty(i,j,i_rho), &
              output_data%swansea_s_uncertainty_scale, &
              output_data%swansea_s_uncertainty_offset, &
              output_data%swansea_s_uncertainty_vmin, &
              output_data%swansea_s_uncertainty_vmax, &
              MissingSn, output_data%swansea_s_uncertainty_vmax, &
              control=SPixel%Sn(ISS(kk),ISS(kk)))

   !----------------------------------------------------------------------------
   ! diffuse_frac, diffuse_frac_uncertainty
   !----------------------------------------------------------------------------
         call prepare_short_packed_float( &
              Diag%diffuse_frac(k), output_data%diffuse_frac(i,j,i_rho), &
              output_data%diffuse_frac_scale, &
              output_data%diffuse_frac_offset, &
              output_data%diffuse_frac_vmin, &
              output_data%diffuse_frac_vmax, &
              sreal_fill_value, output_data%diffuse_frac_vmax)

         temp_real = sqrt(Diag%diffuse_frac_s(k))
         call prepare_short_packed_float( &
              temp_real, output_data%diffuse_frac_uncertainty(i,j,i_rho), &
              output_data%diffuse_frac_uncertainty_scale, &
              output_data%diffuse_frac_uncertainty_offset, &
              output_data%diffuse_frac_uncertainty_vmin, &
              output_data%diffuse_frac_uncertainty_vmax, &
              sreal_fill_value, output_data%diffuse_frac_uncertainty_vmax, &
              control=Diag%diffuse_frac_s(k))
      end if
   end do

   !----------------------------------------------------------------------------
   ! swansea_p, swansea_p_uncertainty
   !----------------------------------------------------------------------------
   do k = 1, Ctrl%Ind%NViews
      if (any(SPixel%X .eq. ISP(k))) then
         call prepare_short_packed_float( &
              SPixel%Xn(ISP(k)), output_data%swansea_p(i,j,k), &
              output_data%swansea_p_scale, &
              output_data%swansea_p_offset, &
              output_data%swansea_p_vmin, &
              output_data%swansea_p_vmax, &
              MissingXn, output_data%swansea_p_vmax)

         temp_real = sqrt(SPixel%Sn(ISP(k),ISP(k)))
         call prepare_short_packed_float( &
              temp_real, output_data%swansea_p_uncertainty(i,j,k), &
              output_data%swansea_p_uncertainty_scale, &
              output_data%swansea_p_uncertainty_offset, &
              output_data%swansea_p_uncertainty_vmin, &
              output_data%swansea_p_uncertainty_vmax, &
              MissingSn, output_data%swansea_p_uncertainty_vmax, &
              control=SPixel%Sn(ISP(k),ISP(k)))
      end if
   end do
end if

if (Ctrl%Ind%flags%do_cloud) then
   !----------------------------------------------------------------------------
   ! cot, cot_uncertainty
   !----------------------------------------------------------------------------
   call prepare_short_packed_float( &
        SPixel%Xn(ITau), output_data%cot(i,j), &
        output_data%cot_scale, output_data%cot_offset, &
        output_data%cot_vmin, output_data%cot_vmax, &
        MissingXn, output_data%cot_vmax, &
        control=SPixel%Xn(ITau))

   temp_real = sqrt(SPixel%Sn(ITau,ITau))
   call prepare_short_packed_float( &
        temp_real, output_data%cot_uncertainty(i,j), &
        output_data%cot_uncertainty_scale, output_data%cot_uncertainty_offset, &
        output_data%cot_uncertainty_vmin, output_data%cot_uncertainty_vmax, &
        MissingSn, output_data%cot_uncertainty_vmax, &
        control=SPixel%Sn(ITau,ITau))

   !----------------------------------------------------------------------------
   ! cer, cer_uncertainty
   !----------------------------------------------------------------------------
   call prepare_short_packed_float( &
        SPixel%Xn(IRe), output_data%cer(i,j), &
        output_data%cer_scale, output_data%cer_offset, &
        output_data%cer_vmin, output_data%cer_vmax, &
        MissingXn, output_data%cer_vmax)

   temp_real = sqrt(SPixel%Sn(IRe,IRe))
   call prepare_short_packed_float( &
        temp_real, output_data%cer_uncertainty(i,j), &
        output_data%cer_uncertainty_scale, output_data%cer_uncertainty_offset, &
        output_data%cer_uncertainty_vmin, output_data%cer_uncertainty_vmax, &
        MissingSn, output_data%cer_uncertainty_vmax, &
        control=SPixel%Sn(IRe,IRe))

   !----------------------------------------------------------------------------
   ! ctp, ctp_uncertainty
   !----------------------------------------------------------------------------
   call prepare_short_packed_float( &
        SPixel%Xn(IPc), output_data%ctp(i,j), &
        output_data%ctp_scale, output_data%ctp_offset, &
        output_data%ctp_vmin, output_data%ctp_vmax, &
        MissingXn, output_data%ctp_vmax)

   if (SPixel%Sn(IPc,IPc) .eq. MissingSn) then
      temp_real_ctp_uncertainty = sreal_fill_value
   else
      temp_real_ctp_uncertainty = sqrt(SPixel%Sn(IPc,IPc))
      temp_short_ctp_uncertainty = int((temp_real_ctp_uncertainty - &
                                  output_data%ctp_uncertainty_offset) / &
                                  output_data%ctp_uncertainty_scale, kind=sint)
   end if
   call prepare_short_packed_float( &
        temp_real_ctp_uncertainty, output_data%ctp_uncertainty(i,j), &
        output_data%ctp_uncertainty_scale, output_data%ctp_uncertainty_offset, &
        output_data%ctp_uncertainty_vmin, output_data%ctp_uncertainty_vmax, &
        sreal_fill_value, output_data%ctp_uncertainty_vmax)

   !----------------------------------------------------------------------------
   ! ctp_corrected, ctp_corrected_uncertainty
   !----------------------------------------------------------------------------
   call prepare_short_packed_float( &
        SPixel%CTP_corrected, output_data%ctp_corrected(i,j), &
        output_data%ctp_scale, output_data%ctp_offset, &
        output_data%ctp_vmin, output_data%ctp_vmax, &
        MissingXn, output_data%ctp_vmax)

   call prepare_short_packed_float( &
        SPixel%CTP_corrected_uncertainty, &
        output_data%ctp_corrected_uncertainty(i,j), &
        output_data%ctp_uncertainty_scale, output_data%ctp_uncertainty_offset, &
        output_data%ctp_uncertainty_vmin, output_data%ctp_uncertainty_vmax, &
        MissingSn, output_data%ctp_uncertainty_vmax)

   !----------------------------------------------------------------------------
   ! cc_total, cc_total_uncertainty
   !----------------------------------------------------------------------------
   call prepare_short_packed_float( &
        SPixel%Xn(IFr), output_data%cc_total(i,j), &
        output_data%cc_total_scale, output_data%cc_total_offset, &
        output_data%cc_total_vmin, output_data%cc_total_vmax, &
        MissingXn, output_data%cc_total_vmax)

   temp_real = sqrt(SPixel%Sn(IFr,IFr))
   call prepare_short_packed_float( &
        temp_real, output_data%cc_total_uncertainty(i,j), &
        output_data%cc_total_uncertainty_scale, &
        output_data%cc_total_uncertainty_offset, &
        output_data%cc_total_uncertainty_vmin, &
        output_data%cc_total_uncertainty_vmax, &
        MissingSn, output_data%cc_total_uncertainty_vmax, &
        SPixel%Sn(IFr,IFr))

   !----------------------------------------------------------------------------
   ! stemp, stemp_uncertainty
   !----------------------------------------------------------------------------
   call prepare_short_packed_float( &
        SPixel%Xn(ITs), output_data%stemp(i,j), &
        output_data%stemp_scale, output_data%stemp_offset, &
        output_data%stemp_vmin, output_data%stemp_vmax, &
        MissingXn, output_data%stemp_vmax)

   temp_real = sqrt(SPixel%Sn(ITs,ITs))
   call prepare_short_packed_float( &
        temp_real, output_data%stemp_uncertainty(i,j), &
        output_data%stemp_uncertainty_scale, &
        output_data%stemp_uncertainty_offset, &
        output_data%stemp_uncertainty_vmin, &
        output_data%stemp_uncertainty_vmax, &
        MissingSn, output_data%stemp_uncertainty_vmax, &
        control=SPixel%Sn(ITs,ITs))

   !----------------------------------------------------------------------------
   ! cth, cth_uncertainty
   !----------------------------------------------------------------------------
   temp_real = RTM_Pc(1)%Hc / g_wmo / 1000. ! now it's in km
   call prepare_short_packed_float( &
        temp_real, output_data%cth(i,j), &
        output_data%cth_scale, output_data%cth_offset, &
        output_data%cth_vmin, output_data%cth_vmax, &
        MissingXn, output_data%cth_vmax, &
        control=RTM_Pc(1)%Hc)

   ! If ctp_uncertainty is good compute cth_uncertainty
   if (temp_real_ctp_uncertainty .eq. sreal_fill_value) then
      output_data%cth_uncertainty(i,j) = sint_fill_value
   else if (temp_short_ctp_uncertainty .lt. output_data%ctp_uncertainty_vmin) then
      output_data%cth_uncertainty(i,j) = sint_fill_value
   else if (temp_short_ctp_uncertainty .gt. output_data%ctp_uncertainty_vmax) then
      output_data%cth_uncertainty(i,j) = output_data%cth_uncertainty_vmax
   else
      temp_real = abs(RTM_Pc(1)%dHc_dPc / g_wmo / 1000.) * temp_real_ctp_uncertainty
      call prepare_short_packed_float( &
           temp_real, output_data%cth_uncertainty(i,j), &
           output_data%cth_uncertainty_scale, &
           output_data%cth_uncertainty_offset, &
           output_data%cth_uncertainty_vmin, &
           output_data%cth_uncertainty_vmax, &
           sreal_fill_value, output_data%cth_uncertainty_vmax)
   end if

   !----------------------------------------------------------------------------
   ! cth_corrected, cth_corrected_uncertainty
   !----------------------------------------------------------------------------
   temp_real = SPixel%CTH_corrected / g_wmo / 1000. ! now it's in km
   call prepare_short_packed_float( &
        temp_real, output_data%cth_corrected(i,j), &
        output_data%cth_scale, output_data%cth_offset, &
        output_data%cth_vmin, output_data%cth_vmax, &
        MissingXn, output_data%cth_vmax, &
        control=SPixel%CTH_corrected)

   temp_real = SPixel%CTH_corrected_uncertainty / g_wmo / 1000. ! now it's in km
   call prepare_short_packed_float( &
        temp_real, output_data%cth_corrected_uncertainty(i,j), &
        output_data%cth_uncertainty_scale, output_data%cth_uncertainty_offset, &
        output_data%cth_uncertainty_vmin, output_data%cth_uncertainty_vmax, &
        MissingSn, output_data%cth_uncertainty_vmax, &
        control=SPixel%CTH_corrected_uncertainty)

   !----------------------------------------------------------------------------
   ! ctt, ctt_uncertainty
   !----------------------------------------------------------------------------
   call prepare_short_packed_float( &
        RTM_Pc(1)%Tc, output_data%ctt(i,j), &
        output_data%ctt_scale, output_data%ctt_offset, &
        output_data%ctt_vmin, output_data%ctt_vmax, &
        MissingXn, output_data%ctt_vmax)

   ! If ctp_uncertainty is good compute ctt_uncertainty
   if (temp_real_ctp_uncertainty .eq. sreal_fill_value) then
      output_data%ctt_uncertainty(i,j) = sint_fill_value
   else if (temp_short_ctp_uncertainty .lt. output_data%ctp_uncertainty_vmin) then
      output_data%ctt_uncertainty(i,j) = sint_fill_value
   else if (temp_short_ctp_uncertainty .gt. output_data%ctp_uncertainty_vmax) then
      output_data%ctt_uncertainty(i,j) = output_data%ctt_uncertainty_vmax
   else
      temp_real = abs(RTM_Pc(1)%dTc_dPc) * temp_real_ctp_uncertainty
      call prepare_short_packed_float( &
           temp_real, output_data%ctt_uncertainty(i,j), &
           output_data%ctt_uncertainty_scale, output_data%ctt_uncertainty_offset, &
           output_data%ctt_uncertainty_vmin, output_data%ctt_uncertainty_vmax, &
           sreal_fill_value, output_data%ctt_uncertainty_vmax)
   end if

   !----------------------------------------------------------------------------
   ! ctt_corrected, ctt_corrected_uncertainty
   !----------------------------------------------------------------------------
   call prepare_short_packed_float( &
           SPixel%CTT_corrected, output_data%ctt_corrected(i,j), &
           output_data%ctt_scale, output_data%ctt_offset, &
           output_data%ctt_vmin, output_data%ctt_vmax, &
           MissingXn, output_data%ctt_vmax)

   call prepare_short_packed_float( &
        SPixel%CTT_corrected_uncertainty, &
        output_data%ctt_corrected_uncertainty(i,j), &
        output_data%ctt_uncertainty_scale, output_data%ctt_uncertainty_offset, &
        output_data%ctt_uncertainty_vmin, output_data%ctt_uncertainty_vmax, &
        MissingSn, output_data%ctt_uncertainty_vmax)

   !----------------------------------------------------------------------------
   ! cwp, cwp_uncertainty
   !----------------------------------------------------------------------------
   call prepare_short_packed_float( &
        SPixel%CWP, output_data%cwp(i,j), &
        output_data%cwp_scale, output_data%cwp_offset, &
        output_data%cwp_vmin, output_data%cwp_vmax, &
        MissingXn, output_data%cwp_vmax)

   temp_real = sqrt(SPixel%CWP_uncertainty)
   call prepare_short_packed_float( &
        temp_real, output_data%cwp_uncertainty(i,j), &
        output_data%cwp_uncertainty_scale, output_data%cwp_uncertainty_offset, &
        output_data%cwp_uncertainty_vmin, output_data%cwp_uncertainty_vmax, &
        MissingSn, output_data%cwp_uncertainty_vmax, &
        control=SPixel%CWP_uncertainty)

   !----------------------------------------------------------------------------
   ! cloud_albedo, cloud_albedo_uncertainty
   !----------------------------------------------------------------------------
   do k = 1, SPixel%Ind%NSolar
      kk = SPixel%spixel_y_solar_to_alb_terms(k)

      if (kk >= 0) then
         call prepare_short_packed_float( &
              Diag%cloud_albedo(k), output_data%cloud_albedo(i,j,kk), &
              output_data%cloud_albedo_scale, output_data%cloud_albedo_offset, &
              output_data%cloud_albedo_vmin, output_data%cloud_albedo_vmax, &
              sreal_fill_value, output_data%cloud_albedo_vmax)

         temp_real = sqrt(Diag%cloud_albedo_s(k))
         call prepare_short_packed_float( &
              temp_real, output_data%cloud_albedo_uncertainty(i,j,kk), &
              output_data%cloud_albedo_uncertainty_scale, &
              output_data%cloud_albedo_uncertainty_offset, &
              output_data%cloud_albedo_uncertainty_vmin, &
              output_data%cloud_albedo_uncertainty_vmax, &
              sreal_fill_value, output_data%cloud_albedo_uncertainty_vmax, &
              control=Diag%cloud_albedo_s(k))
      end if
   end do

   !----------------------------------------------------------------------------
   ! cee, cee_uncertainty
   !----------------------------------------------------------------------------
   do k = 1, SPixel%Ind%NThermal
      kk = SPixel%spixel_y_thermal_to_cee_terms(k)

      if (kk >= 0) then
         call prepare_short_packed_float( &
              Diag%cloud_emissivity(k), output_data%cee(i,j,kk), &
              output_data%cee_scale, output_data%cee_offset, &
              output_data%cee_vmin, output_data%cee_vmax, &
              sreal_fill_value, output_data%cee_vmax)

         temp_real = sqrt(Diag%cloud_emissivity_s(k))
         call prepare_short_packed_float( &
              temp_real, output_data%cee_uncertainty(i,j,kk), &
              output_data%cee_uncertainty_scale, &
              output_data%cee_uncertainty_offset, &
              output_data%cee_uncertainty_vmin, &
              output_data%cee_uncertainty_vmax, &
              sreal_fill_value, output_data%cee_uncertainty_vmax, &
              control=Diag%cloud_emissivity_s(k))
      end if
   end do

   !----------------------------------------------------------------------------
   ! cccot_pre
   !----------------------------------------------------------------------------
   do k = 1, Ctrl%Ind%NViews
      call prepare_short_packed_float( &
           MSI_Data%cccot_pre(SPixel%Loc%X0, SPixel%Loc%Y0,k), &
           output_data%cccot_pre(i,j,k), &
           output_data%cccot_pre_scale, output_data%cccot_pre_offset, &
           output_data%cccot_pre_vmin, output_data%cccot_pre_vmax, &
           sreal_fill_value, sint_fill_value)
   end do
end if

if (Ctrl%Ind%flags%do_cloud_layer_2) then
   !----------------------------------------------------------------------------
   ! cot2, cot2_uncertainty
   !----------------------------------------------------------------------------
   call prepare_short_packed_float( &
        SPixel%Xn(ITau2), output_data%cot2(i,j), &
        output_data%cot_scale, output_data%cot_offset, &
        output_data%cot_vmin, output_data%cot_vmax, &
        MissingXn, output_data%cot_vmax, &
        control=SPixel%Xn(ITau2))

   temp_real = sqrt(SPixel%Sn(ITau2,ITau2))
   call prepare_short_packed_float( &
        temp_real, output_data%cot2_uncertainty(i,j), &
        output_data%cot_uncertainty_scale, output_data%cot_uncertainty_offset, &
        output_data%cot_uncertainty_vmin, output_data%cot_uncertainty_vmax, &
        MissingSn, output_data%cot_uncertainty_vmax, &
        control=SPixel%Sn(ITau2,ITau2))

   !----------------------------------------------------------------------------
   ! cer2, cer2_uncertainty
   !----------------------------------------------------------------------------
   call prepare_short_packed_float( &
        SPixel%Xn(IRe2), output_data%cer2(i,j), &
        output_data%cer_scale, output_data%cer_offset, &
        output_data%cer_vmin, output_data%cer_vmax, &
        MissingXn, output_data%cer_vmax)

   temp_real = sqrt(SPixel%Sn(IRe2,IRe2))
   call prepare_short_packed_float( &
        temp_real, output_data%cer2_uncertainty(i,j), &
        output_data%cer_uncertainty_scale, output_data%cer_uncertainty_offset, &
        output_data%cer_uncertainty_vmin, output_data%cer_uncertainty_vmax, &
        MissingSn, output_data%cer_uncertainty_vmax, &
        control=SPixel%Sn(IRe2,IRe2))

   !----------------------------------------------------------------------------
   ! ctp2, ctp2_uncertainty
   !----------------------------------------------------------------------------
   call prepare_short_packed_float( &
        SPixel%Xn(IPc2), output_data%ctp2(i,j), &
        output_data%ctp_scale, output_data%ctp_offset, &
        output_data%ctp_vmin, output_data%ctp_vmax, &
        MissingXn, output_data%ctp_vmax)

   if (SPixel%Sn(IPc2,IPc2) .eq. MissingSn) then
      temp_real_ctp_uncertainty = sreal_fill_value
   else
      temp_real_ctp_uncertainty = sqrt(SPixel%Sn(IPc2,IPc2))
      temp_short_ctp_uncertainty = int((temp_real_ctp_uncertainty - &
                                  output_data%ctp_uncertainty_offset) / &
                                  output_data%ctp_uncertainty_scale, kind=sint)
   end if
   call prepare_short_packed_float( &
        temp_real_ctp_uncertainty, output_data%ctp2_uncertainty(i,j), &
        output_data%ctp_uncertainty_scale, output_data%ctp_uncertainty_offset, &
        output_data%ctp_uncertainty_vmin, output_data%ctp_uncertainty_vmax, &
        sreal_fill_value, output_data%ctp_uncertainty_vmax)

   !----------------------------------------------------------------------------
   ! cc_total2, cc_total2_uncertainty
   !----------------------------------------------------------------------------
   call prepare_short_packed_float( &
        SPixel%Xn(IFr2), output_data%cc_total2(i,j), &
        output_data%cc_total_scale, output_data%cc_total_offset, &
        output_data%cc_total_vmin, output_data%cc_total_vmax, &
        MissingXn, output_data%cc_total_vmax)

   temp_real = sqrt(SPixel%Sn(IFr2,IFr2))
   call prepare_short_packed_float( &
        temp_real, output_data%cc_total2_uncertainty(i,j), &
        output_data%cc_total_uncertainty_scale, &
        output_data%cc_total_uncertainty_offset, &
        output_data%cc_total_uncertainty_vmin, &
        output_data%cc_total_uncertainty_vmax, &
        MissingSn, output_data%cc_total_uncertainty_vmax, &
        SPixel%Sn(IFr2,IFr2))

   !----------------------------------------------------------------------------
   ! cth2, cth2_uncertainty
   !----------------------------------------------------------------------------
   temp_real = RTM_Pc(2)%Hc / g_wmo / 1000. ! now it's in km
   call prepare_short_packed_float( &
        temp_real, output_data%cth2(i,j), &
        output_data%cth_scale, output_data%cth_offset, &
        output_data%cth_vmin, output_data%cth_vmax, &
        MissingXn, output_data%cth_vmax, &
        control=RTM_Pc(2)%Hc)

   ! If ctp_uncertainty is good compute cth_uncertainty
   if (temp_real_ctp_uncertainty .eq. sreal_fill_value) then
      output_data%cth2_uncertainty(i,j) = sint_fill_value
   else if (temp_short_ctp_uncertainty .lt. output_data%ctp_uncertainty_vmin) then
      output_data%cth2_uncertainty(i,j) = sint_fill_value
   else if (temp_short_ctp_uncertainty .gt. output_data%ctp_uncertainty_vmax) then
      output_data%cth2_uncertainty(i,j) = output_data%cth_uncertainty_vmax
   else
      temp_real = abs(RTM_Pc(2)%dHc_dPc / g_wmo / 1000.) * temp_real_ctp_uncertainty
      call prepare_short_packed_float( &
           temp_real, output_data%cth2_uncertainty(i,j), &
           output_data%cth_uncertainty_scale, &
           output_data%cth_uncertainty_offset, &
           output_data%cth_uncertainty_vmin, &
           output_data%cth_uncertainty_vmax, &
           sreal_fill_value, output_data%cth_uncertainty_vmax)
   end if

   !----------------------------------------------------------------------------
   ! ctt2, ctt2_uncertainty
   !----------------------------------------------------------------------------
   call prepare_short_packed_float( &
        RTM_Pc(2)%Tc, output_data%ctt2(i,j), &
        output_data%ctt_scale, output_data%ctt_offset, &
        output_data%ctt_vmin, output_data%ctt_vmax, &
        MissingXn, output_data%ctt_vmax)

   ! If ctp_uncertainty is good compute ctt_uncertainty
   if (temp_real_ctp_uncertainty .eq. sreal_fill_value) then
      output_data%ctt2_uncertainty(i,j) = sint_fill_value
   else if (temp_short_ctp_uncertainty .lt. output_data%ctp_uncertainty_vmin) then
      output_data%ctt2_uncertainty(i,j) = sint_fill_value
   else if (temp_short_ctp_uncertainty .gt. output_data%ctp_uncertainty_vmax) then
      output_data%ctt2_uncertainty(i,j) = output_data%ctt_uncertainty_vmax
   else
      temp_real = abs(RTM_Pc(2)%dTc_dPc) * temp_real_ctp_uncertainty
      call prepare_short_packed_float( &
           temp_real, output_data%ctt2_uncertainty(i,j), &
           output_data%ctt_uncertainty_scale, output_data%ctt_uncertainty_offset, &
           output_data%ctt_uncertainty_vmin, output_data%ctt_uncertainty_vmax, &
           sreal_fill_value, output_data%ctt_uncertainty_vmax)
   end if

   !----------------------------------------------------------------------------
   ! cwp, cwp_uncertainty
   !----------------------------------------------------------------------------
   call prepare_short_packed_float( &
        SPixel%CWP2, output_data%cwp2(i,j), &
        output_data%cwp_scale, output_data%cwp_offset, &
        output_data%cwp_vmin, output_data%cwp_vmax, &
        MissingXn, output_data%cwp_vmax)

   temp_real = sqrt(SPixel%CWP2_uncertainty)
   call prepare_short_packed_float( &
        temp_real, output_data%cwp2_uncertainty(i,j), &
        output_data%cwp_uncertainty_scale, output_data%cwp_uncertainty_offset, &
        output_data%cwp_uncertainty_vmin, output_data%cwp_uncertainty_vmax, &
        MissingSn, output_data%cwp_uncertainty_vmax, &
        control=SPixel%CWP_uncertainty)
end if

   !----------------------------------------------------------------------------
   ! convergence, niter
   !----------------------------------------------------------------------------
   if (Diag%QCFlag .eq. -1) then
      output_data%niter(i,j) = byte_fill_value
   else
      if (Diag%Converged) then
         output_data%niter(i,j) = int(Diag%Iterations, kind=byte)
      else
         output_data%niter(i,j) = byte_fill_value
      end if
   end if

   !----------------------------------------------------------------------------
   ! costja
   !----------------------------------------------------------------------------
   call prepare_float_packed_float( &
        Diag%Ja, output_data%costja(i,j), &
        output_data%costja_scale, output_data%costja_offset, &
        output_data%costja_vmin, output_data%costja_vmax, &
        MissingSn, sreal_fill_value)

   !----------------------------------------------------------------------------
   ! costjm
   !----------------------------------------------------------------------------
   call prepare_float_packed_float( &
        Diag%Jm, output_data%costjm(i,j), &
        output_data%costjm_scale, output_data%costjm_offset, &
        output_data%costjm_vmin, output_data%costjm_vmax, &
        MissingSn, sreal_fill_value)

   !----------------------------------------------------------------------------
   ! qcflag, channels_used, variables_retrieved
   !----------------------------------------------------------------------------
   output_data%qcflag(i,j) = Diag%QCFlag
   output_data%channels_used(i,j) = SPixel%channels_used
   output_data%variables_retrieved(i,j) = SPixel%variables_retrieved

   !----------------------------------------------------------------------------
   ! lsflag
   !----------------------------------------------------------------------------
   output_data%lsflag(i,j) = int(MSI_Data%LSFlags(SPixel%Loc%X0, SPixel%Loc%Y0), &
                                 kind=byte)

   !----------------------------------------------------------------------------
   ! lusflag
   !----------------------------------------------------------------------------
   output_data%lusflag(i,j) = int(MSI_Data%lusflags(SPixel%Loc%X0, &
                                                    SPixel%Loc%Y0), kind=byte)

   !----------------------------------------------------------------------------
   ! dem
   !----------------------------------------------------------------------------
   output_data%dem(i,j) = int(MSI_Data%dem(SPixel%Loc%X0, SPixel%Loc%Y0), &
                              kind=sint)

   !----------------------------------------------------------------------------
   ! illum
   !----------------------------------------------------------------------------
   output_data%illum(i,j) = int(MSI_Data%illum(SPixel%Loc%X0, &
                                               SPixel%Loc%Y0, 1), kind=byte)

   !----------------------------------------------------------------------------
   ! cldtype
   !----------------------------------------------------------------------------
   output_data%cldtype(i,j,:) = int(MSI_Data%cldtype(SPixel%Loc%X0, &
                                                     SPixel%Loc%Y0,:), kind=byte)

   !----------------------------------------------------------------------------
   ! cldmask
   !----------------------------------------------------------------------------
if (Ctrl%Ind%flags%do_cldmask) then
   output_data%cldmask(i,j,:) = int(MSI_Data%cldmask(SPixel%Loc%X0, &
                                                     SPixel%Loc%Y0,:), kind=byte)
end if
if (Ctrl%Ind%flags%do_cldmask_uncertainty) then
   !----------------------------------------------------------------------------
   ! cldmask_uncertainty
   !----------------------------------------------------------------------------
   do k = 1, Ctrl%Ind%NViews
      call prepare_short_packed_float( &
           MSI_Data%cldmask_uncertainty(SPixel%Loc%X0, SPixel%Loc%Y0, k), &
           output_data%cldmask_uncertainty(i,j,k), &
           output_data%cldmask_uncertainty_scale, &
           output_data%cldmask_uncertainty_offset, &
           output_data%cldmask_uncertainty_vmin, &
           output_data%cldmask_uncertainty_vmax, &
           sreal_fill_value, sint_fill_value)
   end do
end if

    !----------------------------------------------------------------------------
! ann_phase
   !----------------------------------------------------------------------------
if (Ctrl%Ind%flags%do_ann_phase) then
   output_data%ann_phase(i,j,:) = int(MSI_Data%ann_phase(SPixel%Loc%X0, &
        SPixel%Loc%Y0,:), kind=byte)
   do k = 1, Ctrl%Ind%NViews
      call prepare_short_packed_float( &
           MSI_Data%cphcot(SPixel%Loc%X0, SPixel%Loc%Y0, k), &
           output_data%cphcot(i,j,k), &
           output_data%cphcot_scale, &
           output_data%cphcot_offset, &
           output_data%cphcot_vmin, &
           output_data%cphcot_vmax, &
           sreal_fill_value, sint_fill_value)
   end do
end if

if (Ctrl%Ind%flags%do_ann_phase_uncertainty) then
   !----------------------------------------------------------------------------
   ! ann_phase_uncertainty
   !----------------------------------------------------------------------------
   do k = 1, Ctrl%Ind%NViews
      call prepare_short_packed_float( &
           MSI_Data%ann_phase_uncertainty(SPixel%Loc%X0, SPixel%Loc%Y0, k), &
           output_data%ann_phase_uncertainty(i,j,k), &
           output_data%ann_phase_uncertainty_scale, &
           output_data%ann_phase_uncertainty_offset, &
           output_data%ann_phase_uncertainty_vmin, &
           output_data%ann_phase_uncertainty_vmax, &
           sreal_fill_value, sint_fill_value)
   end do
end if

  !----------------------------------------------------------------------------
   ! phase
   !----------------------------------------------------------------------------
if (Ctrl%Ind%flags%do_phase) then
   output_data%phase(i,j) = 1_byte
end if

end subroutine prepare_output_primary
