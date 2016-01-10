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
! Diag        struct  In          Diagonstic structure
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
! 2014/10/24, OS: Added variables cldtype, cldmask, cccot_pre, lusflags,
!    dem, and nisemask.
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
! 2016/01/06, AP: Wrap do_* flags into output_flags structure.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine prepare_output_primary(Ctrl, i, j, MSI_Data, RTM_Pc, SPixel, Diag, &
                                  output_data, output_flags)

   use CTRL_def
   use Data_def
   use Diag_def
   use orac_ncdf
   use orac_output
   use RTM_Pc_def
   use SPixel_def

   implicit none

   type(CTRL_t),              intent(in)    :: Ctrl
   integer,                   intent(in)    :: i, j
   type(Data_t),              intent(in)    :: MSI_Data
   type(RTM_Pc_t),            intent(in)    :: RTM_Pc
   type(SPixel_t),            intent(in)    :: SPixel
   type(Diag_t),              intent(in)    :: Diag
   type(output_data_primary), intent(inout) :: output_data
   type(output_data_flags),   intent(in)    :: output_flags

   integer            :: k, kk
   integer(kind=sint) :: temp_short_ctp_error
   real(kind=sreal)   :: temp_real, temp_real_ot, temp_real_ctp_error


   !----------------------------------------------------------------------------
   ! time
   !----------------------------------------------------------------------------
   output_data%time(i,j) = MSI_Data%time(SPixel%Loc%X0, SPixel%Loc%Y0)


   !----------------------------------------------------------------------------
   ! lat, lon
   !----------------------------------------------------------------------------
   output_data%lat(i,j) = MSI_Data%Location%Lat(SPixel%Loc%X0, SPixel%Loc%Y0)
   output_data%lon(i,j) = MSI_Data%Location%Lon(SPixel%Loc%X0, SPixel%Loc%Y0)

   !----------------------------------------------------------------------------
   ! sol_zen, sat_zen, rel_azi
   !----------------------------------------------------------------------------
   do k=1,Ctrl%Ind%NViews
      output_data%sol_zen(i,j,k) = MSI_Data%Geometry%Sol(SPixel%Loc%X0, SPixel%Loc%Y0,k)
      output_data%sat_zen(i,j,k) = MSI_Data%Geometry%Sat(SPixel%Loc%X0, SPixel%Loc%Y0,k)
      output_data%rel_azi(i,j,k) = MSI_Data%Geometry%Azi(SPixel%Loc%X0, SPixel%Loc%Y0,k)
   end do

if (output_flags%do_aerosol) then
   !----------------------------------------------------------------------------
   ! aot, aot_error
   !----------------------------------------------------------------------------
   if (SPixel%Xn(ITau) .eq. MissingXn) then
      temp_real_ot = sreal_fill_value
   else
      temp_real_ot = 10.0**SPixel%Xn(ITau)
   end if
   call prepare_short_packed_float( &
           temp_real_ot, output_data%aot550(i,j), &
           output_data%aot550_scale, output_data%aot550_offset, &
           sreal_fill_value, sint_fill_value, &
           output_data%aot550_vmin, output_data%aot550_vmax, &
           output_data%aot550_vmax)

   if (SPixel%Sn(ITau,ITau) .eq. MissingSn) then
      temp_real = sreal_fill_value
   else
      temp_real = sqrt(SPixel%Sn(ITau,ITau)) * temp_real_ot * alog(10.0)
   end if
   call prepare_short_packed_float( &
           temp_real, output_data%aot550_error(i,j), &
           output_data%aot550_error_scale, output_data%aot550_error_offset, &
           sreal_fill_value, sint_fill_value, &
           output_data%aot550_error_vmin, output_data%aot550_error_vmax, &
           output_data%aot550_error_vmax)

   !----------------------------------------------------------------------------
   ! aer, aer_error
   !----------------------------------------------------------------------------
   if (SPixel%Xn(IRe) .eq. MissingXn) then
      temp_real_ot = sreal_fill_value
   else
      temp_real_ot = 10.0**SPixel%Xn(IRe)
   end if
   call prepare_short_packed_float( &
           temp_real_ot, output_data%aer(i,j), &
           output_data%aer_scale, output_data%aer_offset, &
           sreal_fill_value, sint_fill_value, &
           output_data%aer_vmin, output_data%aer_vmax, &
           output_data%aer_vmax)

   if (SPixel%Sn(IRe,IRe) .eq. MissingSn) then
      temp_real = sreal_fill_value
   else
      temp_real = sqrt(SPixel%Sn(IRe,IRe)) * temp_real_ot * alog(10.0)
   end if
   call prepare_short_packed_float( &
           temp_real, output_data%aer_error(i,j), &
           output_data%aer_error_scale, output_data%aer_error_offset, &
           sreal_fill_value, sint_fill_value, &
           output_data%aer_error_vmin, output_data%aer_error_vmax, &
           output_data%aer_error_vmax)
end if

if (output_flags%do_swansea) then
   do k=1,SPixel%Ind%NSolar
      kk = SPixel%spixel_y_solar_to_ctrl_y_solar_index(k)

   !----------------------------------------------------------------------------
   ! swansea_s, swansea_s_error
   !----------------------------------------------------------------------------
      if (SPixel%Xn(ISS(kk)) .eq. MissingXn) then
         temp_real = sreal_fill_value
      else
         temp_real = SPixel%Xn(ISS(kk))
      end if
      call prepare_short_packed_float( &
           temp_real, output_data%swansea_s(i,j,kk), &
           output_data%swansea_s_scale, &
           output_data%swansea_s_offset, &
           sreal_fill_value, sint_fill_value, &
           output_data%swansea_s_vmin, &
           output_data%swansea_s_vmax, &
           sint_fill_value)

      if (SPixel%Sn(ISS(kk),ISS(kk)) .eq. MissingSn) then
         temp_real = sreal_fill_value
      else
         temp_real = sqrt(SPixel%Sn(ISS(kk),ISS(kk)))
      end if
      call prepare_short_packed_float( &
           temp_real, output_data%swansea_s_error(i,j,kk), &
           output_data%swansea_s_error_scale, &
           output_data%swansea_s_error_offset, &
           sreal_fill_value, sint_fill_value, &
           output_data%swansea_s_error_vmin, &
           output_data%swansea_s_error_vmax, &
           sint_fill_value)
   end do

   !----------------------------------------------------------------------------
   ! swansea_p, swansea_p_error
   !----------------------------------------------------------------------------
   do k=1,Ctrl%Ind%NViews
      if (any(SPixel%X .eq. ISP(k))) then
         if (SPixel%Xn(ISP(k)) .eq. MissingXn) then
            temp_real = sreal_fill_value
         else
            temp_real = SPixel%Xn(ISP(k))
         end if
         call prepare_short_packed_float( &
              temp_real, output_data%swansea_p(i,j,k), &
              output_data%swansea_p_scale, &
              output_data%swansea_p_offset, &
              sreal_fill_value, sint_fill_value, &
              output_data%swansea_p_vmin, &
              output_data%swansea_p_vmax, &
              sint_fill_value)

         if (SPixel%Sn(ISP(k),ISP(k)) .eq. MissingSn) then
            temp_real = sreal_fill_value
         else
            temp_real = sqrt(SPixel%Sn(ISP(k),ISP(k)))
         end if
         call prepare_short_packed_float( &
              temp_real, output_data%swansea_p_error(i,j,k), &
              output_data%swansea_p_error_scale, &
              output_data%swansea_p_error_offset, &
              sreal_fill_value, sint_fill_value, &
              output_data%swansea_p_error_vmin, &
              output_data%swansea_p_error_vmax, &
              sint_fill_value)
      end if
   end do
end if

if (output_flags%do_cloud) then
   !----------------------------------------------------------------------------
   ! cot, cot_error
   !----------------------------------------------------------------------------
   if (SPixel%Xn(ITau) .eq. MissingXn) then
      temp_real_ot = sreal_fill_value
   else
      temp_real_ot = 10.0**SPixel%Xn(ITau)
   end if
   call prepare_short_packed_float( &
           temp_real_ot, output_data%cot(i,j), &
           output_data%cot_scale, output_data%cot_offset, &
           sreal_fill_value, sint_fill_value, &
           output_data%cot_vmin, output_data%cot_vmax, &
           output_data%cot_vmax)

   if (SPixel%Sn(ITau,ITau) .eq. MissingSn) then
      temp_real = sreal_fill_value
   else
      temp_real = sqrt(SPixel%Sn(ITau,ITau)) * temp_real_ot * alog(10.0)
   end if
   call prepare_short_packed_float( &
           temp_real, output_data%cot_error(i,j), &
           output_data%cot_error_scale, output_data%cot_error_offset, &
           sreal_fill_value, sint_fill_value, &
           output_data%cot_error_vmin, output_data%cot_error_vmax, &
           output_data%cot_error_vmax)

   !----------------------------------------------------------------------------
   ! cer, cer_error
   !----------------------------------------------------------------------------
   if (SPixel%Xn(IRe) .eq. MissingXn) then
      temp_real = sreal_fill_value
   else
      temp_real = SPixel%Xn(IRe)
   end if
   call prepare_short_packed_float( &
           temp_real, output_data%cer(i,j), &
           output_data%cer_scale, output_data%cer_offset, &
           sreal_fill_value, sint_fill_value, &
           output_data%cer_vmin, output_data%cer_vmax, &
           output_data%cer_vmax)

   if (SPixel%Sn(IRe,IRe) .eq. MissingSn) then
      temp_real = sreal_fill_value
   else
      temp_real = sqrt(SPixel%Sn(IRe,IRe))
   end if
   call prepare_short_packed_float( &
           temp_real, output_data%cer_error(i,j), &
           output_data%cer_error_scale, output_data%cer_error_offset, &
           sreal_fill_value, sint_fill_value, &
           output_data%cer_error_vmin, output_data%cer_error_vmax, &
           output_data%cer_error_vmax)

   !----------------------------------------------------------------------------
   ! ctp, ctp_error
   !----------------------------------------------------------------------------
   if (SPixel%Xn(IPc) .eq. MissingXn) then
      temp_real = sreal_fill_value
   else
      temp_real = SPixel%Xn(IPc)
   end if
   call prepare_short_packed_float( &
           temp_real, output_data%ctp(i,j), &
           output_data%ctp_scale, output_data%ctp_offset, &
           sreal_fill_value, sint_fill_value, &
           output_data%ctp_vmin, output_data%ctp_vmax, &
           output_data%ctp_vmax)

   if (SPixel%Sn(IPc,IPc) .eq. MissingSn) then
      temp_real_ctp_error=sreal_fill_value
   else
      temp_real_ctp_error = sqrt(SPixel%Sn(IPc,IPc))
      temp_short_ctp_error = int((temp_real_ctp_error - &
                                  output_data%ctp_error_offset) / &
                                  output_data%ctp_error_scale, kind=sint)
   end if
   call prepare_short_packed_float( &
           temp_real_ctp_error, output_data%ctp_error(i,j), &
           output_data%ctp_error_scale, output_data%ctp_error_offset, &
           sreal_fill_value, sint_fill_value, &
           output_data%ctp_error_vmin, output_data%ctp_error_vmax, &
           output_data%ctp_error_vmax)

   !----------------------------------------------------------------------------
   ! cct, cct_error
   !----------------------------------------------------------------------------
   if (SPixel%Xn(IFr) .eq. MissingXn) then
      temp_real = sreal_fill_value
   else
      temp_real = SPixel%Xn(IFr)
   end if
   call prepare_short_packed_float( &
           temp_real, output_data%cct(i,j), &
           output_data%cct_scale, output_data%cct_offset, &
           sreal_fill_value, sint_fill_value, &
           output_data%cct_vmin, output_data%cct_vmax, &
           sint_fill_value)

   if (SPixel%Sn(IFr,IFr) .eq. MissingSn) then
      temp_real = sreal_fill_value
   else
      temp_real = sqrt(SPixel%Sn(IFr,IFr))
   end if
   call prepare_short_packed_float( &
           temp_real, output_data%cct_error(i,j), &
           output_data%cct_error_scale, output_data%cct_error_offset, &
           sreal_fill_value, sint_fill_value, &
           output_data%cct_error_vmin, output_data%cct_error_vmax, &
           sint_fill_value)

   !----------------------------------------------------------------------------
   ! stemp, stemp_error
   !----------------------------------------------------------------------------
   if (SPixel%Xn(ITs) .eq. MissingXn) then
      temp_real = sreal_fill_value
   else
      temp_real = SPixel%Xn(ITs)
   end if
   call prepare_short_packed_float( &
           temp_real, output_data%stemp(i,j), &
           output_data%stemp_scale, output_data%stemp_offset, &
           sreal_fill_value, sint_fill_value, &
           output_data%stemp_vmin, output_data%stemp_vmax, &
           output_data%stemp_vmax)

   if (SPixel%Sn(ITs,ITs) .eq. MissingSn) then
      temp_real = sreal_fill_value
   else
      temp_real = sqrt(SPixel%Sn(ITs,ITs))
   end if
   call prepare_short_packed_float( &
           temp_real, output_data%stemp_error(i,j), &
           output_data%stemp_error_scale, output_data%stemp_error_offset, &
           sreal_fill_value, sint_fill_value, &
           output_data%stemp_error_vmin, output_data%stemp_error_vmax, &
           output_data%stemp_error_vmax)

   !----------------------------------------------------------------------------
   ! cth, cth_error
   !----------------------------------------------------------------------------
   if (RTM_Pc%Hc .eq. MissingXn) then
      temp_real = sreal_fill_value
   else
      temp_real = RTM_Pc%Hc/g_wmo/1000. ! now it's in km
   end if
   call prepare_short_packed_float( &
           temp_real, output_data%cth(i,j), &
           output_data%cth_scale, output_data%cth_offset, &
           sreal_fill_value, sint_fill_value, &
           output_data%cth_vmin, output_data%cth_vmax, &
           output_data%cth_vmax)

   ! If ctp_error is good compute cth_error
   if (temp_real_ctp_error .eq. sreal_fill_value) then
      output_data%cth_error(i,j)=sint_fill_value
   else if (temp_short_ctp_error .lt. output_data%ctp_error_vmin) then
      output_data%cth_error(i,j)=sint_fill_value
   else if (temp_short_ctp_error .gt. output_data%ctp_error_vmax) then
      output_data%cth_error(i,j)=output_data%cth_error_vmax
   else
      temp_real=abs(RTM_Pc%dHc_dPc/g_wmo/1000.)*temp_real_ctp_error
      call prepare_short_packed_float( &
           temp_real, output_data%cth_error(i,j), &
           output_data%cth_error_scale, output_data%cth_error_offset, &
           sreal_fill_value, sint_fill_value, &
           output_data%cth_error_vmin, output_data%cth_error_vmax, &
           output_data%cth_error_vmax)
   end if

   !----------------------------------------------------------------------------
   ! cth_corrected, cth_corrected_error
   !----------------------------------------------------------------------------
   if (SPixel%CTH_corrected .eq. MissingXn) then
      temp_real = sreal_fill_value
   else
      temp_real = SPixel%CTH_corrected/g_wmo/1000. ! now it's in km
   end if
   call prepare_short_packed_float( &
           temp_real, output_data%cth_corrected(i,j), &
           output_data%cth_scale, output_data%cth_offset, &
           sreal_fill_value, sint_fill_value, &
           output_data%cth_vmin, output_data%cth_vmax, &
           output_data%cth_vmax)

   ! If ctp_error is good compute cth_corrected_error
   if (SPixel%CTH_corrected_error .eq. MissingSn) then
      temp_real = sreal_fill_value
   else
      temp_real = SPixel%CTH_corrected_error/g_wmo/1000. ! now it's in km
   end if
   call prepare_short_packed_float( &
        temp_real, output_data%cth_corrected_error(i,j), &
        output_data%cth_error_scale, output_data%cth_error_offset, &
        sreal_fill_value, sint_fill_value, &
        output_data%cth_error_vmin, output_data%cth_error_vmax, &
        output_data%cth_error_vmax)

   !----------------------------------------------------------------------------
   ! ctt, ctt_error
   !----------------------------------------------------------------------------
   if (RTM_Pc%Tc .eq. MissingXn) then
      temp_real = sreal_fill_value
   else
      temp_real = RTM_Pc%Tc
   end if
   call prepare_short_packed_float( &
           temp_real, output_data%ctt(i,j), &
           output_data%ctt_scale, output_data%ctt_offset, &
           sreal_fill_value, sint_fill_value, &
           output_data%ctt_vmin, output_data%ctt_vmax, &
           output_data%ctt_vmax)

   ! If ctp_error is good compute ctt_error
   if (temp_real_ctp_error .eq. sreal_fill_value) then
      output_data%ctt_error(i,j)=sint_fill_value
   else if (temp_short_ctp_error .lt. output_data%ctp_error_vmin) then
      output_data%ctt_error(i,j)=sint_fill_value
   else if (temp_short_ctp_error .gt. output_data%ctp_error_vmax) then
      output_data%ctt_error(i,j)=output_data%ctt_error_vmax
   else
      temp_real=abs(RTM_Pc%dTc_dPc)*temp_real_ctp_error
      call prepare_short_packed_float( &
           temp_real, output_data%ctt_error(i,j), &
           output_data%ctt_error_scale, output_data%ctt_error_offset, &
           sreal_fill_value, sint_fill_value, &
           output_data%ctt_error_vmin, output_data%ctt_error_vmax, &
           output_data%ctt_error_vmax)
   end if

   !----------------------------------------------------------------------------
   ! cwp, cwp_error
   !----------------------------------------------------------------------------
   if (SPixel%CWP .eq. MissingXn) then
      temp_real = sreal_fill_value
   else
      temp_real = SPixel%CWP
   end if
   call prepare_short_packed_float( &
           temp_real, output_data%cwp(i,j), &
           output_data%cwp_scale, output_data%cwp_offset, &
           sreal_fill_value, sint_fill_value, &
           output_data%cwp_vmin, output_data%cwp_vmax, &
           output_data%cwp_vmax)

   if (SPixel%CWP_error .eq. MissingSn) then
      temp_real = sreal_fill_value
   else
      temp_real = sqrt(SPixel%CWP_error)
   end if
   call prepare_short_packed_float( &
           temp_real, output_data%cwp_error(i,j), &
           output_data%cwp_error_scale, output_data%cwp_error_offset, &
           sreal_fill_value, sint_fill_value, &
           output_data%cwp_error_vmin, output_data%cwp_error_vmax, &
           output_data%cwp_error_vmax)

   !----------------------------------------------------------------------------
   ! cloud_albedo, cloud_albedo_error
   !----------------------------------------------------------------------------
   do k=1,SPixel%Ind%NSolar
      kk = SPixel%spixel_y_solar_to_ctrl_y_solar_index(k)

      temp_real=Diag%cloud_albedo(k)
      call prepare_short_packed_float( &
           temp_real, output_data%cloud_albedo(i,j,kk), &
           output_data%cloud_albedo_scale, output_data%cloud_albedo_offset, &
           sreal_fill_value, sint_fill_value, &
           output_data%cloud_albedo_vmin, output_data%cloud_albedo_vmax, &
           sint_fill_value)

      if (Diag%cloud_albedo_s(k) .eq. sreal_fill_value) then
         temp_real = sreal_fill_value
      else
         temp_real = sqrt(Diag%cloud_albedo_s(k))
      end if
      call prepare_short_packed_float( &
           temp_real, output_data%cloud_albedo_error(i,j,kk), &
           output_data%cloud_albedo_error_scale, output_data%cloud_albedo_error_offset, &
           sreal_fill_value, sint_fill_value, &
           output_data%cloud_albedo_error_vmin, output_data%cloud_albedo_error_vmax, &
           sint_fill_value)
   end do

   !----------------------------------------------------------------------------
   ! cccot_pre
   !----------------------------------------------------------------------------
   temp_real=MSI_Data%cccot_pre(SPixel%Loc%X0, SPixel%Loc%Y0)
   call prepare_short_packed_float( &
           temp_real, output_data%cccot_pre(i,j), &
           output_data%cccot_pre_scale, output_data%cccot_pre_offset, &
           sreal_fill_value, sint_fill_value, &
           output_data%cccot_pre_vmin, output_data%cccot_pre_vmax, &
           sint_fill_value)
end if

   !----------------------------------------------------------------------------
   ! convergence, niter
   !----------------------------------------------------------------------------
   if (Diag%Converged .eq. byte_fill_value) then
      output_data%convergence(i,j) = byte_fill_value
      output_data%niter(i,j) = byte_fill_value
   else
      if (Diag%Converged .ne. 0) then
         output_data%convergence(i,j) = 0_byte
         output_data%niter(i,j) = int(Diag%Iterations,kind=byte)
      else
         output_data%convergence(i,j) = 1_byte
         output_data%niter(i,j) = byte_fill_value
      end if
   end if

   !----------------------------------------------------------------------------
   ! costja
   !----------------------------------------------------------------------------
   if (Diag%Ja .eq. MissingSn) then
      temp_real = sreal_fill_value
   else
      temp_real = Diag%Ja
   end if
   call prepare_float_packed_float( &
           temp_real, output_data%costja(i,j), &
           output_data%costja_scale, output_data%costja_offset, &
           sreal_fill_value, sreal_fill_value, &
           output_data%costja_vmin, output_data%costja_vmax, &
           sreal_fill_value)

   !----------------------------------------------------------------------------
   ! costjm
   !----------------------------------------------------------------------------
   if (Diag%Jm .eq. MissingSn) then
      temp_real = sreal_fill_value
   else
      temp_real = Diag%Jm
   end if
   call prepare_float_packed_float( &
           temp_real, output_data%costjm(i,j), &
           output_data%costjm_scale, output_data%costjm_offset, &
           sreal_fill_value, sreal_fill_value, &
           output_data%costjm_vmin, output_data%costjm_vmax, &
           sreal_fill_value)

   !----------------------------------------------------------------------------
   ! qcflag
   !----------------------------------------------------------------------------
   output_data%qcflag(i,j)=int(Diag%QCFlag,kind=lint)

   !----------------------------------------------------------------------------
   ! lsflag
   !----------------------------------------------------------------------------
   output_data%lsflag(i,j)=int(MSI_Data%LSFlags(SPixel%Loc%X0, SPixel%Loc%Y0), kind=byte)

   !----------------------------------------------------------------------------
   ! lusflag
   !----------------------------------------------------------------------------
   output_data%lusflag(i,j)=int(MSI_Data%lusflags(SPixel%Loc%X0, SPixel%Loc&
        %Y0), kind=byte)

   !----------------------------------------------------------------------------
   ! dem
   !----------------------------------------------------------------------------
   output_data%dem(i,j)=int(MSI_Data%dem(SPixel%Loc%X0, SPixel%Loc%Y0), kind=sint)

   !----------------------------------------------------------------------------
   ! nisemask
   !----------------------------------------------------------------------------
   output_data%nisemask(i,j)=int(MSI_Data%nisemask(SPixel%Loc%X0, SPixel%Loc&
        %Y0), kind=byte)

   !----------------------------------------------------------------------------
   ! illum
   !----------------------------------------------------------------------------
   output_data%illum(i,j)=MSI_Data%illum(SPixel%Loc%X0, SPixel%Loc%Y0,1)

   !----------------------------------------------------------------------------
   ! cldtype
   !----------------------------------------------------------------------------
   output_data%cldtype(i,j)=int(MSI_Data%cldtype(SPixel%Loc%X0, SPixel%Loc&
        %Y0), kind=byte)

   !----------------------------------------------------------------------------
   ! cldmask
   !----------------------------------------------------------------------------
if (output_flags%do_cldmask) then
   output_data%cldmask(i,j)=int(MSI_Data%cldmask(SPixel%Loc%X0, SPixel%Loc&
        %Y0), kind=byte)
end if
if (output_flags%do_cldmask_uncertainty) then
   !----------------------------------------------------------------------------
   ! cldmask_uncertainty
   !----------------------------------------------------------------------------
   temp_real=MSI_Data%cldmask_uncertainty(SPixel%Loc%X0, SPixel%Loc%Y0)
   call prepare_short_packed_float( &
           temp_real, output_data%cldmask_uncertainty(i,j), &
           output_data%cldmask_uncertainty_scale, output_data%cldmask_uncertainty_offset, &
           sreal_fill_value, sint_fill_value, &
           output_data%cldmask_uncertainty_vmin, output_data%cldmask_uncertainty_vmax, &
           sint_fill_value)
end if

   !----------------------------------------------------------------------------
   ! phase
   !----------------------------------------------------------------------------
   output_data%phase(i,j)=int(1,kind=byte)

end subroutine prepare_output_primary
