!-------------------------------------------------------------------------------
! Name: prepare_output_secondary.F90
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
! lcovar      logical In          If set, output covariance matrices
! i           int     In          Across-track pixel to output
! j           int     In          Along-track pixel to output
! MSI_Data    struct  In          Imager data structure
! SPixel      struct  In          Retrieval pixel structure
! Diag        struct  In          Diagonstic structure
! output_data struct  Both        Results structure
!
! History:
! 2011/12/19, MJ: Creates initial version
! 2012/01/05, CP: Add in reflectances and brightness temperature
! 2012/01/15, CP: Changed how offset was applied
! 2012/09/20, CP: Remove scaling factor from albedo
! 2012/09/20, CP: Chanaged how svan value is set
! 2013/05/29, GT: Added degrees of freedom for signal
! 2014/01/30, GM: Fixed writing of the residuals and first guess in the case of
!    nighttime pixels.
! 2014/06/13, GM: Put the code into a subroutine.
! 2014/06/13, GM: Cleaned up the code.
! 2014/08/31, GM: Update to use general routines in the current module.
! 2014/01/30, AP: Replace YSeg0 with Y0 as superpixeling removed.
! 2015/09/06, GM: Output fill_value instead of zero for degrees of freedom for
!    signal for non-retrieved pixels.
! 2015/09/14, GM: Change output cot_ap and cot_fg from log10 space to linear
!    space.
! 2015/12/28, AP: Add output fields for aerosol retrievals.
! 2015/12/30, AP: Have all albedo fields use the same values.
! 2016/01/06, AP: Wrap do_* flags into output_flags structure.
! 2016/01/07, AP: Check for valid AK values relied on short-circuiting, which
!    isn't standard Fortran.
! 2016/03/04, AP: Tidy prepare_*_packed_float. Make MissingXn the only value
!    checked for in X, X0, Xb (sreal_fill_value had been ocassionally checked).
! 2016/07/27, GM: Add output fields for the multilayer retrieval.
! 2023/10/10, GT: Added optional output of measurement uncertainties
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine prepare_output_secondary(Ctrl, i, j, MSI_Data, SPixel, Diag, &
                                    output_data)

   use Ctrl_m
   use Data_m
   use Diag_m
   use orac_ncdf_m
   use orac_output_m
   use SPixel_m

   implicit none

   type(Ctrl_t),                  intent(in)    :: Ctrl
   integer,                       intent(in)    :: i, j
   type(Data_t),                  intent(in)    :: MSI_Data
   type(SPixel_t),                intent(in)    :: SPixel
   type(Diag_t),                  intent(in)    :: Diag
   type(output_data_secondary_t), intent(inout) :: output_data

   integer          :: k, kk, l, ll, i_rho
   real(kind=sreal) :: dummyreal


   !----------------------------------------------------------------------------
   ! scanline_u, scanline_v
   !----------------------------------------------------------------------------
   output_data%scanline_u(i,j) = i
   output_data%scanline_v(i,j) = j

if (Ctrl%Ind%flags%do_aerosol) then
   !----------------------------------------------------------------------------
   ! aot550_ap, aot550_fg
   !----------------------------------------------------------------------------
   dummyreal = 10.0**SPixel%Xb(ITau)
   call prepare_short_packed_float( &
        dummyreal, output_data%aot550_ap(i,j), &
        output_data%aot550_ap_scale, output_data%aot550_ap_offset, &
        output_data%aot550_ap_vmin, output_data%aot550_ap_vmax, &
        MissingXn, output_data%aot550_ap_vmax, &
        control=SPixel%Xb(ITau))

   dummyreal = 10.0**SPixel%X0(ITau)
   call prepare_short_packed_float( &
        dummyreal, output_data%aot550_fg(i,j), &
        output_data%aot550_fg_scale, output_data%aot550_fg_offset, &
        output_data%aot550_fg_vmin, output_data%aot550_fg_vmax, &
        MissingXn, output_data%aot550_fg_vmax, &
        control=SPixel%X0(ITau))

   !----------------------------------------------------------------------------
   ! aer_ap, aer_fg
   !----------------------------------------------------------------------------
   dummyreal = 10.0**SPixel%Xb(IRe)
   call prepare_short_packed_float( &
        dummyreal, output_data%aer_ap(i,j), &
        output_data%aer_ap_scale, output_data%aer_ap_offset, &
        output_data%aer_ap_vmin, output_data%aer_ap_vmax, &
        MissingXn, output_data%aer_ap_vmax, &
        control=SPixel%Xb(IRe))

   dummyreal = 10.0**SPixel%X0(IRe)
   call prepare_short_packed_float( &
        dummyreal, output_data%aer_fg(i,j), &
        output_data%aer_fg_scale, output_data%aer_fg_offset, &
        output_data%aer_fg_vmin, output_data%aer_fg_vmax, &
        MissingXn, output_data%aer_fg_vmax, &
        control=SPixel%X0(IRe))
end if

if (Ctrl%Ind%flags%do_rho) then
   !----------------------------------------------------------------------------
   ! rho_ap, rho_fg
   !----------------------------------------------------------------------------
   i_rho = 0
   do k = 1, SPixel%Ind%NSolar
      kk = SPixel%spixel_y_solar_to_ctrl_y_solar_index(k)

      do l = 1, MaxRho_XX
         if (Ctrl%Ind%rho_terms(kk,l)) then
            i_rho = i_rho + 1

            call prepare_short_packed_float( &
                 SPixel%Xb(IRs(kk,l)), output_data%rho_ap(i,j,i_rho), &
                 output_data%rho_ap_scale, output_data%rho_ap_offset, &
                 output_data%rho_ap_vmin, output_data%rho_ap_vmax, &
                 MissingXn, sint_fill_value)

            call prepare_short_packed_float( &
                 SPixel%X0(IRs(kk,l)), output_data%rho_fg(i,j,i_rho), &
                 output_data%rho_fg_scale, output_data%rho_fg_offset, &
                 output_data%rho_fg_vmin, output_data%rho_fg_vmax, &
                 MissingXn, sint_fill_value)
         end if
      end do
   end do
end if

if (Ctrl%Ind%flags%do_swansea) then
   !----------------------------------------------------------------------------
   ! swansea_s_ap, swansea_s_fg
   !----------------------------------------------------------------------------
   i_rho = 0
   do k = 1, SPixel%Ind%NSolar
      kk = SPixel%spixel_y_solar_to_ctrl_y_solar_index(k)

      if (Ctrl%Ind%ss_terms(kk)) then
         i_rho = i_rho + 1

         call prepare_short_packed_float( &
              SPixel%Xb(ISS(kk)), output_data%swansea_s_ap(i,j,i_rho), &
              output_data%swansea_s_ap_scale, output_data%swansea_s_ap_offset, &
              output_data%swansea_s_ap_vmin, output_data%swansea_s_ap_vmax, &
              MissingXn, output_data%swansea_s_ap_vmax)

         call prepare_short_packed_float( &
              SPixel%X0(ISS(kk)), output_data%swansea_s_fg(i,j,i_rho), &
              output_data%swansea_s_fg_scale, output_data%swansea_s_fg_offset, &
              output_data%swansea_s_fg_vmin, output_data%swansea_s_fg_vmax, &
              MissingXn, output_data%swansea_s_fg_vmax)
      end if
   end do

   do k = 1, Ctrl%Ind%NViews
      if (any(SPixel%X .eq. ISP(k))) then
         call prepare_short_packed_float( &
              SPixel%Xb(ISP(k)), output_data%swansea_p_ap(i,j,k), &
              output_data%swansea_p_ap_scale, output_data%swansea_p_ap_offset, &
              output_data%swansea_p_ap_vmin, output_data%swansea_p_ap_vmax, &
              MissingXn, output_data%swansea_p_ap_vmax)

         call prepare_short_packed_float( &
              SPixel%X0(ISP(k)), output_data%swansea_p_fg(i,j,k), &
              output_data%swansea_p_fg_scale, output_data%swansea_p_fg_offset, &
              output_data%swansea_p_fg_vmin, output_data%swansea_p_fg_vmax, &
              MissingXn, output_data%swansea_p_fg_vmax)
      end if
   end do
end if

if (Ctrl%Ind%flags%do_cloud) then
   !----------------------------------------------------------------------------
   ! cot_ap, cot_fg
   !----------------------------------------------------------------------------
   dummyreal = 10.0**SPixel%Xb(ITau)
   call prepare_short_packed_float( &
        dummyreal, output_data%cot_ap(i,j), &
        output_data%cot_ap_scale, output_data%cot_ap_offset, &
        output_data%cot_ap_vmin, output_data%cot_ap_vmax, &
        MissingXn, output_data%cot_ap_vmax, &
        control=SPixel%Xb(ITau))

   dummyreal = 10.0**SPixel%X0(ITau)
   call prepare_short_packed_float( &
        dummyreal, output_data%cot_fg(i,j), &
        output_data%cot_fg_scale, output_data%cot_fg_offset, &
        output_data%cot_fg_vmin, output_data%cot_fg_vmax, &
        MissingXn, output_data%cot_fg_vmax, &
        control=SPixel%X0(ITau))

   !----------------------------------------------------------------------------
   ! cer_ap, cer_fg
   !----------------------------------------------------------------------------
   call prepare_short_packed_float( &
        SPixel%Xb(IRe), output_data%cer_ap(i,j), &
        output_data%cer_ap_scale, output_data%cer_ap_offset, &
        output_data%cer_ap_vmin, output_data%cer_ap_vmax, &
        MissingXn, output_data%cer_ap_vmax)

   call prepare_short_packed_float( &
        SPixel%X0(IRe), output_data%cer_fg(i,j), &
        output_data%cer_fg_scale, output_data%cer_fg_offset, &
        output_data%cer_fg_vmin, output_data%cer_fg_vmax, &
        MissingXn, output_data%cer_fg_vmax)

   !----------------------------------------------------------------------------
   ! ctp_ap, ctp_fg
   !----------------------------------------------------------------------------
   call prepare_short_packed_float( &
        SPixel%Xb(IPc), output_data%ctp_ap(i,j), &
        output_data%ctp_ap_scale, output_data%ctp_ap_offset, &
        output_data%ctp_ap_vmin, output_data%ctp_ap_vmax, &
        MissingXn, output_data%ctp_ap_vmax)

   call prepare_short_packed_float( &
        SPixel%X0(IPc), output_data%ctp_fg(i,j), &
        output_data%ctp_fg_scale, output_data%ctp_fg_offset, &
        output_data%ctp_fg_vmin, output_data%ctp_fg_vmax, &
        MissingXn, output_data%ctp_fg_vmax)

   !----------------------------------------------------------------------------
   ! stemp_ap, stemp_fg
   !----------------------------------------------------------------------------
   call prepare_short_packed_float( &
        SPixel%X0(ITs), output_data%stemp_ap(i,j), &
        output_data%stemp_ap_scale, output_data%stemp_ap_offset, &
        output_data%stemp_ap_vmin, output_data%stemp_ap_vmax, &
        MissingXn, output_data%stemp_ap_vmax)

   call prepare_short_packed_float( &
        SPixel%X0(ITs), output_data%stemp_fg(i,j), &
        output_data%stemp_fg_scale, output_data%stemp_fg_offset, &
        output_data%stemp_fg_vmin, output_data%stemp_fg_vmax, &
        MissingXn, output_data%stemp_fg_vmax)

   !----------------------------------------------------------------------------
   ! albedo
   !----------------------------------------------------------------------------
   do k = 1, Ctrl%Ind%NSolar
      call prepare_short_packed_float( &
           MSI_Data%ALB(SPixel%Loc%X0,SPixel%Loc%Y0,k), &
           output_data%albedo(i,j,k), &
           output_data%albedo_scale, output_data%albedo_offset, &
           output_data%albedo_vmin, output_data%albedo_vmax, &
           sreal_fill_value, sint_fill_value)
   end do
end if

if (Ctrl%Ind%flags%do_cloud_layer_2) then
   !----------------------------------------------------------------------------
   ! cot2_ap, cot2_fg
   !----------------------------------------------------------------------------
   dummyreal = 10.0**SPixel%Xb(ITau2)
   call prepare_short_packed_float( &
        dummyreal, output_data%cot2_ap(i,j), &
        output_data%cot_ap_scale, output_data%cot_ap_offset, &
        output_data%cot_ap_vmin, output_data%cot_ap_vmax, &
        MissingXn, output_data%cot_ap_vmax, &
        control=SPixel%Xb(ITau2))

   dummyreal = 10.0**SPixel%X0(ITau2)
   call prepare_short_packed_float( &
        dummyreal, output_data%cot2_fg(i,j), &
        output_data%cot_fg_scale, output_data%cot_fg_offset, &
        output_data%cot_fg_vmin, output_data%cot_fg_vmax, &
        MissingXn, output_data%cot_fg_vmax, &
        control=SPixel%X0(ITau2))

   !----------------------------------------------------------------------------
   ! cer2_ap, cer2_fg
   !----------------------------------------------------------------------------
   call prepare_short_packed_float( &
        SPixel%Xb(IRe2), output_data%cer2_ap(i,j), &
        output_data%cer_ap_scale, output_data%cer_ap_offset, &
        output_data%cer_ap_vmin, output_data%cer_ap_vmax, &
        MissingXn, output_data%cer_ap_vmax)

   call prepare_short_packed_float( &
        SPixel%X0(IRe2), output_data%cer2_fg(i,j), &
        output_data%cer_fg_scale, output_data%cer_fg_offset, &
        output_data%cer_fg_vmin, output_data%cer_fg_vmax, &
        MissingXn, output_data%cer_fg_vmax)

   !----------------------------------------------------------------------------
   ! ctp2_ap, ctp2_fg
   !----------------------------------------------------------------------------
   call prepare_short_packed_float( &
        SPixel%Xb(IPc2), output_data%ctp2_ap(i,j), &
        output_data%ctp_ap_scale, output_data%ctp_ap_offset, &
        output_data%ctp_ap_vmin, output_data%ctp_ap_vmax, &
        MissingXn, output_data%ctp_ap_vmax)

   call prepare_short_packed_float( &
        SPixel%X0(IPc2), output_data%ctp2_fg(i,j), &
        output_data%ctp_fg_scale, output_data%ctp_fg_offset, &
        output_data%ctp_fg_vmin, output_data%ctp_fg_vmax, &
        MissingXn, output_data%ctp_fg_vmax)
end if

   !----------------------------------------------------------------------------
   ! channels
   !----------------------------------------------------------------------------
   do k = 1, Ctrl%Ind%Ny
      call prepare_short_packed_float( &
           MSI_Data%MSI(SPixel%Loc%X0, SPixel%Loc%Y0, k), &
           output_data%channels(i,j,k), &
           output_data%channels_scale(k), output_data%channels_offset(k), &
           output_data%channels_vmin(k), output_data%channels_vmax(k), &
           sreal_fill_value, sint_fill_value)
   end do

   !----------------------------------------------------------------------------
   ! Measurement error (diagonals)
   !----------------------------------------------------------------------------
   if (Ctrl%Ind%flags%do_meas_error) then
      do k = 1, SPixel%Ind%Ny
         kk = SPixel%spixel_y_to_ctrl_y_index(k)

         call prepare_short_packed_float( &
              sqrt(SPixel%Sy(k,k)), output_data%Sy(i,j,kk), &
              output_data%Sy_scale(kk), output_data%Sy_offset(kk), &
              output_data%Sy_vmin(kk), output_data%Sy_vmax(kk), &
              sreal_fill_value, sint_fill_value)
      end do
   end if

   !----------------------------------------------------------------------------
   ! y0
   !----------------------------------------------------------------------------
   do k = 1, SPixel%Ind%Ny
      kk = SPixel%spixel_y_to_ctrl_y_index(k)

      call prepare_short_packed_float( &
           Diag%Y0(k), output_data%y0(i,j,kk), &
           output_data%y0_scale(kk), output_data%y0_offset(kk), &
           output_data%y0_vmin(kk), output_data%y0_vmax(kk), &
           sreal_fill_value, sint_fill_value)
   end do

   !----------------------------------------------------------------------------
   ! residuals
   !----------------------------------------------------------------------------
   do k = 1, SPixel%Ind%Ny
      kk = SPixel%spixel_y_to_ctrl_y_index(k)

      call prepare_short_packed_float( &
           Diag%YmFit(k), output_data%residuals(i,j,kk), &
           output_data%residuals_scale(kk), output_data%residuals_offset(kk), &
           output_data%residuals_vmin(kk), output_data%residuals_vmax(kk), &
           sreal_fill_value, sint_fill_value)
   end do

   !----------------------------------------------------------------------------
   ! ds
   !----------------------------------------------------------------------------
   if (SPixel%Nx .eq. 0) then
      dummyreal = sreal_fill_value
   else if (any(Diag%AK(SPixel%X, SPixel%X) .eq. MissingXn)) then
      dummyreal = sreal_fill_value
   else
      dummyreal = 0.0

      do k = 1, SPixel%Nx
         dummyreal = dummyreal + Diag%AK(SPixel%X(k),SPixel%X(k))
      end do
   end if
   call prepare_short_packed_float( &
        dummyreal, output_data%ds(i,j), &
        output_data%ds_scale, output_data%ds_offset, &
        output_data%ds_vmin, output_data%ds_vmax, &
        sreal_fill_value, sint_fill_value)

   !----------------------------------------------------------------------------
   ! covariance
   !----------------------------------------------------------------------------
if (Ctrl%Ind%flags%do_covariance) then
   do k = 1, SPixel%Nx
      kk = SPixel%spixel_y_to_ctrl_y_index(k)
      do l = 1, SPixel%Nx
         ll = SPixel%spixel_y_to_ctrl_y_index(l)
         call prepare_float_packed_float( &
             real(SPixel%Sn(k,l), kind=sreal), output_data%covariance(i,j,kk,ll), &
             1._sreal, 0._sreal, 0._sreal, huge(dummyreal), &
             sreal_fill_value, sreal_fill_value)
      end do
   end do
end if

end subroutine prepare_output_secondary
