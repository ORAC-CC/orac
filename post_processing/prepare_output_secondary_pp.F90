!-------------------------------------------------------------------------------
! Name: prepare_output_secondary.F90
!
! Purpose:
! The file contains a collection of subroutines which define netcdf output for
! different attribute/variable type combinations.
!
! Description and Algorithm details:
!
! Arguments:
! Name Type In/Out/Both Description
!
! History:
! xxxx/xx/xx, MJ: Original version
! 2012/01/05, CP: Add in reflectances and brightness temperature
! 2012/01/15, CP: Changed how offset was applied
! 2015/07/16, GM: Major cleanup.
! 2015/10/07, OS: Renamed to *_pp.F90, as we have to avoid duplicate subroutine
!    names for wrapper
! 2015/12/30, AP: Have all albedo fields use the same values.
! 2016/03/04, AP: Tidy prepare_*_packed_float.
! 2017/01/09, CP: ML additions.
! 2023/10/10, GT: Added optional output of measurement uncertainties
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine prepare_output_secondary_pp(i, j, indexing, input_data, output_data)

   use orac_input_m
   use orac_ncdf_m
   use orac_output_m

   implicit none

   integer,                      intent(in)    :: i, j
   type(common_indices_t),       intent(in)    :: indexing
   type(input_data_secondary_t), intent(in)    :: input_data
   type(output_data_secondary_t),intent(inout) :: output_data

   integer :: k, l, i_rho

   output_data%scanline_u(i,j) = j
   output_data%scanline_v(i,j) = i

if (indexing%flags%do_aerosol) then
   !----------------------------------------------------------------------------
   ! aot550_ap, aot550_fg
   !----------------------------------------------------------------------------
   call prepare_short_packed_float( &
        input_data%aot550_ap(i,j), output_data%aot550_ap(i,j), &
        output_data%aot550_ap_scale, output_data%aot550_ap_offset, &
        output_data%aot550_ap_vmin, output_data%aot550_ap_vmax, &
        sreal_fill_value, output_data%aot550_ap_vmax)

   call prepare_short_packed_float( &
        input_data%aot550_fg(i,j), output_data%aot550_fg(i,j), &
        output_data%aot550_fg_scale, output_data%aot550_fg_offset, &
        output_data%aot550_fg_vmin, output_data%aot550_fg_vmax, &
        sreal_fill_value, output_data%aot550_fg_vmax)

   !----------------------------------------------------------------------------
   ! aer_ap, aer_fg
   !----------------------------------------------------------------------------
   call prepare_short_packed_float( &
        input_data%aer_ap(i,j), output_data%aer_ap(i,j), &
        output_data%aer_ap_scale, output_data%aer_ap_offset, &
        output_data%aer_ap_vmin, output_data%aer_ap_vmax, &
        sreal_fill_value, output_data%aer_ap_vmax)

   call prepare_short_packed_float( &
        input_data%aer_fg(i,j), output_data%aer_fg(i,j), &
        output_data%aer_fg_scale, output_data%aer_fg_offset, &
        output_data%aer_fg_vmin, output_data%aer_fg_vmax, &
        sreal_fill_value, output_data%aer_fg_vmax)
end if

if (indexing%flags%do_rho) then
   !----------------------------------------------------------------------------
   ! rho_ap, rho_fg
   !----------------------------------------------------------------------------
   i_rho = 0
   do k = 1, indexing%NSolar
      do l = 1, MaxRho_XX
         if (indexing%rho_terms(k,l)) then
            i_rho = i_rho + 1

            call prepare_short_packed_float( &
                 input_data%rho_ap(i,j,i_rho), output_data%rho_ap(i,j,i_rho), &
                 output_data%rho_ap_scale, output_data%rho_ap_offset, &
                 output_data%rho_ap_vmin, output_data%rho_ap_vmax, &
                 sreal_fill_value, output_data%rho_ap_vmax)

            call prepare_short_packed_float( &
                 input_data%rho_fg(i,j,i_rho), output_data%rho_fg(i,j,i_rho), &
                 output_data%rho_fg_scale, output_data%rho_fg_offset, &
                 output_data%rho_fg_vmin, output_data%rho_fg_vmax, &
                 sreal_fill_value, output_data%rho_fg_vmax)
         end if
      end do
   end do
end if

if (indexing%flags%do_swansea) then
   !----------------------------------------------------------------------------
   ! swansea_s_ap, swansea_s_fg
   !----------------------------------------------------------------------------
   i_rho = 0
   do k = 1, indexing%NSolar
      if (indexing%ss_terms(k)) then
         i_rho = i_rho + 1

         call prepare_short_packed_float( &
              input_data%swansea_s_ap(i,j,i_rho), &
              output_data%swansea_s_ap(i,j,i_rho), &
              output_data%swansea_s_ap_scale, &
              output_data%swansea_s_ap_offset, &
              output_data%swansea_s_ap_vmin, &
              output_data%swansea_s_ap_vmax, &
              sreal_fill_value, output_data%swansea_s_ap_vmax)

         call prepare_short_packed_float( &
              input_data%swansea_s_fg(i,j,i_rho), &
              output_data%swansea_s_fg(i,j,i_rho), &
              output_data%swansea_s_fg_scale, &
              output_data%swansea_s_fg_offset, &
              output_data%swansea_s_fg_vmin, &
              output_data%swansea_s_fg_vmax, &
              sreal_fill_value, output_data%swansea_s_fg_vmax)
      end if
   end do

   do k = 1, indexing%NViews
      call prepare_short_packed_float( &
           input_data%swansea_p_ap(i,j,k), output_data%swansea_p_ap(i,j,k), &
           output_data%swansea_p_ap_scale, output_data%swansea_p_ap_offset, &
           output_data%swansea_p_ap_vmin, output_data%swansea_p_ap_vmax, &
           sreal_fill_value, output_data%swansea_p_ap_vmax)

      call prepare_short_packed_float( &
           input_data%swansea_p_fg(i,j,k), output_data%swansea_p_fg(i,j,k), &
           output_data%swansea_p_fg_scale, output_data%swansea_p_fg_offset, &
           output_data%swansea_p_fg_vmin, output_data%swansea_p_fg_vmax, &
           sreal_fill_value, output_data%swansea_p_fg_vmax)
   end do
end if

if (indexing%flags%do_cloud) then
   !----------------------------------------------------------------------------
   ! cot_ap, cot_fg
   !----------------------------------------------------------------------------
   call prepare_short_packed_float( &
        input_data%cot_ap(i,j), output_data%cot_ap(i,j), &
        output_data%cot_ap_scale, output_data%cot_ap_offset, &
        output_data%cot_ap_vmin, output_data%cot_ap_vmax, &
        sreal_fill_value, output_data%cot_ap_vmax)

   call prepare_short_packed_float( &
        input_data%cot_fg(i,j), output_data%cot_fg(i,j), &
        output_data%cot_fg_scale, output_data%cot_fg_offset, &
        output_data%cot_fg_vmin, output_data%cot_fg_vmax, &
        sreal_fill_value, output_data%cot_fg_vmax)

   !----------------------------------------------------------------------------
   ! cer_ap, cer_fg
   !----------------------------------------------------------------------------
   call prepare_short_packed_float( &
        input_data%cer_ap(i,j), output_data%cer_ap(i,j), &
        output_data%cer_ap_scale, output_data%cer_ap_offset, &
        output_data%cer_ap_vmin, output_data%cer_ap_vmax, &
        sreal_fill_value, output_data%cer_ap_vmax)

   call prepare_short_packed_float( &
        input_data%cer_fg(i,j), output_data%cer_fg(i,j), &
        output_data%cer_fg_scale, output_data%cer_fg_offset, &
        output_data%cer_fg_vmin, output_data%cer_fg_vmax, &
        sreal_fill_value, output_data%cer_fg_vmax)

   !----------------------------------------------------------------------------
   ! ctp_ap, ctp_fg
   !----------------------------------------------------------------------------
   call prepare_short_packed_float( &
        input_data%ctp_ap(i,j), output_data%ctp_ap(i,j), &
        output_data%ctp_ap_scale, output_data%ctp_ap_offset, &
        output_data%ctp_ap_vmin, output_data%ctp_ap_vmax, &
        sreal_fill_value, output_data%ctp_ap_vmax)

   call prepare_short_packed_float( &
        input_data%ctp_fg(i,j), output_data%ctp_fg(i,j), &
        output_data%ctp_fg_scale, output_data%ctp_fg_offset, &
        output_data%ctp_fg_vmin, output_data%ctp_fg_vmax, &
        sreal_fill_value, output_data%ctp_fg_vmax)

   !----------------------------------------------------------------------------
   ! stemp_ap, stemp_fg
   !----------------------------------------------------------------------------
   call prepare_short_packed_float( &
        input_data%stemp_ap(i,j), output_data%stemp_ap(i,j), &
        output_data%stemp_ap_scale, output_data%stemp_ap_offset, &
        output_data%stemp_ap_vmin, output_data%stemp_ap_vmax, &
        sreal_fill_value, output_data%stemp_ap_vmax)

   call prepare_short_packed_float( &
        input_data%stemp_fg(i,j), output_data%stemp_fg(i,j), &
        output_data%stemp_fg_scale, output_data%stemp_fg_offset, &
        output_data%stemp_fg_vmin, output_data%stemp_fg_vmax, &
        sreal_fill_value, output_data%stemp_fg_vmax)

   !----------------------------------------------------------------------------
   ! albedo
   !----------------------------------------------------------------------------
   do k = 1, indexing%NSolar
      call prepare_short_packed_float( &
           input_data%albedo(i,j,k), output_data%albedo(i,j,k), &
           output_data%albedo_scale, output_data%albedo_offset, &
           output_data%albedo_vmin, output_data%albedo_vmax, &
           sreal_fill_value, sint_fill_value)
   end do
end if

if (indexing%flags%do_cloud_layer_2) then
   !----------------------------------------------------------------------------
   ! cot2_ap, cot2_fg
   !----------------------------------------------------------------------------
   call prepare_short_packed_float( &
        input_data%cot2_ap(i,j), output_data%cot2_ap(i,j), &
        output_data%cot_ap_scale, output_data%cot_ap_offset, &
        output_data%cot_ap_vmin, output_data%cot_ap_vmax, &
        sreal_fill_value, output_data%cot_ap_vmax)

   call prepare_short_packed_float( &
        input_data%cot2_fg(i,j), output_data%cot2_fg(i,j), &
        output_data%cot_fg_scale, output_data%cot_fg_offset, &
        output_data%cot_fg_vmin, output_data%cot_fg_vmax, &
        sreal_fill_value, output_data%cot_fg_vmax)

   !----------------------------------------------------------------------------
   ! cer2_ap, cer2_fg
   !----------------------------------------------------------------------------
   call prepare_short_packed_float( &
        input_data%cer2_ap(i,j), output_data%cer2_ap(i,j), &
        output_data%cer_ap_scale, output_data%cer_ap_offset, &
        output_data%cer_ap_vmin, output_data%cer_ap_vmax, &
        sreal_fill_value, output_data%cer_ap_vmax)

   call prepare_short_packed_float( &
        input_data%cer2_fg(i,j), output_data%cer2_fg(i,j), &
        output_data%cer_fg_scale, output_data%cer_fg_offset, &
        output_data%cer_fg_vmin, output_data%cer_fg_vmax, &
        sreal_fill_value, output_data%cer_fg_vmax)

   !----------------------------------------------------------------------------
   ! ctp_ap, ctp_fg
   !----------------------------------------------------------------------------
   call prepare_short_packed_float( &
        input_data%ctp2_ap(i,j), output_data%ctp2_ap(i,j), &
        output_data%ctp_ap_scale, output_data%ctp_ap_offset, &
        output_data%ctp_ap_vmin, output_data%ctp_ap_vmax, &
        sreal_fill_value, output_data%ctp_ap_vmax)

   call prepare_short_packed_float( &
        input_data%ctp2_fg(i,j), output_data%ctp2_fg(i,j), &
        output_data%ctp_fg_scale, output_data%ctp_fg_offset, &
        output_data%ctp_fg_vmin, output_data%ctp_fg_vmax, &
        sreal_fill_value, output_data%ctp_fg_vmax)


end if

   !----------------------------------------------------------------------------
   ! channels
   !----------------------------------------------------------------------------
   do k = 1, indexing%Ny
      call prepare_short_packed_float( &
           input_data%channels(i,j,k), output_data%channels(i,j,k), &
           output_data%channels_scale(k), output_data%channels_offset(k), &
           output_data%channels_vmin(k), output_data%channels_vmax(k), &
           sreal_fill_value, sint_fill_value)
   end do

   !----------------------------------------------------------------------------
   ! Measurement error (diagonals)
   !----------------------------------------------------------------------------
   if (indexing%flags%do_meas_error) then
      do k = 1, indexing%Ny
         call prepare_short_packed_float( &
              input_data%Sy(i,j,k), output_data%Sy(i,j,k), &
              output_data%Sy_scale(k), output_data%Sy_offset(k), &
              output_data%Sy_vmin(k), output_data%Sy_vmax(k), &
              sreal_fill_value, sint_fill_value)
      end do
   end if

   !----------------------------------------------------------------------------
   ! y0
   !----------------------------------------------------------------------------
   do k = 1, indexing%Ny
      call prepare_short_packed_float( &
           input_data%y0(i,j,k), output_data%y0(i,j,k), &
           output_data%y0_scale(k), output_data%y0_offset(k), &
           output_data%y0_vmin(k), output_data%y0_vmax(k), &
           sreal_fill_value, sint_fill_value)
   end do

   !----------------------------------------------------------------------------
   ! residuals
   !----------------------------------------------------------------------------
   do k = 1, indexing%Ny
      call prepare_short_packed_float( &
           input_data%residuals(i,j,k), output_data%residuals(i,j,k), &
           output_data%residuals_scale(k), output_data%residuals_offset(k), &
           output_data%residuals_vmin(k), output_data%residuals_vmax(k), &
           sreal_fill_value, sint_fill_value)
   end do

   !----------------------------------------------------------------------------
   ! ds
   !----------------------------------------------------------------------------
   call prepare_short_packed_float( &
        input_data%ds(i,j), output_data%ds(i,j), &
        output_data%ds_scale, output_data%ds_offset, &
        output_data%ds_vmin, output_data%ds_vmax, &
        sreal_fill_value, output_data%ds_vmax)

   !----------------------------------------------------------------------------
   ! covariance
   !----------------------------------------------------------------------------
if (indexing%flags%do_covariance) then

end if

end subroutine prepare_output_secondary_pp
