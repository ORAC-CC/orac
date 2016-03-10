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
! xx/xx/xxxx, MJ: Original version
! 05/01/2012, CP: Add in reflectances and brightness temperature
! 15/01/2012, CP: Changed how offset was applied
! 2015/07/16, GM: Major cleanup.
! 2015/10/07, OS: Renamed to *_pp.F90, as we have to avoid duplicate subroutine
!    names for wrapper
! 2015/12/30, AP: Have all albedo fields use the same values.
! 2016/03/04, AP: Tidy prepare_*_packed_float.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine prepare_output_secondary_pp(i, j, indexing, input_data, output_data, &
                                    do_covariance)

   use orac_input
   use orac_ncdf
   use orac_output

   implicit none

   integer,                     intent(in)    :: i, j
   type(common_indices),        intent(in)    :: indexing
   type(input_data_secondary),  intent(in)    :: input_data
   type(output_data_secondary), intent(inout) :: output_data
   logical,                     intent(in)    :: do_covariance

   integer     :: k
   real(sreal) :: dummyreal


   output_data%scanline_u(i,j) = j
   output_data%scanline_v(i,j) = i

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
   do k=1,indexing%NSolar
      call prepare_short_packed_float( &
           input_data%albedo(i,j,k), output_data%albedo(i,j,k), &
           output_data%albedo_scale, output_data%albedo_offset, &
           output_data%albedo_vmin, output_data%albedo_vmax, &
           sreal_fill_value, sint_fill_value)
   end do

   !----------------------------------------------------------------------------
   ! channels
   !----------------------------------------------------------------------------
   do k=1,indexing%Ny
      call prepare_short_packed_float( &
           input_data%channels(i,j,k), output_data%channels(i,j,k), &
           output_data%channels_scale(k), output_data%channels_offset(k), &
           output_data%channels_vmin(k), output_data%channels_vmax(k), &
           sreal_fill_value, sint_fill_value)
   end do

   !----------------------------------------------------------------------------
   ! y0
   !----------------------------------------------------------------------------
   do k=1,indexing%Ny
      call prepare_short_packed_float( &
           input_data%y0(i,j,k), output_data%y0(i,j,k), &
           output_data%y0_scale(k), output_data%y0_offset(k), &
           output_data%y0_vmin(k), output_data%y0_vmax(k), &
           sreal_fill_value, sint_fill_value)
   end do

   !----------------------------------------------------------------------------
   ! residuals
   !----------------------------------------------------------------------------
   do k=1,indexing%Ny
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
   if (do_covariance) then

   end if

end subroutine prepare_output_secondary_pp
