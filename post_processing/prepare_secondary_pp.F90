!-------------------------------------------------------------------------------
! Name: read_inter_file.F90
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
! xx/xx/xxxx, Matthias Jerg: Original version
! 05/01/2012, Caroline Poulsen: add in reflectances and brightness temperature
! 15/01/2012, Caroline Poulsen: changed how offset was applied
! 2015/07/16, Greg McGarragh: Major cleanup.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine prepare_secondary_pp(i, j, indexing, input_data, output_data)

   use input_routines
   use orac_ncdf
   use output_routines

   implicit none

   integer,                        intent(in)    :: i, j
   type(counts_and_indexes),       intent(in)    :: indexing
   type(input_data_secondary),     intent(in)    :: input_data
   type(output_data_secondary_pp), intent(inout) :: output_data

   logical     :: lcovar = .false.
   integer     :: k
   real(sreal) :: dummyreal


   output_data%scanline_u(i,j) = j
   output_data%scanline_v(i,j) = i

   !----------------------------------------------------------------------------
   ! cot_ap, cot_fg
   !----------------------------------------------------------------------------
   dummyreal=input_data%cot_ap(i,j)
   call prepare_short_packed_float( &
           dummyreal, output_data%cot_ap(i,j), &
           output_data%cot_ap_scale, output_data%cot_ap_offset, &
           sreal_fill_value, sint_fill_value, &
           output_data%cot_ap_vmin, output_data%cot_ap_vmax, &
           output_data%cot_ap_vmax)

   dummyreal=input_data%cot_fg(i,j)
   call prepare_short_packed_float( &
           dummyreal, output_data%cot_fg(i,j), &
           output_data%cot_fg_scale, output_data%cot_fg_offset, &
           sreal_fill_value, sint_fill_value, &
           output_data%cot_fg_vmin, output_data%cot_fg_vmax, &
           output_data%cot_fg_vmax)

   !----------------------------------------------------------------------------
   ! ref_ap, ref_fg
   !----------------------------------------------------------------------------
   dummyreal=input_data%ref_ap(i,j)
   call prepare_short_packed_float( &
           dummyreal, output_data%ref_ap(i,j), &
           output_data%ref_ap_scale, output_data%ref_ap_offset, &
           sreal_fill_value, sint_fill_value, &
           output_data%ref_ap_vmin, output_data%ref_ap_vmax, &
           output_data%ref_ap_vmax)

   dummyreal=input_data%ref_fg(i,j)
   call prepare_short_packed_float( &
           dummyreal, output_data%ref_fg(i,j), &
           output_data%ref_fg_scale, output_data%ref_fg_offset, &
           sreal_fill_value, sint_fill_value, &
           output_data%ref_fg_vmin, output_data%ref_fg_vmax, &
           output_data%ref_fg_vmax)

   !----------------------------------------------------------------------------
   ! ctp_ap, ctp_fg
   !----------------------------------------------------------------------------
   dummyreal=input_data%ctp_ap(i,j)
   call prepare_short_packed_float( &
           dummyreal, output_data%ctp_ap(i,j), &
           output_data%ctp_ap_scale, output_data%ctp_ap_offset, &
           sreal_fill_value, sint_fill_value, &
           output_data%ctp_ap_vmin, output_data%ctp_ap_vmax, &
           output_data%ctp_ap_vmax)

   dummyreal=input_data%ctp_fg(i,j)
   call prepare_short_packed_float( &
           dummyreal, output_data%ctp_fg(i,j), &
           output_data%ctp_fg_scale, output_data%ctp_fg_offset, &
           sreal_fill_value, sint_fill_value, &
           output_data%ctp_fg_vmin, output_data%ctp_fg_vmax, &
           output_data%ctp_fg_vmax)

   !----------------------------------------------------------------------------
   ! stemp_ap, stemp_fg
   !----------------------------------------------------------------------------
   dummyreal=input_data%stemp_ap(i,j)
   call prepare_short_packed_float( &
           dummyreal, output_data%stemp_ap(i,j), &
           output_data%stemp_ap_scale, output_data%stemp_ap_offset, &
           sreal_fill_value, sint_fill_value, &
           output_data%stemp_ap_vmin, output_data%stemp_ap_vmax, &
           output_data%stemp_ap_vmax)

   dummyreal=input_data%stemp_fg(i,j)
   call prepare_short_packed_float( &
           dummyreal, output_data%stemp_fg(i,j), &
           output_data%stemp_fg_scale, output_data%stemp_fg_offset, &
           sreal_fill_value, sint_fill_value, &
           output_data%stemp_fg_vmin, output_data%stemp_fg_vmax, &
           output_data%stemp_fg_vmax)

   !----------------------------------------------------------------------------
   ! albedo
   !----------------------------------------------------------------------------
   do k=1,indexing%NSolar
      dummyreal=input_data%albedo(i,j,k)
      call prepare_short_packed_float( &
           dummyreal, output_data%albedo(i,j,k), &
           output_data%albedo_scale(k), output_data%albedo_offset(k), &
           sreal_fill_value, sint_fill_value, &
           output_data%albedo_vmin(k), output_data%albedo_vmax(k), &
           sint_fill_value)
   end do

   !----------------------------------------------------------------------------
   ! channels
   !----------------------------------------------------------------------------
   do k=1,indexing%Ny
      dummyreal=input_data%channels(i,j,k)
      call prepare_short_packed_float( &
           dummyreal, output_data%channels(i,j,k), &
           output_data%channels_scale(k), output_data%channels_offset(k), &
           sreal_fill_value, sint_fill_value, &
           output_data%channels_vmin(k), output_data%channels_vmax(k), &
           sint_fill_value)
   end do

   !----------------------------------------------------------------------------
   ! y0
   !----------------------------------------------------------------------------
   do k=1,indexing%Ny
      dummyreal=input_data%y0(i,j,k)
      call prepare_short_packed_float( &
           dummyreal, output_data%y0(i,j,k), &
           output_data%y0_scale(k), output_data%y0_offset(k), &
           sreal_fill_value, sint_fill_value, &
           output_data%y0_vmin(k), output_data%y0_vmax(k), &
           sint_fill_value)
   end do

   !----------------------------------------------------------------------------
   ! residuals
   !----------------------------------------------------------------------------
   do k=1,indexing%Ny
      dummyreal=input_data%residuals(i,j,k)
      call prepare_short_packed_float( &
           dummyreal, output_data%residuals(i,j,k), &
           output_data%residuals_scale(k), output_data%residuals_offset(k), &
           sreal_fill_value, sint_fill_value, &
           output_data%residuals_vmin(k), output_data%residuals_vmax(k), &
           sint_fill_value)
   end do

   !----------------------------------------------------------------------------
   ! ds
   !----------------------------------------------------------------------------
#ifdef CRAP
   dummyreal = 0.0

   do k=1,SPixel%_x
      dummyreal = dummyreal + Diag%AK(k,k)
   end do

   dummyreal = (dummyreal-output_data%ds_offset)/output_data%ds_scale
   call prepare_short_packed_float( &
           dummyreal, output_data%ds(i,j), &
           output_data%ds_scale, output_data%ds_offset, &
           sreal_fill_value, sint_fill_value, &
           output_data%ds_vmin, output_data%ds_vmax, &
           sint_fill_value)
#endif
   !----------------------------------------------------------------------------
   ! covariance
   !----------------------------------------------------------------------------
   if (lcovar) then

   end if

end subroutine prepare_secondary_pp
