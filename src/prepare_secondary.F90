!-------------------------------------------------------------------------------
! Name: prepare_secondary.F90
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
! Return value:
! Name Type Description
!
! Local variables:
! Name Type Description
!
! History:
! 2011/12/19, Matthias Jerg: Creates initial version
! 2012/01/05, Caroline Poulsen: Add in reflectances and brightness temperature
! 2012/01/15, Caroline Poulsen: Changed how offset was applied
! 2012/09/20, Caroline Poulsen: Remove scaling factor from albedo
! 2012/09/20, Caroline Poulsen: Chanaged how svan value is set
! 2013/05/29, Gareth Thomas: Added degrees of freedom for signal
! 2014/01/30, Greg McGarragh: Fixed writing of the residuals and first guess in
!    the case of nighttime pixels.
! 2014/06/13, Greg McGarragh: Put the code into a subroutine.
! 2014/06/13, Greg McGarragh: Cleaned up the code.
! 2014/08/31, Greg McGarragh: Update to use general routines in the current
!    module.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine prepare_secondary(Ctrl, lcovar, i, j, MSI_Data, SPixel, Diag, &
                             output_data)

   use CTRL_def
   use Data_def
   use Diag_def
   use SPixel_def

   implicit none

   type(CTRL_t),                intent(in)    :: Ctrl
   logical,                     intent(in)    :: lcovar
   integer,                     intent(in)    :: i, j
   type(Data_t),                intent(in)    :: MSI_Data
   type(SPixel_t),              intent(in)    :: SPixel
   type(Diag_t),                intent(in)    :: Diag
   type(output_data_secondary), intent(inout) :: output_data

   integer          :: ii,k,l
   real(kind=sreal) :: dummyreal


   !----------------------------------------------------------------------------
   ! scanline_u, scanline_v
   !----------------------------------------------------------------------------
   output_data%scanline_u(i,j)=i
   output_data%scanline_v(i,j)=j

   !----------------------------------------------------------------------------
   ! cot_ap, cot_fg
   !----------------------------------------------------------------------------
   dummyreal=SPixel%Xb(1)
   call prepare_short_packed_float( &
           dummyreal, output_data%cot_ap(i,j), &
           output_data%cot_ap_scale, output_data%cot_ap_offset, &
           sreal_fill_value, sint_fill_value, &
           output_data%cot_ap_vmin, output_data%cot_ap_vmax, &
           output_data%cot_ap_vmax)

   dummyreal=SPixel%X0(1)
   call prepare_short_packed_float( &
           dummyreal, output_data%cot_fg(i,j), &
           output_data%cot_fg_scale, output_data%cot_fg_offset, &
           sreal_fill_value, sint_fill_value, &
           output_data%cot_fg_vmin, output_data%cot_fg_vmax, &
           output_data%cot_fg_vmax)

   !----------------------------------------------------------------------------
   ! ref_ap, ref_fg
   !----------------------------------------------------------------------------
   dummyreal=SPixel%Xb(2)
   call prepare_short_packed_float( &
           dummyreal, output_data%ref_ap(i,j), &
           output_data%ref_ap_scale, output_data%ref_ap_offset, &
           sreal_fill_value, sint_fill_value, &
           output_data%ref_ap_vmin, output_data%ref_ap_vmax, &
           output_data%ref_ap_vmax)

   dummyreal=SPixel%X0(2)
   call prepare_short_packed_float( &
           dummyreal, output_data%ref_fg(i,j), &
           output_data%ref_fg_scale, output_data%ref_fg_offset, &
           sreal_fill_value, sint_fill_value, &
           output_data%ref_fg_vmin, output_data%ref_fg_vmax, &
           output_data%ref_fg_vmax)

   !----------------------------------------------------------------------------
   ! ctp_ap, ctp_fg
   !----------------------------------------------------------------------------
   dummyreal=SPixel%Xb(3)
   call prepare_short_packed_float( &
           dummyreal, output_data%ctp_ap(i,j), &
           output_data%ctp_ap_scale, output_data%ctp_ap_offset, &
           sreal_fill_value, sint_fill_value, &
           output_data%ctp_ap_vmin, output_data%ctp_ap_vmax, &
           output_data%ctp_ap_vmax)

   dummyreal=SPixel%X0(3)
   call prepare_short_packed_float( &
           dummyreal, output_data%ctp_fg(i,j), &
           output_data%ctp_fg_scale, output_data%ctp_fg_offset, &
           sreal_fill_value, sint_fill_value, &
           output_data%ctp_fg_vmin, output_data%ctp_fg_vmax, &
           output_data%ctp_fg_vmax)

   !----------------------------------------------------------------------------
   ! stemp_ap, stemp_fg
   !----------------------------------------------------------------------------
   dummyreal=SPixel%X0(5)
   call prepare_short_packed_float( &
           dummyreal, output_data%stemp_ap(i,j), &
           output_data%stemp_ap_scale, output_data%stemp_ap_offset, &
           sreal_fill_value, sint_fill_value, &
           output_data%stemp_ap_vmin, output_data%stemp_ap_vmax, &
           output_data%stemp_ap_vmax)

   dummyreal=SPixel%X0(5)
   call prepare_short_packed_float( &
           dummyreal, output_data%stemp_fg(i,j), &
           output_data%stemp_fg_scale, output_data%stemp_fg_offset, &
           sreal_fill_value, sint_fill_value, &
           output_data%stemp_fg_vmin, output_data%stemp_fg_vmax, &
           output_data%stemp_fg_vmax)

   !----------------------------------------------------------------------------
   ! albedo
   !----------------------------------------------------------------------------
   do k=1,Ctrl%Ind%NSolar
      dummyreal=MSI_Data%ALB(SPixel%Loc%X0,SPixel%Loc%YSeg0,k)
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
   do k=1,Ctrl%Ind%Ny
      dummyreal=MSI_Data%MSI(SPixel%Loc%X0, SPixel%Loc%YSeg0, k)
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
   do k=1,SPixel%Ind%NY
      ii = SPixel%spixel_y_to_ctrl_y_index(k)

      dummyreal=Diag%Y0(k)
      call prepare_short_packed_float( &
           dummyreal, output_data%y0(i,j,ii), &
           output_data%y0_scale(ii), output_data%y0_offset(ii), &
           sreal_fill_value, sint_fill_value, &
           output_data%y0_vmin(ii), output_data%y0_vmax(ii), &
           sint_fill_value)
   end do

   !----------------------------------------------------------------------------
   ! residuals
   !----------------------------------------------------------------------------
   do k=1,SPixel%Ind%NY
      ii = SPixel%spixel_y_to_ctrl_y_index(k)

      dummyreal=Diag%YmFit(k)
      call prepare_short_packed_float( &
           dummyreal, output_data%residuals(i,j,ii), &
           output_data%residuals_scale(ii), output_data%residuals_offset(ii), &
           sreal_fill_value, sint_fill_value, &
           output_data%residuals_vmin(ii), output_data%residuals_vmax(ii), &
           sint_fill_value)
   end do

   !----------------------------------------------------------------------------
   ! ds
   !----------------------------------------------------------------------------
   dummyreal = 0.0

   do k=1,SPixel%Nx
      dummyreal = dummyreal + Diag%AK(k,k)
   end do

   dummyreal = (dummyreal-output_data%ds_offset)/output_data%ds_scale
   call prepare_short_packed_float( &
           dummyreal, output_data%ds(i,j), &
           output_data%ds_scale, output_data%ds_offset, &
           sreal_fill_value, sint_fill_value, &
           output_data%ds_vmin, output_data%ds_vmax, &
           sint_fill_value)

   !----------------------------------------------------------------------------
   ! covariance
   !----------------------------------------------------------------------------
   if (lcovar) then
      do k=1,SPixel%Nx
         do l=1,SPixel%Nx
           call prepare_float_packed_float( &
                   SPixel%Sn(k,l), output_data%covariance(i,j,k,l), &
                   1., 0., &
                   sreal_fill_value, sreal_fill_value, &
                   0., huge(dummyreal), &
                   sreal_fill_value)
         end do
      end do
   end if

end subroutine prepare_secondary
