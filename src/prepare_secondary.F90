!-------------------------------------------------------------------------------
! Name: prepare_secondary.F90
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
! 2014/01/30, GM: Fixed writing of the residuals and first guess in
!    the case of nighttime pixels.
! 2014/06/13, GM: Put the code into a subroutine.
! 2014/06/13, GM: Cleaned up the code.
! 2014/08/31, GM: Update to use general routines in the current
!    module.
! 2014/01/30, AP: Replace YSeg0 with Y0 as superpixeling removed.
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
   use orac_ncdf
   use SPixel_def

   implicit none

   type(CTRL_t),                intent(in)    :: Ctrl
   logical,                     intent(in)    :: lcovar
   integer,                     intent(in)    :: i, j
   type(Data_t),                intent(in)    :: MSI_Data
   type(SPixel_t),              intent(in)    :: SPixel
   type(Diag_t),                intent(in)    :: Diag
   type(output_data_secondary), intent(inout) :: output_data

   integer          :: k,kk,l
   real(kind=sreal) :: dummyreal


   !----------------------------------------------------------------------------
   ! scanline_u, scanline_v
   !----------------------------------------------------------------------------
   output_data%scanline_u(i,j)=i
   output_data%scanline_v(i,j)=j

   !----------------------------------------------------------------------------
   ! cot_ap, cot_fg
   !----------------------------------------------------------------------------
   dummyreal=SPixel%Xb(ITau)
   call prepare_short_packed_float( &
           dummyreal, output_data%cot_ap(i,j), &
           output_data%cot_ap_scale, output_data%cot_ap_offset, &
           sreal_fill_value, sint_fill_value, &
           output_data%cot_ap_vmin, output_data%cot_ap_vmax, &
           output_data%cot_ap_vmax)

   dummyreal=SPixel%X0(ITau)
   call prepare_short_packed_float( &
           dummyreal, output_data%cot_fg(i,j), &
           output_data%cot_fg_scale, output_data%cot_fg_offset, &
           sreal_fill_value, sint_fill_value, &
           output_data%cot_fg_vmin, output_data%cot_fg_vmax, &
           output_data%cot_fg_vmax)

   !----------------------------------------------------------------------------
   ! ref_ap, ref_fg
   !----------------------------------------------------------------------------
   dummyreal=SPixel%Xb(IRe)
   call prepare_short_packed_float( &
           dummyreal, output_data%ref_ap(i,j), &
           output_data%ref_ap_scale, output_data%ref_ap_offset, &
           sreal_fill_value, sint_fill_value, &
           output_data%ref_ap_vmin, output_data%ref_ap_vmax, &
           output_data%ref_ap_vmax)

   dummyreal=SPixel%X0(IRe)
   call prepare_short_packed_float( &
           dummyreal, output_data%ref_fg(i,j), &
           output_data%ref_fg_scale, output_data%ref_fg_offset, &
           sreal_fill_value, sint_fill_value, &
           output_data%ref_fg_vmin, output_data%ref_fg_vmax, &
           output_data%ref_fg_vmax)

   !----------------------------------------------------------------------------
   ! ctp_ap, ctp_fg
   !----------------------------------------------------------------------------
   dummyreal=SPixel%Xb(IPc)
   call prepare_short_packed_float( &
           dummyreal, output_data%ctp_ap(i,j), &
           output_data%ctp_ap_scale, output_data%ctp_ap_offset, &
           sreal_fill_value, sint_fill_value, &
           output_data%ctp_ap_vmin, output_data%ctp_ap_vmax, &
           output_data%ctp_ap_vmax)

   dummyreal=SPixel%X0(IPc)
   call prepare_short_packed_float( &
           dummyreal, output_data%ctp_fg(i,j), &
           output_data%ctp_fg_scale, output_data%ctp_fg_offset, &
           sreal_fill_value, sint_fill_value, &
           output_data%ctp_fg_vmin, output_data%ctp_fg_vmax, &
           output_data%ctp_fg_vmax)

   !----------------------------------------------------------------------------
   ! stemp_ap, stemp_fg
   !----------------------------------------------------------------------------
   dummyreal=SPixel%X0(ITs)
   call prepare_short_packed_float( &
           dummyreal, output_data%stemp_ap(i,j), &
           output_data%stemp_ap_scale, output_data%stemp_ap_offset, &
           sreal_fill_value, sint_fill_value, &
           output_data%stemp_ap_vmin, output_data%stemp_ap_vmax, &
           output_data%stemp_ap_vmax)

   dummyreal=SPixel%X0(ITs)
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
      dummyreal=MSI_Data%ALB(SPixel%Loc%X0,SPixel%Loc%Y0,k)
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
      dummyreal=MSI_Data%MSI(SPixel%Loc%X0, SPixel%Loc%Y0, k)
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
   do k=1,SPixel%Ind%Ny
      kk = SPixel%spixel_y_to_ctrl_y_index(k)

      dummyreal=Diag%Y0(k)
      call prepare_short_packed_float( &
           dummyreal, output_data%y0(i,j,kk), &
           output_data%y0_scale(kk), output_data%y0_offset(kk), &
           sreal_fill_value, sint_fill_value, &
           output_data%y0_vmin(kk), output_data%y0_vmax(kk), &
           sint_fill_value)
   end do

   !----------------------------------------------------------------------------
   ! residuals
   !----------------------------------------------------------------------------
   do k=1,SPixel%Ind%Ny
      kk = SPixel%spixel_y_to_ctrl_y_index(k)

      dummyreal=Diag%YmFit(k)
      call prepare_short_packed_float( &
           dummyreal, output_data%residuals(i,j,kk), &
           output_data%residuals_scale(kk), output_data%residuals_offset(kk), &
           sreal_fill_value, sint_fill_value, &
           output_data%residuals_vmin(kk), output_data%residuals_vmax(kk), &
           sint_fill_value)
   end do

   !----------------------------------------------------------------------------
   ! ds
   !----------------------------------------------------------------------------
   dummyreal = 0.0

   do k=1,SPixel%Nx
      dummyreal = dummyreal + Diag%AK(SPixel%X(k),SPixel%X(k))
   end do

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
