!-------------------------------------------------------------------------------
! Name: prepare_primary.F90
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
! 2012/01/06, Matthias Jerg: Added in cwp
! 2012/01/16, Caroline Poulsen: Bug fix, changed how offset applied
! 2012/06/16, Caroline Poulsen: Change illum arry size
! 2012/07/17, MJ: Fixes bug in CWP write.
! 2012/08/10, CP: Fixed bug in illum read 2d array instead of 3d
! 2012/11/03, MST: Converted height to km
! 2012/11/03, CP: Changed log cot to COT using 10*
! 2013/01/17, MJ: Adds code to accommodate uncertainties of ctt and cth
! 2013/xx/xx, MJ: Make various modifications to define range of variables more
!    thoroughly.
! 2014/06/13, GM: Put the code into a subroutine.
! 2014/06/13, GM: Cleaned up the code.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine prepare_primary(Ctrl, convergence, i, j, MSI_Data, RTM_Pc, SPixel, &
                           Diag, output_data, status)

   use CTRL_def
   use Data_def
   use Diag_def
   use RTM_Pc_def
   use SPixel_def

   implicit none

   type(CTRL_t),              intent(in)    :: Ctrl
   integer,                   intent(in)    :: convergence
   integer,                   intent(in)    :: i, j
   type(Data_t),              intent(in)    :: MSI_Data
   type(RTM_Pc_t),            intent(in)    :: RTM_Pc
   type(SPixel_t),            intent(in)    :: SPixel
   type(Diag_t),              intent(in)    :: Diag
   type(output_data_primary), intent(inout) :: output_data
   integer,                   intent(inout) :: status

   integer          :: k
   real(kind=sreal) :: temp_real, temp_real_ctp_error

   !----------------------------------------------------------------------------
   ! time
   !----------------------------------------------------------------------------
   output_data%time(i,j)=MSI_Data%time(SPixel%Loc%X0, SPixel%Loc%YSeg0)

   !----------------------------------------------------------------------------
   ! lat, lon
   !----------------------------------------------------------------------------
   output_data%lat(i,j)=SPixel%Loc%Lat/output_data%lat_scale
   output_data%lon(i,j)=SPixel%Loc%Lon/output_data%lon_scale

   !----------------------------------------------------------------------------
   ! sol_zen, sat_zen, rel_azi
   !----------------------------------------------------------------------------
   do k=1,Ctrl%Ind%NViews
      output_data%sol_zen(i,j,k)=MSI_Data%Geometry%Sol(SPixel%Loc%X0, SPixel%Loc%YSeg0,k)
      output_data%sat_zen(i,j,k)=MSI_Data%Geometry%Sat(SPixel%Loc%X0, SPixel%Loc%YSeg0,k)
      output_data%rel_azi(i,j,k)=MSI_Data%Geometry%Azi(SPixel%Loc%X0, SPixel%Loc%YSeg0,k)
   end do

   !-------------------------------------------------------------------------------
   ! cot, cot_error
   !-------------------------------------------------------------------------------
   temp_real=10.0**SPixel%Xn(1)
   call prepare_short_packed_float &
           (temp_real, output_data%cot(i,j), &
            output_data%cot_scale, output_data%cot_offset, &
            sreal_fill_value, sint_fill_value, &
            output_data%cot_vmin, output_data%cot_vmax, &
            output_data%cot_vmax)

   temp_real=sqrt(SPixel%Sn(1,1))
   call prepare_short_packed_float &
           (temp_real, output_data%cot_error(i,j), &
            output_data%cot_error_scale, output_data%cot_error_offset, &
            sreal_fill_value, sint_fill_value, &
            output_data%cot_error_vmin, output_data%cot_error_vmax, &
            output_data%cot_error_vmax)

   !-------------------------------------------------------------------------------
   ! ref, ref_error
   !-------------------------------------------------------------------------------
   temp_real=SPixel%Xn(2)
   call prepare_short_packed_float &
           (temp_real, output_data%ref(i,j), &
            output_data%ref_error_scale, output_data%ref_error_offset, &
            sreal_fill_value, sint_fill_value, &
            output_data%ref_vmin, output_data%ref_vmax, &
            output_data%ref_vmax)

   temp_real=sqrt(SPixel%Sn(2,2))
   call prepare_short_packed_float &
           (temp_real, output_data%ref_error(i,j), &
            output_data%ref_error_scale, output_data%ref_error_offset, &
            sreal_fill_value, sint_fill_value, &
            output_data%ref_error_vmin, output_data%ref_error_vmax, &
            output_data%ref_error_vmax)

   !-------------------------------------------------------------------------------
   ! ctp, ctp_error
   !-------------------------------------------------------------------------------
   temp_real=SPixel%Xn(3)
   call prepare_short_packed_float &
           (temp_real, output_data%ctp(i,j), &
            output_data%ctp_scale, output_data%ctp_offset, &
            sreal_fill_value, sint_fill_value, &
            output_data%ctp_vmin, output_data%ctp_vmax, &
            output_data%ctp_vmax)

   temp_real=sqrt(SPixel%Sn(3,3))
   temp_real_ctp_error=(sqrt(SPixel%Sn(3,3))-output_data%ctp_error_offset)/ &
                                             output_data%ctp_error_scale
   call prepare_short_packed_float &
           (temp_real, output_data%ctp_error(i,j), &
            output_data%ctp_error_scale, output_data%ctp_error_offset, &
            sreal_fill_value, sint_fill_value, &
            output_data%ctp_error_vmin, output_data%ctp_error_vmax, &
            output_data%ctp_error_vmax)

   !-------------------------------------------------------------------------------
   ! cct, cct_error
   !-------------------------------------------------------------------------------
   temp_real=SPixel%Xn(4)
   call prepare_short_packed_float &
           (temp_real, output_data%cct(i,j), &
            output_data%cct_scale, output_data%cct_offset, &
            sreal_fill_value, sint_fill_value, &
            output_data%cct_vmin, output_data%cct_vmax, &
            sint_fill_value)

   temp_real=sqrt(SPixel%Sn(4,4))
   call prepare_short_packed_float &
           (temp_real, output_data%cct_error(i,j), &
            output_data%cct_error_scale, output_data%cct_error_offset, &
            sreal_fill_value, sint_fill_value, &
            output_data%cct_error_vmin, output_data%cct_error_vmax, &
            sint_fill_value)

   !-------------------------------------------------------------------------------
   ! stemp, stemp_error
   !-------------------------------------------------------------------------------
   temp_real=SPixel%Xn(5)
   call prepare_short_packed_float &
           (temp_real, output_data%stemp(i,j), &
            output_data%stemp_error_scale, output_data%stemp_error_offset, &
            sreal_fill_value, sint_fill_value, &
            output_data%stemp_vmin, output_data%stemp_vmax, &
            output_data%stemp_vmax)

   temp_real=sqrt(SPixel%Sn(5,5))
   call prepare_short_packed_float &
           (temp_real, output_data%stemp_error(i,j), &
            output_data%stemp_error_scale, output_data%stemp_error_offset, &
            sreal_fill_value, sint_fill_value, &
            output_data%stemp_error_vmin, output_data%stemp_error_vmax, &
            output_data%stemp_error_vmax)

   !-------------------------------------------------------------------------------
   ! cth, cth_error
   !-------------------------------------------------------------------------------
   temp_real=RTM_Pc%Hc/10./1000. ! now it's in km
   call prepare_short_packed_float &
           (temp_real, output_data%cth(i,j), &
            output_data%cth_scale, output_data%cth_offset, &
            sreal_fill_value, sint_fill_value, &
            output_data%cth_vmin, output_data%cth_vmax, &
            output_data%cth_vmax)

   ! If ctp_error is good compute cth_error
   if (temp_real_ctp_error .ge. real(output_data%ctp_error_vmin,kind=sreal) .and. &
       temp_real_ctp_error .le. real(output_data%ctp_error_vmax,kind=sreal)) then

      temp_real=abs(RTM_Pc%dHc_dPc/10./1000.)*temp_real_ctp_error
      call prepare_short_packed_float &
           (temp_real, output_data%cth_error(i,j), &
            output_data%cth_error_scale, output_data%cth_error_offset, &
            sreal_fill_value, sint_fill_value, &
            output_data%cth_error_vmin, output_data%cth_error_vmax, &
            output_data%cth_error_vmax)
   else if (temp_real_ctp_error .lt. real(output_data%ctp_error_vmin,kind=sreal)) then
      output_data%cth_error(i,j)=sint_fill_value
   else if (temp_real_ctp_error .gt. real(output_data%ctp_error_vmax,kind=sreal)) then
      output_data%cth_error(i,j)=output_data%cth_error_vmax
   end if

   !-------------------------------------------------------------------------------
   ! ctt, ctt_error
   !-------------------------------------------------------------------------------
   temp_real=RTM_Pc%Tc
   call prepare_short_packed_float &
           (temp_real, output_data%ctt(i,j), &
            output_data%ctt_scale, output_data%ctt_offset, &
            sreal_fill_value, sint_fill_value, &
            output_data%ctt_vmin, output_data%ctt_vmax, &
            output_data%ctt_vmax)

   ! If ctp_error is good compute ctt_error
   if (temp_real_ctp_error .ge. real(output_data%ctp_error_vmin,kind=sreal) .and. &
       temp_real_ctp_error .le. real(output_data%ctp_error_vmax,kind=sreal)) then

      temp_real=abs(RTM_Pc%dTc_dPc)*temp_real_ctp_error
      call prepare_short_packed_float &
           (temp_real, output_data%ctt_error(i,j), &
            output_data%ctt_error_scale, output_data%ctt_error_offset, &
            sreal_fill_value, sint_fill_value, &
            output_data%ctt_error_vmin, output_data%ctt_error_vmax, &
            output_data%ctt_error_vmax)
   else if (temp_real_ctp_error .lt. real(output_data%ctp_error_vmin,kind=sreal)) then
      output_data%ctt_error(i,j)=sint_fill_value
   else if (temp_real_ctp_error .gt. real(output_data%ctp_error_vmax,kind=sreal)) then
      output_data%ctt_error(i,j)=output_data%ctt_error_vmax
   end if

   !-------------------------------------------------------------------------------
   ! cwp, cwp_error
   !-------------------------------------------------------------------------------
   temp_real=SPixel%CWP
   call prepare_short_packed_float &
           (temp_real, output_data%cwp(i,j), &
            output_data%cwp_scale, output_data%cwp_offset, &
            sreal_fill_value, sint_fill_value, &
            output_data%cwp_vmin, output_data%cwp_vmax, &
            output_data%cwp_vmax)

   temp_real=sqrt(SPixel%CWP_error)
   call prepare_short_packed_float &
           (temp_real, output_data%cwp_error(i,j), &
            output_data%cwp_scale, output_data%cwp_offset, &
            sreal_fill_value, sint_fill_value, &
            output_data%cwp_error_vmin, output_data%cwp_error_vmax, &
            output_data%cwp_error_vmax)

   !-------------------------------------------------------------------------------
   ! convergence, niter
   !-------------------------------------------------------------------------------
   output_data%convergence(i,j)=int(convergence,kind=byte)

   if (convergence .eq. 0 ) output_data%niter(i,j)=int(Diag%Iterations,kind=byte)
   if (convergence .eq. 1 ) output_data%niter(i,j)=int(byte_fill_value,kind=byte)

   !-------------------------------------------------------------------------------
   ! phase
   !-------------------------------------------------------------------------------
   output_data%phase(i,j)=int(1,kind=byte)

   !-------------------------------------------------------------------------------
   ! costja
   !-------------------------------------------------------------------------------
   temp_real=Diag%Ja
   call prepare_float_packed_float &
           (temp_real, output_data%costja(i,j), &
            output_data%costja_scale, output_data%costja_offset, &
            sreal_fill_value, sreal_fill_value, &
            output_data%costja_vmin, output_data%costja_vmax, &
            sreal_fill_value)

   !-------------------------------------------------------------------------------
   ! costjm
   !-------------------------------------------------------------------------------
   temp_real=Diag%Jm
   call prepare_float_packed_float &
           (temp_real, output_data%costjm(i,j), &
            output_data%costjm_scale, output_data%costjm_offset, &
            sreal_fill_value, sreal_fill_value, &
            output_data%costjm_vmin, output_data%costjm_vmax, &
            sreal_fill_value)

   !-------------------------------------------------------------------------------
   ! lsflag
   !-------------------------------------------------------------------------------
   output_data%lsflag(i,j)=int(MSI_Data%LSFlags(SPixel%Loc%X0, SPixel%Loc%YSeg0), kind=byte)

   !-------------------------------------------------------------------------------
   ! qcflag
   !-------------------------------------------------------------------------------
   output_data%qcflag(i,j)=int(Diag%QCFlag,kind=sint)

   !----------------------------------------------------------------------------
   ! illum
   !----------------------------------------------------------------------------
   output_data%illum(i,j)=MSI_Data%illum(SPixel%Loc%X0, SPixel%Loc%YSeg0,1)

end subroutine prepare_primary
