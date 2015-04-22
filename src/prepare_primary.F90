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
! 2014/08/31, GM: Update to use general routines in the current module.
! 2014/09/17, GM: Fixed handling of missing values in some cases.
! 2014/10/24, OS: Added variables cldtype, cloudmask, cccot_pre, lusflags,
!    dem, and nisemask.
! 2014/11/15, CP: Added cloud albedo.
! 2014/11/25, AP: Fixed bug in writing cth|ctt_uncertainty.
! 2014/01/30, AP: Replace YSeg0 with Y0 as superpixeling removed.
! 2015/03/11, GM: Fixed an indexing bug in writing the cloud albedo.
! 2015/03/19, OS: CTH now .ge. 0
! 2015/03/19, OS: undid previous change in file; CTH is allowed to be negative again
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine prepare_primary(Ctrl, convergence, i, j, MSI_Data, RTM_Pc, SPixel, &
                           Diag, output_data)

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

   integer            :: k, kk
   integer(kind=sint) :: temp_short_ctp_error
   real(kind=sreal)   :: temp_real, temp_real_ctp_error
   real(kind=sreal)   :: dummyreal


   !----------------------------------------------------------------------------
   ! time
   !----------------------------------------------------------------------------
   output_data%time(i,j)=MSI_Data%time(SPixel%Loc%X0, SPixel%Loc%Y0)

   !----------------------------------------------------------------------------
   ! lat, lon
   !----------------------------------------------------------------------------
   output_data%lat(i,j)=SPixel%Loc%Lat/output_data%lat_scale
   output_data%lon(i,j)=SPixel%Loc%Lon/output_data%lon_scale

   !----------------------------------------------------------------------------
   ! sol_zen, sat_zen, rel_azi
   !----------------------------------------------------------------------------
   do k=1,Ctrl%Ind%NViews
      output_data%sol_zen(i,j,k)=MSI_Data%Geometry%Sol(SPixel%Loc%X0, SPixel%Loc%Y0,k)
      output_data%sat_zen(i,j,k)=MSI_Data%Geometry%Sat(SPixel%Loc%X0, SPixel%Loc%Y0,k)
      output_data%rel_azi(i,j,k)=MSI_Data%Geometry%Azi(SPixel%Loc%X0, SPixel%Loc%Y0,k)
   end do

   !----------------------------------------------------------------------------
   ! cot, cot_error
   !----------------------------------------------------------------------------
   if (SPixel%Xn(ITau) .eq. MissingXn) then
      temp_real = sreal_fill_value
   else
      temp_real = 10.0**SPixel%Xn(ITau)
   end if
   call prepare_short_packed_float( &
           temp_real, output_data%cot(i,j), &
           output_data%cot_scale, output_data%cot_offset, &
           sreal_fill_value, sint_fill_value, &
           output_data%cot_vmin, output_data%cot_vmax, &
           output_data%cot_vmax)

   if (SPixel%Sn(ITau,ITau) .eq. MissingSn) then
      temp_real = sreal_fill_value
   else
      temp_real = sqrt(SPixel%Sn(ITau,ITau))
   end if
   call prepare_short_packed_float( &
           temp_real, output_data%cot_error(i,j), &
           output_data%cot_error_scale, output_data%cot_error_offset, &
           sreal_fill_value, sint_fill_value, &
           output_data%cot_error_vmin, output_data%cot_error_vmax, &
           output_data%cot_error_vmax)

   !----------------------------------------------------------------------------
   ! ref, ref_error
   !----------------------------------------------------------------------------
   if (SPixel%Xn(IRe) .eq. MissingXn) then
      temp_real = sreal_fill_value
   else
      temp_real = SPixel%Xn(IRe)
   end if
   call prepare_short_packed_float( &
           temp_real, output_data%ref(i,j), &
           output_data%ref_scale, output_data%ref_offset, &
           sreal_fill_value, sint_fill_value, &
           output_data%ref_vmin, output_data%ref_vmax, &
           output_data%ref_vmax)

   if (SPixel%Sn(IRe,IRe) .eq. MissingSn) then
      temp_real = sreal_fill_value
   else
      temp_real = sqrt(SPixel%Sn(IRe,IRe))
   end if
   call prepare_short_packed_float( &
           temp_real, output_data%ref_error(i,j), &
           output_data%ref_error_scale, output_data%ref_error_offset, &
           sreal_fill_value, sint_fill_value, &
           output_data%ref_error_vmin, output_data%ref_error_vmax, &
           output_data%ref_error_vmax)

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
      temp_short_ctp_error = ( int(temp_real_ctp_error, kind=sint) - &
                               output_data%ctp_error_scale &
                             ) / output_data%ctp_error_scale
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
      temp_real = RTM_Pc%Hc/10./1000. ! now it's in km
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
      temp_real=abs(RTM_Pc%dHc_dPc/10./1000.)*temp_real_ctp_error
      call prepare_short_packed_float( &
           temp_real, output_data%cth_error(i,j), &
           output_data%cth_error_scale, output_data%cth_error_offset, &
           sreal_fill_value, sint_fill_value, &
           output_data%cth_error_vmin, output_data%cth_error_vmax, &
           output_data%cth_error_vmax)
   end if

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
   ! convergence, niter
   !----------------------------------------------------------------------------
   output_data%convergence(i,j)=int(convergence,kind=byte)

   if (convergence .eq. 0 ) output_data%niter(i,j)=int(Diag%Iterations,kind=byte)
   if (convergence .eq. 1 ) output_data%niter(i,j)=int(byte_fill_value,kind=byte)

   !----------------------------------------------------------------------------
   ! phase
   !----------------------------------------------------------------------------
   output_data%phase(i,j)=int(1,kind=byte)

   !----------------------------------------------------------------------------
   ! costja
   !----------------------------------------------------------------------------
   temp_real=Diag%Ja
   call prepare_float_packed_float( &
           temp_real, output_data%costja(i,j), &
           output_data%costja_scale, output_data%costja_offset, &
           sreal_fill_value, sreal_fill_value, &
           output_data%costja_vmin, output_data%costja_vmax, &
           sreal_fill_value)

   !----------------------------------------------------------------------------
   ! costjm
   !----------------------------------------------------------------------------
   temp_real=Diag%Jm
   call prepare_float_packed_float( &
           temp_real, output_data%costjm(i,j), &
           output_data%costjm_scale, output_data%costjm_offset, &
           sreal_fill_value, sreal_fill_value, &
           output_data%costjm_vmin, output_data%costjm_vmax, &
           sreal_fill_value)

   !----------------------------------------------------------------------------
   ! lsflag
   !----------------------------------------------------------------------------
   output_data%lsflag(i,j)=int(MSI_Data%LSFlags(SPixel%Loc%X0, SPixel%Loc%Y0), kind=byte)

   !----------------------------------------------------------------------------
   ! qcflag
   !----------------------------------------------------------------------------
   output_data%qcflag(i,j)=int(Diag%QCFlag,kind=sint)

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
   output_data%cldmask(i,j)=int(MSI_Data%cloudmask(SPixel%Loc%X0, SPixel%Loc&
        %Y0), kind=byte)

   !----------------------------------------------------------------------------
   ! cccot_pre
   !----------------------------------------------------------------------------
   temp_real=MSI_Data%cccot_pre(SPixel%Loc%X0, SPixel%Loc%Y0)
   call prepare_float_packed_float( &
           temp_real, output_data%cccot_pre(i,j), &
           output_data%cccot_pre_scale, output_data%cccot_pre_offset, &
           sreal_fill_value, sreal_fill_value, &
           output_data%cccot_pre_vmin, output_data%cccot_pre_vmax, &
           sreal_fill_value)

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
   ! lusflag
   !----------------------------------------------------------------------------
   output_data%nisemask(i,j)=int(MSI_Data%nisemask(SPixel%Loc%X0, SPixel%Loc&
        %Y0), kind=byte)

   !----------------------------------------------------------------------------
   ! cloud_albedo
   !----------------------------------------------------------------------------
   do k=1,SPixel%Ind%NSolar
      kk = SPixel%spixel_y_solar_to_ctrl_y_solar_index(k)

      dummyreal=Diag%cloud_albedo(k)
      call prepare_short_packed_float( &
           dummyreal, output_data%cloud_albedo(i,j,kk), &
           output_data%cloud_albedo_scale(kk), output_data%cloud_albedo_offset(kk), &
           sreal_fill_value, sint_fill_value, &
           output_data%cloud_albedo_vmin(kk), output_data%cloud_albedo_vmax(kk), &
           sint_fill_value)
   end do

end subroutine prepare_primary
