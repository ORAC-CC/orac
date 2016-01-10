!-------------------------------------------------------------------------------
! Name: alloc_output_data.F90
!
! Purpose:
! The file contains a collection of two subroutines which allocate the array
! parts of the output variable types, stored within module output_routines.
!
! History:
! 2011/12/19, MJ: creates initial file.
! 2012/01/05, CP: added in channel information
! 2012/01/06, CP: added in cwp
! 2012/01/15, CP: added in chan definitions
! 2012/01/28, CP: added in albedo
! 2012/07/08, CP: fixed memory access error
! 2013/01/17, MJ: Adds code to accommodate uncertainties of ctt and cth.
! 2013/01/23, CP: Changed illum from byte to int
! 2013/10/02, CP/GT: Added allocation statement for DOFS
! 2014/01/01, GM: Fixed the range in NY for initializations. Plus,
!    no need for explicit indexing in these cases anyway.
! 2014/05/27, GM: Some cleanup.
! 2014/10/24, OS: added allocation of cldtype, cldmask, cccot_pre,
!    lusflag, dem, nisemask
! 2014/12/01, CP: Added in cloud albedo
! 2015/07/01, CP: Added corrected cth
! 2015/09/06, GM: Move into common/ from src/ and changes related to sharing
!    with post_processing/.
! 2015/09/07, GM: Add cldmask_uncertainty.
! 2015/10/22, GM: Add cloud albedo uncertainty.
! 2015/12/30, AP: Move declarations of scale/offset/vmin/vmax here from def_
!    routines for fields that could be BTs or reflectances. Have all albedo
!    fields use the same values.
! 2016/01/06, AP: Wrap do_* flags into output_flags structure.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! Name: alloc_output_data_primary
!
! Purpose:
! Allocate storage for primary output file.
!
! Description and Algorithm details:
! 1) Allocate all arrays, writing fill values to them.
!
! Arguments:
! Name        Type   In/Out/Both Description
! ------------------------------------------------------------------------------
! ixstart     int    In   First index of across-track (first) dimension
! ixstop      int    In   Last index of across-track (first) dimension
! iystart     int    In   First index of along-track (second) dimension
! iystop      int    In   Last index of along-track (second) dimension
! NViews      int    In   Number of viewing angles
! Ny          int    In   Total number of measurments
! output_data struct Both Structure of arrays to be allocated
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------
subroutine alloc_output_data_primary(ixstart, ixstop, iystart, iystop, &
     NViews, Ny, MaxIter, output_data, output_flags)

   implicit none

   integer,                   intent(in)    :: ixstart
   integer,                   intent(in)    :: ixstop
   integer,                   intent(in)    :: iystart
   integer,                   intent(in)    :: iystop
   integer,                   intent(in)    :: NViews
   integer,                   intent(in)    :: Ny
   integer,                   intent(in)    :: MaxIter
   type(output_data_primary), intent(inout) :: output_data
   type(output_data_flags),   intent(in)    :: output_flags


   allocate(output_data%vid_sol_zen(NViews))
   output_data%vid_sol_zen=0

   allocate(output_data%vid_sat_zen(NViews))
   output_data%vid_sat_zen=0

   allocate(output_data%vid_rel_azi(NViews))
   output_data%vid_rel_azi=0


   allocate(output_data%vid_cloud_albedo(Ny))
   output_data%vid_cloud_albedo=0
   allocate(output_data%vid_cloud_albedo_error(Ny))
   output_data%vid_cloud_albedo_error=0

   allocate(output_data%cot(ixstart:ixstop,iystart:iystop))
   output_data%cot=sint_fill_value
   allocate(output_data%cot_error(ixstart:ixstop,iystart:iystop))
   output_data%cot_error=sint_fill_value

   allocate(output_data%cer(ixstart:ixstop,iystart:iystop))
   output_data%cer=sint_fill_value
   allocate(output_data%cer_error(ixstart:ixstop,iystart:iystop))
   output_data%cer_error=sint_fill_value

   allocate(output_data%ctp(ixstart:ixstop,iystart:iystop))
   output_data%ctp=sint_fill_value
   allocate(output_data%ctp_error(ixstart:ixstop,iystart:iystop))
   output_data%ctp_error=sint_fill_value

   allocate(output_data%cct(ixstart:ixstop,iystart:iystop))
   output_data%cct=sint_fill_value
   allocate(output_data%cct_error(ixstart:ixstop,iystart:iystop))
   output_data%cct_error=sint_fill_value

   allocate(output_data%stemp(ixstart:ixstop,iystart:iystop))
   output_data%stemp=sint_fill_value
   allocate(output_data%stemp_error(ixstart:ixstop,iystart:iystop))
   output_data%stemp_error=sint_fill_value


   allocate(output_data%ctt(ixstart:ixstop,iystart:iystop))
   output_data%ctt=sint_fill_value
   allocate(output_data%ctt_error(ixstart:ixstop,iystart:iystop))
   output_data%ctt_error=sint_fill_value

   allocate(output_data%cth(ixstart:ixstop,iystart:iystop))
   output_data%cth=sint_fill_value
   allocate(output_data%cth_error(ixstart:ixstop,iystart:iystop))
   output_data%cth_error=sint_fill_value

   allocate(output_data%cth_corrected(ixstart:ixstop,iystart:iystop))
   output_data%cth_corrected=sint_fill_value
   allocate(output_data%cth_corrected_error(ixstart:ixstop,iystart:iystop))
   output_data%cth_corrected_error=sint_fill_value

   allocate(output_data%cwp(ixstart:ixstop,iystart:iystop))
   output_data%cwp=sint_fill_value
   allocate(output_data%cwp_error(ixstart:ixstop,iystart:iystop))
   output_data%cwp_error=sint_fill_value


   allocate(output_data%cloud_albedo(ixstart:ixstop,iystart:iystop,Ny))
   output_data%cloud_albedo=sint_fill_value
   allocate(output_data%cloud_albedo_error(ixstart:ixstop,iystart:iystop,Ny))
   output_data%cloud_albedo_error=sint_fill_value

   allocate(output_data%cccot_pre(ixstart:ixstop,iystart:iystop))
   output_data%cccot_pre=sint_fill_value


   allocate(output_data%time(ixstart:ixstop,iystart:iystop))
   output_data%time=sreal_fill_value


   allocate(output_data%lat(ixstart:ixstop,iystart:iystop))
   output_data%lat=sreal_fill_value

   allocate(output_data%lon(ixstart:ixstop,iystart:iystop))
   output_data%lon=sreal_fill_value


   allocate(output_data%sol_zen(ixstart:ixstop,iystart:iystop,NViews))
   output_data%sol_zen=sreal_fill_value

   allocate(output_data%sat_zen(ixstart:ixstop,iystart:iystop,NViews))
   output_data%sat_zen=sreal_fill_value

   allocate(output_data%rel_azi(ixstart:ixstop,iystart:iystop,NViews))
   output_data%rel_azi=sreal_fill_value


   allocate(output_data%convergence(ixstart:ixstop,iystart:iystop))
   output_data%convergence=byte_fill_value

   allocate(output_data%niter(ixstart:ixstop,iystart:iystop))
   output_data%niter=byte_fill_value

   allocate(output_data%costja(ixstart:ixstop,iystart:iystop))
   output_data%costja=sint_fill_value

   allocate(output_data%costjm(ixstart:ixstop,iystart:iystop))
   output_data%costjm=sint_fill_value

   allocate(output_data%qcflag(ixstart:ixstop,iystart:iystop))
   output_data%qcflag=sint_fill_value


   allocate(output_data%lsflag(ixstart:ixstop,iystart:iystop))
   output_data%lsflag=byte_fill_value

   allocate(output_data%lusflag(ixstart:ixstop,iystart:iystop))
   output_data%lusflag=byte_fill_value

   allocate(output_data%dem(ixstart:ixstop,iystart:iystop))
   output_data%dem=sint_fill_value

   allocate(output_data%nisemask(ixstart:ixstop,iystart:iystop))
   output_data%nisemask=byte_fill_value


   allocate(output_data%illum(ixstart:ixstop,iystart:iystop))
   output_data%illum=byte_fill_value


   allocate(output_data%cldtype(ixstart:ixstop,iystart:iystop))
   output_data%cldtype=byte_fill_value

   allocate(output_data%cldmask(ixstart:ixstop,iystart:iystop))
   output_data%cldmask=byte_fill_value
if (output_flags%do_cldmask_uncertainty) then
   allocate(output_data%cldmask_uncertainty(ixstart:ixstop,iystart:iystop))
   output_data%cldmask_uncertainty=sint_fill_value
end if

   allocate(output_data%phase(ixstart:ixstop,iystart:iystop))
   output_data%phase=byte_fill_value

if (output_flags%do_phase_pavolonis) then
   allocate(output_data%phase_pavolonis(ixstart:ixstop,iystart:iystop))
   output_data%phase_pavolonis=byte_fill_value
end if


   ! Set scale/offset/vmin/vmax dynamically as required
   output_data%niter_vmax=MaxIter

end subroutine alloc_output_data_primary


!-------------------------------------------------------------------------------
! Name: alloc_output_data_secondary
!
! Purpose:
! Allocate storage for primary output file.
!
! Description and Algorithm details:
! 1) Allocate all arrays, writing fill values to them.
!
! Arguments:
! Name        Type   In/Out/Both Description
! ------------------------------------------------------------------------------
! ixstart     int    In   First index of across-track (first) dimension
! ixstop      int    In   Last index of across-track (first) dimension
! iystart     int    In   First index of along-track (second) dimension
! iystop      int    In   Last index of along-track (second) dimension
! Ny          int    In   Total number of measurments
! Nx          int    In   Total number of retrieval parameters
! lcovar      logic  Both Switch to allocate covariance matricies
! output_data struct Both Structure of arrays to be allocated
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------
subroutine alloc_output_data_secondary(ixstart, ixstop, iystart, iystop, &
     NSolar, Ny, Nx, Ch_Is, ThermalBit, Xmax, Ymax, output_data, output_flags)

   implicit none

   integer,                     intent(in)    :: ixstart
   integer,                     intent(in)    :: ixstop
   integer,                     intent(in)    :: iystart
   integer,                     intent(in)    :: iystop
   integer,                     intent(in)    :: NSolar
   integer,                     intent(in)    :: Ny
   integer,                     intent(in)    :: Nx
   integer,                     intent(in)    :: Ch_Is(:)
   integer,                     intent(in)    :: ThermalBit
   integer,                     intent(in)    :: Xmax
   integer,                     intent(in)    :: Ymax
   type(output_data_secondary), intent(inout) :: output_data
   type(output_data_flags),     intent(in)    :: output_flags

   integer :: i



   allocate(output_data%cot_ap(ixstart:ixstop,iystart:iystop))
   output_data%cot_ap=sint_fill_value
   allocate(output_data%cot_fg(ixstart:ixstop,iystart:iystop))
   output_data%cot_fg=sint_fill_value

   allocate(output_data%cer_ap(ixstart:ixstop,iystart:iystop))
   output_data%cer_ap=sint_fill_value
   allocate(output_data%cer_fg(ixstart:ixstop,iystart:iystop))
   output_data%cer_fg=sint_fill_value

   allocate(output_data%ctp_ap(ixstart:ixstop,iystart:iystop))
   output_data%ctp_ap=sint_fill_value
   allocate(output_data%ctp_fg(ixstart:ixstop,iystart:iystop))
   output_data%ctp_fg=sint_fill_value

   allocate(output_data%stemp_ap(ixstart:ixstop,iystart:iystop))
   output_data%stemp_ap=sint_fill_value
   allocate(output_data%stemp_fg(ixstart:ixstop,iystart:iystop))
   output_data%stemp_fg=sint_fill_value

   allocate(output_data%vid_albedo(Ny))
   output_data%vid_albedo=0
   allocate(output_data%albedo(ixstart:ixstop,iystart:iystop,Ny))
   output_data%albedo=sint_fill_value

   allocate(output_data%vid_channels(Ny))
   output_data%vid_channels=0
   allocate(output_data%channels_scale(Ny))
   output_data%channels_scale=sreal_fill_value
   allocate(output_data%channels_offset(Ny))
   output_data%channels_offset=sreal_fill_value
   allocate(output_data%channels_vmin(Ny))
   output_data%channels_vmin=sint_fill_value
   allocate(output_data%channels_vmax(Ny))
   output_data%channels_vmax=sint_fill_value
   allocate(output_data%channels(ixstart:ixstop,iystart:iystop,Ny))
   output_data%channels=sint_fill_value

   allocate(output_data%vid_y0(Ny))
   output_data%vid_y0=0
   allocate(output_data%y0_scale(Ny))
   output_data%y0_scale=sreal_fill_value
   allocate(output_data%y0_offset(Ny))
   output_data%y0_offset=sreal_fill_value
   allocate(output_data%y0_vmin(Ny))
   output_data%y0_vmin=sint_fill_value
   allocate(output_data%y0_vmax(Ny))
   output_data%y0_vmax=sint_fill_value
   allocate(output_data%y0(ixstart:ixstop,iystart:iystop,Ny))
   output_data%y0=sint_fill_value

   allocate(output_data%vid_residuals(Ny))
   output_data%vid_residuals=0
   allocate(output_data%residuals_scale(Ny))
   output_data%residuals_scale=sreal_fill_value
   allocate(output_data%residuals_offset(Ny))
   output_data%residuals_offset=sreal_fill_value
   allocate(output_data%residuals_vmin(Ny))
   output_data%residuals_vmin=sint_fill_value
   allocate(output_data%residuals_vmax(Ny))
   output_data%residuals_vmax=sint_fill_value
   allocate(output_data%residuals(ixstart:ixstop,iystart:iystop,Ny))
   output_data%residuals=sint_fill_value


   allocate(output_data%scanline_u(ixstart:ixstop,iystart:iystop))
   output_data%scanline_u=lint_fill_value
   allocate(output_data%scanline_v(ixstart:ixstop,iystart:iystop))
   output_data%scanline_v=lint_fill_value


   allocate(output_data%ds(ixstart:ixstop,iystart:iystop))
   output_data%ds=sint_fill_value

if (output_flags%do_covariance) then
   allocate(output_data%vid_covariance(Nx,Nx))
   output_data%vid_covariance=0

   allocate(output_data%covariance(ixstart:ixstop,iystart:iystop,Nx,Nx))
   output_data%covariance=sreal_fill_value
end if


   ! Set scale/offset/vmin/vmax dynamically as required
   output_data%scanline_u_vmax=Xmax
   output_data%scanline_v_vmax=Ymax

   do i=1,Ny
      if (btest(Ch_Is(i), ThermalBit)) then
         output_data%channels_scale(i)=0.01
         output_data%channels_offset(i)=100.0
         output_data%channels_vmin(i)=0
         output_data%channels_vmax(i)=32000

         output_data%y0_scale(i)=0.01
         output_data%y0_offset(i)=100.0
         output_data%y0_vmin(i)=0
         output_data%y0_vmax(i)=32000

         output_data%residuals_scale(i)=0.01
         output_data%residuals_offset(i)=100.0
         output_data%residuals_vmin(i)=-32000
         output_data%residuals_vmax(i)=32000
      else
         output_data%channels_scale(i)=0.0001
         output_data%channels_offset(i)=0.0
         output_data%channels_vmin(i)=0
         output_data%channels_vmax(i)=10000

         output_data%y0_scale(i)=0.0001
         output_data%y0_offset(i)=0.0
         output_data%y0_vmin(i)=0
         output_data%y0_vmax(i)=10000

         output_data%residuals_scale(i)=0.0001
         output_data%residuals_offset(i)=0.0
         output_data%residuals_vmin(i)=-10000
         output_data%residuals_vmax(i)=10000
      end if
   end do

end subroutine alloc_output_data_secondary
