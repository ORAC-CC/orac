!-------------------------------------------------------------------------------
! Name: alloc_output_data.F90
!
! Purpose:
! The file contains a collection of three subroutines which allocate the array
! parts of the output variable types.
!
! Description and Algorithm details:
!
! Arguments:
! Name Type In/Out/Both Description
!
! Local variables:
! Name Type Description
!
! History:
! 2011/12/19, Matthias Jerg: creates initial file.
! 2012/01/05, Caroline Poulsen: added in channel information
! 2012/01/06, Caroline Poulsen: added in cwp
! 2012/01/15, Caroline Poulsen: added in chan definitions
! 2012/01/28, Caroline Poulsen: added in albedo
! 2012/07/08, Caroline Poulsen: fixed memory access error
! 2013/01/17, Matthias Jerg: Adds code to accommodate uncertainties of ctt and
!    cth.
! 2013/01/23, Caroline Poulsen: Changed illum from byte to int
! 2013/10/02, CP/GT: Added allocation statement for DOFS
! 2014/01/01, Greg McGarragh: Fixed the range in NY for initializations. Plus,
!    no need for explicit indexing in these cases anyway.
! 2014/05/27, Greg McGarragh: Some cleanup.
!
! $Id: alloc_output_data.F90 2311 2014-08-15 17:06:07Z gmcgarragh $
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine alloc_output_data_primary(ixstart,ixstop,iystart,iystop,NViews, &
                                     output_data)

   use ECP_Constants

   implicit none

   integer,                   intent(in)    :: ixstart
   integer,                   intent(in)    :: ixstop
   integer,                   intent(in)    :: iystart
   integer,                   intent(in)    :: iystop
   integer,                   intent(in)    :: NViews
   type(output_data_primary), intent(inout) :: output_data


   allocate(output_data%vid_sol_zen(NViews))
   output_data%vid_sol_zen=0

   allocate(output_data%vid_sat_zen(NViews))
   output_data%vid_sat_zen=0

   allocate(output_data%vid_rel_azi(NViews))
   output_data%vid_rel_azi=0


   allocate(output_data%time(ixstart:ixstop,iystart:iystop))
   output_data%time(ixstart:ixstop,iystart:iystop)=sreal_fill_value


   allocate(output_data%lon(ixstart:ixstop,iystart:iystop))
   output_data%lon(ixstart:ixstop,iystart:iystop)=sreal_fill_value

   allocate(output_data%lat(ixstart:ixstop,iystart:iystop))
   output_data%lat(ixstart:ixstop,iystart:iystop)=sreal_fill_value


   allocate(output_data%sol_zen(ixstart:ixstop,iystart:iystop,NViews))
   output_data%sol_zen(ixstart:ixstop,iystart:iystop,NViews)=sreal_fill_value

   allocate(output_data%sat_zen(ixstart:ixstop,iystart:iystop,NViews))
   output_data%sat_zen(ixstart:ixstop,iystart:iystop,NViews)=sreal_fill_value

   allocate(output_data%rel_azi(ixstart:ixstop,iystart:iystop,NViews))
   output_data%rel_azi(ixstart:ixstop,iystart:iystop,NViews)=sreal_fill_value


   allocate(output_data%cot(ixstart:ixstop,iystart:iystop))
   output_data%cot(ixstart:ixstop,iystart:iystop)=sint_fill_value
   allocate(output_data%cot_error(ixstart:ixstop,iystart:iystop))
   output_data%cot_error(ixstart:ixstop,iystart:iystop)=sint_fill_value

   allocate(output_data%ref(ixstart:ixstop,iystart:iystop))
   output_data%ref(ixstart:ixstop,iystart:iystop)=sint_fill_value
   allocate(output_data%ref_error(ixstart:ixstop,iystart:iystop))
   output_data%ref_error(ixstart:ixstop,iystart:iystop)=sint_fill_value

   allocate(output_data%ctp(ixstart:ixstop,iystart:iystop))
   output_data%ctp(ixstart:ixstop,iystart:iystop)=sint_fill_value
   allocate(output_data%ctp_error(ixstart:ixstop,iystart:iystop))
   output_data%ctp_error(ixstart:ixstop,iystart:iystop)=sint_fill_value

   allocate(output_data%cct(ixstart:ixstop,iystart:iystop))
   output_data%cct(ixstart:ixstop,iystart:iystop)=sint_fill_value
   allocate(output_data%cct_error(ixstart:ixstop,iystart:iystop))
   output_data%cct_error(ixstart:ixstop,iystart:iystop)=sint_fill_value

   allocate(output_data%stemp(ixstart:ixstop,iystart:iystop))
   output_data%stemp(ixstart:ixstop,iystart:iystop)=sint_fill_value
   allocate(output_data%stemp_error(ixstart:ixstop,iystart:iystop))
   output_data%stemp_error(ixstart:ixstop,iystart:iystop)=sint_fill_value


   allocate(output_data%ctt(ixstart:ixstop,iystart:iystop))
   output_data%ctt(ixstart:ixstop,iystart:iystop)=sint_fill_value
   allocate(output_data%ctt_error(ixstart:ixstop,iystart:iystop))
   output_data%ctt_error(ixstart:ixstop,iystart:iystop)=sint_fill_value

   allocate(output_data%cth(ixstart:ixstop,iystart:iystop))
   output_data%cth(ixstart:ixstop,iystart:iystop)=sint_fill_value
   allocate(output_data%cth_error(ixstart:ixstop,iystart:iystop))
   output_data%cth_error(ixstart:ixstop,iystart:iystop)=sint_fill_value

   allocate(output_data%cwp(ixstart:ixstop,iystart:iystop))
   output_data%cwp(ixstart:ixstop,iystart:iystop)=sint_fill_value
   allocate(output_data%cwp_error(ixstart:ixstop,iystart:iystop))
   output_data%cwp_error(ixstart:ixstop,iystart:iystop)=sint_fill_value


   allocate(output_data%convergence(ixstart:ixstop,iystart:iystop))
   output_data%convergence(ixstart:ixstop,iystart:iystop)=byte_fill_value

   allocate(output_data%niter(ixstart:ixstop,iystart:iystop))
   output_data%niter(ixstart:ixstop,iystart:iystop)=byte_fill_value

   allocate(output_data%phase(ixstart:ixstop,iystart:iystop))
   output_data%phase(ixstart:ixstop,iystart:iystop)=byte_fill_value

   allocate(output_data%costja(ixstart:ixstop,iystart:iystop))
   output_data%costja(ixstart:ixstop,iystart:iystop)=sint_fill_value

   allocate(output_data%costjm(ixstart:ixstop,iystart:iystop))
   output_data%costjm(ixstart:ixstop,iystart:iystop)=sint_fill_value

   allocate(output_data%lsflag(ixstart:ixstop,iystart:iystop))
   output_data%lsflag(ixstart:ixstop,iystart:iystop)=byte_fill_value

   allocate(output_data%qcflag(ixstart:ixstop,iystart:iystop))
   output_data%qcflag(ixstart:ixstop,iystart:iystop)=sint_fill_value

   allocate(output_data%illum(ixstart:ixstop,iystart:iystop))
   output_data%illum(ixstart:ixstop,iystart:iystop)=byte_fill_value

end subroutine alloc_output_data_primary


subroutine alloc_output_data_secondary(ixstart,ixstop,iystart,iystop,Ny,Nx, &
                                       lcovar,output_data)

   use ECP_Constants

   implicit none

   integer,                     intent(in)    :: ixstart
   integer,                     intent(in)    :: ixstop
   integer,                     intent(in)    :: iystart
   integer,                     intent(in)    :: iystop
   integer,                     intent(in)    :: Ny
   integer,                     intent(in)    :: Nx
   logical,                     intent(inout) :: lcovar
   type(output_data_secondary), intent(inout) :: output_data


   allocate(output_data%vid_albedo(Ny))
   output_data%vid_albedo=0

   allocate(output_data%vid_channels(Ny))
   output_data%vid_channels=0

   allocate(output_data%vid_y0(Ny))
   output_data%vid_y0=0

   allocate(output_data%vid_residuals(Ny))
   output_data%vid_residuals=0


   allocate(output_data%albedo_scale(Ny))
   output_data%albedo_scale=sreal_fill_value
   allocate(output_data%albedo_offset(Ny))
   output_data%albedo_offset=sreal_fill_value
   allocate(output_data%albedo_vmin(Ny))
   output_data%albedo_vmin=sint_fill_value
   allocate(output_data%albedo_vmax(Ny))
   output_data%albedo_vmax=sint_fill_value

   allocate(output_data%channels_scale(Ny))
   output_data%channels_scale=sreal_fill_value
   allocate(output_data%channels_offset(Ny))
   output_data%channels_offset=sreal_fill_value
   allocate(output_data%channels_vmin(Ny))
   output_data%channels_vmin=sint_fill_value
   allocate(output_data%channels_vmax(Ny))
   output_data%channels_vmax=sint_fill_value

   allocate(output_data%y0_scale(Ny))
   output_data%y0_scale=sreal_fill_value
   allocate(output_data%y0_offset(Ny))
   output_data%y0_offset=sreal_fill_value
   allocate(output_data%y0_vmin(Ny))
   output_data%y0_vmin=sint_fill_value
   allocate(output_data%y0_vmax(Ny))
   output_data%y0_vmax=sint_fill_value

   allocate(output_data%residuals_scale(Ny))
   output_data%residuals_scale=sreal_fill_value
   allocate(output_data%residuals_offset(Ny))
   output_data%residuals_offset=sreal_fill_value
   allocate(output_data%residuals_vmin(Ny))
   output_data%residuals_vmin=sint_fill_value
   allocate(output_data%residuals_vmax(Ny))
   output_data%residuals_vmax=sint_fill_value


   allocate(output_data%scanline_u(ixstart:ixstop,iystart:iystop))
   output_data%scanline_u(ixstart:ixstop,iystart:iystop)=lint_fill_value

   allocate(output_data%scanline_v(ixstart:ixstop,iystart:iystop))
   output_data%scanline_v(ixstart:ixstop,iystart:iystop)=lint_fill_value


   allocate(output_data%cot_ap(ixstart:ixstop,iystart:iystop))
   output_data%cot_ap(ixstart:ixstop,iystart:iystop)=sint_fill_value
   allocate(output_data%cot_fg(ixstart:ixstop,iystart:iystop))
   output_data%cot_fg(ixstart:ixstop,iystart:iystop)=sint_fill_value

   allocate(output_data%ref_ap(ixstart:ixstop,iystart:iystop))
   output_data%ref_ap(ixstart:ixstop,iystart:iystop)=sint_fill_value
   allocate(output_data%ref_fg(ixstart:ixstop,iystart:iystop))
   output_data%ref_fg(ixstart:ixstop,iystart:iystop)=sint_fill_value

   allocate(output_data%ctp_ap(ixstart:ixstop,iystart:iystop))
   output_data%ctp_ap(ixstart:ixstop,iystart:iystop)=sint_fill_value
   allocate(output_data%ctp_fg(ixstart:ixstop,iystart:iystop))
   output_data%ctp_fg(ixstart:ixstop,iystart:iystop)=sint_fill_value

   allocate(output_data%stemp_ap(ixstart:ixstop,iystart:iystop))
   output_data%stemp_ap(ixstart:ixstop,iystart:iystop)=sint_fill_value
   allocate(output_data%stemp_fg(ixstart:ixstop,iystart:iystop))
   output_data%stemp_fg(ixstart:ixstop,iystart:iystop)=sint_fill_value


   allocate(output_data%albedo(ixstart:ixstop,iystart:iystop,Ny))
   output_data%albedo=sint_fill_value

   allocate(output_data%channels(ixstart:ixstop,iystart:iystop,Ny))
   output_data%channels=sint_fill_value

   allocate(output_data%y0(ixstart:ixstop,iystart:iystop,Ny))
   output_data%y0=sint_fill_value

   allocate(output_data%residuals(ixstart:ixstop,iystart:iystop,Ny))
   output_data%residuals=sint_fill_value

   allocate(output_data%ds(ixstart:ixstop,iystart:iystop))
   output_data%ds(ixstart:ixstop,iystart:iystop)=sint_fill_value

   if (lcovar) then
      allocate(output_data%vid_covariance(Nx,Nx))
      output_data%vid_covariance=0

      allocate(output_data%covariance(ixstart:ixstop,iystart:iystop,Nx,Nx))
      output_data%covariance=sreal_fill_value
   end if

end subroutine alloc_output_data_secondary
