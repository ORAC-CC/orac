!-------------------------------------------------------------------------------
! Name: alloc_output_data_pp.F90
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
! History:
! 2011/12/19, Matthias Jerg: creates initial file.
! 2012/01/05, Caroline Poulsen: added in channel information
! 2012/01/06, Caroline Poulsen: added in cwp
! 2012/01/15, Caroline Poulsen: added in chan definitions
! 2012/03/06, Caroline Poulsen: Modified to create post processed netcdf files
!    with best phase selected!
! 2012/03/07, Martin Stengel: added missing stemp_ap
! 2012/07/06, MJ: extensively overhauls and restructures the code
! 2013/01/17, MJ: Adds code to accommodate uncertainties of ctt and cth
! 2014/10/24, OS: added variables cccot_pre, lusflag, cldtype, cloudmask, DEM
!  (currently deactivated), and nisemask
! 2014/11/20, OS: added allocation of pavolonis phase
! 2014/11/26, CP: added allocation of cloud_albedo
! 2015/02/05, OS: changed nint to lint
! 2015/02/07, CP: changed to common constants
! 2015/07/16, GM: Major cleanup.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine alloc_output_data_primary_pp(ixstart,ixstop,iystart,iystop,indexing, &
                                        output_data)

   use common_constants
   use input_routines
   use output_routines
   use postproc_constants

   implicit none

   integer(kind=lint),           intent(in)    :: ixstart, ixstop, &
                                                  iystart, iystop
   type(counts_and_indexes),     intent(in)    :: indexing
   type(output_data_primary_pp), intent(inout) :: output_data

   allocate(output_data%vid_sol_zen(indexing%NViews))
   output_data%vid_sol_zen=0

   allocate(output_data%vid_sat_zen(indexing%NViews))
   output_data%vid_sat_zen=0

   allocate(output_data%vid_rel_azi(indexing%NViews))
   output_data%vid_rel_azi=0

   allocate(output_data%vid_cloud_albedo(indexing%NSolar))
   output_data%vid_cloud_albedo=0

   allocate(output_data%time(ixstart:ixstop,iystart:iystop))
   output_data%time(ixstart:ixstop,iystart:iystop)=sreal_fill_value

   allocate(output_data%lat(ixstart:ixstop,iystart:iystop))
   output_data%lat(ixstart:ixstop,iystart:iystop)=sreal_fill_value
   allocate(output_data%lon(ixstart:ixstop,iystart:iystop))
   output_data%lon(ixstart:ixstop,iystart:iystop)=sreal_fill_value

   allocate(output_data%sol_zen(ixstart:ixstop,iystart:iystop))
   output_data%sol_zen(ixstart:ixstop,iystart:iystop)=sreal_fill_value
   allocate(output_data%sat_zen(ixstart:ixstop,iystart:iystop))
   output_data%sat_zen(ixstart:ixstop,iystart:iystop)=sreal_fill_value
   allocate(output_data%rel_azi(ixstart:ixstop,iystart:iystop))
   output_data%rel_azi(ixstart:ixstop,iystart:iystop)=sreal_fill_value

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

   allocate(output_data%cth_corrected(ixstart:ixstop,iystart:iystop))
   output_data%cth_corrected(ixstart:ixstop,iystart:iystop)=sint_fill_value
   allocate(output_data%cth_corrected_error(ixstart:ixstop,iystart:iystop))
   output_data%cth_corrected_error(ixstart:ixstop,iystart:iystop)=sint_fill_value

   allocate(output_data%cwp(ixstart:ixstop,iystart:iystop))
   output_data%cwp(ixstart:ixstop,iystart:iystop)=sint_fill_value
   allocate(output_data%cwp_error(ixstart:ixstop,iystart:iystop))
   output_data%cwp_error(ixstart:ixstop,iystart:iystop)=sint_fill_value

   allocate(output_data%cloud_albedo(ixstart:ixstop,iystart:iystop,indexing%NSolar))
   output_data%cloud_albedo(ixstart:ixstop,iystart:iystop,indexing%NSolar)=sint_fill_value

   allocate(output_data%convergence(ixstart:ixstop,iystart:iystop))
   output_data%convergence(ixstart:ixstop,iystart:iystop)=byte_fill_value

   allocate(output_data%niter(ixstart:ixstop,iystart:iystop))
   output_data%niter(ixstart:ixstop,iystart:iystop)=byte_fill_value

   allocate(output_data%phase(ixstart:ixstop,iystart:iystop))
   output_data%phase(ixstart:ixstop,iystart:iystop)=byte_fill_value

   allocate(output_data%phase_pavolonis(ixstart:ixstop,iystart:iystop))
   output_data%phase_pavolonis(ixstart:ixstop,iystart:iystop)=byte_fill_value

   allocate(output_data%costja(ixstart:ixstop,iystart:iystop))
   output_data%costja(ixstart:ixstop,iystart:iystop)=sreal_fill_value

   allocate(output_data%costjm(ixstart:ixstop,iystart:iystop))
   output_data%costjm(ixstart:ixstop,iystart:iystop)=sreal_fill_value

   allocate(output_data%lsflag(ixstart:ixstop,iystart:iystop))
   output_data%lsflag(ixstart:ixstop,iystart:iystop)=byte_fill_value

   allocate(output_data%qcflag(ixstart:ixstop,iystart:iystop))
   output_data%qcflag(ixstart:ixstop,iystart:iystop)=sreal_fill_value

   allocate(output_data%illum(ixstart:ixstop,iystart:iystop))
   output_data%illum(ixstart:ixstop,iystart:iystop)=byte_fill_value

   allocate(output_data%cldtype(ixstart:ixstop,iystart:iystop))
   output_data%cldtype(ixstart:ixstop,iystart:iystop)=byte_fill_value

   allocate(output_data%cldmask(ixstart:ixstop,iystart:iystop))
   output_data%cldmask(ixstart:ixstop,iystart:iystop)=byte_fill_value

   allocate(output_data%cccot(ixstart:ixstop,iystart:iystop))
   output_data%cccot(ixstart:ixstop,iystart:iystop)=sint_fill_value
   allocate(output_data%cccot_pre(ixstart:ixstop,iystart:iystop))
   output_data%cccot_pre(ixstart:ixstop,iystart:iystop)=sint_fill_value

   allocate(output_data%lusflag(ixstart:ixstop,iystart:iystop))
   output_data%lusflag(ixstart:ixstop,iystart:iystop)=byte_fill_value

!  allocate(output_data%dem(ixstart:ixstop,iystart:iystop))
!  output_data%dem(ixstart:ixstop,iystart:iystop)=sint_fill_value

   allocate(output_data%nisemask(ixstart:ixstop,iystart:iystop))
   output_data%nisemask(ixstart:ixstop,iystart:iystop)=byte_fill_value

end subroutine alloc_output_data_primary_pp


subroutine alloc_output_data_secondary_pp(ixstart,ixstop,iystart,iystop,indexing, &
                                          output_data)

   use common_constants
   use input_routines
   use output_routines
   use postproc_constants

   implicit none

   integer(kind=lint),             intent(in)    :: ixstart, ixstop, &
                                                    iystart, iystop
   type(counts_and_indexes),       intent(in)    :: indexing
   type(output_data_secondary_pp), intent(inout) :: output_data

   logical :: lcovar = .false.


   allocate(output_data%vid_albedo(indexing%Ny))
   output_data%vid_albedo=0

   allocate(output_data%vid_channels(indexing%Ny))
   output_data%vid_channels=0

   allocate(output_data%vid_y0(indexing%Ny))
   output_data%vid_y0=0

   allocate(output_data%vid_residuals(indexing%Ny))
   output_data%vid_residuals=0


   allocate(output_data%albedo_scale(indexing%NSolar))
   output_data%albedo_scale=sreal_fill_value
   allocate(output_data%albedo_offset(indexing%NSolar))
   output_data%albedo_offset=sreal_fill_value
   allocate(output_data%albedo_vmin(indexing%NSolar))
   output_data%albedo_vmin=sint_fill_value
   allocate(output_data%albedo_vmax(indexing%NSolar))
   output_data%albedo_vmax=sint_fill_value

   allocate(output_data%channels_scale(indexing%Ny))
   output_data%channels_scale=sreal_fill_value
   allocate(output_data%channels_offset(indexing%Ny))
   output_data%channels_offset=sreal_fill_value
   allocate(output_data%channels_vmin(indexing%Ny))
   output_data%channels_vmin=sint_fill_value
   allocate(output_data%channels_vmax(indexing%Ny))
   output_data%channels_vmax=sint_fill_value

   allocate(output_data%y0_scale(indexing%Ny))
   output_data%y0_scale=sreal_fill_value
   allocate(output_data%y0_offset(indexing%Ny))
   output_data%y0_offset=sreal_fill_value
   allocate(output_data%y0_vmin(indexing%Ny))
   output_data%y0_vmin=sint_fill_value
   allocate(output_data%y0_vmax(indexing%Ny))
   output_data%y0_vmax=sint_fill_value

   allocate(output_data%residuals_scale(indexing%Ny))
   output_data%residuals_scale=sreal_fill_value
   allocate(output_data%residuals_offset(indexing%Ny))
   output_data%residuals_offset=sreal_fill_value
   allocate(output_data%residuals_vmin(indexing%Ny))
   output_data%residuals_vmin=sint_fill_value
   allocate(output_data%residuals_vmax(indexing%Ny))
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


   allocate(output_data%albedo(ixstart:ixstop,iystart:iystop,indexing%NSolar))
   output_data%albedo=sint_fill_value

   allocate(output_data%channels(ixstart:ixstop,iystart:iystop,indexing%Ny))
   output_data%channels=sint_fill_value

   allocate(output_data%y0(ixstart:ixstop,iystart:iystop,indexing%Ny))
   output_data%y0=sint_fill_value

   allocate(output_data%residuals(ixstart:ixstop,iystart:iystop,indexing%Ny))
   output_data%residuals=sint_fill_value

   allocate(output_data%ds(ixstart:ixstop,iystart:iystop))
   output_data%ds(ixstart:ixstop,iystart:iystop)=sint_fill_value

   if (lcovar) then
      allocate(output_data%vid_covariance(indexing%Nx,indexing%Nx))
      output_data%vid_covariance=0

      allocate(output_data%covariance(ixstart:ixstop,iystart:iystop,indexing%Nx,indexing%Nx))
      output_data%covariance=sreal_fill_value
   end if

end subroutine alloc_output_data_secondary_pp
