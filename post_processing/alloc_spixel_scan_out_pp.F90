!-------------------------------------------------------------------------------
! Name: alloc_spixel_scan_out_pp.F90
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

subroutine alloc_spixel_scan_out_pp(ixstart,ixstop,iystart,iystop,indexing, &
                                    spixel_scan_out)

   use common_constants
   use scanline_structure
   use structures_pp
   use vartypes_pp

   implicit none

   integer(kind=lint),                   intent(in)    :: ixstart, ixstop, &
                                                          iystart, iystop
   type(counts_and_indexes),             intent(in)    :: indexing
   type(spixel_scanline_primary_output), intent(inout) :: spixel_scan_out

   allocate(spixel_scan_out%vid_sol_zen(indexing%NViews))
   spixel_scan_out%vid_sol_zen=0

   allocate(spixel_scan_out%vid_sat_zen(indexing%NViews))
   spixel_scan_out%vid_sat_zen=0

   allocate(spixel_scan_out%vid_rel_azi(indexing%NViews))
   spixel_scan_out%vid_rel_azi=0

   allocate(spixel_scan_out%vid_cloud_albedo(indexing%NSolar))
   spixel_scan_out%vid_cloud_albedo=0

   allocate(spixel_scan_out%time(ixstart:ixstop,iystart:iystop))
   spixel_scan_out%time(ixstart:ixstop,iystart:iystop)=sreal_fill_value

   allocate(spixel_scan_out%lat(ixstart:ixstop,iystart:iystop))
   spixel_scan_out%lat(ixstart:ixstop,iystart:iystop)=sreal_fill_value
   allocate(spixel_scan_out%lon(ixstart:ixstop,iystart:iystop))
   spixel_scan_out%lon(ixstart:ixstop,iystart:iystop)=sreal_fill_value

   allocate(spixel_scan_out%sol_zen(ixstart:ixstop,iystart:iystop))
   spixel_scan_out%sol_zen(ixstart:ixstop,iystart:iystop)=sreal_fill_value
   allocate(spixel_scan_out%sat_zen(ixstart:ixstop,iystart:iystop))
   spixel_scan_out%sat_zen(ixstart:ixstop,iystart:iystop)=sreal_fill_value
   allocate(spixel_scan_out%rel_azi(ixstart:ixstop,iystart:iystop))
   spixel_scan_out%rel_azi(ixstart:ixstop,iystart:iystop)=sreal_fill_value

   allocate(spixel_scan_out%cot(ixstart:ixstop,iystart:iystop))
   spixel_scan_out%cot(ixstart:ixstop,iystart:iystop)=sint_fill_value
   allocate(spixel_scan_out%cot_error(ixstart:ixstop,iystart:iystop))
   spixel_scan_out%cot_error(ixstart:ixstop,iystart:iystop)=sint_fill_value

   allocate(spixel_scan_out%ref(ixstart:ixstop,iystart:iystop))
   spixel_scan_out%ref(ixstart:ixstop,iystart:iystop)=sint_fill_value
   allocate(spixel_scan_out%ref_error(ixstart:ixstop,iystart:iystop))
   spixel_scan_out%ref_error(ixstart:ixstop,iystart:iystop)=sint_fill_value

   allocate(spixel_scan_out%ctp(ixstart:ixstop,iystart:iystop))
   spixel_scan_out%ctp(ixstart:ixstop,iystart:iystop)=sint_fill_value
   allocate(spixel_scan_out%ctp_error(ixstart:ixstop,iystart:iystop))
   spixel_scan_out%ctp_error(ixstart:ixstop,iystart:iystop)=sint_fill_value

   allocate(spixel_scan_out%cct(ixstart:ixstop,iystart:iystop))
   spixel_scan_out%cct(ixstart:ixstop,iystart:iystop)=sint_fill_value
   allocate(spixel_scan_out%cct_error(ixstart:ixstop,iystart:iystop))
   spixel_scan_out%cct_error(ixstart:ixstop,iystart:iystop)=sint_fill_value

   allocate(spixel_scan_out%stemp(ixstart:ixstop,iystart:iystop))
   spixel_scan_out%stemp(ixstart:ixstop,iystart:iystop)=sint_fill_value
   allocate(spixel_scan_out%stemp_error(ixstart:ixstop,iystart:iystop))
   spixel_scan_out%stemp_error(ixstart:ixstop,iystart:iystop)=sint_fill_value

   allocate(spixel_scan_out%ctt(ixstart:ixstop,iystart:iystop))
   spixel_scan_out%ctt(ixstart:ixstop,iystart:iystop)=sint_fill_value
   allocate(spixel_scan_out%ctt_error(ixstart:ixstop,iystart:iystop))
   spixel_scan_out%ctt_error(ixstart:ixstop,iystart:iystop)=sint_fill_value

   allocate(spixel_scan_out%cth(ixstart:ixstop,iystart:iystop))
   spixel_scan_out%cth(ixstart:ixstop,iystart:iystop)=sint_fill_value
   allocate(spixel_scan_out%cth_error(ixstart:ixstop,iystart:iystop))
   spixel_scan_out%cth_error(ixstart:ixstop,iystart:iystop)=sint_fill_value

   allocate(spixel_scan_out%cth_corrected(ixstart:ixstop,iystart:iystop))
   spixel_scan_out%cth_corrected(ixstart:ixstop,iystart:iystop)=sint_fill_value
   allocate(spixel_scan_out%cth_corrected_error(ixstart:ixstop,iystart:iystop))
   spixel_scan_out%cth_corrected_error(ixstart:ixstop,iystart:iystop)=sint_fill_value

   allocate(spixel_scan_out%cwp(ixstart:ixstop,iystart:iystop))
   spixel_scan_out%cwp(ixstart:ixstop,iystart:iystop)=sint_fill_value
   allocate(spixel_scan_out%cwp_error(ixstart:ixstop,iystart:iystop))
   spixel_scan_out%cwp_error(ixstart:ixstop,iystart:iystop)=sint_fill_value

   allocate(spixel_scan_out%cloud_albedo(ixstart:ixstop,iystart:iystop,indexing%NSolar))
   spixel_scan_out%cloud_albedo(ixstart:ixstop,iystart:iystop,indexing%NSolar)=sint_fill_value

   allocate(spixel_scan_out%convergence(ixstart:ixstop,iystart:iystop))
   spixel_scan_out%convergence(ixstart:ixstop,iystart:iystop)=byte_fill_value

   allocate(spixel_scan_out%niter(ixstart:ixstop,iystart:iystop))
   spixel_scan_out%niter(ixstart:ixstop,iystart:iystop)=byte_fill_value

   allocate(spixel_scan_out%phase(ixstart:ixstop,iystart:iystop))
   spixel_scan_out%phase(ixstart:ixstop,iystart:iystop)=byte_fill_value

   allocate(spixel_scan_out%phase_pavolonis(ixstart:ixstop,iystart:iystop))
   spixel_scan_out%phase_pavolonis(ixstart:ixstop,iystart:iystop)=byte_fill_value

   allocate(spixel_scan_out%costja(ixstart:ixstop,iystart:iystop))
   spixel_scan_out%costja(ixstart:ixstop,iystart:iystop)=sreal_fill_value

   allocate(spixel_scan_out%costjm(ixstart:ixstop,iystart:iystop))
   spixel_scan_out%costjm(ixstart:ixstop,iystart:iystop)=sreal_fill_value

   allocate(spixel_scan_out%lsflag(ixstart:ixstop,iystart:iystop))
   spixel_scan_out%lsflag(ixstart:ixstop,iystart:iystop)=byte_fill_value

   allocate(spixel_scan_out%qcflag(ixstart:ixstop,iystart:iystop))
   spixel_scan_out%qcflag(ixstart:ixstop,iystart:iystop)=sreal_fill_value

   allocate(spixel_scan_out%illum(ixstart:ixstop,iystart:iystop))
   spixel_scan_out%illum(ixstart:ixstop,iystart:iystop)=byte_fill_value

   allocate(spixel_scan_out%cldtype(ixstart:ixstop,iystart:iystop))
   spixel_scan_out%cldtype(ixstart:ixstop,iystart:iystop)=byte_fill_value

   allocate(spixel_scan_out%cldmask(ixstart:ixstop,iystart:iystop))
   spixel_scan_out%cldmask(ixstart:ixstop,iystart:iystop)=byte_fill_value

   allocate(spixel_scan_out%cccot(ixstart:ixstop,iystart:iystop))
   spixel_scan_out%cccot(ixstart:ixstop,iystart:iystop)=sint_fill_value
   allocate(spixel_scan_out%cccot_pre(ixstart:ixstop,iystart:iystop))
   spixel_scan_out%cccot_pre(ixstart:ixstop,iystart:iystop)=sint_fill_value

   allocate(spixel_scan_out%lusflag(ixstart:ixstop,iystart:iystop))
   spixel_scan_out%lusflag(ixstart:ixstop,iystart:iystop)=byte_fill_value

!  allocate(spixel_scan_out%dem(ixstart:ixstop,iystart:iystop))
!  spixel_scan_out%dem(ixstart:ixstop,iystart:iystop)=sint_fill_value

   allocate(spixel_scan_out%nisemask(ixstart:ixstop,iystart:iystop))
   spixel_scan_out%nisemask(ixstart:ixstop,iystart:iystop)=byte_fill_value

end subroutine alloc_spixel_scan_out_pp


subroutine alloc_spixel_scan_out_sec_pp(ixstart,ixstop,iystart,iystop,indexing, &
                                        spixel_scan_out)

   use common_constants
   use scanline_structure
   use structures_pp
   use vartypes_pp

   implicit none

   integer(kind=lint),                     intent(in)    :: ixstart, ixstop, &
                                                            iystart, iystop
   type(counts_and_indexes),               intent(in)    :: indexing
   type(spixel_scanline_secondary_output), intent(inout) :: spixel_scan_out

   logical :: lcovar = .false.


   allocate(spixel_scan_out%vid_albedo(indexing%Ny))
   spixel_scan_out%vid_albedo=0

   allocate(spixel_scan_out%vid_channels(indexing%Ny))
   spixel_scan_out%vid_channels=0

   allocate(spixel_scan_out%vid_y0(indexing%Ny))
   spixel_scan_out%vid_y0=0

   allocate(spixel_scan_out%vid_residuals(indexing%Ny))
   spixel_scan_out%vid_residuals=0


   allocate(spixel_scan_out%albedo_scale(indexing%NSolar))
   spixel_scan_out%albedo_scale=sreal_fill_value
   allocate(spixel_scan_out%albedo_offset(indexing%NSolar))
   spixel_scan_out%albedo_offset=sreal_fill_value
   allocate(spixel_scan_out%albedo_vmin(indexing%NSolar))
   spixel_scan_out%albedo_vmin=sint_fill_value
   allocate(spixel_scan_out%albedo_vmax(indexing%NSolar))
   spixel_scan_out%albedo_vmax=sint_fill_value

   allocate(spixel_scan_out%channels_scale(indexing%Ny))
   spixel_scan_out%channels_scale=sreal_fill_value
   allocate(spixel_scan_out%channels_offset(indexing%Ny))
   spixel_scan_out%channels_offset=sreal_fill_value
   allocate(spixel_scan_out%channels_vmin(indexing%Ny))
   spixel_scan_out%channels_vmin=sint_fill_value
   allocate(spixel_scan_out%channels_vmax(indexing%Ny))
   spixel_scan_out%channels_vmax=sint_fill_value

   allocate(spixel_scan_out%y0_scale(indexing%Ny))
   spixel_scan_out%y0_scale=sreal_fill_value
   allocate(spixel_scan_out%y0_offset(indexing%Ny))
   spixel_scan_out%y0_offset=sreal_fill_value
   allocate(spixel_scan_out%y0_vmin(indexing%Ny))
   spixel_scan_out%y0_vmin=sint_fill_value
   allocate(spixel_scan_out%y0_vmax(indexing%Ny))
   spixel_scan_out%y0_vmax=sint_fill_value

   allocate(spixel_scan_out%residuals_scale(indexing%Ny))
   spixel_scan_out%residuals_scale=sreal_fill_value
   allocate(spixel_scan_out%residuals_offset(indexing%Ny))
   spixel_scan_out%residuals_offset=sreal_fill_value
   allocate(spixel_scan_out%residuals_vmin(indexing%Ny))
   spixel_scan_out%residuals_vmin=sint_fill_value
   allocate(spixel_scan_out%residuals_vmax(indexing%Ny))
   spixel_scan_out%residuals_vmax=sint_fill_value


   allocate(spixel_scan_out%scanline_u(ixstart:ixstop,iystart:iystop))
   spixel_scan_out%scanline_u(ixstart:ixstop,iystart:iystop)=lint_fill_value

   allocate(spixel_scan_out%scanline_v(ixstart:ixstop,iystart:iystop))
   spixel_scan_out%scanline_v(ixstart:ixstop,iystart:iystop)=lint_fill_value


   allocate(spixel_scan_out%cot_ap(ixstart:ixstop,iystart:iystop))
   spixel_scan_out%cot_ap(ixstart:ixstop,iystart:iystop)=sint_fill_value
   allocate(spixel_scan_out%cot_fg(ixstart:ixstop,iystart:iystop))
   spixel_scan_out%cot_fg(ixstart:ixstop,iystart:iystop)=sint_fill_value

   allocate(spixel_scan_out%ref_ap(ixstart:ixstop,iystart:iystop))
   spixel_scan_out%ref_ap(ixstart:ixstop,iystart:iystop)=sint_fill_value
   allocate(spixel_scan_out%ref_fg(ixstart:ixstop,iystart:iystop))
   spixel_scan_out%ref_fg(ixstart:ixstop,iystart:iystop)=sint_fill_value

   allocate(spixel_scan_out%ctp_ap(ixstart:ixstop,iystart:iystop))
   spixel_scan_out%ctp_ap(ixstart:ixstop,iystart:iystop)=sint_fill_value
   allocate(spixel_scan_out%ctp_fg(ixstart:ixstop,iystart:iystop))
   spixel_scan_out%ctp_fg(ixstart:ixstop,iystart:iystop)=sint_fill_value

   allocate(spixel_scan_out%stemp_ap(ixstart:ixstop,iystart:iystop))
   spixel_scan_out%stemp_ap(ixstart:ixstop,iystart:iystop)=sint_fill_value
   allocate(spixel_scan_out%stemp_fg(ixstart:ixstop,iystart:iystop))
   spixel_scan_out%stemp_fg(ixstart:ixstop,iystart:iystop)=sint_fill_value


   allocate(spixel_scan_out%albedo(ixstart:ixstop,iystart:iystop,indexing%NSolar))
   spixel_scan_out%albedo=sint_fill_value

   allocate(spixel_scan_out%channels(ixstart:ixstop,iystart:iystop,indexing%Ny))
   spixel_scan_out%channels=sint_fill_value

   allocate(spixel_scan_out%y0(ixstart:ixstop,iystart:iystop,indexing%Ny))
   spixel_scan_out%y0=sint_fill_value

   allocate(spixel_scan_out%residuals(ixstart:ixstop,iystart:iystop,indexing%Ny))
   spixel_scan_out%residuals=sint_fill_value

   allocate(spixel_scan_out%ds(ixstart:ixstop,iystart:iystop))
   spixel_scan_out%ds(ixstart:ixstop,iystart:iystop)=sint_fill_value

   if (lcovar) then
      allocate(spixel_scan_out%vid_covariance(indexing%Nx,indexing%Nx))
      spixel_scan_out%vid_covariance=0

      allocate(spixel_scan_out%covariance(ixstart:ixstop,iystart:iystop,indexing%Nx,indexing%Nx))
      spixel_scan_out%covariance=sreal_fill_value
   end if

end subroutine alloc_spixel_scan_out_sec_pp
